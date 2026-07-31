(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
module Terminal = Terminal_process

let contains text substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec loop index =
    index + substring_length <= text_length
    && (String.equal (String.sub text index substring_length) substring
       || loop (index + 1))
  in
  substring_length = 0 || loop 0

let substring_index_from text ~from substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec loop index =
    if index + substring_length > text_length then None
    else if String.equal (String.sub text index substring_length) substring then
      Some index
    else loop (index + 1)
  in
  loop from

let print_fact label value = Printf.printf "%s: %b\n" label value

let rec waitpid_blocking pid =
  match Unix.waitpid [] pid with
  | _, status -> status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_blocking pid

let rec await_process pid limit =
  match Unix.waitpid [ Unix.WNOHANG ] pid with
  | 0, _ when Mtime.Span.to_float_ns (Mtime_clock.elapsed ()) *. 1e-9 < limit ->
      Unix.sleepf 0.01;
      await_process pid limit
  | 0, _ -> false
  | _, _ -> true
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> await_process pid limit
  | exception Unix.Unix_error (Unix.ECHILD, _, _) -> true

let stop_process pid =
  let now () = Mtime.Span.to_float_ns (Mtime_clock.elapsed ()) *. 1e-9 in
  if not (await_process pid (now ())) then begin
    (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
    if not (await_process pid (now () +. 0.5)) then begin
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      ignore (waitpid_blocking pid : Unix.process_status)
    end
  end

let create_provider_process ~executable ~arguments ~environment ~log =
  let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close null)
    (fun () ->
      let log_fd =
        Unix.openfile log [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY ] 0o600
      in
      Fun.protect
        ~finally:(fun () -> Unix.close log_fd)
        (fun () ->
          Unix.create_process_env executable arguments environment null log_fd
            log_fd))

let with_null f =
  let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close null) (fun () -> f null)

let wait_for_success label pid =
  match waitpid_blocking pid with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED status ->
      failwith (Printf.sprintf "%s exited with status %d" label status)
  | Unix.WSIGNALED signal ->
      failwith (Printf.sprintf "%s was killed by signal %d" label signal)
  | Unix.WSTOPPED signal ->
      failwith (Printf.sprintf "%s stopped on signal %d" label signal)

let with_provider project f =
  let script = Project.scratch project "terminal/provider.jsonl" in
  let capture = Project.scratch project "terminal/capture" in
  let port_file = Project.scratch project "terminal/port" in
  let log = Project.scratch project "terminal/provider.log" in
  Project.write_path script
    {|{"expect":{"body_contains":["show the title"]},"delay_ms":1000,"response":{"id":"terminal-title","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"Title transition complete."}]}]}}
|};
  let executable = Project.resolve_env_path "MENTAT_FAKE_PROVIDER_BIN" in
  let arguments =
    [|
      executable;
      "--script";
      script;
      "--capture";
      capture;
      "--port-file";
      port_file;
      "--accept-timeout";
      "30";
    |]
  in
  let pid =
    create_provider_process ~executable ~arguments
      ~environment:(Project.env_array project)
      ~log
  in
  Fun.protect
    ~finally:(fun () -> stop_process pid)
    (fun () ->
      Project.wait_for_file port_file;
      let port = Project.read_path port_file |> String.trim in
      f ("http://127.0.0.1:" ^ port ^ "/v1"))

let seed_trust project environment =
  let executable = Project.resolve_env_path "MENTAT_BIN" in
  let arguments = [| executable; "trust"; Project.root project |] in
  let pid =
    with_null (fun null ->
        Unix.create_process_env executable arguments environment null null null)
  in
  wait_for_success "trust seed" pid

let terminal_environment ?openai_base_url ?(extra = []) project =
  (* A PTY transports terminal bytes but has no emulator to answer capability
     queries. Use Matrix's tested Ghostty profile so synchronized output is a
     deterministic part of this raw-byte host contract, without enabling the
     Kitty keyboard protocol. *)
  Project.env_array ?openai_base_url
    ~extra:(("TERM", "xterm-ghostty") :: extra)
    project

let with_mentat ?openai_base_url ?(extra = []) project f =
  let executable = Project.resolve_env_path "MENTAT_BIN" in
  let environment = terminal_environment ?openai_base_url ~extra project in
  seed_trust project environment;
  Terminal.with_spawn ~env:environment ~cwd:(Project.root project)
    ~prog:executable
    ~args:[ "--cwd"; Project.root project ]
    f

let idle_title project =
  "\027]0;✳ mentat — " ^ Filename.basename (Project.root project) ^ "\027\\"

let working_titles project =
  let leaf = "mentat — " ^ Filename.basename (Project.root project) in
  [ "\027]0;⠂ " ^ leaf ^ "\027\\"; "\027]0;⠐ " ^ leaf ^ "\027\\" ]

let has_any text candidates = List.exists (contains text) candidates

let observed_osc_titles output =
  let prefix = "\027]0;" in
  let rec terminator index =
    if index >= String.length output then None
    else if Char.equal output.[index] '\007' then Some (index, index + 1, "BEL")
    else if
      Char.equal output.[index] '\027'
      && index + 1 < String.length output
      && Char.equal output.[index + 1] '\\'
    then Some (index, index + 2, "ST")
    else terminator (index + 1)
  in
  let rec collect from titles =
    match substring_index_from output ~from prefix with
    | None -> List.rev titles
    | Some first -> (
        let payload_first = first + String.length prefix in
        match terminator payload_first with
        | None -> List.rev (("<unterminated>", "none") :: titles)
        | Some (payload_last, next, ending) ->
            let payload =
              String.sub output payload_first (payload_last - payload_first)
            in
            collect next ((payload, ending) :: titles))
  in
  collect 0 []

let render_osc_titles output =
  match observed_osc_titles output with
  | [] -> "<none>"
  | titles ->
      titles
      |> List.map (fun (payload, ending) ->
          Printf.sprintf "%S terminated by %s" payload ending)
      |> String.concat "; "

let synchronized_output_begin = "\027[?2026h"
let synchronized_output_end = "\027[?2026l"

let contains_synchronized_boot output =
  let rec search from =
    match substring_index_from output ~from synchronized_output_begin with
    | None -> false
    | Some first -> (
        let content_first = first + String.length synchronized_output_begin in
        match
          substring_index_from output ~from:content_first
            synchronized_output_end
        with
        | None -> false
        | Some last ->
            let frame =
              String.sub output content_first (last - content_first)
            in
            if contains frame "message mentat" then true
            else search (last + String.length synchronized_output_end))
  in
  search 0

let wait_until_booted terminal =
  Terminal.wait_raw terminal contains_synchronized_boot

let%expect_test "raw OSC title bytes follow the real idle and working states" =
  Project.with_temp "terminal-host-title" @@ fun project ->
  with_provider project @@ fun openai_base_url ->
  with_mentat ~openai_base_url project @@ fun terminal ->
  wait_until_booted terminal;
  let boot_output = Terminal.raw terminal in
  let idle_title_emitted = contains boot_output (idle_title project) in
  if not idle_title_emitted then
    failwith
      (Printf.sprintf "idle OSC title missing; observed OSC 0 titles: %s"
         (render_osc_titles boot_output));
  print_fact "initial boot frame synchronized"
    (contains_synchronized_boot boot_output);
  print_fact "idle title OSC emitted" idle_title_emitted;
  Terminal.send terminal "show the title";
  Terminal.send terminal "\r";
  (match
     Terminal.wait_raw terminal (fun output ->
         has_any output (working_titles project))
   with
  | () -> ()
  | exception Failure reason ->
      let output = Terminal.raw terminal in
      failwith
        (Printf.sprintf
           "working OSC title missing; observed OSC 0 titles: %s; submitted \
            text painted: %b; wait failure: %s"
           (render_osc_titles output)
           (contains output "show the title")
           reason));
  let output = Terminal.raw terminal in
  print_fact "working title OSC emitted"
    (has_any output (working_titles project));
  Terminal.quit terminal;
  [%expect
    {|
    initial boot frame synchronized: true
    idle title OSC emitted: true
    working title OSC emitted: true |}]

let%expect_test "a real PTY resize delivers SIGWINCH and causes fresh output" =
  Project.with_temp "terminal-host-resize" @@ fun project ->
  with_mentat project @@ fun terminal ->
  wait_until_booted terminal;
  Terminal.wait_quiet terminal;
  let before = String.length (Terminal.raw terminal) in
  Terminal.resize terminal ~rows:12 ~cols:40;
  let synchronized_repaint output =
    let length = String.length output in
    if length <= before then false
    else
      let delta = String.sub output before (length - before) in
      contains delta "\027[?2026h" && contains delta "\027[?2026l"
  in
  Terminal.wait_raw terminal synchronized_repaint;
  print_fact "SIGWINCH caused a synchronized repaint"
    (synchronized_repaint (Terminal.raw terminal));
  Terminal.quit terminal;
  [%expect {| SIGWINCH caused a synchronized repaint: true |}]

let%expect_test "exit restores the terminal's original termios mode" =
  Project.with_temp "terminal-host-restore" @@ fun project ->
  let executable = Project.resolve_env_path "MENTAT_BIN" in
  let environment = terminal_environment project in
  seed_trust project environment;
  let before = Project.scratch project "terminal/stty-before" in
  let after = Project.scratch project "terminal/stty-after" in
  Project.write_path before "";
  let script =
    {|
before=$(stty -g) || exit 70
printf '%s\n' "$before" > "$MENTAT_STTY_BEFORE"
"$MENTAT_UNDER_TEST" --cwd "$MENTAT_TEST_CWD"
status=$?
after=$(stty -g) || exit 71
printf '%s\n' "$after" > "$MENTAT_STTY_AFTER"
exit "$status"
|}
  in
  let environment =
    terminal_environment project
      ~extra:
        [
          ("MENTAT_STTY_BEFORE", before);
          ("MENTAT_STTY_AFTER", after);
          ("MENTAT_UNDER_TEST", executable);
          ("MENTAT_TEST_CWD", Project.root project);
        ]
  in
  Terminal.with_spawn ~env:environment ~cwd:(Project.root project)
    ~prog:"/bin/sh" ~args:[ "-c"; script ]
  @@ fun terminal ->
  wait_until_booted terminal;
  Terminal.quit terminal;
  let before_mode = Project.read_path before |> String.trim in
  let after_mode = Project.read_path after |> String.trim in
  print_fact "termios restored" (String.equal before_mode after_mode);
  [%expect {| termios restored: true |}]

let%expect_test "farewell bytes follow alternate-screen restoration" =
  Project.with_temp "terminal-host-goodbye" @@ fun project ->
  with_mentat ~extra:[ ("NO_COLOR", "1") ] project @@ fun terminal ->
  wait_until_booted terminal;
  Terminal.quit terminal;
  let output = Terminal.raw terminal in
  let alternate_off = "\027[?1049l" in
  (* The MENTAT lockup's second row, byte-for-byte from lib/tui/theme.ml —
     the farewell frame ends on the lockup, so this row marks it. *)
  let lockup = "█ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂" in
  let farewell_after_alt =
    match substring_index_from output ~from:0 alternate_off with
    | None -> false
    | Some restored ->
        Option.is_some
          (substring_index_from output
             ~from:(restored + String.length alternate_off)
             lockup)
  in
  print_fact "alternate screen exited" (contains output alternate_off);
  print_fact "farewell followed restoration" farewell_after_alt;
  [%expect
    {|
    alternate screen exited: true
    farewell followed restoration: true |}]

exception Cleanup_probe

let%expect_test
    "exceptional PTY cleanup terminates, escalates, reaps, and closes resources"
    =
  let cleanup = ref None in
  let callback_exception_preserved =
    match
      Terminal.with_spawn
        ~observe_forced_cleanup:(fun report -> cleanup := Some report)
        ~prog:"/bin/sh"
        ~args:
          [
            "-c";
            "trap '' TERM; printf 'stubborn child ready\\n'; while :; do read \
             -r line; done";
          ]
      @@ fun terminal ->
      Terminal.wait_raw terminal (fun output ->
          contains output "stubborn child ready");
      raise Cleanup_probe
    with
    | exception Cleanup_probe -> true
    | () -> false
  in
  let cleanup =
    match !cleanup with
    | Some cleanup -> cleanup
    | None -> failwith "forced cleanup observer did not receive a report"
  in
  print_fact "callback exception preserved" callback_exception_preserved;
  print_fact "SIGTERM sent" cleanup.Terminal.sigterm_sent;
  print_fact "SIGKILL sent after ignored SIGTERM" cleanup.Terminal.sigkill_sent;
  print_fact "child reaped" cleanup.Terminal.child_reaped;
  print_fact "PTY resources closed" cleanup.Terminal.resources_closed;
  [%expect
    {|
    callback exception preserved: true
    SIGTERM sent: true
    SIGKILL sent after ignored SIGTERM: true
    child reaped: true
    PTY resources closed: true |}]

