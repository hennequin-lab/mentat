(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary tests for the background [shell] triad (with background
   terminals): [shell] with background=true, [shell_output], and
   [shell_kill], all sharing one per-session [Registry] bound to the test
   switch. Assertions (not expect blocks) so nondeterministic pids and output
   timing never pin a golden; determinism comes from the tools' own read/kill
   seams — [shell_kill] settles the drains, and [shell_output] waits for output
   and reads incrementally until the process has exited and no new bytes
   remain. Every
   child is a stable system inode ([/bin/sh], [/bin/sleep]) so no fresh script
   inode pays the macOS syspolicyd tax. *)

open Windtrap
open Test_tools_support
module Json = Jsont.Json
module Shell = Mentat_tools.Shell
module Registry = Mentat_tools.Shell.Registry
module Tool = Mentat_tool
module Workspace = Mentat_workspace

type world = {
  registry : Registry.t;
  clock : Eio.Time.Mono.ty Eio.Std.r;
  shell : Tool.t;
  shell_output : Tool.t;
  shell_kill : Tool.t;
}

let abs = Lpath.Abs.of_string_exn

let with_world name fn =
  Eio_main.run @@ fun stdenv ->
  let stdenv = (stdenv :> Eio_unix.Stdenv.base) in
  let raw = Filename.temp_dir ("mentat-bg-" ^ name ^ "-") "" in
  Fun.protect ~finally:(fun () -> remove_tree raw) @@ fun () ->
  let base = Unix.realpath raw in
  let ws_dir = Filename.concat base "workspace" in
  let tmp_dir = Filename.concat base "tmp" in
  List.iter mkdir [ ws_dir; tmp_dir ];
  let logical = Workspace.single (Workspace.Root.of_dir (abs ws_dir)) in
  let saved = Sys.getenv_opt "TMPDIR" in
  Unix.putenv "TMPDIR" tmp_dir;
  Fun.protect ~finally:(fun () ->
      match saved with
      | Some v -> Unix.putenv "TMPDIR" v
      | None -> Unix.unsetenv "TMPDIR")
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let io = resolve_exn ~sw ~stdenv ~logical () in
  let clock = Eio.Stdenv.mono_clock stdenv in
  let registry = Registry.create ~sw in
  fn
    {
      registry;
      clock;
      shell = Shell.make ~registry io ~clock ~shell:"/bin/sh";
      shell_output = Shell.Shell_output.make registry;
      shell_kill = Shell.Shell_kill.make registry;
    }

(* Tool call plumbing. *)

let run ?(cancelled = fun () -> false) tool input =
  match Tool.Call.decode tool (model_call tool input) with
  | Ok call -> finished (Tool.Call.run call ~cancelled)
  | Error error ->
      failf "call decode failed: %s" (Tool.Call.Decode_error.message error)

let bg_input ?background ?timeout_ms ?escalate command =
  let add name value fields =
    match value with None -> fields | Some v -> fields @ [ (name, v) ]
  in
  [ ("command", Json.string command) ]
  |> add "background" (Option.map (fun v -> Json.bool v) background)
  |> add "timeout_ms" (Option.map (fun v -> Json.int v) timeout_ms)
  |> add "escalate" (Option.map (fun v -> Json.bool v) escalate)
  |> json_object

let handle_input ?wait_ms handle =
  json_object
    (("handle", Json.string handle)
    ::
    (match wait_ms with
    | None -> []
    | Some wait_ms -> [ ("wait_ms", Json.int wait_ms) ]))

(* The monotonic span a call took, in seconds — the only way to observe that a
   read waited rather than answering at once. *)
let elapsed ~clock fn =
  let start = Eio.Time.Mono.now clock in
  let value = fn () in
  let span = Mtime.span start (Eio.Time.Mono.now clock) in
  (value, Mtime.Span.to_float_ns span /. 1e9)

let json_of result = Tool.Output.json (output_exn result)

let json_str result name =
  match Option.bind (json_of result) (json_member name) with
  | Some (Jsont.String (value, _)) -> value
  | _ -> failf "output json has no string member %S" name

let json_int result name =
  match Option.bind (json_of result) (json_member name) with
  | Some (Jsont.Number (value, _)) -> int_of_float value
  | _ -> failf "output json has no numeric member %S" name

let completed_handle result =
  (match Tool.Result.status result with
  | Tool.Result.Completed -> ()
  | Tool.Result.Failed { kind; message; _ } ->
      failf "expected completed, got failed %s: %s" (failure_name kind) message
  | Tool.Result.Interrupted { reason; _ } ->
      failf "expected completed, got interrupted: %s" reason);
  json_str result "handle"

(* [shell_output] is incremental and destructive; accumulate its stdout and new
   byte count until the process has exited and a read yields nothing new. Each
   read waits, so the loop is bounded by the process rather than by a spin. The
   safety timeout only bounds a genuine hang. *)
let find_from ~sub s from =
  let sub_len = String.length sub and s_len = String.length s in
  let rec loop i =
    if i + sub_len > s_len then None
    else if String.equal (String.sub s i sub_len) sub then Some i
    else loop (i + 1)
  in
  loop from

let stdout_section text =
  match find_from ~sub:"stdout:\n" text 0 with
  | None -> ""
  | Some at -> (
      let start = at + String.length "stdout:\n" in
      match find_from ~sub:"\nstderr:\n" text start with
      | None -> String.sub text start (String.length text - start)
      | Some stderr_at -> String.sub text start (stderr_at - start))

let drain_output ~clock world handle =
  match
    Eio.Time.Timeout.run (Eio.Time.Timeout.seconds clock 10.0) (fun () ->
        let rec loop acc_out acc_bytes =
          let result = run world.shell_output (handle_input handle) in
          let status = json_str result "status" in
          let new_bytes = json_int result "new_bytes" in
          let acc_out =
            acc_out ^ stdout_section (Tool.Output.text (output_exn result))
          in
          let acc_bytes = acc_bytes + new_bytes in
          if (not (String.equal status "running")) && new_bytes = 0 then
            Ok (acc_out, acc_bytes, status)
          else begin
            Eio.Time.Mono.sleep clock 0.001;
            loop acc_out acc_bytes
          end
        in
        loop "" 0)
  with
  | Ok triple -> triple
  | Error `Timeout -> fail "shell_output never drained to a settled status"

(* Yield until [handle] has left the running set — the drains get to run, so a
   process blocked writing past the pipe buffer finishes and exits. Bounded by
   the safety timeout; synchronizes on the observable running set, not a clock. *)
let settle_exited ~clock world handle =
  let still_running () =
    List.exists
      (fun (view : Registry.View.t) ->
        String.equal view.Registry.View.handle handle)
      (Registry.running world.registry)
  in
  match
    Eio.Time.Timeout.run (Eio.Time.Timeout.seconds clock 10.0) (fun () ->
        let rec loop () =
          if still_running () then begin
            Eio.Time.Mono.sleep clock 0.001;
            loop ()
          end
          else Ok ()
        in
        loop ())
  with
  | Ok () -> ()
  | Error `Timeout -> fail "the background command never exited"

let%test "a background start returns a handle and completes at once" =
  with_world "start" @@ fun world ->
  let result =
    run world.shell (bg_input ~background:true "printf hi; sleep 30")
  in
  equal string ~msg:"the first background handle is bg_1" "bg_1"
    (completed_handle result);
  is_true ~msg:"the receipt carries a real pid" (json_int result "pid" > 0);
  is_true ~msg:"the receipt text names the handle and the poll tool"
    (String.includes ~affix:"bg_1" (Tool.Output.text (output_exn result))
    && String.includes ~affix:"shell_output"
         (Tool.Output.text (output_exn result)));
  match Registry.running world.registry with
  | [ view ] ->
      equal string ~msg:"the process is registered and running" "bg_1"
        view.Registry.View.handle
  | views -> failf "expected one running process, got %d" (List.length views)

let%test "timeout_ms is ignored for a background command" =
  with_world "timeout" @@ fun world ->
  (* A foreground [sleep 30] with timeout_ms=50 would settle Timed_out; a
     background one returns at once and keeps running — no turn-bounded
     timeout applies. *)
  let result =
    run world.shell (bg_input ~background:true ~timeout_ms:50 "sleep 30")
  in
  equal string ~msg:"the background start completes immediately" "bg_1"
    (completed_handle result);
  is_true ~msg:"the command is still running well past the ignored timeout"
    (List.length (Registry.running world.registry) = 1)

let%test "shell_output reads incremental output and reports the exit" =
  with_world "output" @@ fun world ->
  let handle =
    completed_handle
      (run world.shell (bg_input ~background:true "printf 'hello\\n'"))
  in
  let stdout, total_bytes, status =
    drain_output ~clock:world.clock world handle
  in
  equal string ~msg:"the accumulated output is exactly what the command wrote"
    "hello\n" stdout;
  equal int ~msg:"incremental reads partition the output — no byte seen twice" 6
    total_bytes;
  equal string ~msg:"a self-exited command reports its exit status" "exited"
    status

let%test "shell_output caps a large read to its tail" =
  with_world "output-cap" @@ fun world ->
  (* 300 000 bytes is well over both the ring (128 KiB) and the per-read cap
     (64 KiB): the command cannot finish writing until the drains cycle far more
     than one ring through, so once it has exited the ring is full and a single
     poll must return only the capped tail. *)
  let handle =
    completed_handle
      (run world.shell (bg_input ~background:true "printf '%*s' 300000 ''"))
  in
  settle_exited ~clock:world.clock world handle;
  let result = run world.shell_output (handle_input handle) in
  (match Tool.Result.status result with
  | Tool.Result.Completed -> ()
  | _ -> fail "the poll must complete");
  is_true ~msg:"a read past the cap is flagged truncated"
    (Tool.Output.truncated (output_exn result));
  is_true ~msg:"the read reports the older bytes it could not return"
    (json_int result "dropped" > 0);
  is_true ~msg:"the rendered output is bounded by the per-read cap"
    (String.length (stdout_section (Tool.Output.text (output_exn result)))
    <= Shell.Shell_output.max_read_bytes)

(* A read is a wait with a deadline, not a snapshot. The command says nothing
   for a second, so a read issued straight after the start can only return
   "late" by having waited for it — and it must come back well before its
   budget, because the wait ends on the write rather than on the clock. *)
let%test "shell_output waits for output and returns on the write" =
  with_world "output-wait" @@ fun world ->
  let handle =
    completed_handle
      (run world.shell (bg_input ~background:true "sleep 1; printf late"))
  in
  let result, seconds =
    elapsed ~clock:world.clock (fun () ->
        run world.shell_output (handle_input handle))
  in
  is_true ~msg:"the read waited for the command to write"
    (String.includes ~affix:"late"
       (stdout_section (Tool.Output.text (output_exn result))));
  is_true ~msg:"it did not answer before the command wrote" (seconds >= 0.9);
  is_true ~msg:"it returned on the write, not on its budget"
    (seconds < float_of_int Shell.Shell_output.default_wait_ms /. 1_000. *. 0.8)

(* A silent command costs the reader its whole budget, and the result says so:
   an empty read that looks free is what invites a read to be repeated in place
   of work. *)
let%test "an empty read spends its budget and names it" =
  with_world "output-budget" @@ fun world ->
  let handle =
    completed_handle (run world.shell (bg_input ~background:true "sleep 30"))
  in
  let result, seconds =
    elapsed ~clock:world.clock (fun () ->
        run world.shell_output (handle_input handle))
  in
  let budget = float_of_int Shell.Shell_output.min_wait_ms /. 1_000. in
  is_true ~msg:"the read waited its whole budget for a silent command"
    (seconds >= budget *. 0.9);
  is_true ~msg:"an empty read names the budget it spent"
    (String.includes
       ~affix:
         (Printf.sprintf "no new output in %dms" Shell.Shell_output.min_wait_ms)
       (Tool.Output.text (output_exn result)))

(* The budget is declared in the schema and enforced, never clamped: a read
   that asked for a deadline it would not get would reason about the wrong
   one. *)
let%test "a wait_ms outside the accepted range is refused" =
  with_world "output-range" @@ fun world ->
  let refused wait_ms =
    let input = handle_input ~wait_ms "bg_1" in
    match
      Tool.Call.decode world.shell_output (model_call world.shell_output input)
    with
    | Ok _ -> false
    | Error error ->
        String.includes ~affix:"wait_ms" (Tool.Call.Decode_error.message error)
  in
  is_true ~msg:"a budget below the floor is refused, not raised"
    (refused (Shell.Shell_output.min_wait_ms - 1));
  is_true ~msg:"a budget above the ceiling is refused, not lowered"
    (refused (Shell.Shell_output.max_wait_ms + 1));
  is_true ~msg:"the floor itself is accepted"
    (not (refused Shell.Shell_output.min_wait_ms));
  is_true ~msg:"the ceiling itself is accepted"
    (not (refused Shell.Shell_output.max_wait_ms))

(* A waiting read is not a wedged turn: the engine's cooperative stop ends the
   wait, and the read reports the interruption rather than the empty chunk it
   happened to be holding. *)
let%test "a stop during the wait interrupts the read" =
  with_world "output-stop" @@ fun world ->
  let handle =
    completed_handle (run world.shell (bg_input ~background:true "sleep 30"))
  in
  (* False on the entry check, true from the wait onward: the call starts and
     is stopped while waiting, which is the case the deadline must not own. *)
  let calls = ref 0 in
  let cancelled () =
    incr calls;
    !calls > 1
  in
  let result, seconds =
    elapsed ~clock:world.clock (fun () ->
        run ~cancelled world.shell_output (handle_input handle))
  in
  is_true ~msg:"the stop ends the wait well inside the budget" (seconds < 1.0);
  match Tool.Result.status result with
  | Tool.Result.Interrupted _ -> ()
  | _ -> fail "a stop during the wait must interrupt, not return an empty read"

let%test "shell_output on an unknown handle is not_found" =
  with_world "output-unknown" @@ fun world ->
  let result = run world.shell_output (handle_input "bg_99") in
  match Tool.Result.status result with
  | Tool.Result.Failed { kind = `Not_found; message; _ } ->
      is_true ~msg:"the failure names the missing handle"
        (String.includes ~affix:"bg_99" message)
  | _ -> fail "an unknown handle must fail Not_found"

let%test "shell_kill terminates a running command and is idempotent" =
  with_world "kill" @@ fun world ->
  let handle =
    completed_handle (run world.shell (bg_input ~background:true "sleep 30"))
  in
  let first = run world.shell_kill (handle_input handle) in
  equal string ~msg:"the kill completes" handle (completed_handle first);
  equal string ~msg:"our kill settles Terminated" "terminated"
    (json_str first "status");
  is_true ~msg:"the killed command is no longer running"
    (Registry.running world.registry = []);
  let again = run world.shell_kill (handle_input handle) in
  equal string ~msg:"a second kill is idempotent, keeping the status" handle
    (completed_handle again);
  equal string ~msg:"idempotent kill still reports terminated" "terminated"
    (json_str again "status")

let%test "shell_kill on an unknown handle is not_found" =
  with_world "kill-unknown" @@ fun world ->
  match Tool.Result.status (run world.shell_kill (handle_input "bg_5")) with
  | Tool.Result.Failed { kind = `Not_found; _ } -> ()
  | _ -> fail "an unknown handle must fail Not_found"

let%test "a large final tail is capped and reported truncated" =
  with_world "kill-cap" @@ fun world ->
  (* The command emits 100 000 bytes, well over the per-read cap. Settling lets
     the drains keep the pipe flowing so the write completes and the process
     exits; [shell_kill] then awaits the drain to EOF and returns only the
     capped tail, flagged truncated. *)
  let handle =
    completed_handle
      (run world.shell (bg_input ~background:true "printf '%*s' 100000 ''"))
  in
  settle_exited ~clock:world.clock world handle;
  let result = run world.shell_kill (handle_input handle) in
  equal string ~msg:"the kill completes" handle (completed_handle result);
  is_true ~msg:"an over-cap tail is flagged truncated"
    (Tool.Output.truncated (output_exn result));
  is_true ~msg:"the text notes the rolled-off bytes"
    (String.includes ~affix:"rolled off" (Tool.Output.text (output_exn result)))

let%test "background starts are capped at max_concurrent" =
  with_world "cap" @@ fun world ->
  for index = 1 to Registry.max_concurrent do
    let expected = "bg_" ^ string_of_int index in
    equal string
      ~msg:(Printf.sprintf "start %d mints %s" index expected)
      expected
      (completed_handle
         (run world.shell (bg_input ~background:true "sleep 30")))
  done;
  let over = run world.shell (bg_input ~background:true "sleep 30") in
  match Tool.Result.status over with
  | Tool.Result.Failed { kind = `Unavailable; message; _ } ->
      is_true ~msg:"the refusal names the running handles that hold the slots"
        (String.includes ~affix:"bg_1" message
        && String.includes ~affix:"bg_8" message)
  | _ -> fail "a start past the cap must fail Unavailable"

let%test "a background start requests the ordinary shell permission" =
  with_world "permission" @@ fun world ->
  let requests_of input =
    match Tool.Call.decode world.shell (model_call world.shell input) with
    | Ok call -> Tool.Call.permissions call
    | Error error ->
        failf "decode failed: %s" (Tool.Call.Decode_error.message error)
  in
  let ordinary = requests_of (bg_input ~background:true "dune build") in
  equal int ~msg:"a plain background start requests exactly one shell fact" 1
    (List.length ordinary)

(* A background command cannot be escalated in v1: the request is refused before
   launch with a steering message, and it observes no path so requests nothing. *)
let%test "a background command cannot be escalated in v1" =
  with_world "escalate" @@ fun world ->
  let input = bg_input ~background:true ~escalate:true "dune build" in
  (match Tool.Call.decode world.shell (model_call world.shell input) with
  | Ok call ->
      equal int
        ~msg:"a refused escalation observes no path and requests nothing" 0
        (List.length (Tool.Call.permissions call))
  | Error error ->
      failf "decode failed: %s" (Tool.Call.Decode_error.message error));
  match Tool.Result.status (run world.shell input) with
  | Tool.Result.Failed { kind = `Invalid_input; message; _ } ->
      is_true ~msg:"the refusal steers toward foreground escalation"
        (String.includes ~affix:"foreground" message
        && String.includes ~affix:"escalate" message)
  | _ -> fail "a background escalation request must fail with a steer"

