(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Real-process coverage for the one interactive path every hermetic suite
   misses: a model-requested subprocess tool driven through the permission
   prompt under the DEFAULT sandbox (macOS seatbelt), rather than the
   [danger-full-access] the other pty and blackbox suites force. That path
   spawns a sandboxed child on the same Eio domain as the render loop; a
   render-loop regression makes the completion frame never arrive, which shows
   here as a timed-out [Pty.wait].

   Opt-in via [MENTAT_TUI_LIVE_TEST] (mirroring the Dune-RPC build-health live
   test's [MENTAT_DUNE_RPC_LIVE_TEST] gate): the default sandbox needs the host
   sandbox binary, and until the matrix-eio wake fix ships in the pinned mosaic
   the path can flake, so CI stays hermetic while the gap is covered on demand
   with [MENTAT_TUI_LIVE_TEST=1 dune test test/tui/pty]. *)

open Windtrap
module Pty = Pty_session
module Project = Tui_harness.Project
module Screen = Tui_harness.Screen

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

let create_provider_process ~executable ~arguments ~log =
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
          Unix.create_process_env executable arguments (Unix.environment ())
            null log_fd log_fd))

(* A two-response script: the first turn asks for a [shell] tool call, the
   second acknowledges its result and answers. The permission prompt sits
   between the two, driven interactively by the test. *)
let with_shell_tool_provider project ~prompt ~command ~answer f =
  let script = Project.scratch project "provider/script.jsonl" in
  let capture = Project.scratch project "provider/capture" in
  let port_file = Project.scratch project "provider/port" in
  let log = Project.scratch project "provider/log" in
  Project.write_path script
    (Printf.sprintf
       {|{"expect":{"body_contains":[%S,"\"name\":\"shell\""]},"response":{"id":"tool-call","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"item-shell","call_id":"call-shell","name":"shell","arguments":%S}]}}
{"expect":{"body_contains":["function_call_output","call-shell"]},"response":{"id":"tool-done","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":%S}]}]}}
|}
       prompt
       (Printf.sprintf {|{"command":%S}|} command)
       answer);
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
  let pid = create_provider_process ~executable ~arguments ~log in
  Fun.protect
    ~finally:(fun () -> stop_process pid)
    (fun () ->
      Project.wait_for_file port_file;
      let port = Project.read_path port_file |> String.trim in
      f ("http://127.0.0.1:" ^ port ^ "/v1"))

let home_ready screen = Screen.contains screen "message mentat"

let test_shell_tool_through_permission () =
  match Sys.getenv_opt "MENTAT_TUI_LIVE_TEST" with
  | None ->
      skip
        ~reason:
          "set MENTAT_TUI_LIVE_TEST=1 to drive the live TUI shell tool through \
           the permission prompt under the default sandbox"
        ()
  | Some _ ->
      Project.with_temp "cli-tool-permission" @@ fun project ->
      with_shell_tool_provider project ~prompt:"run the shell tool"
        ~command:"pwd" ~answer:"SHELL TOOL FINISHED"
      @@ fun base_url ->
      Pty.run
        ~env:
          [
            ("OPENAI_API_KEY", "test-key"); ("MENTAT_OPENAI_BASE_URL", base_url);
          ]
          (* Unset the suite default so the real host default (macOS seatbelt)
             seals the sandbox — the freeze-prone subprocess-tool path. *)
        ~unset:[ "MENTAT_SANDBOX_MODE" ] ~ready:home_ready project
      @@ fun terminal ->
      Pty.send terminal "run the shell tool";
      Pty.wait terminal (Screen.has "run the shell tool");
      Pty.send terminal "\r";
      (* The model requests [shell]; the permission prompt must appear. *)
      Pty.wait terminal (fun screen ->
          Screen.contains screen "Yes, run once"
          || Screen.contains screen "Run this command");
      Pty.send terminal "1";
      Pty.send terminal "\r";
      (* The sandboxed child runs and its result feeds back; the final frame
         carries the answer. A render-loop regression strands the loop here and
         this [Pty.wait] times out. *)
      Pty.wait terminal (Screen.has "SHELL TOOL FINISHED");
      Pty.quit terminal

let () =
  run "mentat.tui.pty.tool_permission"
    [
      group "Tool through permission (live, default sandbox)"
        [
          test "model shell tool runs and settles after permission accept"
            ~timeout:45. test_shell_tool_through_permission;
        ];
    ]
