(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for [shell]. Every invocation
   is a real child launched by [Mentat_workspace_io.Command] after provider
   JSON crosses [Mentat_tool.Call.decode]. Tests inspect only cached public
   permissions and erased [Result]/[Output] values, including their durable
   codecs; no typed Shell input/output or private process seam is exposed.

   Coverage ledger against the legacy expect suite:
   - retained and strengthened: strict input, exact cwd, configured shell and
     stripped environment, coarse command permission, all confinement routes,
     escalation, exit/signal/timeout/stop, head-tail capture, workdir refusal,
     erased output and cancellation;
   - newly pinned: safe-integer decoding, the fixed 60s/600s product bounds,
     duplicate-member rejection, permission caching, null stdin, exact
     both-stream 64KiB boundaries, literal elision-marker completeness, invalid
     UTF-8 repair, pre-spawn failures, surviving-descendant pipe handling,
     durable replay, and Claim_scope's D2 non-authority invariant;
   - intentionally delegated to workspace-IO's real-process suite: injected
     stdin-source failures and parent-fiber cancellation. Shell always supplies
     null stdin and receives parent cancellation from its caller, so neither is
     independently inducible through the provider surface without a fake. *)

open Windtrap
open Test_tools_support
module Access = Mentat_permission.Access
module Json = Jsont.Json
module Permission = Mentat_permission
module Sandbox = Mentat_sandbox
module Shell = Mentat_tools.Shell
module Tool = Mentat_tool
module Wio = Mentat_workspace_io
module Workspace = Mentat_workspace

type world = { ws_dir : string; out_dir : string; io : Wio.t; tool : Tool.t }

let input ?workdir ?timeout_ms ?description ?escalate command =
  let add name make value fields =
    match value with
    | None -> fields
    | Some value -> fields @ [ (name, make value) ]
  in
  [ ("command", Json.string command) ]
  |> add "workdir" (fun value -> Json.string value) workdir
  |> add "timeout_ms" (fun value -> Json.int value) timeout_ms
  |> add "description" (fun value -> Json.string value) description
  |> add "escalate" (fun value -> Json.bool value) escalate
  |> json_object

let decode_call_result tool provider_input =
  Tool.Call.decode tool (model_call tool provider_input)

let decode_call tool provider_input =
  match decode_call_result tool provider_input with
  | Ok call -> call
  | Error error ->
      failf "call decode failed: %s" (Tool.Call.Decode_error.message error)

let run_call ?(cancelled = fun () -> false) call =
  Tool.Call.run call ~cancelled |> finished

let run ?cancelled world provider_input =
  run_call ?cancelled (decode_call world.tool provider_input)

let process_exn result =
  match
    Mentat_tools_output.Codec.decode Mentat_tools_output.Process.jsont
      (output_exn result)
  with
  | Some process -> process
  | None -> fail "shell output carried no process semantics"

let termination_name = function
  | Mentat_tools_output.Process.Exited _ -> "exited"
  | Mentat_tools_output.Process.Signaled _ -> "signaled"
  | Mentat_tools_output.Process.Timed_out -> "timed_out"
  | Mentat_tools_output.Process.Stopped -> "stopped"
  | Mentat_tools_output.Process.Output_limit _ -> "output_limit"
  | Mentat_tools_output.Process.Supervision_failed -> "supervision_failed"

let exit_code = function
  | Mentat_tools_output.Process.Exited code -> string_of_int code
  | Mentat_tools_output.Process.Signaled _
  | Mentat_tools_output.Process.Timed_out | Mentat_tools_output.Process.Stopped
  | Mentat_tools_output.Process.Output_limit _
  | Mentat_tools_output.Process.Supervision_failed ->
      "none"

let print_status ?(normalize = Fun.id) result =
  match Tool.Result.status result with
  | Tool.Result.Completed -> print_endline "status: completed"
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf "status: failed %s\nmessage: %s\nmetadata: %b\n"
        (failure_name kind) (normalize message) (Option.is_some metadata)
  | Tool.Result.Interrupted { reason; cancelled } ->
      Printf.printf "status: interrupted cancelled=%b\nreason: %s\n" cancelled
        reason

let find_substring ~from needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec loop index =
    if index + needle_length > haystack_length then None
    else if String.equal (String.sub haystack index needle_length) needle then
      Some index
    else loop (index + 1)
  in
  loop from

let line_with prefix text =
  String.split_on_char '\n' text
  |> List.find_opt (String.starts_with ~prefix)
  |> Option.value ~default:("<missing " ^ prefix ^ ">")

let transcript_streams output =
  let text = Tool.Output.text output in
  let stdout_marker = "\nstdout:\n" in
  let stderr_marker = "\nstderr:\n" in
  match find_substring ~from:0 stdout_marker text with
  | None -> fail "missing stdout transcript marker"
  | Some stdout_at -> (
      let stdout_start = stdout_at + String.length stdout_marker in
      match find_substring ~from:stdout_start stderr_marker text with
      | None -> fail "missing stderr transcript marker"
      | Some stderr_at ->
          let stdout =
            String.sub text stdout_start (stderr_at - stdout_start)
          in
          let stderr_start = stderr_at + String.length stderr_marker in
          let stderr =
            String.sub text stderr_start (String.length text - stderr_start)
          in
          (stdout, stderr))

let evidence_kind output =
  let prefix = "Sandbox:" in
  let line = line_with prefix (Tool.Output.text output) in
  String.sub line (String.length prefix)
    (String.length line - String.length prefix)
  |> String.trim |> String.split_on_char ' ' |> List.hd

let print_output_summary result =
  let output = output_exn result in
  let process = process_exn result in
  let stdout, stderr = transcript_streams output in
  Printf.printf "%s\n%s\n%s\n%s\n"
    (line_with "Status:" (Tool.Output.text output))
    (line_with "Timeout:" (Tool.Output.text output))
    (line_with "Sandbox:" (Tool.Output.text output))
    (Printf.sprintf "streams: stdout=%S stderr=%S" stdout stderr);
  Printf.printf
    "semantics: termination=%s exit=%s duration_nonnegative=%b evidence=%s\n"
    (termination_name (Mentat_tools_output.Process.termination process))
    (exit_code (Mentat_tools_output.Process.termination process))
    (Mentat_tools_output.Process.duration_ms process >= 0)
    (evidence_kind output);
  Printf.printf "truncated: %b\n" (Tool.Output.truncated output)

let abs = Lpath.Abs.of_string_exn

let write_disk path contents =
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel contents)

let with_env bindings fn =
  let saved =
    List.map (fun (name, _) -> (name, Sys.getenv_opt name)) bindings
  in
  let set (name, value) =
    match value with
    | Some value -> Unix.putenv name value
    | None -> Unix.unsetenv name
  in
  List.iter set bindings;
  Fun.protect ~finally:(fun () -> List.iter set saved) fn

let with_world ?(mode = Mentat_config.Mode.Danger_full_access)
    ?(read = Mentat_config.Read.All)
    ?(network = Sandbox.Policy.Network.Restricted) ?(shell = fun _ -> "/bin/sh")
    name fn =
  Eio_main.run @@ fun stdenv ->
  let stdenv = (stdenv :> Eio_unix.Stdenv.base) in
  let raw = Filename.temp_dir ("mentat-shell-" ^ name ^ "-") "" in
  Fun.protect ~finally:(fun () -> remove_tree raw) @@ fun () ->
  let base = Unix.realpath raw in
  let ws_dir = Filename.concat base "workspace" in
  let out_dir = Filename.concat base "outside" in
  let tmp_dir = Filename.concat base "tmp" in
  List.iter mkdir [ ws_dir; out_dir; tmp_dir ];
  let subject = Filename.concat ws_dir "subject" in
  let nested = Filename.concat subject "nested" in
  List.iter mkdir [ subject; nested ];
  write_disk (Filename.concat ws_dir "not-a-dir") "file\n";
  Unix.symlink out_dir (Filename.concat ws_dir "outside-link");
  let shell = shell ws_dir in
  let logical = Workspace.single (Workspace.Root.of_dir (abs ws_dir)) in
  with_env
    [
      ("TMPDIR", Some tmp_dir);
      ("MENTAT_SHELL_EXPECT_SECRET", Some "must-not-leak");
    ]
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let io = resolve_exn ~sw ~stdenv ~logical ~mode ~read ~network () in
  let clock = Eio.Stdenv.mono_clock stdenv in
  fn { ws_dir; out_dir; io; tool = Shell.make io ~clock ~shell }

let relative world path = Filename.concat world.ws_dir path

let normalize_paths world text =
  text
  |> String.replace_all ~sub:world.ws_dir ~by:"<workspace>"
  |> String.replace_all ~sub:world.out_dir ~by:"<outside>"

let decode_verdict tool label provider_input =
  match decode_call_result tool provider_input with
  | Ok call ->
      Printf.printf "%s: accepted canonical=%s\n" label
        (json_string (Tool.Call.input call))
  | Error error ->
      Printf.printf "%s: rejected diagnostic=%b\n" label
        (not (String.is_empty (Tool.Call.Decode_error.message error)))

let execution_name = Access.Command.execution_to_string

let print_permissions ?(normalize = Fun.id) call =
  let requests = Tool.Call.permissions call in
  Printf.printf "requests: %d\n" (List.length requests);
  List.iter
    (fun request ->
      Printf.printf "source: %s display: %s\n"
        (Option.value ~default:"none" (Permission.Request.source request))
        (Option.value ~default:"none" (Permission.Request.display request)
        |> normalize);
      List.iter
        (function
          | Access.Command (Access.Command.Shell { text; cwd; execution }) ->
              let cwd =
                match cwd with
                | Access.Path_scope.Workspace path ->
                    Workspace.Path.display path
                | Access.Path_scope.Outside_workspace path ->
                    "outside:" ^ Lpath.Abs.to_string path
                | Access.Path_scope.Unknown path -> "unknown:" ^ path
              in
              Printf.printf "shell: command=%S cwd=%s execution=%s\n"
                (normalize text) (normalize cwd) (execution_name execution)
          | Access.Command (Access.Command.Argv _ | Access.Command.Code _) ->
              fail "shell permission was weakened into a non-shell command"
          | Access.Custom { name; subject } ->
              Printf.printf "custom: name=%s subject=%s\n" name
                (Option.value ~default:"none" subject |> normalize)
          | Access.Path _ | Access.Network _ ->
              fail "shell permission contained a non-command access")
        (Permission.Request.accesses request))
    requests

let require_enforced world =
  match Wio.evidence world.io with
  | Sandbox.Evidence.Enforced _ -> ()
  | Sandbox.Evidence.Not_requested | Sandbox.Evidence.Refused _
  | Sandbox.Evidence.Declared_external ->
      skip ~reason:"no enforcing sandbox backend on this host" ()

let%expect_test "declaration and provider decoding enforce exact JSON types" =
  with_world "schema" @@ fun world ->
  let declaration = Tool.declaration world.tool in
  Printf.printf "name: %s\n" (Mentat_llm.Tool.name declaration);
  Printf.printf "schema: %s\n"
    (json_string (Mentat_llm.Tool.input_schema declaration));
  decode_verdict world.tool "minimal" (input "printf ok");
  decode_verdict world.tool "full"
    (input ~workdir:"subject" ~timeout_ms:250 ~description:"show subject"
       ~escalate:true "pwd");
  decode_verdict world.tool "missing command" (json_object []);
  decode_verdict world.tool "unknown member"
    (json_object [ ("command", Json.string "pwd"); ("extra", Json.bool true) ]);
  decode_verdict world.tool "non-object input" (Json.array [||]);
  decode_verdict world.tool "duplicate command"
    (json_object
       [
         ("command", Json.string "printf first");
         ("command", Json.string "printf second");
       ]);
  decode_verdict world.tool "duplicate escalation"
    (json_object
       [
         ("command", Json.string "pwd");
         ("escalate", Json.bool false);
         ("escalate", Json.bool true);
       ]);
  decode_verdict world.tool "empty command" (input "");
  decode_verdict world.tool "nul command" (input "bad\000command");
  decode_verdict world.tool "empty workdir" (input ~workdir:"" "pwd");
  decode_verdict world.tool "nul workdir" (input ~workdir:"bad\000path" "pwd");
  decode_verdict world.tool "empty description" (input ~description:"" "pwd");
  decode_verdict world.tool "nul description"
    (input ~description:"bad\000description" "pwd");
  decode_verdict world.tool "zero timeout" (input ~timeout_ms:0 "pwd");
  decode_verdict world.tool "negative timeout" (input ~timeout_ms:(-1) "pwd");
  decode_verdict world.tool "fractional timeout"
    (json_object
       [ ("command", Json.string "pwd"); ("timeout_ms", Json.number 1.5) ]);
  decode_verdict world.tool "string timeout"
    (json_object
       [ ("command", Json.string "pwd"); ("timeout_ms", Json.string "10") ]);
  decode_verdict world.tool "largest safe integer"
    (json_object
       [
         ("command", Json.string "pwd");
         ("timeout_ms", Json.number 9_007_199_254_740_991.);
       ]);
  decode_verdict world.tool "unsafe integer"
    (json_object
       [
         ("command", Json.string "pwd");
         ("timeout_ms", Json.number 9_007_199_254_740_992.);
       ]);
  [%expect
    {|
    name: shell
    schema: {"type":"object","properties":{"command":{"type":"string","description":"Non-empty shell command text.","minLength":1},"workdir":{"type":"string","description":"Workspace-relative or workspace-contained absolute working directory. Defaults to the workspace current directory.","minLength":1},"timeout_ms":{"type":"integer","description":"Optional positive command timeout in milliseconds, bounded by host policy.","minimum":1,"maximum":9007199254740991},"description":{"type":"string","description":"Optional reviewer and UI metadata.","minLength":1},"escalate":{"type":"boolean","description":"Request this command outside the enforcing profile. Use only after a sandbox restriction, with the reason in description; separate approval is required."},"background":{"type":"boolean","description":"Run the command in the background and return a handle to poll with shell_output and stop with shell_kill. Use for dev servers, watchers, and long log tails. timeout_ms is ignored for a background command; a background command runs confined and cannot be escalated (escalate a command in the foreground instead)."}},"required":["command"],"additionalProperties":false}
    minimal: accepted canonical={"command":"printf ok"}
    full: accepted canonical={"command":"pwd","description":"show subject","escalate":true,"timeout_ms":250,"workdir":"subject"}
    missing command: rejected diagnostic=true
    unknown member: rejected diagnostic=true
    non-object input: rejected diagnostic=true
    duplicate command: rejected diagnostic=true
    duplicate escalation: rejected diagnostic=true
    empty command: rejected diagnostic=true
    nul command: rejected diagnostic=true
    empty workdir: rejected diagnostic=true
    nul workdir: rejected diagnostic=true
    empty description: rejected diagnostic=true
    nul description: rejected diagnostic=true
    zero timeout: rejected diagnostic=true
    negative timeout: rejected diagnostic=true
    fractional timeout: rejected diagnostic=true
    string timeout: rejected diagnostic=true
    largest safe integer: accepted canonical={"command":"pwd","timeout_ms":9007199254740991}
    unsafe integer: rejected diagnostic=true |}]

let%expect_test "fixed timeout policy accepts 60s and 600s without waiting" =
  with_world "timeouts" @@ fun world ->
  let default = run world (input "printf default") in
  print_status default;
  Printf.printf "%s\n"
    (line_with "Timeout:" (Tool.Output.text (output_exn default)));
  let maximum =
    run world (input ~timeout_ms:Shell.max_timeout_ms "printf max")
  in
  print_status maximum;
  Printf.printf "%s\n"
    (line_with "Timeout:" (Tool.Output.text (output_exn maximum)));
  let above =
    run world
      (input ~workdir:"missing" ~timeout_ms:(Shell.max_timeout_ms + 1)
         "printf no")
  in
  print_status above;
  Printf.printf "above output: %b\n" (Option.is_some (Tool.Result.output above));
  [%expect
    {|
    status: completed
    Timeout: 60000ms
    status: completed
    Timeout: 600000ms
    status: failed invalid_input
    message: timeout_ms must be <= 600000
    metadata: false
    above output: false |}]

let%expect_test "constructor rejects an unrepresentable shell executable" =
  let check label shell =
    match with_world ~shell:(fun _ -> shell) label (fun _ -> ()) with
    | () -> Printf.printf "%s: accepted\n" label
    | exception Invalid_argument message ->
        Printf.printf "%s: invalid %s\n" label message
  in
  check "empty" "";
  check "nul" "bad\000shell";
  [%expect
    {|
    empty: invalid shell must not be empty
    nul: invalid shell must not contain NUL |}]

let%expect_test "permissions stay one exact coarse shell fact cached at decode"
    =
  with_world "permission-cache" @@ fun world ->
  let source = "printf hi > produced.txt; echo $(pwd); dune build" in
  let call = decode_call world.tool (input ~workdir:"subject" source) in
  print_permissions call;
  Unix.rename (relative world "subject") (relative world "renamed-subject");
  write_disk (relative world "subject") "now a file\n";
  print_permissions call;
  let result = run_call call in
  print_status result;
  Printf.printf "output: %b produced: %b\n"
    (Option.is_some (Tool.Result.output result))
    (Sys.file_exists (relative world "subject/produced.txt"));
  [%expect
    {|
    requests: 1
    source: shell display: printf hi > produced.txt; echo $(pwd); dune build
    shell: command="printf hi > produced.txt; echo $(pwd); dune build" cwd=subject execution=direct
    requests: 1
    source: shell display: printf hi > produced.txt; echo $(pwd); dune build
    shell: command="printf hi > produced.txt; echo $(pwd); dune build" cwd=subject execution=direct
    status: failed invalid_input
    message: subject: expected a directory, found regular file
    metadata: false
    output: false produced: false |}]

let%expect_test "direct and external routes expose their exact command facts" =
  with_world "direct-permission" @@ fun direct ->
  print_endline "-- direct --";
  print_permissions (decode_call direct.tool (input "dune build"));
  with_world ~mode:Mentat_config.Mode.External_sandbox "external-permission"
  @@ fun external_world ->
  print_endline "-- external --";
  print_permissions (decode_call external_world.tool (input "dune build"));
  [%expect
    {|
    -- direct --
    requests: 1
    source: shell display: dune build
    shell: command="dune build" cwd=. execution=direct
    -- external --
    requests: 1
    source: shell display: dune build
    shell: command="dune build" cwd=. execution=external |}]

let%expect_test "confined permissions and escalation match the selected route" =
  with_world ~mode:Mentat_config.Mode.Workspace_write "confined-permission"
  @@ fun writable ->
  require_enforced writable;
  print_endline "-- ordinary --";
  print_permissions (decode_call writable.tool (input "dune build"));
  print_endline "-- escalated --";
  print_permissions
    (decode_call writable.tool
       (input ~description:"needs network" ~escalate:true "dune build"));
  with_world ~mode:Mentat_config.Mode.Read_only "readonly-permission"
  @@ fun readonly ->
  require_enforced readonly;
  print_endline "-- read-only ordinary --";
  print_permissions (decode_call readonly.tool (input "dune build"));
  print_endline "-- read-only escalation --";
  let escalating =
    decode_call readonly.tool (input ~escalate:true "dune build")
  in
  print_permissions escalating;
  print_status (run_call escalating);
  print_endline "-- read-only escalation with missing cwd --";
  let missing =
    decode_call readonly.tool (input ~workdir:"missing" ~escalate:true "pwd")
  in
  print_permissions missing;
  print_status (run_call missing);
  [%expect
    {|
    -- ordinary --
    requests: 1
    source: shell display: dune build
    shell: command="dune build" cwd=. execution=enforced(all,workspace,restricted)
    -- escalated --
    requests: 1
    source: shell display: dune build
    shell: command="dune build" cwd=. execution=direct
    custom: name=shell.escalate subject=dune build
    -- read-only ordinary --
    requests: 1
    source: shell display: dune build
    shell: command="dune build" cwd=. execution=enforced(all,read-only,restricted)
    -- read-only escalation --
    requests: 0
    status: failed invalid_input
    message: the sealed policy has no writable roots: a read-only sandbox admits no escalation
    metadata: false
    -- read-only escalation with missing cwd --
    requests: 0
    status: failed invalid_input
    message: the sealed policy has no writable roots: a read-only sandbox admits no escalation
    metadata: false |}]

let%expect_test
    "configured shell cwd null stdin and stripped environment are real" =
  let shell ws_dir =
    let wrapper = Filename.concat ws_dir "configured-shell" in
    write_disk wrapper
      "#!/bin/sh\nprintf 'wrapper:%s\\n' \"$1\" >&2\nexec /bin/sh \"$@\"\n";
    Unix.chmod wrapper 0o755;
    wrapper
  in
  with_world ~shell "configured" @@ fun world ->
  let command =
    "printf 'cwd=%s\\n' \"$PWD\"; if read value; then printf 'stdin=%s\\n' \
     \"$value\"; else printf 'stdin=null\\n'; fi; [ \"$HOME\" = \"$TMPDIR\" ] \
     && printf 'scratch=shared\\n'; printf 'secret=%s\\n' \
     \"${MENTAT_SHELL_EXPECT_SECRET-unset}\""
  in
  let result = run world (input ~workdir:"subject/nested" command) in
  print_status result;
  let output = output_exn result in
  let stdout, stderr = transcript_streams output in
  let normalized =
    String.replace_all ~sub:world.ws_dir ~by:"<workspace>" stdout
  in
  Printf.printf "stdout: %S\nstderr: %S\n" normalized stderr;
  Printf.printf "configured command ran: %b shell flag exact: %b\n"
    (String.includes ~affix:"secret=unset" stdout)
    (String.equal stderr "wrapper:-lc\n");
  [%expect
    {|
    status: completed
    stdout: "cwd=<workspace>/subject/nested\nstdin=null\nscratch=shared\nsecret=unset\n"
    stderr: "wrapper:-lc\n"
    configured command ran: true shell flag exact: true |}]

let%expect_test "success failure signal and pre-spawn refusal preserve taxonomy"
    =
  with_world "terminal" @@ fun world ->
  let success = run world (input "printf out; printf err >&2") in
  print_endline "-- success --";
  print_status success;
  print_output_summary success;
  let failure =
    run world (input "printf partial; printf problem >&2; exit 7")
  in
  print_endline "-- exit --";
  print_status failure;
  print_output_summary failure;
  let signal = run world (input "kill -TERM $$") in
  print_endline "-- signal --";
  (match Tool.Result.status signal with
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf "status: failed %s signal-diagnostic=%b metadata=%b\n"
        (failure_name kind)
        (String.starts_with ~prefix:"command terminated by signal " message)
        (Option.is_some metadata)
  | Tool.Result.Completed | Tool.Result.Interrupted _ ->
      fail "self-signalled command unexpectedly completed");
  let signal_termination =
    process_exn signal |> Mentat_tools_output.Process.termination
  in
  Printf.printf "termination: %s status-line=%b exit-none: %b output: %b\n"
    (termination_name signal_termination)
    (line_with "Status:" (Tool.Output.text (output_exn signal))
    |> String.starts_with ~prefix:"Status: signaled ")
    (String.equal (exit_code signal_termination) "none")
    (Option.is_some (Tool.Result.output signal));
  with_world ~shell:(fun ws -> Filename.concat ws "missing-shell") "spawn"
  @@ fun missing ->
  print_endline "-- spawn --";
  let refused = run missing (input "printf never") in
  (match Tool.Result.status refused with
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf "status: failed %s diagnostic=%b metadata=%b\n"
        (failure_name kind)
        (not (String.is_empty message))
        (Option.is_some metadata)
  | Tool.Result.Completed | Tool.Result.Interrupted _ ->
      fail "missing shell unexpectedly ran");
  Printf.printf "output: %b\n" (Option.is_some (Tool.Result.output refused));
  [%expect
    {|
    -- success --
    status: completed
    Status: exited 0
    Timeout: 60000ms
    Sandbox: not_requested
    streams: stdout="out" stderr="err"
    semantics: termination=exited exit=0 duration_nonnegative=true evidence=not_requested
    truncated: false
    -- exit --
    status: failed failed
    message: command exited with status 7
    metadata: false
    Status: exited 7
    Timeout: 60000ms
    Sandbox: not_requested
    streams: stdout="partial" stderr="problem"
    semantics: termination=exited exit=7 duration_nonnegative=true evidence=not_requested
    truncated: false
    -- signal --
    status: failed failed signal-diagnostic=true metadata=false
    termination: signaled status-line=true exit-none: true output: true
    -- spawn --
    status: failed unavailable diagnostic=true metadata=false
    output: false |}]

let%expect_test "timeout and cooperative stop retain post-spawn output" =
  with_world "stop" @@ fun world ->
  let timed =
    run world (input ~timeout_ms:120 "printf before-timeout; exec /bin/sleep 5")
  in
  print_endline "-- timeout --";
  print_status timed;
  print_output_summary timed;
  let deadline = Unix.gettimeofday () +. 0.12 in
  let stopped =
    run
      ~cancelled:(fun () -> Unix.gettimeofday () >= deadline)
      world
      (input ~timeout_ms:5_000 "printf before-stop; exec /bin/sleep 5")
  in
  print_endline "-- running stop --";
  print_status stopped;
  print_output_summary stopped;
  let before =
    run
      ~cancelled:(fun () -> true)
      world
      (input ~workdir:"missing" ~timeout_ms:(Shell.max_timeout_ms + 1)
         "printf never")
  in
  print_endline "-- pre-run stop --";
  print_status before;
  Printf.printf "output: %b\n" (Option.is_some (Tool.Result.output before));
  [%expect
    {|
    -- timeout --
    status: failed timed_out
    message: command timed out after 120ms
    metadata: false
    Status: timed out after 120ms
    Timeout: 120ms
    Sandbox: not_requested
    streams: stdout="before-timeout" stderr=""
    semantics: termination=timed_out exit=none duration_nonnegative=true evidence=not_requested
    truncated: false
    -- running stop --
    status: interrupted cancelled=true
    reason: tool call cancelled
    Status: stopped
    Timeout: 5000ms
    Sandbox: not_requested
    streams: stdout="before-stop" stderr=""
    semantics: termination=stopped exit=none duration_nonnegative=true evidence=not_requested
    truncated: false
    -- pre-run stop --
    status: interrupted cancelled=true
    reason: tool call cancelled
    output: false |}]

let%expect_test "64KiB head-tail capture is structural for both streams" =
  with_world "head-tail" @@ fun world ->
  let command =
    "head -c 70000 /dev/zero | tr '\\0' O; head -c 70000 /dev/zero | tr '\\0' \
     E >&2"
  in
  let result = run world (input command) in
  print_status result;
  let output = output_exn result in
  let stdout, stderr = transcript_streams output in
  let marker = "\n... 4464 bytes omitted ...\n" in
  let summarize name stream =
    Printf.printf
      "%s: valid_utf8=%b rendered_bytes=%d marker=%b begins=%S ends=%S\n" name
      (String.is_valid_utf_8 stream)
      (String.length stream)
      (String.includes ~affix:marker stream)
      (String.sub stream 0 4)
      (String.sub stream (String.length stream - 4) 4)
  in
  summarize "stdout" stdout;
  summarize "stderr" stderr;
  Printf.printf "both streams elided: %b truncated=%b\n"
    (String.includes ~affix:marker stdout
    && String.includes ~affix:marker stderr)
    (Tool.Output.truncated output);
  let literal =
    run world (input "printf '\\n... 7 bytes omitted ...\\n'") |> output_exn
  in
  let literal_stdout, _ = transcript_streams literal in
  Printf.printf "literal: %S truncated=%b\n" literal_stdout
    (Tool.Output.truncated literal);
  [%expect
    {|
    status: completed
    stdout: valid_utf8=true rendered_bytes=65564 marker=true begins="OOOO" ends="OOOO"
    stderr: valid_utf8=true rendered_bytes=65564 marker=true begins="EEEE" ends="EEEE"
    both streams elided: true truncated=true
    literal: "\n... 7 bytes omitted ...\n" truncated=false |}]

let%expect_test "the per-stream byte cap is inclusive at exactly 64KiB" =
  with_world "head-tail-boundary" @@ fun world ->
  let exact =
    run world
      (input
         "head -c 65536 /dev/zero | tr '\\0' O; head -c 65536 /dev/zero | tr \
          '\\0' E >&2")
    |> output_exn
  in
  let exact_stdout, exact_stderr = transcript_streams exact in
  Printf.printf "exact: stdout=%d stderr=%d truncated=%b\n"
    (String.length exact_stdout)
    (String.length exact_stderr)
    (Tool.Output.truncated exact);
  let over =
    run world
      (input
         "head -c 65537 /dev/zero | tr '\\0' O; head -c 65537 /dev/zero | tr \
          '\\0' E >&2")
    |> output_exn
  in
  let over_stdout, over_stderr = transcript_streams over in
  let one_byte_marker = "\n... 1 bytes omitted ...\n" in
  Printf.printf "over: stdout_marker=%b stderr_marker=%b truncated=%b\n"
    (String.includes ~affix:one_byte_marker over_stdout)
    (String.includes ~affix:one_byte_marker over_stderr)
    (Tool.Output.truncated over);
  [%expect
    {|
    exact: stdout=65536 stderr=65536 truncated=false
    over: stdout_marker=true stderr_marker=true truncated=true |}]

let%expect_test "a surviving descendant cannot hold transcript return open" =
  with_world "descendant" @@ fun world ->
  let result =
    run world
      (input "trap '' HUP; /bin/sleep 1 2>/dev/null & printf parent; exit 0")
  in
  print_status result;
  print_output_summary result;
  [%expect
    {|
    status: completed
    Status: exited 0
    Timeout: 60000ms
    Sandbox: not_requested
    streams: stdout="parent\n... bytes omitted ...\n" stderr=""
    semantics: termination=exited exit=0 duration_nonnegative=true evidence=not_requested
    truncated: true |}]

let%expect_test
    "invalid process bytes and split codepoints become durable UTF-8" =
  with_world "utf8" @@ fun world ->
  let invalid_result = run world (input "printf '\\377bad\\n'") in
  let invalid = output_exn invalid_result in
  let invalid_stdout, _ = transcript_streams invalid in
  ignore (encode result_codec invalid_result);
  Printf.printf "invalid repaired: %b replacement: %b durable: %b\n"
    (String.is_valid_utf_8 invalid_stdout)
    (String.includes ~affix:"\239\191\189" invalid_stdout)
    true;
  let split =
    run world
      (input
         "head -c 32767 /dev/zero | tr '\\0' a; printf '\\342\\202\\254'; head \
          -c 32767 /dev/zero | tr '\\0' b")
    |> output_exn
  in
  let split_stdout, _ = transcript_streams split in
  ignore (encode Tool.Output.jsont split);
  Printf.printf "split repaired: %b truncated: %b durable: %b\n"
    (String.is_valid_utf_8 split_stdout)
    (Tool.Output.truncated split)
    true;
  [%expect
    {|
    invalid repaired: true replacement: true durable: true
    split repaired: true truncated: true durable: true |}]

let%expect_test "workdir refusals happen before spawn and return no transcript"
    =
  with_world "workdir" @@ fun world ->
  let cases =
    [
      ("missing", "missing");
      ("file", "not-a-dir");
      ("escaping symlink", "outside-link");
      ("absolute outside", world.out_dir);
    ]
  in
  List.iter
    (fun (label, workdir) ->
      Printf.printf "-- %s --\n" label;
      let call = decode_call world.tool (input ~workdir "printf never") in
      Printf.printf "permissions: %d\n"
        (List.length (Tool.Call.permissions call));
      let result = run_call call in
      print_status ~normalize:(normalize_paths world) result;
      Printf.printf "output: %b\n" (Option.is_some (Tool.Result.output result)))
    cases;
  [%expect
    {|
    -- missing --
    permissions: 1
    status: failed invalid_input
    message: missing: path does not exist
    metadata: false
    output: false
    -- file --
    permissions: 1
    status: failed invalid_input
    message: not-a-dir: expected a directory, found regular file
    metadata: false
    output: false
    -- escaping symlink --
    permissions: 1
    status: failed invalid_input
    message: outside-link: path resolves outside workspace
    metadata: false
    output: false
    -- absolute outside --
    permissions: 0
    status: failed invalid_input
    message: path is outside workspace: <outside>
    metadata: false
    output: false |}]

let%expect_test "sandbox denial is explained and escalation uses run_escalated"
    =
  with_world ~mode:Mentat_config.Mode.Workspace_write "sandbox" @@ fun world ->
  require_enforced world;
  let outside_file = Filename.concat world.out_dir "blocked.txt" in
  (* The refused redirection is real, but its status is pinned by the script
     rather than by the host shell: a failed redirection exits 1 under bash and
     2 under dash, so an unguarded write would make this golden depend on which
     /bin/sh the platform ships. *)
  let ordinary_command =
    Printf.sprintf "printf blocked > %s || exit 7" (Filename.quote outside_file)
  in
  let ordinary = run world (input ordinary_command) in
  print_endline "-- ordinary confinement --";
  print_status ordinary;
  Printf.printf "outside exists: %b evidence: %s\n"
    (Sys.file_exists outside_file)
    (evidence_kind (output_exn ordinary));
  let diagnostic =
    run world
      (input
         "printf 'curl: (6) Could not resolve host: example.invalid\\n' >&2; \
          exit 6")
  in
  print_endline "-- network denial diagnostic --";
  (match Tool.Result.status diagnostic with
  | Tool.Result.Failed { message; _ } ->
      Printf.printf "policy note: %b\n"
        (String.includes ~affix:"policy restriction" message
        && String.includes ~affix:"escalate=true" message)
  | _ -> fail "network-shaped failure unexpectedly completed");
  let escalated_file = Filename.concat world.out_dir "escalated.txt" in
  let escalated_command =
    Printf.sprintf
      "printf escaped > %s; printf 'secret=%%s\\n' \
       \"${MENTAT_SHELL_EXPECT_SECRET-unset}\""
      (Filename.quote escalated_file)
  in
  let call = decode_call world.tool (input ~escalate:true escalated_command) in
  print_endline "-- escalation --";
  print_permissions ~normalize:(normalize_paths world) call;
  let escalated = run_call call in
  print_status escalated;
  let stdout, _ = transcript_streams (output_exn escalated) in
  Printf.printf "outside: %S stdout: %S evidence: %s\n"
    (read_disk escalated_file) stdout
    (evidence_kind (output_exn escalated));
  [%expect
    {|
    -- ordinary confinement --
    status: failed failed
    message: command exited with status 7

    This command ran inside a sandbox that confines writes to the workspace while leaving reads unrestricted, and its output looks like a refused write. This is a policy restriction, not a transient error: retry the exact command with escalate=true only if the write is genuinely needed, or ask the user to add the path to sandbox.writable_roots for a standing grant.
    metadata: false
    outside exists: false evidence: enforced
    -- network denial diagnostic --
    policy note: true
    -- escalation --
    requests: 1
    source: shell display: printf escaped > '<outside>/escalated.txt'; printf 'secret=%s\n' "${MENTAT_SHELL_EXPECT_SECRET-unset}"
    shell: command="printf escaped > '<outside>/escalated.txt'; printf 'secret=%s\\n' \"${MENTAT_SHELL_EXPECT_SECRET-unset}\"" cwd=. execution=direct
    custom: name=shell.escalate subject=printf escaped > '<outside>/escalated.txt'; printf 'secret=%s\n' "${MENTAT_SHELL_EXPECT_SECRET-unset}"
    status: completed
    outside: "escaped" stdout: "secret=unset\n" evidence: not_requested |}]

let%expect_test "a confined read denial is explained with the escalate path" =
  with_world ~mode:Mentat_config.Mode.Workspace_write
    ~read:Mentat_config.Read.Project "read-denial"
  @@ fun world ->
  require_enforced world;
  (* Reproduce the OS wording an out-of-project read is refused with, rather than
     triggering a real denial, so the diagnosis is deterministic across backends.
     Under the confined read scope the note names the escalate path and both
     standing-grant knobs — read vs write is indistinguishable from the wording. *)
  let diagnostic =
    run world
      (input "printf 'cat: /etc/hosts: Operation not permitted\\n' >&2; exit 1")
  in
  (match Tool.Result.status diagnostic with
  | Tool.Result.Failed { message; _ } ->
      Printf.printf "policy note: %b\n"
        (String.includes ~affix:"policy restriction" message
        && String.includes ~affix:"escalate=true" message
        && String.includes ~affix:"sandbox.readable_roots" message
        && String.includes ~affix:"sandbox.writable_roots" message)
  | _ -> fail "read-denial-shaped failure unexpectedly completed");
  [%expect {| policy note: true |}]

let%expect_test "a write denial is explained under an unconfined read scope" =
  with_world ~mode:Mentat_config.Mode.Workspace_write
    ~read:Mentat_config.Read.All "write-denial"
  @@ fun world ->
  require_enforced world;
  (* Bubblewrap refuses a write against a read-only bind with [EROFS], not the
     permission wording seatbelt uses, and a workspace-write route confines
     writes whether or not its reads are scoped. Both together are the shape a
     build tool hits when it writes outside the project, so neither the backend
     spelling nor the read scope may decide whether the refusal is explained.
     The read scope does decide the wording: with reads unrestricted the note
     must not send the user to [sandbox.readable_roots], which the resolver
     discards on an unscoped route. *)
  let diagnostic =
    run world
      (input
         "printf 'dune: open(/root/.cache/dune/rev-store.lock): Read-only file \
          system\\n' >&2; exit 1")
  in
  (match Tool.Result.status diagnostic with
  | Tool.Result.Failed { message; _ } ->
      Printf.printf "policy note: %b\n"
        (String.includes ~affix:"policy restriction" message
        && String.includes ~affix:"escalate=true" message
        && String.includes ~affix:"sandbox.writable_roots" message
        && not (String.includes ~affix:"sandbox.readable_roots" message))
  | _ -> fail "write-denial-shaped failure unexpectedly completed");
  [%expect {| policy note: true |}]

let%expect_test "durable replay retains presentation but no mutation authority"
    =
  with_world "durable" @@ fun world ->
  let scope = Wio.open_claim_scope world.io in
  let result =
    run world (input "printf durable > shell-created.txt; printf replay")
  in
  let evidence = Wio.Claim_scope.close scope in
  let durable_json = encode result_codec result in
  let replay = decode result_codec durable_json in
  let original_output = output_exn result in
  let replay_output = output_exn replay in
  Printf.printf "disk: %S\n" (read_disk (relative world "shell-created.txt"));
  let replay_process = process_exn replay in
  Printf.printf "durable equal: %b text valid: %b termination: %s\n"
    (Tool.Output.equal original_output replay_output)
    (String.is_valid_utf_8 (Tool.Output.text replay_output))
    (termination_name (Mentat_tools_output.Process.termination replay_process));
  let forbidden =
    [
      "receipt";
      "checkpoint";
      "mutation";
      "before";
      "after";
      "claim";
      "handle";
      "capability";
    ]
  in
  let names = json_member_names durable_json in
  let leaked = List.filter (fun name -> List.mem name forbidden) names in
  Printf.printf "authority fields: %d claim applies: %d observations: %d\n"
    (List.length leaked)
    (List.length evidence.Mentat_edit.Apply_evidence.applies)
    (List.length evidence.Mentat_edit.Apply_evidence.observed);
  [%expect
    {|
    disk: "durable"
    durable equal: true text valid: true termination: exited
    authority fields: 0 claim applies: 0 observations: 0 |}]

[%%run_tests "mentat.tools.shell"]
