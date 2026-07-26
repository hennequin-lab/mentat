(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let max_output_bytes = 64 * 1024
let name = "shell"
let default_timeout_ms = 60_000
let max_timeout_ms = 600_000
let capture_head_bytes = max_output_bytes / 2
let capture_tail_bytes = max_output_bytes - capture_head_bytes
let escalation_access_name = "shell.escalate"
let json_string value = Jsont.Json.string value
let json_int value = Jsont.Json.int value
let json_bool value = Jsont.Json.bool value

module Input = struct
  type t = {
    command : string;
    workdir : string option;
    timeout_ms : int option;
    description : string option;
    escalate : bool;
    background : bool;
  }

  let validate_string member value =
    if String.is_empty value then invalid_arg (member ^ " must not be empty");
    if String.contains value '\000' then
      invalid_arg (member ^ " must not contain NUL")

  let make command workdir timeout_ms description escalate background =
    Mentat_tool.Codec.decode_invalid_arg @@ fun () ->
    validate_string "shell command" command;
    Option.iter (validate_string "workdir") workdir;
    Option.iter (validate_string "description") description;
    (match timeout_ms with
    | Some timeout_ms when timeout_ms <= 0 ->
        invalid_arg
          ("timeout_ms must be positive, got " ^ string_of_int timeout_ms)
    | Some _ | None -> ());
    {
      command;
      workdir;
      timeout_ms;
      description;
      escalate = Option.value escalate ~default:false;
      background = Option.value background ~default:false;
    }

  let max_input_integer =
    Float.min 9_007_199_254_740_991. (float_of_int Int.max_int)

  let exact_integer =
    let decode = function
      | Jsont.Number (value, _)
        when Float.is_integer value && Float.abs value <= max_input_integer ->
          int_of_float value
      | Jsont.Number _ | Jsont.Null _ | Jsont.Bool _ | Jsont.String _
      | Jsont.Array _ | Jsont.Object _ ->
          Codec.decode_error "expected an integer in JSON's safe integer range"
    in
    Jsont.map ~kind:"integer" ~dec:decode ~enc:json_int Jsont.json

  let object_codec =
    Jsont.Object.map ~kind:"shell input" make
    |> Jsont.Object.mem "command" Jsont.string ~enc:(fun input -> input.command)
    |> Jsont.Object.opt_mem "workdir" Jsont.string ~enc:(fun input ->
        input.workdir)
    |> Jsont.Object.opt_mem "timeout_ms" exact_integer ~enc:(fun input ->
        input.timeout_ms)
    |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun input ->
        input.description)
    |> Jsont.Object.opt_mem "escalate" Jsont.bool ~enc:(fun input ->
        if input.escalate then Some true else None)
    |> Jsont.Object.opt_mem "background" Jsont.bool ~enc:(fun input ->
        if input.background then Some true else None)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec = Codec.strict_object ~kind:"strict shell input" object_codec

  let property kind description fields =
    Codec.obj
      (("type", json_string kind)
      :: ("description", json_string description)
      :: fields)

  let schema =
    Codec.obj
      [
        ("type", json_string "object");
        ( "properties",
          Codec.obj
            [
              ( "command",
                property "string" "Non-empty shell command text."
                  [ ("minLength", json_int 1) ] );
              ( "workdir",
                property "string"
                  "Workspace-relative or workspace-contained absolute working \
                   directory. Defaults to the workspace current directory."
                  [ ("minLength", json_int 1) ] );
              ( "timeout_ms",
                property "integer"
                  (Printf.sprintf
                     "Optional command timeout in milliseconds, from 1 to %d. \
                      Defaults to %d."
                     max_timeout_ms default_timeout_ms)
                  [
                    ("minimum", json_int 1); ("maximum", json_int max_timeout_ms);
                  ] );
              ( "description",
                property "string" "Optional reviewer and UI metadata."
                  [ ("minLength", json_int 1) ] );
              ( "escalate",
                property "boolean"
                  "Request this command outside the enforcing profile. Use \
                   only after a sandbox restriction, with the reason in \
                   description; separate approval is required."
                  [] );
              ( "background",
                property "boolean"
                  "Run the command in the background and return a handle to \
                   read with shell_output and stop with shell_kill. Use for \
                   dev servers, watchers, and long log tails. timeout_ms is \
                   ignored for a background command; a background command runs \
                   confined and cannot be escalated (escalate a command in the \
                   foreground instead)."
                  [] );
            ] );
        ("required", Jsont.Json.list [ json_string "command" ]);
        ("additionalProperties", json_bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema

  let effective_timeout_ms input =
    Option.value input.timeout_ms ~default:default_timeout_ms
end

type route = Ordinary | Escalated | Escalation_denied

(* [route] is the escalation route decision, and also drives the permission
   projection. A background command is supervised through
   [Command.start_session], which has no escalated variant: a plain background
   command runs [Ordinary] (its permission is the ordinary shell fact), while a
   background command that {e requests} escalation cannot have it — it is
   refused with a steering message at launch ({!run}) and projected as needing no
   request here, so [route] answers [Escalation_denied] to yield no permission.
   [run] dispatches a background command before this match, so the value never
   selects a foreground path. *)
let route escalation input =
  if input.Input.background then
    if input.Input.escalate then Escalation_denied else Ordinary
  else if not input.Input.escalate then Ordinary
  else
    match escalation with
    | Mentat_sandbox.Available -> Escalated
    | Mentat_sandbox.Denied _ -> Escalation_denied
    | Mentat_sandbox.Ignored -> Ordinary

let shell_argv shell command =
  let executable = String.lowercase_ascii (Filename.basename shell) in
  if
    String.equal Sys.os_type "Win32"
    && (String.equal executable "cmd" || String.equal executable "cmd.exe")
  then [ shell; "/C"; command ]
  else if
    String.equal Sys.os_type "Win32"
    && (String.equal executable "powershell"
       || String.equal executable "powershell.exe"
       || String.equal executable "pwsh"
       || String.equal executable "pwsh.exe")
  then [ shell; "-NoLogo"; "-NoProfile"; "-Command"; command ]
  else [ shell; "-lc"; command ]

let resolve_workdir workspace_io input =
  let requested = Option.value input.Input.workdir ~default:"." in
  match Mentat_workspace_io.resolve_path workspace_io requested with
  | Error error -> Error (Mentat_workspace.Resolve_error.message error)
  | Ok path -> (
      match Mentat_workspace_io.File.stat workspace_io path with
      | Error error -> Error (Fs_error.message error)
      | Ok stat -> (
          match stat.Eio.File.Stat.kind with
          | `Directory -> Ok path
          | kind ->
              Error
                (Mentat_workspace.Path.display path
                ^ ": expected a directory, found " ^ Stat_kind.kind_name kind)))

let permissions workspace_io ~ordinary_execution ~escalation input =
  let requested = Option.value input.Input.workdir ~default:"." in
  match Mentat_workspace_io.resolve_path workspace_io requested with
  | Error _ -> []
  | Ok cwd -> (
      let command_access execution =
        Mentat_permission.Access.shell
          ~cwd:(Mentat_permission.Access.Path_scope.workspace cwd)
          ~execution input.Input.command
      in
      let accesses =
        match route escalation input with
        | Escalation_denied -> []
        | Ordinary -> [ command_access ordinary_execution ]
        | Escalated ->
            [
              command_access Confinement.escalated;
              Mentat_permission.Access.custom ~subject:input.Input.command
                escalation_access_name;
            ]
      in
      match accesses with
      | [] -> []
      | _ :: _ ->
          [
            Mentat_permission.Request.of_accesses ~source:name
              ~display:input.Input.command accesses;
          ])

module Output = struct
  type t = {
    input : Input.t;
    workdir : Mentat_workspace.Path.t;
    timeout_ms : int;
    outcome : Mentat_workspace_io.Command.outcome;
  }

  let make ~input ~workdir ~timeout_ms outcome =
    { input; workdir; timeout_ms; outcome }

  let duration_ms output =
    output.outcome.Mentat_workspace_io.Command.duration
    |> Mtime.Span.to_float_ns
    |> fun nanoseconds -> int_of_float (nanoseconds /. 1_000_000.)

  let semantic_termination output =
    match output.outcome.Mentat_workspace_io.Command.termination with
    | Mentat_workspace_io.Command.Exited (`Exited code) ->
        Mentat_tools_output.Process.Exited code
    | Mentat_workspace_io.Command.Exited (`Signaled signal) ->
        Mentat_tools_output.Process.Signaled signal
    | Mentat_workspace_io.Command.Timed_out ->
        Mentat_tools_output.Process.Timed_out
    | Mentat_workspace_io.Command.Stopped -> Mentat_tools_output.Process.Stopped
    | Mentat_workspace_io.Command.Output_limit { stream; limit } ->
        let stream =
          match stream with
          | Mentat_workspace_io.Command.Stdout ->
              Mentat_tools_output.Process.Stdout
          | Mentat_workspace_io.Command.Stderr ->
              Mentat_tools_output.Process.Stderr
        in
        Mentat_tools_output.Process.Output_limit { stream; limit }
    | Mentat_workspace_io.Command.Supervision_failed _ ->
        Mentat_tools_output.Process.Supervision_failed

  let stream_name = function
    | Mentat_workspace_io.Command.Stdout -> "stdout"
    | Mentat_workspace_io.Command.Stderr -> "stderr"

  let status_text output =
    match output.outcome.Mentat_workspace_io.Command.termination with
    | Mentat_workspace_io.Command.Exited (`Exited code) ->
        "exited " ^ string_of_int code
    | Mentat_workspace_io.Command.Exited (`Signaled signal) ->
        "signaled " ^ string_of_int signal
    | Mentat_workspace_io.Command.Timed_out ->
        "timed out after " ^ string_of_int output.timeout_ms ^ "ms"
    | Mentat_workspace_io.Command.Stopped -> "stopped"
    | Mentat_workspace_io.Command.Output_limit { stream; limit } ->
        Printf.sprintf "%s exceeded %d-byte output limit" (stream_name stream)
          limit
    | Mentat_workspace_io.Command.Supervision_failed error ->
        Format.asprintf "supervision failed: %a" Eio.Exn.pp_err error

  let evidence_text = function
    | Mentat_sandbox.Evidence.Not_requested -> "not_requested"
    | Mentat_sandbox.Evidence.Enforced { backend; profile } ->
        Printf.sprintf "enforced backend=%s profile_hash=%s"
          (Mentat_sandbox.Backend.id backend)
          (Mentat_digest.to_hex profile)
    | Mentat_sandbox.Evidence.Refused error ->
        "refused: " ^ Mentat_sandbox.Error.message error
    | Mentat_sandbox.Evidence.Declared_external -> "declared_external"

  let complete stream = Mentat_workspace_io.Command.Captured.is_complete stream

  let truncated output =
    (not (complete output.outcome.Mentat_workspace_io.Command.stdout))
    || not (complete output.outcome.Mentat_workspace_io.Command.stderr)

  let text output =
    let outcome = output.outcome in
    let stdout =
      Mentat_workspace_io.Command.Captured.render
        outcome.Mentat_workspace_io.Command.stdout
      |> Text_helpers.utf8_lossy
    in
    let stderr =
      Mentat_workspace_io.Command.Captured.render
        outcome.Mentat_workspace_io.Command.stderr
      |> Text_helpers.utf8_lossy
    in
    Printf.sprintf
      "Command: %s\n\
       Workdir: %s\n\
       Status: %s\n\
       Duration: %dms\n\
       Timeout: %dms\n\
       Sandbox: %s\n\n\
       stdout:\n\
       %s\n\
       stderr:\n\
       %s"
      output.input.Input.command
      (Mentat_workspace.Path.display output.workdir)
      (status_text output) (duration_ms output) output.timeout_ms
      (evidence_text outcome.Mentat_workspace_io.Command.sandbox_evidence)
      stdout stderr

  let encode output =
    let semantic =
      Mentat_tools_output.Process.make
        ~termination:(semantic_termination output)
        ~duration_ms:(duration_ms output)
    in
    Mentat_tools_output.Codec.encode Mentat_tools_output.Process.jsont
      ~text:(text output) ~truncated:(truncated output) semantic
end

let command_error_message error =
  Mentat_workspace_io.Command.Error.message error

let command_error_failure = function
  | Mentat_workspace_io.Command.Error.Sandbox
      ( Mentat_sandbox.Error.Escalation_denied
      | Mentat_sandbox.Error.Escalation_irrelevant
      | Mentat_sandbox.Error.Empty_program | Mentat_sandbox.Error.Nul_in_argv _
        ) ->
      `Invalid_input
  | Mentat_workspace_io.Command.Error.Unknown_cwd_root _ -> `Invalid_input
  | Mentat_workspace_io.Command.Error.Sandbox
      ( Mentat_sandbox.Error.Unavailable _
      | Mentat_sandbox.Error.Cwd_outside_scope _
      | Mentat_sandbox.Error.Stale_policy _ )
  | Mentat_workspace_io.Command.Error.Spawn _
  | Mentat_workspace_io.Command.Error.Io _ ->
      `Unavailable

let result_of_output output =
  let outcome = output.Output.outcome in
  let note =
    Confinement.denial_note outcome.Mentat_workspace_io.Command.confinement
  in
  match outcome.Mentat_workspace_io.Command.termination with
  | Mentat_workspace_io.Command.Exited (`Exited 0) ->
      Mentat_tool.Result.completed ~output ()
  | Mentat_workspace_io.Command.Exited (`Exited code) ->
      Mentat_tool.Result.failed ~output `Failed
        ("command exited with status " ^ string_of_int code ^ note)
  | Mentat_workspace_io.Command.Exited (`Signaled signal) ->
      Mentat_tool.Result.failed ~output `Failed
        ("command terminated by signal " ^ string_of_int signal ^ note)
  | Mentat_workspace_io.Command.Timed_out ->
      Mentat_tool.Result.failed ~output `Timed_out
        ("command timed out after "
        ^ string_of_int output.Output.timeout_ms
        ^ "ms" ^ note)
  | Mentat_workspace_io.Command.Stopped ->
      Mentat_tool.Result.cancelled ~output ()
  | Mentat_workspace_io.Command.Output_limit { stream; limit } ->
      Mentat_tool.Result.failed ~output `Failed
        (Printf.sprintf "command %s exceeded %d-byte output limit%s"
           (Output.stream_name stream)
           limit note)
  | Mentat_workspace_io.Command.Supervision_failed error ->
      Mentat_tool.Result.failed ~output `Failed
        (Format.asprintf "command supervision failed: %a%s" Eio.Exn.pp_err error
           note)

let run_command workspace_io ~clock ~shell ~escalation ~cancelled input workdir
    timeout_ms =
  let argv = shell_argv shell input.Input.command in
  let capture =
    Mentat_workspace_io.Command.Head_tail
      { head = capture_head_bytes; tail = capture_tail_bytes }
  in
  let timeout =
    Eio.Time.Timeout.seconds clock (float_of_int timeout_ms /. 1_000.)
  in
  let result =
    match route escalation input with
    | Ordinary ->
        Mentat_workspace_io.Command.run workspace_io ~cwd:workdir ~capture
          ~timeout ~cancelled argv
    | Escalated | Escalation_denied ->
        Mentat_workspace_io.Command.run_escalated workspace_io ~cwd:workdir
          ~capture ~timeout ~cancelled argv
  in
  match result with
  | Error error ->
      Mentat_tool.Result.failed
        (command_error_failure error)
        (command_error_message error)
  | Ok outcome ->
      Output.make ~input ~workdir ~timeout_ms outcome
      |> result_of_output
      |> Mentat_tool.Result.map Output.encode

(* Background start receipt: the handle and pid live here, in the model-visible
   output, not in any durable fact — a resume finds no such handle. *)
let background_receipt ~handle ~pid ~command ~workdir =
  let text =
    Printf.sprintf
      "Started background command: %s\n\
       Workdir: %s\n\
       Handle: %s (pid %d)\n\
       Read its output with shell_output(handle=\"%s\"), which waits for the \
       command to say something, and stop it with shell_kill(handle=\"%s\"). \
       It keeps running across turns until it exits or is killed. The kill \
       reaches this process and the workers it forked; one that ignores the \
       graceful signal and outlives it may keep running."
      command
      (Mentat_workspace.Path.display workdir)
      handle pid handle handle
  in
  let json =
    Codec.obj [ ("handle", json_string handle); ("pid", json_int pid) ]
  in
  Mentat_tool.Output.make ~text ~json ()

(* A background command that requests escalation is refused with a message that
   steers the model to a route that works, rather than silently confining it: an
   escalated profile that outlives the turn is the open sandbox question v1 does
   not pre-empt. *)
let background_escalation_refused () =
  Mentat_tool.Result.failed `Invalid_input
    "background commands run confined in this version. To use escalation, \
     either run this command in the foreground (background=false) with \
     escalate=true, or start it in the background without escalate."

(* A background start is a short call: resolve the workdir, spawn through the
   registry under the ordinary confined route, and return a receipt at once.
   [timeout_ms] is ignored; the process outlives the call. *)
let run_background workspace_io ~registry ~shell input =
  match registry with
  | None ->
      Mentat_tool.Result.failed `Unavailable
        "background execution is not available in this session"
  | Some registry -> (
      match resolve_workdir workspace_io input with
      | Error message -> Mentat_tool.Result.failed `Invalid_input message
      | Ok workdir -> (
          let argv = shell_argv shell input.Input.command in
          match
            Registry.start registry workspace_io ~label:input.Input.command
              ~cwd:workdir argv
          with
          | Ok started ->
              Mentat_tool.Result.completed
                ~output:
                  (background_receipt ~handle:started.Registry.handle
                     ~pid:started.Registry.pid ~command:input.Input.command
                     ~workdir)
                ()
          | Error (Registry.At_capacity { running }) ->
              Mentat_tool.Result.failed `Unavailable
                (Printf.sprintf
                   "cannot start another background command: %d already \
                    running (%s); stop one with shell_kill first"
                   (List.length running)
                   (String.concat ", " running))
          | Error (Registry.Refused error) ->
              Mentat_tool.Result.failed
                (command_error_failure error)
                (command_error_message error)))

let interrupted () = Mentat_tool.Result.cancelled ()

let escalation_denied () =
  Mentat_tool.Result.failed `Invalid_input
    (Mentat_sandbox.Error.message Mentat_sandbox.Error.Escalation_denied)

let run workspace_io ~clock ~shell ~registry ~escalation ~cancelled input =
  if cancelled () then interrupted ()
  else if input.Input.background then
    if input.Input.escalate then background_escalation_refused ()
    else run_background workspace_io ~registry ~shell input
  else
    let timeout_ms = Input.effective_timeout_ms input in
    if timeout_ms > max_timeout_ms then
      Mentat_tool.Result.failed `Invalid_input
        ("timeout_ms must be <= " ^ string_of_int max_timeout_ms)
    else
      match route escalation input with
      | Escalation_denied -> escalation_denied ()
      | Ordinary | Escalated -> (
          match resolve_workdir workspace_io input with
          | Error message -> Mentat_tool.Result.failed `Invalid_input message
          | Ok workdir ->
              run_command workspace_io ~clock ~shell ~escalation ~cancelled
                input workdir timeout_ms)

let make ?registry workspace_io ~clock ~shell =
  if String.is_empty shell then invalid_arg "shell must not be empty";
  if String.contains shell '\000' then invalid_arg "shell must not contain NUL";
  let ordinary_execution = Confinement.confined workspace_io in
  let escalation = Mentat_workspace_io.escalation workspace_io in
  Mentat_tool.make ~name ~description:Mentat_prompts.Tools.shell
    ~input:Input.contract ~output:Fun.id
    ~permissions:(permissions workspace_io ~ordinary_execution ~escalation)
    ~run:(fun ~cancelled input ->
      run workspace_io ~clock ~shell ~registry ~escalation ~cancelled input)
    ()

module Registry = Registry
module Shell_output = Shell_output
module Shell_kill = Shell_kill
