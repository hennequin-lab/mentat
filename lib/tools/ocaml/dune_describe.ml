(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let max_detail_bytes = 4 * 1024
let max_output_bytes = 64 * 1024

module Project = Mentat_ocaml.Project
module Workspace = Mentat_workspace

let name = "ocaml_dune_describe"
let command_timeout_seconds = 30.

(* The per-group listing cap. It exists so one enormous group cannot crowd the
   other out of the byte budget, not to summarize: a project's libraries and
   tests are the inventory this tool is asked for, and answering a 77-test
   project with 20 names and a count leaves the caller to find the rest some
   other way. At this width both groups fit whole for any project of ordinary
   size, and {!max_output_bytes} remains the bound that actually protects the
   context window. *)
let max_display_items = 200

module Input = struct
  let object_codec =
    Jsont.Object.map ~kind:"ocaml_dune_describe input" ()
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec =
    Codec.strict_object ~kind:"strict ocaml_dune_describe input" object_codec

  let contract =
    Mentat_tool.Input.make codec ~schema:Mentat_llm.Tool.no_input_schema
end

let component_kind_text = function
  | Project.Component.Kind.Local_library -> "local library"
  | Project.Component.Kind.External_library -> "external library"
  | Project.Component.Kind.Executable -> "executable"

let component_line component =
  Printf.sprintf "- %s %s (id=%s)"
    (component_kind_text (Project.Component.kind component))
    (Project.Component.name component)
    (Project.Component.Id.to_string (Project.Component.id component))

let test_line test =
  let status = if Project.Test.enabled test then "enabled" else "disabled" in
  Printf.sprintf "- %s: %s in %s (%s)" (Project.Test.name test)
    (Project.Test.target test)
    (Workspace.Path.display (Project.Test.source_dir test))
    status

let add_page buffer ~heading ~more_label ~line items =
  match List.take max_display_items items with
  | [] -> ()
  | shown ->
      Buffer.add_string buffer ("\n" ^ heading ^ ":\n");
      List.iter
        (fun item ->
          Buffer.add_string buffer (line item);
          Buffer.add_char buffer '\n')
        shown;
      let remaining = List.length items - List.length shown in
      if remaining > 0 then
        Printf.bprintf buffer "- ... %d more %s(s)\n" remaining more_label

let project_text project =
  let components = Project.components project in
  let local_components = Project.local_components project in
  let external_components = Project.external_components project in
  let tests = Project.tests project in
  let buffer = Buffer.create 512 in
  Buffer.add_string buffer "OCaml Dune project\n";
  Printf.bprintf buffer "root: %s\n"
    (match Project.root project with
    | None -> "<unknown>"
    | Some root -> Workspace.Path.display root);
  Printf.bprintf buffer "build_context: %s\n"
    (Option.value ~default:"<unknown>" (Project.build_context project));
  Printf.bprintf buffer "components: %d local=%d external=%d\n"
    (List.length components)
    (List.length local_components)
    (List.length external_components);
  Printf.bprintf buffer "tests: %d\n" (List.length tests);
  add_page buffer ~heading:"local components" ~more_label:"local component"
    ~line:component_line local_components;
  add_page buffer ~heading:"tests" ~more_label:"test" ~line:test_line tests;
  String.trim (Buffer.contents buffer)

let encode_output project =
  let local_components = Project.local_components project in
  let tests = Project.tests project in
  let text = project_text project |> Text_helpers.utf8_lossy in
  let text, byte_truncated =
    if String.length text <= max_output_bytes then (text, false)
    else (Text_helpers.valid_utf8_prefix text max_output_bytes, true)
  in
  let semantic =
    Mentat_tools_output.Ocaml.Project.make
      ~components:(List.length (Project.components project))
      ~tests:(List.length tests)
  in
  Mentat_tools_output.Codec.encode Mentat_tools_output.Ocaml.Project.jsont ~text
    ~truncated:
      (byte_truncated
      || List.length local_components > max_display_items
      || List.length tests > max_display_items)
    semantic

let command_error_message error =
  Mentat_workspace_io.Command.Error.message error
  |> Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes

let command_error_failure = function
  | Mentat_workspace_io.Command.Error.Sandbox
      ( Mentat_sandbox.Error.Empty_program | Mentat_sandbox.Error.Nul_in_argv _
      | Mentat_sandbox.Error.Grant_denied _
      | Mentat_sandbox.Error.Grant_not_a_directory _ )
  | Mentat_workspace_io.Command.Error.Unknown_cwd_root _ ->
      `Invalid_input
  | Mentat_workspace_io.Command.Error.Sandbox
      ( Mentat_sandbox.Error.Escalation_denied
      | Mentat_sandbox.Error.Escalation_irrelevant
      | Mentat_sandbox.Error.Unavailable _
      | Mentat_sandbox.Error.Cwd_outside_scope _
      | Mentat_sandbox.Error.Stale_policy _ )
  | Mentat_workspace_io.Command.Error.Spawn _
  | Mentat_workspace_io.Command.Error.Io _ ->
      `Unavailable

let interrupted () = Mentat_tool.Result.cancelled ()

let captured_text captured =
  Mentat_workspace_io.Command.Captured.render captured

let command_detail outcome =
  let stderr =
    captured_text outcome.Mentat_workspace_io.Command.stderr
    |> Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes
  in
  if String.is_empty stderr then
    captured_text outcome.Mentat_workspace_io.Command.stdout
    |> Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes
  else stderr

let stream_name = function
  | Mentat_workspace_io.Command.Stdout -> "stdout"
  | Mentat_workspace_io.Command.Stderr -> "stderr"

let command_args program describe_args =
  match describe_args with
  | [] -> assert false
  | _dune :: args -> program @ args

let run_describe workspace_io ~clock ~program ~cwd ~label ~args ~cancelled =
  let timeout = Eio.Time.Timeout.seconds clock command_timeout_seconds in
  match
    Mentat_workspace_io.Command.run workspace_io ~cwd
      ~capture:Mentat_workspace_io.Command.All ~timeout ~cancelled
      (command_args program args)
  with
  | Error error ->
      Error
        (Mentat_tool.Result.failed
           (command_error_failure error)
           (label ^ " launch failed: " ^ command_error_message error))
  | Ok outcome -> (
      match outcome.Mentat_workspace_io.Command.termination with
      | Mentat_workspace_io.Command.Stopped -> Error (interrupted ())
      | Mentat_workspace_io.Command.Timed_out ->
          Error
            (Mentat_tool.Result.failed `Timed_out
               (Printf.sprintf "%s timed out after %.0fms" label
                  (command_timeout_seconds *. 1000.)))
      | Mentat_workspace_io.Command.Output_limit { stream; limit } ->
          Error
            (Mentat_tool.Result.failed `Failed
               (Printf.sprintf "%s %s exceeded %d-byte output limit" label
                  (stream_name stream) limit))
      | Mentat_workspace_io.Command.Supervision_failed error ->
          Error
            (Mentat_tool.Result.failed `Failed
               (label ^ " supervision failed: "
               ^ (Format.asprintf "%a" Eio.Exn.pp_err error
                 |> Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes
                 )))
      | Mentat_workspace_io.Command.Exited (`Signaled signal) ->
          Error
            (Mentat_tool.Result.failed `Failed
               (Printf.sprintf "%s terminated by signal %d" label signal))
      | Mentat_workspace_io.Command.Exited (`Exited code) ->
          if code <> 0 then
            let detail = command_detail outcome in
            (* This spawns dune, so it is the cold-cache case: a confined
               refusal here reached the model as an unexplained nonzero exit. *)
            let note =
              Confinement.denial_note ~widening:`Through_shell
                outcome.Mentat_workspace_io.Command.confinement
            in
            Error
              (Mentat_tool.Result.failed `Failed
                 ((if String.is_empty detail then
                     Printf.sprintf "%s exited with status %d" label code
                   else detail)
                 ^ note))
          else if
            not
              (Mentat_workspace_io.Command.Captured.is_complete
                 outcome.Mentat_workspace_io.Command.stdout)
          then
            Error
              (Mentat_tool.Result.failed `Failed
                 (label ^ " stdout was incomplete after the child exited"))
          else if
            not
              (Mentat_workspace_io.Command.Captured.is_complete
                 outcome.Mentat_workspace_io.Command.stderr)
          then
            Error
              (Mentat_tool.Result.failed `Failed
                 (label ^ " stderr was incomplete after the child exited"))
          else Ok (captured_text outcome.Mentat_workspace_io.Command.stdout))

type context = { root_path : Workspace.Path.t; workspace : Workspace.t }

let context workspace_io =
  let current = Mentat_workspace_io.cwd workspace_io in
  let root_path = Workspace.Path.root_of current in
  match Mentat_workspace_io.to_abs workspace_io root_path with
  | Error error -> Error (Workspace.Resolve_error.message error)
  | Ok root_dir ->
      let root =
        Workspace.Root.make ~key:(Workspace.Path.root_key current) root_dir
      in
      Ok
        {
          root_path;
          workspace = Workspace.single ~cwd:(Workspace.Path.rel current) root;
        }

let run workspace_io ~clock ~program ~cancelled () =
  if cancelled () then interrupted ()
  else
    match context workspace_io with
    | Error diagnostic -> Mentat_tool.Result.failed `Failed diagnostic
    | Ok context -> (
        match
          run_describe workspace_io ~clock ~program ~cwd:context.root_path
            ~label:"dune describe workspace"
            ~args:(Mentat_ocaml_dune_describe.workspace_args ())
            ~cancelled
        with
        | Error result -> result
        | Ok _ when cancelled () -> interrupted ()
        | Ok workspace_output -> (
            match
              run_describe workspace_io ~clock ~program ~cwd:context.root_path
                ~label:"dune describe tests"
                ~args:(Mentat_ocaml_dune_describe.tests_args ())
                ~cancelled
            with
            | Error result -> result
            | Ok _ when cancelled () -> interrupted ()
            | Ok tests_output -> (
                let decoded =
                  Mentat_ocaml_dune_describe.of_outputs
                    ~workspace:context.workspace ~workspace_output ~tests_output
                in
                if cancelled () then interrupted ()
                else
                  match decoded with
                  | Error error ->
                      Mentat_tool.Result.failed `Failed
                        (Text_helpers.bounded_diagnostic
                           ~max_bytes:max_detail_bytes
                           ("could not decode dune describe output: "
                           ^ Mentat_ocaml_dune_describe.Error.message error))
                  | Ok project ->
                      Mentat_tool.Result.completed ~output:project ())))

let permissions workspace_io ~execution () =
  match context workspace_io with
  | Error _ -> []
  | Ok context ->
      [
        Mentat_permission.Request.of_accesses ~source:name
          [
            Mentat_permission.Access.path ~op:`Read context.root_path;
            Confinement.custom_access execution;
          ];
      ]

let validate_program program =
  if List.is_empty program then
    invalid_arg "Ocaml.Dune_describe.make: program prefix must not be empty";
  List.iteri
    (fun index token ->
      if String.is_empty token then
        invalid_arg
          ("Ocaml.Dune_describe.make: program token " ^ string_of_int index
         ^ " must not be empty");
      if String.contains token '\x00' then
        invalid_arg
          ("Ocaml.Dune_describe.make: program token " ^ string_of_int index
         ^ " must not contain NUL"))
    program

let make workspace_io ~clock ~program =
  validate_program program;
  let execution = Confinement.confined workspace_io in
  Mentat_tool.make ~name ~description:Mentat_prompts.Tools.ocaml_dune_describe
    ~input:Input.contract ~output:encode_output
    ~permissions:(permissions workspace_io ~execution)
    ~run:(run workspace_io ~clock ~program)
    ()
