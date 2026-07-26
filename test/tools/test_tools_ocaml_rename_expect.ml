(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for the staged semantic rename tool.
   Effectful cases enter through provider call decoding and use a real WIO
   capability plus a real scripted Merlin protocol peer. Prepared values cross
   their durable codec before execution; mutation is observed only through the
   claim scope owned by WIO. *)

open Windtrap
open Test_tools_support
module Json = Jsont.Json
module Tool = Mentat_tool
module Rename = Mentat_tools.Ocaml.Rename
module Rename_output = Mentat_tools_output.Ocaml.Rename
module Wio = Mentat_workspace_io
module Workspace = Mentat_workspace
module Permission = Mentat_permission
module Access = Permission.Access

type world = {
  sw : Eio.Switch.t;
  ws_dir : string;
  aux_dir : string;
  outside_dir : string;
  plan_dir : string;
  io : Wio.t;
  tool : Tool.t;
  make_tool : string list -> Tool.t;
}

let json_array values = Json.list values

let add_optional name encode value fields =
  match value with
  | None -> fields
  | Some value -> (name, encode value) :: fields

let input ?dry_run ?max_occurrences ~path ~line ~column ~new_name () =
  [
    ("path", Json.string path);
    ("line", Json.int line);
    ("column", Json.int column);
    ("new_name", Json.string new_name);
  ]
  |> add_optional "dry_run" (fun value -> Json.bool value) dry_run
  |> add_optional "max_occurrences"
       (fun value -> Json.int value)
       max_occurrences
  |> List.rev |> json_object

let member name = function
  | Jsont.Object (members, _) -> (
      match Json.find_mem name members with
      | Some (_, value) -> value
      | None -> failf "no %S member" name)
  | _ -> fail "expected a JSON object"

let member_names = function
  | Jsont.Object (members, _) -> List.map (fun ((name, _), _) -> name) members
  | _ -> fail "expected a JSON object"

let rec all_member_names = function
  | Jsont.Object (members, _) ->
      List.concat_map
        (fun ((name, _), value) -> name :: all_member_names value)
        members
  | Jsont.Array (values, _) -> List.concat_map all_member_names values
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _ -> []

let set_member name value = function
  | Jsont.Object (members, meta) ->
      let found = ref false in
      let members =
        List.map
          (fun (((current, _) as key), previous) ->
            if String.equal current name then begin
              found := true;
              (key, value)
            end
            else (key, previous))
          members
      in
      if not !found then failf "no %S member to set" name;
      Jsont.Object (members, meta)
  | _ -> fail "expected a JSON object"

let add_member name value = function
  | Jsont.Object (members, meta) ->
      Jsont.Object (members @ [ Json.mem (Json.name name) value ], meta)
  | _ -> fail "expected a JSON object"

let map_member name fn json = set_member name (fn (member name json)) json

let map_array_values fn = function
  | Jsont.Array (values, meta) -> Jsont.Array (fn values, meta)
  | _ -> fail "expected a JSON array"

let map_first_array_value fn =
  map_array_values (function
    | value :: values -> fn value :: values
    | [] -> fail "expected a non-empty JSON array")

let reorder_members = function
  | Jsont.Object (members, meta) -> Jsont.Object (List.rev members, meta)
  | _ -> fail "expected a JSON object"

let contains_substring text substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec loop offset =
    if offset + substring_length > text_length then false
    else if String.equal (String.sub text offset substring_length) substring
    then true
    else loop (offset + 1)
  in
  loop 0

let print_status ?(normalize = Fun.id) result =
  match Tool.Result.status result with
  | Tool.Result.Completed -> print_endline "status: completed"
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf "status: failed %s\nmessage: %s\nmetadata: %b\n"
        (failure_name kind) (normalize message) (Option.is_some metadata)
  | Tool.Result.Interrupted { reason; cancelled } ->
      Printf.printf "status: interrupted cancelled=%b\nreason: %s\n" cancelled
        reason

let output_json result =
  match Tool.Output.json (output_exn result) with
  | Some json -> json
  | None -> fail "rename output carried no JSON projection"

let decode_call_result tool provider_input =
  Tool.Call.decode tool (model_call tool provider_input)

let decode_call tool provider_input =
  match decode_call_result tool provider_input with
  | Ok call -> call
  | Error error ->
      failf "call decode failed: %s" (Tool.Call.Decode_error.message error)

let prepared = function
  | Tool.Call.Prepared prepared -> prepared
  | Tool.Call.Finished result ->
      print_status result;
      fail "rename unexpectedly finished during preparation"

let prepare ?(cancelled = fun () -> false) world provider_input =
  Tool.Call.run (decode_call world.tool provider_input) ~cancelled |> prepared

let run_direct ?(cancelled = fun () -> false) world provider_input =
  Tool.Call.run (decode_call world.tool provider_input) ~cancelled |> finished

let reload_prepared ?tamper prepared =
  let envelope = encode Tool.Prepared.jsont prepared in
  let envelope = match tamper with None -> envelope | Some f -> f envelope in
  decode Tool.Prepared.jsont envelope

let resume_call tool provider_input prepared =
  match Tool.Call.resume (decode_call tool provider_input) prepared with
  | Ok call -> call
  | Error error ->
      failf "resume failed: %s" (Tool.Call.Resume_error.message error)

let resume ?(cancelled = fun () -> false) world provider_input prepared =
  Tool.Call.run (resume_call world.tool provider_input prepared) ~cancelled
  |> finished

let resume_error_name = function
  | Tool.Call.Resume_error.Not_staged -> "not_staged"
  | Tool.Call.Resume_error.Tool_mismatch -> "tool_mismatch"
  | Tool.Call.Resume_error.Input_mismatch -> "input_mismatch"
  | Tool.Call.Resume_error.Invalid_prepared _ -> "invalid_prepared"
  | Tool.Call.Resume_error.Prepared_drift -> "prepared_drift"
  | Tool.Call.Resume_error.Permission_drift -> "permission_drift"

let print_resume world provider_input prepared =
  match Tool.Call.resume (decode_call world.tool provider_input) prepared with
  | Ok call ->
      Printf.printf "resume: accepted stage=%s requests=%d\n"
        (Tool.Stage.to_string (Tool.Call.stage call))
        (List.length (Tool.Call.permissions call))
  | Error error ->
      Printf.printf "resume: rejected %s diagnostic=%b\n"
        (resume_error_name error)
        (not (String.is_empty (Tool.Call.Resume_error.message error)))

let print_resume_case label world provider_input prepared =
  Printf.printf "-- %s --\n" label;
  print_resume world provider_input prepared

let abs path = Lpath.Abs.of_string_exn path

let write_disk path contents =
  mkdir_p (Filename.dirname path);
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel contents)

let workspace ws_dir aux_dir =
  let primary = Workspace.Root.make ~key:(root_key "main") (abs ws_dir) in
  let auxiliary = Workspace.Root.make ~key:(root_key "aux") (abs aux_dir) in
  let cwd = Workspace.Path.make ~root_key:(root_key "main") (rel "logical") in
  match Workspace.make ~cwd ~primary ~read_only:[ auxiliary ] () with
  | Ok workspace -> workspace
  | Error error ->
      failf "workspace construction failed: %a" Workspace.Error.pp error

let fake_merlin =
  String.concat "\n"
    [
      "#!/bin/sh";
      "plan=$1";
      "shift";
      "count_file=$plan/count";
      "if [ -f \"$count_file\" ]; then n=$(cat \"$count_file\"); else n=0; fi";
      "n=$((n + 1))";
      "printf '%s' \"$n\" > \"$count_file\"";
      "pwd > \"$plan/cwd-$n\"";
      "printf '%s\\n' \"$@\" > \"$plan/argv-$n\"";
      "cat > \"$plan/stdin-$n\"";
      "if [ \"$HOME\" = \"$TMPDIR\" ]; then scratch=no; else scratch=no; fi";
      "printf 'scratch=%s\\nsecret=%s\\n' \"$scratch\" \
       \"${MENTAT_RENAME_EXPECT_SECRET-unset}\" > \"$plan/env-$n\"";
      "behavior=response";
      "if [ -f \"$plan/behavior-$n\" ]; then behavior=$(cat \
       \"$plan/behavior-$n\"); fi";
      ": > \"$plan/started-$n\"";
      "case \"$behavior\" in";
      "  response) cat \"$plan/response-$n\"; [ ! -f \"$plan/cancel-after-$n\" \
       ] || : > \"$plan/cancel-now\" ;;";
      "  exit7) printf '\\033[31mmerlin failed\\033[0m\\n' >&2; exit 7 ;;";
      "  invalid_exit) printf '\\377bad\\033]0;secret\\007 detail\\n' >&2; \
       exit 9 ;;";
      "  signal) kill -TERM $$ ;;";
      "  overflow) exec /usr/bin/yes x ;;";
      "  sleep) exec /bin/sleep 60 ;;";
      "  incomplete) ( /bin/sleep 3 ) & cat \"$plan/response-$n\" ;;";
      "  *) printf 'unknown behavior: %s\\n' \"$behavior\" >&2; exit 64 ;;";
      "esac";
      "";
    ]

let with_world_using ?clock name fn =
  Eio_main.run @@ fun stdenv ->
  let stdenv = (stdenv :> Eio_unix.Stdenv.base) in
  let raw = Filename.temp_dir ("mentat-rename-" ^ name ^ "-") "" in
  Fun.protect ~finally:(fun () -> remove_tree raw) @@ fun () ->
  let base = Unix.realpath raw in
  let ws_dir = Filename.concat base "workspace" in
  let aux_dir = Filename.concat base "auxiliary" in
  let outside_dir = Filename.concat base "outside" in
  let tmp_dir = Filename.concat base "tmp" in
  let plan_dir = Filename.concat ws_dir ".rename-plan" in
  List.iter mkdir [ ws_dir; aux_dir; outside_dir; tmp_dir; plan_dir ];
  write_disk
    (Filename.concat ws_dir "logical/main.ml")
    "let target = 42\nlet use = target\n";
  write_disk (Filename.concat ws_dir "logical/other.mli") "val target : int\n";
  write_disk
    (Filename.concat ws_dir "lib/second.ml")
    "let result = target + target\n";
  write_disk (Filename.concat aux_dir "aux.ml") "let target = 1\n";
  write_disk (Filename.concat outside_dir "outside.ml") "let target = 2\n";
  let fake = Filename.concat ws_dir "fake-ocamlmerlin" in
  install_executable fake fake_merlin;
  let saved_tmpdir = Sys.getenv_opt "TMPDIR" in
  let saved_secret = Sys.getenv_opt "MENTAT_RENAME_EXPECT_SECRET" in
  Unix.putenv "TMPDIR" tmp_dir;
  Unix.putenv "MENTAT_RENAME_EXPECT_SECRET" "ambient-secret";
  Fun.protect ~finally:(fun () ->
      (match saved_tmpdir with
      | None -> Unix.unsetenv "TMPDIR"
      | Some value -> Unix.putenv "TMPDIR" value);
      match saved_secret with
      | None -> Unix.unsetenv "MENTAT_RENAME_EXPECT_SECRET"
      | Some value -> Unix.putenv "MENTAT_RENAME_EXPECT_SECRET" value)
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let logical = workspace ws_dir aux_dir in
  let io = resolve_exn ~sw ~stdenv ~logical () in
  let tool, make_tool =
    match clock with
    | None ->
        let clock = Eio.Stdenv.mono_clock stdenv in
        let make_tool program = Rename.make io ~clock ~program in
        (make_tool [ fake; plan_dir ], make_tool)
    | Some clock ->
        let make_tool program = Rename.make io ~clock ~program in
        (make_tool [ fake; plan_dir ], make_tool)
  in
  fn { sw; ws_dir; aux_dir; outside_dir; plan_dir; io; tool; make_tool }

let with_world name fn = with_world_using name fn

let plan_file world stem index =
  Filename.concat world.plan_dir (Printf.sprintf "%s-%d" stem index)

let set_response world index response =
  write_disk (plan_file world "response" index) response

let set_behavior world index behavior =
  write_disk (plan_file world "behavior" index) behavior

let invocation_count world =
  match read_disk (Filename.concat world.plan_dir "count") with
  | text -> int_of_string text
  | exception Sys_error _ -> 0

let invocation world stem index = read_disk (plan_file world stem index)

let normalize world text =
  text
  |> String.replace_all ~sub:world.aux_dir ~by:"<auxiliary>"
  |> String.replace_all ~sub:world.outside_dir ~by:"<outside>"
  |> String.replace_all ~sub:world.ws_dir ~by:"<workspace>"

let position ~line ~column = Printf.sprintf {|{"line":%d,"col":%d}|} line column

let occurrence ?(file = "") ~sl ~sc ~el ~ec ?(stale = false) () =
  Printf.sprintf {|{"file":%s,"start":%s,"end":%s,"stale":%b}|}
    (json_string (Json.string file))
    (position ~line:sl ~column:sc)
    (position ~line:el ~column:ec)
    stale

let return value = Printf.sprintf {|{"class":"return","value":%s}|} value

let return_occurrences occurrences =
  return ("[" ^ String.concat "," occurrences ^ "]")

let standard_response =
  return_occurrences
    [
      occurrence ~sl:1 ~sc:4 ~el:1 ~ec:10 ();
      occurrence ~sl:2 ~sc:10 ~el:2 ~ec:16 ();
    ]

let multi_response world =
  let second = Filename.concat world.ws_dir "lib/second.ml" in
  return_occurrences
    [
      occurrence ~file:second ~sl:1 ~sc:22 ~el:1 ~ec:28 ();
      occurrence ~sl:2 ~sc:10 ~el:2 ~ec:16 ();
      occurrence ~sl:1 ~sc:4 ~el:1 ~ec:10 ();
      occurrence ~file:second ~sl:1 ~sc:13 ~el:1 ~ec:19 ();
      occurrence ~sl:2 ~sc:10 ~el:2 ~ec:16 ();
    ]

let provider_input ?dry_run ?max_occurrences () =
  input ?dry_run ?max_occurrences ~path:"main.ml" ~line:1 ~column:4
    ~new_name:"renamed" ()

let print_result ?(json = false) world result =
  print_status ~normalize:(normalize world) result;
  match Tool.Result.output result with
  | None -> ()
  | Some output ->
      Printf.printf "text: %S\ntruncated: %b\n"
        (normalize world (Tool.Output.text output))
        (Tool.Output.truncated output);
      if json then
        Printf.printf "json: %s\n"
          (match Tool.Output.json output with
          | None -> "none"
          | Some value -> normalize world (json_string value))

let print_permissions ?(normalize = Fun.id) requests =
  Printf.printf "requests: %d\n" (List.length requests);
  List.iter
    (fun request ->
      Printf.printf "source: %s display: %s\n"
        (Option.value ~default:"none" (Permission.Request.source request))
        (Option.value ~default:"none" (Permission.Request.display request)
        |> normalize);
      List.iter
        (fun item ->
          let display =
            Option.value ~default:"none" (Permission.Request.Item.display item)
          in
          match Permission.Request.Item.access item with
          | Access.Path { op; scope = Access.Path_scope.Workspace path } ->
              Printf.printf "path: %s key=%s address=%s display=%s change=%b\n"
                (path_op_name op)
                (Workspace.Root.Key.to_string (Workspace.Path.root_key path))
                (Workspace.Path.display path)
                (normalize display)
                (Option.is_some (Permission.Request.Item.change item))
          | Access.Custom { name; subject } ->
              Printf.printf "custom: name=%s subject=%s display=%s change=%b\n"
                name
                (Option.value ~default:"none" subject)
                (normalize display)
                (Option.is_some (Permission.Request.Item.change item))
          | access -> failf "unexpected rename permission: %a" Access.pp access)
        (Permission.Request.items request))
    requests

let close_scope scope =
  let evidence = Wio.Claim_scope.close scope in
  Printf.printf "claim applies: %d\n"
    (List.length evidence.Mentat_edit.Apply_evidence.applies);
  List.iteri
    (fun index apply ->
      match apply with
      | Mentat_edit.Apply_evidence.Applied result ->
          Printf.printf "claim apply %d: applied entries=%d\n" (index + 1)
            (List.length (Mentat_edit.Result.entries result))
      | Mentat_edit.Apply_evidence.Attempted attempt ->
          Printf.printf "claim apply %d: attempted applied=%d uncertain=%b\n"
            (index + 1)
            (List.length attempt.Mentat_edit.Apply_evidence.Attempt.applied)
            (Option.is_some attempt.Mentat_edit.Apply_evidence.Attempt.uncertain))
    evidence.Mentat_edit.Apply_evidence.applies;
  Printf.printf "claim observations: %d\n"
    (List.length evidence.Mentat_edit.Apply_evidence.observed)

let decode_verdict tool label provider_input =
  match decode_call_result tool provider_input with
  | Ok call ->
      Printf.printf "%s: accepted canonical=%s\n" label
        (json_string (Tool.Call.input call))
  | Error error ->
      Printf.printf "%s: rejected diagnostic=%b\n" label
        (not (String.is_empty (Tool.Call.Decode_error.message error)))

let decode_rejects codec json =
  match Json.decode codec json with
  | Ok _ -> false
  | Error diagnostic -> not (String.is_empty diagnostic)

let semantic output =
  match Mentat_tools_output.Codec.decode Rename_output.jsont output with
  | Some value -> value
  | None -> fail "compact rename output did not decode"

let disposition_name = function
  | Rename_output.Applied -> "applied"
  | Rename_output.Previewed -> "previewed"

let summary_from_semantic result =
  let action =
    match Rename_output.disposition result with
    | Rename_output.Applied -> "Renamed"
    | Rename_output.Previewed -> "Would rename"
  in
  let count n singular plural =
    Printf.sprintf "%d %s" n (if n = 1 then singular else plural)
  in
  Printf.sprintf "%s %s in %s" action
    (count (Rename_output.occurrences result) "occurrence" "occurrences")
    (count (Rename_output.files result) "file" "files")

let%expect_test "declaration provider decoding and constructor are strict" =
  with_world "schema" @@ fun world ->
  let declaration = Tool.declaration world.tool in
  Printf.printf "name: %s\n" (Mentat_llm.Tool.name declaration);
  Printf.printf "schema: %s\n"
    (json_string (Mentat_llm.Tool.input_schema declaration));
  decode_verdict world.tool "minimal" (provider_input ());
  decode_verdict world.tool "full"
    (provider_input ~dry_run:true ~max_occurrences:1_000 ());
  List.iter
    (fun (label, provider_input) ->
      decode_verdict world.tool label provider_input)
    [
      ( "missing new_name",
        json_object
          [
            ("path", Json.string "main.ml");
            ("line", Json.int 1);
            ("column", Json.int 4);
          ] );
      ( "unknown member",
        add_member "scope" (Json.string "project") (provider_input ()) );
      ( "duplicate path",
        add_member "path" (Json.string "other.ml") (provider_input ()) );
      ("empty path", input ~path:"" ~line:1 ~column:0 ~new_name:"x" ());
      ("NUL path", input ~path:"bad\000path" ~line:1 ~column:0 ~new_name:"x" ());
      ("empty name", input ~path:"main.ml" ~line:1 ~column:0 ~new_name:"" ());
      ("NUL name", input ~path:"main.ml" ~line:1 ~column:0 ~new_name:"x\000y" ());
      ("line zero", input ~path:"main.ml" ~line:0 ~column:0 ~new_name:"x" ());
      ( "column negative",
        input ~path:"main.ml" ~line:1 ~column:(-1) ~new_name:"x" () );
      ("line string", set_member "line" (Json.string "1") (provider_input ()));
      ( "column fraction",
        set_member "column" (Json.number 4.5) (provider_input ()) );
      ( "line unsafe",
        set_member "line"
          (Json.number 9_007_199_254_740_992.)
          (provider_input ()) );
      ( "column infinity",
        set_member "column" (Json.number Float.infinity) (provider_input ()) );
      ( "dry_run type",
        set_member "dry_run" (Json.string "yes")
          (provider_input ~dry_run:true ()) );
      ("cap zero", provider_input ~max_occurrences:0 ());
      ("cap high", provider_input ~max_occurrences:1_001 ());
    ];
  let empty_program_rejected =
    match world.make_tool [] with
    | _ -> false
    | exception Invalid_argument _ -> true
  in
  Printf.printf "empty program rejected: %b\n" empty_program_rejected;
  [%expect
    {|
    name: ocaml_rename
    schema: {"type":"object","properties":{"path":{"type":"string","description":"Workspace-relative or workspace-contained absolute OCaml source file path.","minLength":1},"line":{"type":"integer","description":"One-based source line of the identifier cursor.","minimum":1,"maximum":9007199254740991},"column":{"type":"integer","description":"Zero-based byte column in the source line, matching OCaml and Merlin locations.","minimum":0,"maximum":9007199254740991},"new_name":{"type":"string","description":"Replacement identifier. Its lowercase-value or uppercase-constructor/module class must match the entity under the cursor.","minLength":1},"dry_run":{"type":"boolean","description":"Validate and report the complete rename without writing. Defaults to false."},"max_occurrences":{"type":"integer","description":"Safety cap on deduplicated occurrences. Exceeding it refuses the rename. Defaults to 200.","minimum":1,"maximum":1000}},"required":["path","line","column","new_name"],"additionalProperties":false}
    minimal: accepted canonical={"column":4,"line":1,"new_name":"renamed","path":"main.ml"}
    full: accepted canonical={"column":4,"dry_run":true,"line":1,"max_occurrences":1000,"new_name":"renamed","path":"main.ml"}
    missing new_name: rejected diagnostic=true
    unknown member: rejected diagnostic=true
    duplicate path: rejected diagnostic=true
    empty path: rejected diagnostic=true
    NUL path: rejected diagnostic=true
    empty name: rejected diagnostic=true
    NUL name: rejected diagnostic=true
    line zero: rejected diagnostic=true
    column negative: rejected diagnostic=true
    line string: rejected diagnostic=true
    column fraction: rejected diagnostic=true
    line unsafe: rejected diagnostic=true
    column infinity: rejected diagnostic=true
    dry_run type: rejected diagnostic=true
    cap zero: rejected diagnostic=true
    cap high: rejected diagnostic=true
    empty program rejected: true |}]

let%expect_test "initial authority and Merlin transport are exact and private" =
  with_world "transport" @@ fun world ->
  let call = decode_call world.tool (provider_input ~dry_run:true ()) in
  print_endline "-- initial permissions --";
  print_permissions (Tool.Call.permissions call);
  Unix.unlink (Filename.concat world.ws_dir "logical/main.ml");
  print_endline "-- cached after source removal --";
  print_permissions (Tool.Call.permissions call);
  write_disk
    (Filename.concat world.ws_dir "logical/main.ml")
    "let target = 42\nlet use = target\n";
  set_response world 1 standard_response;
  let result = Tool.Call.run call ~cancelled:(fun () -> false) |> finished in
  print_endline "-- result --";
  print_result ~json:true world result;
  Printf.printf "cwd: %s\n"
    (normalize world (String.trim (invocation world "cwd" 1)));
  Printf.printf "argv: %S\n" (normalize world (invocation world "argv" 1));
  Printf.printf "stdin exact: %b\nenv: %S\n"
    (String.equal
       (invocation world "stdin" 1)
       "let target = 42\nlet use = target\n")
    (invocation world "env" 1);
  Printf.printf "invocations: %d\n" (invocation_count world);
  let aux_input =
    input
      ~path:(Filename.concat world.aux_dir "aux.ml")
      ~line:1 ~column:4 ~new_name:"renamed" ~dry_run:true ()
  in
  print_endline "-- auxiliary-root authority --";
  let aux_call = decode_call world.tool aux_input in
  print_permissions (Tool.Call.permissions aux_call);
  set_response world 2
    (return_occurrences [ occurrence ~sl:1 ~sc:4 ~el:1 ~ec:10 () ]);
  print_result world
    (Tool.Call.run aux_call ~cancelled:(fun () -> false) |> finished);
  Printf.printf "aux cwd: %s invocations=%d\n"
    (normalize world (String.trim (invocation world "cwd" 2)))
    (invocation_count world);
  [%expect
    {|
    -- initial permissions --
    requests: 1
    source: ocaml_rename display: none
    path: read key=main address=. display=none change=false
    custom: name=command.confinement subject=direct display=none change=false
    -- cached after source removal --
    requests: 1
    source: ocaml_rename display: none
    path: read key=main address=. display=none change=false
    custom: name=command.confinement subject=direct display=none change=false
    -- result --
    status: completed
    text: "OCaml rename: target -> renamed\napplied: false\noccurrences: 2 in 1 file(s)\nindex_status: unknown\nbackend: ocamlmerlin\n- main.ml: 2 occurrence(s)"
    truncated: false
    json: {"version":1,"disposition":"previewed","occurrences":2,"files":1}
    cwd: <workspace>
    argv: "single\noccurrences\n-identifier-at\n1:4\n-scope\nrenaming\n-filename\n<workspace>/logical/main.ml\n"
    stdin exact: true
    env: "scratch=no\nsecret=unset\n"
    invocations: 1
    -- auxiliary-root authority --
    requests: 1
    source: ocaml_rename display: none
    path: read key=aux address=. display=none change=false
    custom: name=command.confinement subject=direct display=none change=false
    status: failed invalid_input
    message: aux.ml: edit target belongs to a read-only workspace root
    metadata: false
    aux cwd: <auxiliary> invocations=2 |}]

let%expect_test
    "dry run validates sorted deduplicated multi-file spans without writes" =
  with_world "preview" @@ fun world ->
  set_response world 1 (multi_response world);
  let before_main =
    read_disk (Filename.concat world.ws_dir "logical/main.ml")
  in
  let before_second =
    read_disk (Filename.concat world.ws_dir "lib/second.ml")
  in
  let scope = Wio.open_claim_scope world.io in
  let result = run_direct world (provider_input ~dry_run:true ()) in
  print_result ~json:true world result;
  close_scope scope;
  Printf.printf "unchanged: main=%b second=%b\n"
    (String.equal before_main
       (read_disk (Filename.concat world.ws_dir "logical/main.ml")))
    (String.equal before_second
       (read_disk (Filename.concat world.ws_dir "lib/second.ml")));
  let semantics = semantic (output_exn result) in
  Printf.printf "compact: disposition=%s occurrences=%d files=%d summary=%S\n"
    (disposition_name (Rename_output.disposition semantics))
    (Rename_output.occurrences semantics)
    (Rename_output.files semantics)
    (summary_from_semantic semantics);
  with_world "preview-read-only-root" @@ fun roots ->
  let auxiliary = Filename.concat roots.aux_dir "aux.ml" in
  set_response roots 1
    (return_occurrences
       [
         occurrence ~file:auxiliary ~sl:1 ~sc:4 ~el:1 ~ec:10 ();
         occurrence ~sl:1 ~sc:4 ~el:1 ~ec:10 ();
         occurrence ~file:auxiliary ~sl:1 ~sc:4 ~el:1 ~ec:10 ();
       ]);
  let root_scope = Wio.open_claim_scope roots.io in
  print_endline "-- normalized multi-root set --";
  print_result roots (run_direct roots (provider_input ~dry_run:true ()));
  close_scope root_scope;
  [%expect
    {|
    status: completed
    text: "OCaml rename: target -> renamed\napplied: false\noccurrences: 4 in 2 file(s)\nindex_status: unknown\nbackend: ocamlmerlin\n- <workspace>/lib/second.ml: 2 occurrence(s)\n- main.ml: 2 occurrence(s)"
    truncated: false
    json: {"version":1,"disposition":"previewed","occurrences":4,"files":2}
    claim applies: 0
    claim observations: 0
    unchanged: main=true second=true
    compact: disposition=previewed occurrences=4 files=2 summary="Would rename 4 occurrences in 2 files"
    -- normalized multi-root set --
    status: failed invalid_input
    message: aux.ml: edit target belongs to a read-only workspace root
    metadata: false
    claim applies: 0
    claim observations: 0 |}]

let%expect_test
    "prepared envelope is closed canonical and final authority is target exact"
    =
  with_world "prepared" @@ fun world ->
  set_response world 1 (multi_response world);
  let provider_input = provider_input () in
  let payload = prepare world provider_input in
  Printf.printf "tool: %s\ndescription: %s\n"
    (Tool.Prepared.tool payload)
    (Tool.Prepared.description payload);
  print_permissions ~normalize:(normalize world)
    (Tool.Prepared.requests payload);
  let envelope = encode Tool.Prepared.jsont payload in
  Printf.printf "envelope fields: %s\n"
    (String.concat "," (List.sort String.compare (member_names envelope)));
  let plan = member "payload" envelope in
  Printf.printf "plan fields: %s\n"
    (String.concat "," (List.sort String.compare (member_names plan)));
  Printf.printf "all private fields: %s\n"
    (String.concat "," (List.sort_uniq String.compare (all_member_names plan)));
  let reloaded = reload_prepared payload in
  Printf.printf "roundtrip equal: %b\n" (Tool.Prepared.equal payload reloaded);
  print_resume world provider_input reloaded;
  let envelope_unknown = add_member "attempt" (Json.int 1) envelope in
  Printf.printf "closed envelope rejects unknown: %b\n"
    (decode_rejects Tool.Prepared.jsont envelope_unknown);
  [%expect
    {|
    tool: ocaml_rename
    description: Rename target to renamed at 4 occurrence(s) in 2 file(s)
    requests: 1
    source: ocaml_rename display: Rename target to renamed at 4 occurrence(s) in 2 file(s)
    path: modify key=main address=lib/second.ml display=<workspace>/lib/second.ml change=false
    path: modify key=main address=logical/main.ml display=main.ml change=false
    envelope fields: description,input,payload,requests,tool,version
    plan fields: new_name,old_name,targets,version
    all private fields: address,after,before,column,end,line,new_name,old_name,path,rel,root,spans,start,targets,version
    roundtrip equal: true
    resume: accepted stage=run requests=1
    closed envelope rejects unknown: true |}]

let%expect_test
    "resume rejects payload drift and request drift before execution" =
  with_world "tamper" @@ fun world ->
  set_response world 1 standard_response;
  let provider_input = provider_input () in
  let original = prepare world provider_input in
  let undecodable =
    reload_prepared
      ~tamper:(fun envelope ->
        set_member "payload" (Json.string "not-a-plan") envelope)
      original
  in
  print_resume_case "wrong payload type" world provider_input undecodable;
  let drifted =
    reload_prepared
      ~tamper:(fun envelope ->
        set_member "payload"
          (reorder_members (member "payload" envelope))
          envelope)
      original
  in
  print_resume_case "member-order drift" world provider_input drifted;
  let unknown_plan_member =
    reload_prepared
      ~tamper:(fun envelope ->
        set_member "payload"
          (add_member "bytes" (Json.string "forbidden")
             (member "payload" envelope))
          envelope)
      original
  in
  print_resume_case "unknown plan member" world provider_input
    unknown_plan_member;
  let wrong_plan_version =
    reload_prepared
      ~tamper:(fun envelope ->
        set_member "payload"
          (set_member "version" (Json.int 2) (member "payload" envelope))
          envelope)
      original
  in
  print_resume_case "plan version" world provider_input wrong_plan_version;
  let empty_targets =
    reload_prepared
      ~tamper:(fun envelope ->
        set_member "payload"
          (set_member "targets" (json_array []) (member "payload" envelope))
          envelope)
      original
  in
  print_resume_case "empty plan" world provider_input empty_targets;
  let request_drift =
    reload_prepared
      ~tamper:(fun envelope -> set_member "requests" (json_array []) envelope)
      original
  in
  print_resume_case "request drift" world provider_input request_drift;
  let wrong_input =
    input ~path:"main.ml" ~line:2 ~column:10 ~new_name:"renamed" ()
  in
  print_resume_case "input drift" world wrong_input original;
  Printf.printf "invocations: %d\n" (invocation_count world);
  [%expect
    {|
    -- wrong payload type --
    resume: rejected invalid_prepared diagnostic=true
    -- member-order drift --
    resume: rejected prepared_drift diagnostic=true
    -- unknown plan member --
    resume: rejected invalid_prepared diagnostic=true
    -- plan version --
    resume: rejected invalid_prepared diagnostic=true
    -- empty plan --
    resume: rejected invalid_prepared diagnostic=true
    -- request drift --
    resume: rejected permission_drift diagnostic=true
    -- input drift --
    resume: rejected input_mismatch diagnostic=true
    invocations: 1 |}]

let%expect_test "private plan codec rejects every persisted invariant breach" =
  with_world "plan-codec" @@ fun world ->
  set_response world 1 (multi_response world);
  let provider_input = provider_input () in
  let original = prepare world provider_input in
  let tamper_plan fn =
    reload_prepared
      ~tamper:(fun envelope -> map_member "payload" fn envelope)
      original
  in
  let tamper_first_target fn =
    tamper_plan (map_member "targets" (map_first_array_value fn))
  in
  let tamper_first_span fn =
    tamper_first_target (map_member "spans" (map_first_array_value fn))
  in
  let check label diagnostic prepared =
    match Tool.Call.resume (decode_call world.tool provider_input) prepared with
    | Error (Tool.Call.Resume_error.Invalid_prepared message) ->
        Printf.printf "%s: invalid_prepared invariant=%b\n" label
          (contains_substring message diagnostic)
    | Error error ->
        failf "%s: wrong resume error: %s" label
          (Tool.Call.Resume_error.message error)
    | Ok _ -> failf "%s: invalid plan was accepted" label
  in
  let reverse = map_array_values List.rev in
  let duplicate_first =
    map_array_values (function
      | value :: _ -> [ value; value ]
      | [] -> fail "expected a non-empty JSON array")
  in
  let position line column =
    json_object [ ("line", Json.int line); ("column", Json.int column) ]
  in
  let range column =
    json_object
      [ ("start", position 1 column); ("end", position 1 (column + 1)) ]
  in
  let too_many_spans =
    List.init (Rename.max_occurrences + 1) range |> json_array
  in
  let oversized_identity =
    Json.string
      ("sha256:" ^ String.make 64 '0' ^ ":"
      ^ string_of_int (Rename.max_file_bytes + 1))
  in
  check "fractional version" "expected an integer"
    (tamper_plan (map_member "version" (fun _ -> Json.number 1.5)));
  check "fractional position" "expected an integer"
    (tamper_first_span
       (map_member "start" (map_member "column" (fun _ -> Json.number 4.5))));
  check "unchanged names" "same as the current name"
    (tamper_plan (fun plan ->
         map_member "new_name" (fun _ -> member "old_name" plan) plan));
  check "empty address" "address must not be empty"
    (tamper_first_target (map_member "address" (fun _ -> Json.string "")));
  check "NUL address" "address must not contain NUL"
    (tamper_first_target
       (map_member "address" (fun _ -> Json.string "bad\000address")));
  check "empty spans" "spans must not be empty"
    (tamper_first_target (map_member "spans" (fun _ -> json_array [])));
  check "unordered spans" "spans must be strictly ordered"
    (tamper_first_target (map_member "spans" reverse));
  check "duplicate spans" "spans must be strictly ordered"
    (tamper_first_target (map_member "spans" duplicate_first));
  check "unchanged identity" "identities must differ"
    (tamper_first_target (fun target ->
         map_member "after" (fun _ -> member "before" target) target));
  check "oversized identity" "identity exceeds the file bound"
    (tamper_first_target (map_member "before" (fun _ -> oversized_identity)));
  check "duplicate targets" "targets must be strictly ordered"
    (tamper_plan (map_member "targets" duplicate_first));
  check "unordered targets" "targets must be strictly ordered"
    (tamper_plan (map_member "targets" reverse));
  check "too many occurrences" "total occurrences exceed"
    (tamper_first_target (map_member "spans" (fun _ -> too_many_spans)));
  Printf.printf "invocations: %d\n" (invocation_count world);
  [%expect
    {|
    fractional version: invalid_prepared invariant=true
    fractional position: invalid_prepared invariant=true
    unchanged names: invalid_prepared invariant=true
    empty address: invalid_prepared invariant=true
    NUL address: invalid_prepared invariant=true
    empty spans: invalid_prepared invariant=true
    unordered spans: invalid_prepared invariant=true
    duplicate spans: invalid_prepared invariant=true
    unchanged identity: invalid_prepared invariant=true
    oversized identity: invalid_prepared invariant=true
    duplicate targets: invalid_prepared invariant=true
    unordered targets: invalid_prepared invariant=true
    too many occurrences: invalid_prepared invariant=true
    invocations: 1 |}]

let%expect_test
    "D2 resume skips Merlin and one combined apply owns all mutation evidence" =
  with_world "apply" @@ fun world ->
  set_response world 1 (multi_response world);
  let provider_input = provider_input () in
  let payload = prepare world provider_input |> reload_prepared in
  let source = Filename.concat world.ws_dir "logical/main.ml" in
  write_disk source "let target = 42\nlet use = target\n";
  let scope = Wio.open_claim_scope world.io in
  let result = resume world provider_input payload in
  print_result ~json:true world result;
  close_scope scope;
  Printf.printf "invocations after resume: %d\n" (invocation_count world);
  Printf.printf "main: %S\nsecond: %S\n" (read_disk source)
    (read_disk (Filename.concat world.ws_dir "lib/second.ml"));
  let stored = encode result_codec result in
  let replayed = decode result_codec stored in
  let stored_again = encode result_codec replayed in
  let semantics = semantic (output_exn replayed) in
  Printf.printf
    "replay equal: %b compact fields=%s disposition=%s occurrences=%d files=%d \
     summary=%S\n"
    (Json.equal stored stored_again)
    (String.concat ","
       (List.sort String.compare (member_names (output_json replayed))))
    (disposition_name (Rename_output.disposition semantics))
    (Rename_output.occurrences semantics)
    (Rename_output.files semantics)
    (summary_from_semantic semantics);
  let invalid_compact occurrences files =
    json_object
      [
        ("version", Json.int 1);
        ("disposition", Json.string "applied");
        ("occurrences", Json.int occurrences);
        ("files", Json.int files);
      ]
  in
  Printf.printf
    "compact rejects: zero-occurrences=%b zero-files=%b files-over=%b\n"
    (decode_rejects Rename_output.jsont (invalid_compact 0 1))
    (decode_rejects Rename_output.jsont (invalid_compact 1 0))
    (decode_rejects Rename_output.jsont (invalid_compact 1 2));
  let constructor_rejects =
    match
      Rename_output.make ~disposition:Rename_output.Applied ~occurrences:0
        ~files:0
    with
    | _ -> false
    | exception Invalid_argument _ -> true
  in
  Printf.printf "compact constructor rejects zero: %b\n" constructor_rejects;
  [%expect
    {|
    status: completed
    text: "OCaml rename: target -> renamed\napplied: true\noccurrences: 4 in 2 file(s)\nindex_status: unknown\nbackend: ocamlmerlin\n- <workspace>/lib/second.ml: 2 occurrence(s)\n- main.ml: 2 occurrence(s)"
    truncated: false
    json: {"version":1,"disposition":"applied","occurrences":4,"files":2}
    claim applies: 1
    claim apply 1: applied entries=2
    claim observations: 0
    invocations after resume: 1
    main: "let renamed = 42\nlet use = renamed\n"
    second: "let result = renamed + renamed\n"
    replay equal: true compact fields=disposition,files,occurrences,version disposition=applied occurrences=4 files=2 summary="Renamed 4 occurrences in 2 files"
    compact rejects: zero-occurrences=true zero-files=true files-over=true
    compact constructor rejects zero: true |}]

let%expect_test "prepared bytes and no-follow targets are re-observed" =
  with_world "reobserve" @@ fun world ->
  set_response world 1 standard_response;
  let provider_json = provider_input () in
  let stale_payload = prepare world provider_json in
  let source = Filename.concat world.ws_dir "logical/main.ml" in
  write_disk source "let target = 0\nlet use = target\n";
  let stale_scope = Wio.open_claim_scope world.io in
  print_endline "-- stale bytes --";
  print_result world (resume world provider_json stale_payload);
  close_scope stale_scope;
  write_disk source "let target = 42\nlet use = target\n";
  set_response world 2 standard_response;
  let symlink_payload = prepare world provider_json in
  Unix.unlink source;
  Unix.symlink (Filename.concat world.outside_dir "outside.ml") source;
  let symlink_scope = Wio.open_claim_scope world.io in
  print_endline "-- symlink target --";
  print_result world (resume world provider_json symlink_payload);
  close_scope symlink_scope;
  Printf.printf "outside unchanged: %b invocations=%d\n"
    (String.equal
       (read_disk (Filename.concat world.outside_dir "outside.ml"))
       "let target = 2\n")
    (invocation_count world);
  with_world "apply-race" @@ fun race ->
  set_response race 1 (multi_response race);
  let race_input = provider_input () in
  let race_payload = prepare race race_input in
  let polls = ref 0 in
  let raced = ref false in
  let concurrent () =
    incr polls;
    if !polls = 8 then begin
      raced := true;
      write_disk
        (Filename.concat race.ws_dir "logical/main.ml")
        "let target = 7\nlet use = target\n"
    end;
    false
  in
  let race_scope = Wio.open_claim_scope race.io in
  print_endline "-- changed immediately before apply --";
  print_result race (resume ~cancelled:concurrent race race_input race_payload);
  close_scope race_scope;
  Printf.printf "polls=%d raced=%b main=%S second=%S\n" !polls !raced
    (read_disk (Filename.concat race.ws_dir "logical/main.ml"))
    (read_disk (Filename.concat race.ws_dir "lib/second.ml"));
  [%expect
    {|
    -- stale bytes --
    status: failed stale
    message: main.ml: source changed after rename preparation; prepare the rename again
    metadata: false
    claim applies: 0
    claim observations: 0
    -- symlink target --
    status: failed invalid_input
    message: logical/main.ml: expected text, found other
    metadata: false
    claim applies: 0
    claim observations: 0
    outside unchanged: true invocations=2
    -- changed immediately before apply --
    status: failed stale
    message: logical/main.ml: stale write
    metadata: false
    claim applies: 0
    claim observations: 0
    polls=8 raced=true main="let target = 7\nlet use = target\n" second="let result = target + target\n" |}]

let%expect_test
    "Merlin protocol refuses stale malformed duplicate and escaped data" =
  let run_case label response =
    with_world ("protocol-" ^ label) @@ fun world ->
    set_response world 1 response;
    Printf.printf "-- %s --\n" label;
    print_result world (run_direct world (provider_input ~dry_run:true ()))
  in
  run_case "stale hidden by duplicate"
    (return_occurrences
       [
         occurrence ~sl:1 ~sc:4 ~el:1 ~ec:10 ();
         occurrence ~sl:1 ~sc:4 ~el:1 ~ec:10 ~stale:true ();
       ]);
  run_case "unknown occurrence member"
    (return
       {|[{"file":"","start":{"line":1,"col":4},"end":{"line":1,"col":10},"stale":false,"kind":"value"}]|});
  run_case "duplicate file"
    (return
       {|[{"file":"","file":"main.ml","start":{"line":1,"col":4},"end":{"line":1,"col":10},"stale":false}]|});
  run_case "fractional position"
    (return
       {|[{"file":"","start":{"line":1,"col":4.5},"end":{"line":1,"col":10},"stale":false}]|});
  run_case "outside"
    (return_occurrences
       [ occurrence ~file:"../outside/outside.ml" ~sl:1 ~sc:4 ~el:1 ~ec:10 () ]);
  run_case "non-array" (return {|{}|});
  run_case "duplicate class" {|{"class":"return","class":"return","value":[]}|};
  run_case "malformed JSON" "not json";
  [%expect
    {|
    -- stale hidden by duplicate --
    status: failed stale
    message: index appears stale: 1 occurrence(s) reported stale; rebuild with `dune build @ocaml-index` and retry
    metadata: false
    -- unknown occurrence member --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: occurrence has unexpected or duplicate members
    metadata: false
    -- duplicate file --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: occurrence has unexpected or duplicate members
    metadata: false
    -- fractional position --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: position members must be safe integers
    metadata: false
    -- outside --
    status: failed failed
    message: could not normalize ocamlmerlin occurrence: path is outside the workspace: <outside>/outside.ml
    metadata: false
    -- non-array --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: occurrences value is not an array
    metadata: false
    -- duplicate class --
    status: failed failed
    message: could not decode ocamlmerlin response: response has duplicate class members
    metadata: false
    -- malformed JSON --
    status: failed failed
    message: could not decode ocamlmerlin response: Expected u while parsing null but found: o
    File "-", line 1, characters 0-2:
    metadata: false |}]

let%expect_test
    "caps apply after dedup and identifier classes fail before Merlin" =
  with_world "cap-dedup" @@ fun dedup ->
  set_response dedup 1
    (return_occurrences
       [
         occurrence ~sl:1 ~sc:4 ~el:1 ~ec:10 ();
         occurrence ~sl:1 ~sc:4 ~el:1 ~ec:10 ();
       ]);
  print_endline "-- duplicate under cap --";
  print_result dedup
    (run_direct dedup (provider_input ~dry_run:true ~max_occurrences:1 ()));
  with_world "cap-distinct" @@ fun distinct ->
  set_response distinct 1 standard_response;
  print_endline "-- distinct over cap --";
  print_result distinct
    (run_direct distinct (provider_input ~dry_run:true ~max_occurrences:1 ()));
  with_world "names" @@ fun names ->
  List.iter
    (fun (label, provider_input) ->
      Printf.printf "-- %s --\n" label;
      print_result names (run_direct names provider_input))
    [
      ("keyword", input ~path:"main.ml" ~line:1 ~column:4 ~new_name:"let" ());
      ("same", input ~path:"main.ml" ~line:1 ~column:4 ~new_name:"target" ());
      ( "cross class",
        input ~path:"main.ml" ~line:1 ~column:4 ~new_name:"Target" () );
      ( "invalid",
        input ~path:"main.ml" ~line:1 ~column:4 ~new_name:"bad-name" () );
    ];
  Printf.printf "name-case invocations: %d\n" (invocation_count names);
  set_response names 1 standard_response;
  print_endline "-- cursor immediately after identifier --";
  print_result names
    (run_direct names
       (input ~path:"main.ml" ~line:1 ~column:10 ~new_name:"renamed"
          ~dry_run:true ()));
  write_disk
    (Filename.concat names.ws_dir "logical/main.ml")
    "type t = Target\nlet value = Target\n";
  set_response names 2
    (return_occurrences
       [
         occurrence ~sl:1 ~sc:9 ~el:1 ~ec:15 ();
         occurrence ~sl:2 ~sc:12 ~el:2 ~ec:18 ();
       ]);
  print_endline "-- uppercase class --";
  print_result names
    (run_direct names
       (input ~path:"main.ml" ~line:1 ~column:9 ~new_name:"Renamed"
          ~dry_run:true ()));
  Printf.printf "accepted-class invocations: %d\n" (invocation_count names);
  [%expect
    {|
    -- duplicate under cap --
    status: completed
    text: "OCaml rename: target -> renamed\napplied: false\noccurrences: 1 in 1 file(s)\nindex_status: unknown\nbackend: ocamlmerlin\n- main.ml: 1 occurrence(s)"
    truncated: false
    -- distinct over cap --
    status: failed failed
    message: 2 occurrences exceed the rename cap 1; the rename is too large to apply safely as one edit
    metadata: false
    -- keyword --
    status: failed invalid_input
    message: new name "let" is an OCaml keyword
    metadata: false
    -- same --
    status: failed invalid_input
    message: new name "target" is the same as the current name
    metadata: false
    -- cross class --
    status: failed invalid_input
    message: new name "Target" is a constructor or module identifier but the entity is a value identifier
    metadata: false
    -- invalid --
    status: failed invalid_input
    message: new name "bad-name" is not a valid OCaml identifier
    metadata: false
    name-case invocations: 0
    -- cursor immediately after identifier --
    status: completed
    text: "OCaml rename: target -> renamed\napplied: false\noccurrences: 2 in 1 file(s)\nindex_status: unknown\nbackend: ocamlmerlin\n- main.ml: 2 occurrence(s)"
    truncated: false
    -- uppercase class --
    status: completed
    text: "OCaml rename: Target -> Renamed\napplied: false\noccurrences: 2 in 1 file(s)\nindex_status: unknown\nbackend: ocamlmerlin\n- main.ml: 2 occurrence(s)"
    truncated: false
    accepted-class invocations: 2 |}]

let%expect_test
    "target syntax bytes size boundaries puns and labels fail closed" =
  let run_source label source response =
    with_world ("target-" ^ label) @@ fun world ->
    write_disk (Filename.concat world.ws_dir "logical/main.ml") source;
    set_response world 1 response;
    Printf.printf "-- %s --\n" label;
    print_result world (run_direct world (provider_input ~dry_run:true ()))
  in
  run_source "range mismatch" "let target = 1\n"
    (return_occurrences [ occurrence ~sl:1 ~sc:4 ~el:1 ~ec:9 () ]);
  run_source "outside source" "let target = 1\n"
    (return_occurrences [ occurrence ~sl:9 ~sc:0 ~el:9 ~ec:6 () ]);
  run_source "non-standalone" "let target = 1\nlet targetx = 2\n"
    (return_occurrences [ occurrence ~sl:2 ~sc:4 ~el:2 ~ec:10 () ]);
  run_source "record pun"
    "let target = 1\ntype t = { target : int }\nlet f target = { target }\n"
    (return_occurrences [ occurrence ~sl:3 ~sc:17 ~el:3 ~ec:23 () ]);
  run_source "labelled argument" "let target = 1\nlet f ~target = target\n"
    (return_occurrences [ occurrence ~sl:2 ~sc:7 ~el:2 ~ec:13 () ]);
  run_source "parse source" "let target =\n"
    (return_occurrences [ occurrence ~sl:1 ~sc:4 ~el:1 ~ec:10 () ]);
  run_source "non UTF-8" "let target = \255\n"
    (return_occurrences [ occurrence ~sl:1 ~sc:4 ~el:1 ~ec:10 () ]);
  let run_second label source =
    with_world ("second-" ^ label) @@ fun world ->
    let second = Filename.concat world.ws_dir "lib/second.ml" in
    write_disk second source;
    set_response world 1
      (return_occurrences
         [ occurrence ~file:second ~sl:1 ~sc:4 ~el:1 ~ec:10 () ]);
    Printf.printf "-- %s target --\n" label;
    print_result world (run_direct world (provider_input ~dry_run:true ()))
  in
  run_second "non UTF-8" "let target = \255\n";
  run_second "parse" "let target =\n";
  run_second "oversized"
    ("let target = 1\n(*" ^ String.make Rename.max_file_bytes 'x');
  with_world "source-size" @@ fun world ->
  let large = "let target = 1\n(*" ^ String.make Rename.max_file_bytes 'x' in
  write_disk (Filename.concat world.ws_dir "logical/main.ml") large;
  print_endline "-- oversized source --";
  print_result world (run_direct world (provider_input ~dry_run:true ()));
  Printf.printf "oversized invocations: %d\n" (invocation_count world);
  with_world "after-size" @@ fun after ->
  let prefix = "let target = 1\n(*" in
  let suffix = "*)\n" in
  let padding =
    String.make
      (Rename.max_file_bytes - String.length prefix - String.length suffix)
      'x'
  in
  write_disk
    (Filename.concat after.ws_dir "logical/main.ml")
    (prefix ^ padding ^ suffix);
  set_response after 1
    (return_occurrences [ occurrence ~sl:1 ~sc:4 ~el:1 ~ec:10 () ]);
  print_endline "-- oversized rewritten source --";
  print_result after (run_direct after (provider_input ~dry_run:true ()));
  [%expect
    {|
    -- range mismatch --
    status: failed stale
    message: main.ml:1:4 no longer holds "target" (found "targe"); rebuild the project index with `dune build @ocaml-index` and retry
    metadata: false
    -- outside source --
    status: failed invalid_input
    message: main.ml:9:0 is outside the current source; the local parse cannot corroborate the rename here
    metadata: false
    -- non-standalone --
    status: failed invalid_input
    message: main.ml:2:4 is not a standalone identifier; the local parse cannot corroborate the rename here, edit it manually
    metadata: false
    -- record pun --
    status: failed invalid_input
    message: main.ml:3:17 is a record-field or labelled-argument pun; this tool does not rewrite label or pun sites, edit it manually
    metadata: false
    -- labelled argument --
    status: failed invalid_input
    message: main.ml:2:7 is a labelled-argument occurrence (~/?); this tool does not rewrite label or pun sites, edit it manually
    metadata: false
    -- parse source --
    status: failed failed
    message: main.ml: could not parse source: Syntaxerr.Error(_)
    metadata: false
    -- non UTF-8 --
    status: failed invalid_input
    message: logical/main.ml: not valid UTF-8 text
    metadata: false
    -- non UTF-8 target --
    status: failed invalid_input
    message: lib/second.ml: not valid UTF-8 text
    metadata: false
    -- parse target --
    status: failed failed
    message: <workspace>/lib/second.ml: could not parse source: Syntaxerr.Error(_)
    metadata: false
    -- oversized target --
    status: failed invalid_input
    message: lib/second.ml: file is too large (1048593 bytes, max 1048576)
    metadata: false
    -- oversized source --
    status: failed invalid_input
    message: logical/main.ml: file is too large (1048593 bytes, max 1048576)
    metadata: false
    oversized invocations: 0
    -- oversized rewritten source --
    status: failed invalid_input
    message: main.ml: renamed source is too large (1048577 bytes, max 1048576)
    metadata: false |}]

let run_process_case label configure =
  with_world ("process-" ^ label) @@ fun world ->
  configure world;
  Printf.printf "-- %s --\n" label;
  print_result world (run_direct world (provider_input ~dry_run:true ()))

let%expect_test
    "process exit signal output drain timeout and cancellation are total" =
  with_world "missing-program" @@ fun world ->
  let tool =
    world.make_tool [ Filename.concat world.ws_dir "missing-merlin" ]
  in
  print_endline "-- spawn --";
  let spawn =
    Tool.Call.run
      (decode_call tool (provider_input ~dry_run:true ()))
      ~cancelled:(fun () -> false)
    |> finished
  in
  (match Tool.Result.status spawn with
  | Tool.Result.Failed { kind = `Unavailable; message; metadata } ->
      Printf.printf "unavailable=true diagnostic=%b metadata=%b output=%b\n"
        (not (String.is_empty message))
        (Option.is_some metadata)
        (Option.is_some (Tool.Result.output spawn))
  | _ -> fail "missing executable was not unavailable");
  List.iter
    (fun behavior ->
      run_process_case behavior (fun world -> set_behavior world 1 behavior))
    [ "exit7"; "invalid_exit" ];
  with_world "process-signal" @@ fun signaled ->
  set_behavior signaled 1 "signal";
  let signal = run_direct signaled (provider_input ~dry_run:true ()) in
  print_endline "-- signal --";
  (match Tool.Result.status signal with
  | Tool.Result.Failed { kind = `Failed; message; metadata } ->
      Printf.printf "signal-diagnostic=%b metadata=%b output=%b\n"
        (String.starts_with ~prefix:"ocamlmerlin was terminated by signal "
           message)
        (Option.is_some metadata)
        (Option.is_some (Tool.Result.output signal))
  | _ -> fail "signaled executable was not failed");
  run_process_case "overflow" (fun world -> set_behavior world 1 "overflow");
  run_process_case "incomplete" (fun world ->
      set_response world 1 standard_response;
      set_behavior world 1 "incomplete");
  let clock = Eio_mock.Clock.Mono.make () in
  with_world_using ~clock "timeout" @@ fun timeout ->
  set_behavior timeout 1 "sleep";
  let rec advance_when_scheduled () =
    Eio.Fiber.yield ();
    if Eio_mock.Clock.Mono.try_advance clock then `Stop_daemon
    else advance_when_scheduled ()
  in
  Eio.Fiber.fork_daemon ~sw:timeout.sw advance_when_scheduled;
  print_endline "-- timeout --";
  print_result timeout (run_direct timeout (provider_input ~dry_run:true ()));
  with_world "cancel-running" @@ fun running ->
  set_behavior running 1 "sleep";
  let deadline = Unix.gettimeofday () +. 0.1 in
  print_endline "-- running cancellation --";
  print_result running
    (run_direct
       ~cancelled:(fun () -> Unix.gettimeofday () >= deadline)
       running
       (provider_input ~dry_run:true ()));
  with_world "cancel-before" @@ fun before ->
  print_endline "-- before source --";
  print_result before
    (run_direct
       ~cancelled:(fun () -> true)
       before
       (provider_input ~dry_run:true ()));
  Printf.printf "before invocations: %d\n" (invocation_count before);
  with_world "cancel-after" @@ fun after ->
  set_response after 1 standard_response;
  write_disk (plan_file after "cancel-after" 1) "";
  print_endline "-- after child --";
  print_result after
    (run_direct
       ~cancelled:(fun () ->
         Sys.file_exists (Filename.concat after.plan_dir "cancel-now"))
       after
       (provider_input ~dry_run:true ()));
  Printf.printf "after invocations: %d\n" (invocation_count after);
  with_world "cancel-target" @@ fun target ->
  set_response target 1 standard_response;
  write_disk (plan_file target "cancel-after" 1) "";
  let after_child_polls = ref 0 in
  let cancel_in_target_planning () =
    if Sys.file_exists (Filename.concat target.plan_dir "cancel-now") then
      incr after_child_polls;
    !after_child_polls = 2
  in
  print_endline "-- target planning --";
  print_result target
    (run_direct ~cancelled:cancel_in_target_planning target
       (provider_input ~dry_run:true ()));
  Printf.printf "target polls after child: %d invocations=%d\n"
    !after_child_polls (invocation_count target);
  [%expect
    {|
    -- spawn --
    unavailable=true diagnostic=true metadata=false output=false
    -- exit7 --
    status: failed failed
    message: merlin failed
    metadata: false
    -- invalid_exit --
    status: failed failed
    message: �bad detail
    metadata: false
    -- signal --
    signal-diagnostic=true metadata=false output=false
    -- overflow --
    status: failed failed
    message: ocamlmerlin stdout exceeded 1048576-byte output limit
    metadata: false
    -- incomplete --
    status: failed failed
    message: ocamlmerlin stdout was incomplete after the child exited
    metadata: false
    -- timeout --
    +mock time is now 30
    status: failed timed_out
    message: ocamlmerlin timed out after 30000ms
    metadata: false
    -- running cancellation --
    status: interrupted cancelled=true
    reason: tool call cancelled
    -- before source --
    status: interrupted cancelled=true
    reason: tool call cancelled
    before invocations: 0
    -- after child --
    status: interrupted cancelled=true
    reason: tool call cancelled
    after invocations: 1
    -- target planning --
    status: interrupted cancelled=true
    reason: tool call cancelled
    target polls after child: 2 invocations=1 |}]

let%expect_test "run-stage cancellation never applies" =
  with_world "run-cancel" @@ fun world ->
  set_response world 1 standard_response;
  let provider_input = provider_input () in
  let payload = prepare world provider_input in
  let scope = Wio.open_claim_scope world.io in
  print_result world
    (resume ~cancelled:(fun () -> true) world provider_input payload);
  close_scope scope;
  Printf.printf "source: %S invocations=%d\n"
    (read_disk (Filename.concat world.ws_dir "logical/main.ml"))
    (invocation_count world);
  set_response world 2 (multi_response world);
  let multi_payload = prepare world provider_input in
  let polls = ref 0 in
  let before_apply () =
    incr polls;
    !polls = 8
  in
  let late_scope = Wio.open_claim_scope world.io in
  print_endline "-- immediately before apply --";
  print_result world
    (resume ~cancelled:before_apply world provider_input multi_payload);
  close_scope late_scope;
  Printf.printf "polls=%d main=%S second=%S invocations=%d\n" !polls
    (read_disk (Filename.concat world.ws_dir "logical/main.ml"))
    (read_disk (Filename.concat world.ws_dir "lib/second.ml"))
    (invocation_count world);
  [%expect
    {|
    status: interrupted cancelled=true
    reason: tool call cancelled
    claim applies: 0
    claim observations: 0
    source: "let target = 42\nlet use = target\n" invocations=1
    -- immediately before apply --
    status: interrupted cancelled=true
    reason: tool call cancelled
    claim applies: 0
    claim observations: 0
    polls=8 main="let target = 42\nlet use = target\n" second="let result = target + target\n" invocations=2 |}]

[%%run_tests "mentat.tools.ocaml_rename"]
