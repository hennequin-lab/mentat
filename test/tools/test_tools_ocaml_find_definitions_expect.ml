(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for Merlin definition lookup.
   Every effectful case crosses provider JSON decoding, cached permissions, the
   erased result boundary, a real workspace capability, and a real child
   process. The scripted child is a protocol peer, not a private-function fake:
   it records its physical cwd, argv, and exact stdin before returning a Merlin
   response selected by invocation number. *)

open Windtrap
open Test_tools_support
module Json = Jsont.Json
module Tool = Mentat_tool
module Find = Mentat_tools.Ocaml.Find_definitions
module Wio = Mentat_workspace_io
module Workspace = Mentat_workspace
module Permission = Mentat_permission
module Access = Permission.Access

type cwd = Primary_root | Primary_logical | Auxiliary_root

type world = {
  sw : Eio.Switch.t;
  ws_dir : string;
  aux_dir : string;
  outside_dir : string;
  plan_dir : string;
  io : Wio.t;
  tool : Tool.t;
}

let add_optional name encode value fields =
  match value with
  | None -> fields
  | Some value -> (name, encode value) :: fields

let input ?identifier ?kind ~path ~line ~column () =
  [
    ("path", Json.string path);
    ("line", Json.int line);
    ("column", Json.int column);
  ]
  |> add_optional "identifier" (fun value -> Json.string value) identifier
  |> add_optional "kind" (fun value -> Json.string value) kind
  |> List.rev |> json_object

let definition_exn result =
  match
    Mentat_tools_output.Codec.decode Mentat_tools_output.Ocaml.Definition.jsont
      (output_exn result)
  with
  | Some definition -> definition
  | None -> fail "find-definitions output carried no definition semantics"

let location_exn result =
  let definition = definition_exn result in
  ( Mentat_tools_output.Ocaml.Definition.path definition,
    Mentat_tools_output.Ocaml.Definition.line definition )

let print_status ?(normalize = Fun.id) result =
  match Tool.Result.status result with
  | Tool.Result.Completed -> print_endline "status: completed"
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf "status: failed %s\nmessage: %s\nmetadata: %b\n"
        (failure_name kind) (normalize message) (Option.is_some metadata)
  | Tool.Result.Interrupted { reason; cancelled } ->
      Printf.printf "status: interrupted cancelled=%b\nreason: %s\n" cancelled
        reason

let decode_call_result tool provider_input =
  Tool.Call.decode tool (model_call tool provider_input)

let decode_call tool provider_input =
  match decode_call_result tool provider_input with
  | Ok call -> call
  | Error error ->
      failf "call decode failed: %s" (Tool.Call.Decode_error.message error)

let run ?(cancelled = fun () -> false) world provider_input =
  Tool.Call.run (decode_call world.tool provider_input) ~cancelled |> finished

let abs path = Lpath.Abs.of_string_exn path

let write_disk path contents =
  mkdir_p (Filename.dirname path);
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel contents)

let workspace ~cwd ws_dir aux_dir =
  let primary = Workspace.Root.make ~key:(root_key "main") (abs ws_dir) in
  let auxiliary = Workspace.Root.make ~key:(root_key "aux") (abs aux_dir) in
  let cwd =
    match cwd with
    | Primary_root ->
        Workspace.Path.make ~root_key:(root_key "main") Lpath.Rel.root
    | Primary_logical ->
        Workspace.Path.make ~root_key:(root_key "main") (rel "logical")
    | Auxiliary_root ->
        Workspace.Path.make ~root_key:(root_key "aux") Lpath.Rel.root
  in
  match Workspace.make ~cwd ~primary ~read_only:[ auxiliary ] () with
  | Ok workspace -> workspace
  | Error error ->
      failf "workspace construction failed: %a" Workspace.Error.pp error

let fake_merlin_script =
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
      "behavior=response";
      "if [ -f \"$plan/behavior-$n\" ]; then behavior=$(cat \
       \"$plan/behavior-$n\"); fi";
      "case \"$behavior\" in";
      "  response)";
      "    cat \"$plan/response-$n\"";
      "    if [ -f \"$plan/cancel-after-$n\" ]; then : > \"$plan/cancel-now\"; \
       fi";
      "    ;;";
      "  exit7) printf '\\033[31mmerlin failed\\033[0m\\n' >&2; exit 7 ;;";
      "  invalid_exit) printf '\\377bad\\033]0;secret\\007 detail\\n' >&2; \
       exit 9 ;;";
      "  signal) kill -TERM $$ ;;";
      "  overflow) exec /usr/bin/yes x ;;";
      "  stderr_overflow) exec /usr/bin/yes x >&2 ;;";
      "  sleep) exec /bin/sleep 60 ;;";
      "  incomplete) ( /bin/sleep 3 ) & cat \"$plan/response-$n\" ;;";
      "  *) printf 'unknown behavior: %s\\n' \"$behavior\" >&2; exit 64 ;;";
      "esac";
      "";
    ]

let with_world ?(cwd = Primary_root)
    ?(mode = Mentat_config.Mode.Danger_full_access) ?clock name fn =
  Eio_main.run @@ fun stdenv ->
  let stdenv = (stdenv :> Eio_unix.Stdenv.base) in
  let raw = Filename.temp_dir ("mentat-find-definitions-" ^ name ^ "-") "" in
  Fun.protect ~finally:(fun () -> remove_tree raw) @@ fun () ->
  let base = Unix.realpath raw in
  let ws_dir = Filename.concat base "workspace" in
  let aux_dir = Filename.concat base "auxiliary" in
  let outside_dir = Filename.concat base "outside" in
  let plan_dir = Filename.concat ws_dir ".merlin-plan" in
  List.iter mkdir [ ws_dir; aux_dir; outside_dir; plan_dir ];
  write_disk
    (Filename.concat ws_dir "logical/main.ml")
    "let answer = \"café ☕\"\nlet use = answer\n";
  write_disk (Filename.concat ws_dir "logical/café.ml") "let value = 42\n";
  write_disk (Filename.concat ws_dir "lib/shared.ml") "let main = 1\n";
  write_disk (Filename.concat aux_dir "lib/shared.ml") "let auxiliary = 2\n";
  write_disk (Filename.concat outside_dir "outside.ml") "let outside = 3\n";
  let fake = Filename.concat ws_dir "fake-ocamlmerlin" in
  install_executable fake fake_merlin_script;
  let logical = workspace ~cwd ws_dir aux_dir in
  Eio.Switch.run @@ fun sw ->
  let io = resolve_exn ~sw ~stdenv ~logical ~mode () in
  let tool =
    match clock with
    | None ->
        Find.make io
          ~clock:(Eio.Stdenv.mono_clock stdenv)
          ~program:[ fake; plan_dir ]
    | Some clock -> Find.make io ~clock ~program:[ fake; plan_dir ]
  in
  fn { sw; ws_dir; aux_dir; outside_dir; plan_dir; io; tool }

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

let return ?(notifications = false) value =
  let notifications =
    if notifications then {|,"notifications":[{"class":"message"}]|} else ""
  in
  Printf.sprintf {|{"class":"return","value":%s%s}|} value notifications

let position ~line ~column = Printf.sprintf {|{"line":%d,"col":%d}|} line column

let target ?file ~line ~column () =
  let file =
    match file with
    | None -> ""
    | Some file ->
        Printf.sprintf {|"file":%s,|} (json_string (Json.string file))
  in
  Printf.sprintf {|{%s"pos":%s}|} file (position ~line ~column)

let return_target ?notifications ?file ~line ~column () =
  return ?notifications (target ?file ~line ~column ())

let return_string text = return (json_string (Json.string text))

let decode_verdict tool label provider_input =
  match decode_call_result tool provider_input with
  | Ok call ->
      Printf.printf "%s: accepted canonical=%s\n" label
        (json_string (Tool.Call.input call))
  | Error error ->
      Printf.printf "%s: rejected diagnostic=%b\n" label
        (not (String.is_empty (Tool.Call.Decode_error.message error)))

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

let print_permissions call =
  let requests = Tool.Call.permissions call in
  Printf.printf "requests: %d\n" (List.length requests);
  List.iter
    (fun request ->
      Printf.printf "source: %s\n"
        (Option.value ~default:"none" (Permission.Request.source request));
      List.iter
        (function
          | Access.Path { op; scope = Access.Path_scope.Workspace path } ->
              Printf.printf "path: %s key=%s address=%s\n" (path_op_name op)
                (Workspace.Root.Key.to_string (Workspace.Path.root_key path))
                (Workspace.Path.display path)
          | Access.Custom { name; subject } ->
              Printf.printf "custom: name=%s subject=%s\n" name
                (Option.value ~default:"none" subject)
          | access ->
              failf "unexpected find-definitions permission: %a" Access.pp
                access)
        (Permission.Request.accesses request))
    requests

let run_protocol_case label configure =
  with_world ("protocol-" ^ label) @@ fun world ->
  configure world;
  Printf.printf "-- %s --\n" label;
  print_result world
    (run world (input ~path:"logical/main.ml" ~line:1 ~column:4 ()))

let run_filesystem_case label setup path =
  with_world ("fs-" ^ label) @@ fun world ->
  setup world;
  Printf.printf "-- %s --\n" label;
  let result = run world (input ~path:(path world) ~line:1 ~column:0 ()) in
  print_result world result;
  Printf.printf "invocations: %d\n" (invocation_count world)

let%expect_test "declaration and provider input are exact, bounded, and safe" =
  with_world "schema" @@ fun world ->
  let declaration = Tool.declaration world.tool in
  Printf.printf "name: %s\n" (Mentat_llm.Tool.name declaration);
  Printf.printf "schema: %s\n"
    (json_string (Mentat_llm.Tool.input_schema declaration));
  decode_verdict world.tool "minimal"
    (input ~path:"logical/main.ml" ~line:1 ~column:4 ());
  decode_verdict world.tool "declaration"
    (input ~path:"logical/main.ml" ~line:2 ~column:8 ~identifier:"answer"
       ~kind:"declaration" ());
  decode_verdict world.tool "type definition"
    (input ~path:"logical/main.ml" ~line:2 ~column:8 ~kind:"type-definition" ());
  List.iter
    (fun (label, provider_input) ->
      decode_verdict world.tool label provider_input)
    [
      ( "missing path",
        json_object [ ("line", Json.int 1); ("column", Json.int 0) ] );
      ( "unknown member",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("column", Json.int 0);
            ("scope", Json.string "project");
          ] );
      ( "duplicate path",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("path", Json.string "logical/other.ml");
            ("line", Json.int 1);
            ("column", Json.int 0);
          ] );
      ( "duplicate line",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("line", Json.int 2);
            ("column", Json.int 0);
          ] );
      ( "duplicate column",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("column", Json.int 0);
            ("column", Json.int 1);
          ] );
      ( "duplicate identifier",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("column", Json.int 0);
            ("identifier", Json.string "answer");
            ("identifier", Json.string "other");
          ] );
      ( "duplicate kind",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("column", Json.int 0);
            ("kind", Json.string "definition");
            ("kind", Json.string "declaration");
          ] );
      ("empty path", input ~path:"" ~line:1 ~column:0 ());
      ("NUL path", input ~path:"bad\000path" ~line:1 ~column:0 ());
      ("line zero", input ~path:"logical/main.ml" ~line:0 ~column:0 ());
      ("column negative", input ~path:"logical/main.ml" ~line:1 ~column:(-1) ());
      ( "line string",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.string "1");
            ("column", Json.int 0);
          ] );
      ( "column fraction",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("column", Json.number 0.5);
          ] );
      ( "line unsafe",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.number 9_007_199_254_740_992.);
            ("column", Json.int 0);
          ] );
      ( "column infinity",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("column", Json.number Float.infinity);
          ] );
      ( "unknown kind",
        input ~path:"logical/main.ml" ~line:1 ~column:0 ~kind:"implementation"
          () );
      ( "kind type",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("column", Json.int 0);
            ("kind", Json.int 1);
          ] );
      ( "empty identifier",
        input ~path:"logical/main.ml" ~line:1 ~column:0 ~identifier:"" () );
      ( "NUL identifier",
        input ~path:"logical/main.ml" ~line:1 ~column:0
          ~identifier:"bad\000name" () );
      ( "identifier with type definition",
        input ~path:"logical/main.ml" ~line:1 ~column:0 ~identifier:"answer"
          ~kind:"type-definition" () );
    ];
  [%expect
    {|
    name: ocaml_find_definitions
    schema: {"type":"object","properties":{"path":{"type":"string","description":"Workspace-relative or workspace-contained absolute OCaml source file path.","minLength":1},"line":{"type":"integer","description":"One-based source line of the lookup cursor.","minimum":1,"maximum":9007199254740991},"column":{"type":"integer","description":"Zero-based byte column in the source line, matching OCaml and Merlin locations.","minimum":0,"maximum":9007199254740991},"identifier":{"type":"string","description":"Optional Merlin locate prefix. Omit it to locate the identifier under the cursor.","minLength":1},"kind":{"type":"string","enum":["definition","declaration","type-definition"],"description":"Lookup kind. Defaults to definition. type-definition cannot be combined with identifier."}},"required":["path","line","column"],"additionalProperties":false}
    minimal: accepted canonical={"column":4,"line":1,"path":"logical/main.ml"}
    declaration: accepted canonical={"column":8,"identifier":"answer","kind":"declaration","line":2,"path":"logical/main.ml"}
    type definition: accepted canonical={"column":8,"kind":"type-definition","line":2,"path":"logical/main.ml"}
    missing path: rejected diagnostic=true
    unknown member: rejected diagnostic=true
    duplicate path: rejected diagnostic=true
    duplicate line: rejected diagnostic=true
    duplicate column: rejected diagnostic=true
    duplicate identifier: rejected diagnostic=true
    duplicate kind: rejected diagnostic=true
    empty path: rejected diagnostic=true
    NUL path: rejected diagnostic=true
    line zero: rejected diagnostic=true
    column negative: rejected diagnostic=true
    line string: rejected diagnostic=true
    column fraction: rejected diagnostic=true
    line unsafe: rejected diagnostic=true
    column infinity: rejected diagnostic=true
    unknown kind: rejected diagnostic=true
    kind type: rejected diagnostic=true
    empty identifier: rejected diagnostic=true
    NUL identifier: rejected diagnostic=true
    identifier with type definition: rejected diagnostic=true |}]

let%expect_test "permissions bind one read and one command-confinement fact" =
  with_world "permissions" @@ fun world ->
  let primary =
    decode_call world.tool (input ~path:"logical/main.ml" ~line:1 ~column:4 ())
  in
  print_endline "-- primary --";
  print_permissions primary;
  print_endline "-- auxiliary --";
  print_permissions
    (decode_call world.tool
       (input
          ~path:(Filename.concat world.aux_dir "lib/shared.ml")
          ~line:1 ~column:4 ()));
  print_endline "-- unresolved --";
  print_permissions
    (decode_call world.tool
       (input ~path:"../outside/nope.ml" ~line:1 ~column:4 ()));
  Unix.unlink (Filename.concat world.ws_dir "logical/main.ml");
  print_endline "-- cached after deletion --";
  print_permissions primary;
  [%expect
    {|
    -- primary --
    requests: 1
    source: ocaml_find_definitions
    path: read key=main address=logical/main.ml
    custom: name=command.confinement subject=direct
    -- auxiliary --
    requests: 1
    source: ocaml_find_definitions
    path: read key=aux address=lib/shared.ml
    custom: name=command.confinement subject=direct
    -- unresolved --
    requests: 0
    -- cached after deletion --
    requests: 1
    source: ocaml_find_definitions
    path: read key=main address=logical/main.ml
    custom: name=command.confinement subject=direct |}]

let%expect_test "the real command boundary receives every exact locate argv" =
  with_world ~cwd:Primary_logical "transport" @@ fun world ->
  set_response world 1 (return_target ~notifications:true ~line:5 ~column:2 ());
  let definition = run world (input ~path:"main.ml" ~line:1 ~column:4 ()) in
  print_endline "-- definition --";
  print_result ~json:true world definition;
  set_response world 2 (return_target ~file:"*buffer*" ~line:6 ~column:3 ());
  let declaration =
    run world
      (input ~path:"main.ml" ~line:2 ~column:8 ~identifier:"answer"
         ~kind:"declaration" ())
  in
  print_endline "-- declaration --";
  print_result world declaration;
  set_response world 3
    (return_target ~file:"logical/main.ml" ~line:7 ~column:1 ());
  let type_definition =
    run world
      (input ~path:"main.ml" ~line:2 ~column:8 ~kind:"type-definition" ())
  in
  print_endline "-- type definition --";
  print_result world type_definition;
  List.iter
    (fun index ->
      Printf.printf "call %d cwd: %s\n" index
        (normalize world (String.trim (invocation world "cwd" index)));
      Printf.printf "call %d argv: %S\n" index
        (normalize world (invocation world "argv" index));
      Printf.printf "call %d stdin exact: %b\n" index
        (String.equal
           (invocation world "stdin" index)
           "let answer = \"café ☕\"\nlet use = answer\n"))
    [ 1; 2; 3 ];
  [%expect
    {|
    -- definition --
    status: completed
    text: "OCaml definitions: 1\n- main.ml:5:2-5:2\nindex_status: unknown"
    truncated: false
    json: {"version":1,"path":"main.ml","line":5}
    -- declaration --
    status: completed
    text: "OCaml definitions: 1\n- main.ml:6:3-6:3\nindex_status: unknown"
    truncated: false
    -- type definition --
    status: completed
    text: "OCaml definitions: 1\n- main.ml:7:1-7:1\nindex_status: unknown"
    truncated: false
    call 1 cwd: <workspace>
    call 1 argv: "single\nlocate\n-position\n1:4\n-look-for\nimplementation\n-filename\n<workspace>/logical/main.ml\n"
    call 1 stdin exact: true
    call 2 cwd: <workspace>
    call 2 argv: "single\nlocate\n-position\n2:8\n-look-for\ninterface\n-filename\n<workspace>/logical/main.ml\n-prefix\nanswer\n"
    call 2 stdin exact: true
    call 3 cwd: <workspace>
    call 3 argv: "single\nlocate-type\n-position\n2:8\n-filename\n<workspace>/logical/main.ml\n"
    call 3 stdin exact: true |}]

let%expect_test
    "target normalization preserves roots sentinels and external identity" =
  with_world ~cwd:Primary_logical "roots" @@ fun world ->
  set_response world 1
    (return_target ~file:"lib/shared.ml" ~line:2 ~column:5 ());
  print_endline "-- primary sibling --";
  let primary =
    run world
      (input
         ~path:(Filename.concat world.ws_dir "lib/shared.ml")
         ~line:1 ~column:4 ())
  in
  print_result ~json:true world primary;
  Printf.printf "cwd: %s\n"
    (normalize world (String.trim (invocation world "cwd" 1)));
  set_response world 2 (return_target ~line:3 ~column:6 ());
  print_endline "-- auxiliary source with omitted file --";
  let auxiliary_path = Filename.concat world.aux_dir "lib/shared.ml" in
  let auxiliary = run world (input ~path:auxiliary_path ~line:1 ~column:4 ()) in
  print_result ~json:true world auxiliary;
  Printf.printf "cwd: %s\n"
    (normalize world (String.trim (invocation world "cwd" 2)));
  let replay_path = location_exn auxiliary |> fst in
  set_response world 3 (return_target ~file:auxiliary_path ~line:4 ~column:7 ());
  print_endline "-- cross-root target --";
  print_result ~json:true world
    (run world (input ~path:"main.ml" ~line:1 ~column:4 ()));
  set_response world 4
    (return_target
       ~file:(Filename.concat world.outside_dir "outside.ml")
       ~line:8 ~column:9 ());
  print_endline "-- external target --";
  print_result ~json:true world
    (run world (input ~path:"main.ml" ~line:1 ~column:4 ()));
  set_response world 5 (return_string "Already at definition point");
  print_endline "-- at origin --";
  print_result ~json:true world
    (run world (input ~path:"main.ml" ~line:2 ~column:8 ()));
  set_response world 6
    (return_target ~file:"logical/café.ml" ~line:1 ~column:4 ());
  print_endline "-- Unicode workspace target --";
  print_result ~json:true world
    (run world (input ~path:"main.ml" ~line:1 ~column:4 ()));
  set_response world 7 (return_target ~line:9 ~column:10 ());
  let replay = run world (input ~path:replay_path ~line:1 ~column:4 ()) in
  Printf.printf "replay target line: %d invocations: %d\n"
    (location_exn replay |> snd)
    (invocation_count world);
  [%expect
    {|
    -- primary sibling --
    status: completed
    text: "OCaml definitions: 1\n- <workspace>/lib/shared.ml:2:5-2:5\nindex_status: unknown"
    truncated: false
    json: {"version":1,"path":"<workspace>/lib/shared.ml","line":2}
    cwd: <workspace>
    -- auxiliary source with omitted file --
    status: completed
    text: "OCaml definitions: 1\n- <auxiliary>/lib/shared.ml:3:6-3:6\nindex_status: unknown"
    truncated: false
    json: {"version":1,"path":"<auxiliary>/lib/shared.ml","line":3}
    cwd: <auxiliary>
    -- cross-root target --
    status: completed
    text: "OCaml definitions: 1\n- <auxiliary>/lib/shared.ml:4:7-4:7\nindex_status: unknown"
    truncated: false
    json: {"version":1,"path":"<auxiliary>/lib/shared.ml","line":4}
    -- external target --
    status: completed
    text: "OCaml definitions: 1\n- <outside>/outside.ml:8:9\nindex_status: unknown"
    truncated: false
    json: {"version":1,"path":"<outside>/outside.ml","line":8}
    -- at origin --
    status: completed
    text: "OCaml definitions: 1\n- main.ml:2:8-2:8\nindex_status: not_applicable"
    truncated: false
    json: {"version":1,"path":"main.ml","line":2}
    -- Unicode workspace target --
    status: completed
    text: "OCaml definitions: 1\n- caf\195\169.ml:1:4-1:4\nindex_status: unknown"
    truncated: false
    json: {"version":1,"path":"café.ml","line":1}
    replay target line: 9 invocations: 7 |}]

let%expect_test "an auxiliary logical cwd keeps its short provider address" =
  with_world ~cwd:Auxiliary_root "aux-cwd" @@ fun world ->
  set_response world 1 (return_target ~file:"*buffer*" ~line:2 ~column:3 ());
  let result = run world (input ~path:"lib/shared.ml" ~line:1 ~column:4 ()) in
  print_result ~json:true world result;
  Printf.printf "cwd: %s\n"
    (normalize world (String.trim (invocation world "cwd" 1)));
  [%expect
    {|
    status: completed
    text: "OCaml definitions: 1\n- lib/shared.ml:2:3-2:3\nindex_status: unknown"
    truncated: false
    json: {"version":1,"path":"lib/shared.ml","line":2}
    cwd: <auxiliary> |}]

let%expect_test "locate targets require exact safe unambiguous shapes" =
  let cases =
    [
      ("null", return "null");
      ("array", return "[]");
      ("missing position", return {|{}|});
      ("extra target member", return {|{"pos":{"line":1,"col":0},"x":1}|});
      ( "duplicate position",
        return {|{"pos":{"line":1,"col":0},"pos":{"line":1,"col":0}}|} );
      ("file type", return {|{"file":null,"pos":{"line":1,"col":0}}|});
      ("empty file", return_target ~file:"" ~line:1 ~column:0 ());
      ("NUL file", return_target ~file:"bad\000path" ~line:1 ~column:0 ());
      ("position type", return {|{"pos":null}|});
      ("position extra", return {|{"pos":{"line":1,"col":0,"x":1}}|});
      ("duplicate line", return {|{"pos":{"line":1,"line":1,"col":0}}|});
      ("line type", return {|{"pos":{"line":"1","col":0}}|});
      ("line fraction", return {|{"pos":{"line":1.5,"col":0}}|});
      ("line unsafe", return {|{"pos":{"line":9007199254740992,"col":0}}|});
      ("line zero", return {|{"pos":{"line":0,"col":0}}|});
      ("column negative", return {|{"pos":{"line":1,"col":-1}}|});
    ]
  in
  with_world "target-shapes" @@ fun world ->
  List.iteri
    (fun index (label, response) ->
      set_response world (index + 1) response;
      Printf.printf "-- %s --\n" label;
      print_result world
        (run world (input ~path:"logical/main.ml" ~line:1 ~column:4 ())))
    cases;
  Printf.printf "invocations: %d\n" (invocation_count world);
  [%expect
    {|
    -- null --
    status: failed failed
    message: could not decode ocamlmerlin locate result: locate value is neither a string nor an object
    metadata: false
    -- array --
    status: failed failed
    message: could not decode ocamlmerlin locate result: locate value is neither a string nor an object
    metadata: false
    -- missing position --
    status: failed failed
    message: could not decode ocamlmerlin locate result: target has unexpected or duplicate members
    metadata: false
    -- extra target member --
    status: failed failed
    message: could not decode ocamlmerlin locate result: target has unexpected or duplicate members
    metadata: false
    -- duplicate position --
    status: failed failed
    message: could not decode ocamlmerlin locate result: target has unexpected or duplicate members
    metadata: false
    -- file type --
    status: failed failed
    message: could not decode ocamlmerlin locate result: target file is not a string
    metadata: false
    -- empty file --
    status: failed failed
    message: could not decode ocamlmerlin locate result: target file is empty
    metadata: false
    -- NUL file --
    status: failed failed
    message: could not decode ocamlmerlin locate result: target file contains NUL
    metadata: false
    -- position type --
    status: failed failed
    message: could not decode ocamlmerlin locate result: position is not an object
    metadata: false
    -- position extra --
    status: failed failed
    message: could not decode ocamlmerlin locate result: position has unexpected or duplicate members
    metadata: false
    -- duplicate line --
    status: failed failed
    message: could not decode ocamlmerlin locate result: position has unexpected or duplicate members
    metadata: false
    -- line type --
    status: failed failed
    message: could not decode ocamlmerlin locate result: position members must be safe integers
    metadata: false
    -- line fraction --
    status: failed failed
    message: could not decode ocamlmerlin locate result: position members must be safe integers
    metadata: false
    -- line unsafe --
    status: failed failed
    message: could not decode ocamlmerlin locate result: position members must be safe integers
    metadata: false
    -- line zero --
    status: failed failed
    message: could not decode ocamlmerlin locate result: Mentat_ocaml.Position.make: line must be >= 1
    metadata: false
    -- column negative --
    status: failed failed
    message: could not decode ocamlmerlin locate result: Mentat_ocaml.Position.make: column must be >= 0
    metadata: false
    invocations: 16 |}]

let repeat count text =
  let buffer = Buffer.create (count * String.length text) in
  for _ = 1 to count do
    Buffer.add_string buffer text
  done;
  Buffer.contents buffer

let%expect_test "lookup misses are bounded sanitized terminal failures" =
  let cases =
    [
      "Not found";
      "Not a valid identifier";
      "Identifier is a builtin";
      "The source file has no cmt";
    ]
  in
  with_world "not-found" @@ fun world ->
  List.iteri
    (fun index message ->
      set_response world (index + 1) (return_string message);
      print_result world
        (run world (input ~path:"logical/main.ml" ~line:1 ~column:4 ())))
    cases;
  set_response world 5 (return_string "\027[31m\027[0m");
  print_result world
    (run world (input ~path:"logical/main.ml" ~line:1 ~column:4 ()));
  set_response world 6 (return_string (repeat 5_000 "é"));
  let long = run world (input ~path:"logical/main.ml" ~line:1 ~column:4 ()) in
  (match Tool.Result.status long with
  | Tool.Result.Failed { kind = `Not_found; message; metadata } ->
      Printf.printf "long: bytes=%d valid=%b metadata=%b output=%b\n"
        (String.length message)
        (String.is_valid_utf_8 message)
        (Option.is_some metadata)
        (Option.is_some (Tool.Result.output long))
  | _ ->
      failf "long lookup miss had wrong status: %s"
        (json_string (encode result_codec long)));
  set_response world 7 (return_string "line one\nline two");
  print_result world
    (run world (input ~path:"logical/main.ml" ~line:1 ~column:4 ()));
  [%expect
    {|
    status: failed not_found
    message: Not found
    metadata: false
    status: failed not_found
    message: Not a valid identifier
    metadata: false
    status: failed not_found
    message: Identifier is a builtin
    metadata: false
    status: failed not_found
    message: The source file has no cmt
    metadata: false
    status: failed not_found
    message: ocamlmerlin could not locate a definition
    metadata: false
    long: bytes=4096 valid=true metadata=false output=false
    status: failed failed
    message: could not decode ocamlmerlin locate result: multi-line locate response
    metadata: false |}]

let%expect_test "Merlin envelopes reject missing duplicate and invalid classes"
    =
  run_protocol_case "not JSON" (fun world -> set_response world 1 "not-json");
  run_protocol_case "not object" (fun world -> set_response world 1 "[]");
  run_protocol_case "missing class" (fun world ->
      set_response world 1 {|{"value":{}}|});
  run_protocol_case "missing value" (fun world ->
      set_response world 1 {|{"class":"return"}|});
  run_protocol_case "duplicate value" (fun world ->
      set_response world 1 {|{"class":"return","value":{},"value":{}}|});
  run_protocol_case "class type" (fun world ->
      set_response world 1 {|{"class":1,"value":{}}|});
  run_protocol_case "unknown class" (fun world ->
      set_response world 1 {|{"class":"wat","value":{}}|});
  run_protocol_case "duplicate class" (fun world ->
      set_response world 1 {|{"class":"return","class":"return","value":{}}|});
  List.iter
    (fun class_ ->
      run_protocol_case class_ (fun world ->
          set_response world 1
            (Printf.sprintf {|{"class":%S,"value":"boom"}|} class_)))
    [ "failure"; "error"; "exception" ];
  [%expect
    {|
    -- not JSON --
    status: failed failed
    message: could not decode ocamlmerlin response: Expected u while parsing null but found: o
    File "-", line 1, characters 0-2:
    metadata: false
    -- not object --
    status: failed failed
    message: could not decode ocamlmerlin response: response is not an object
    metadata: false
    -- missing class --
    status: failed failed
    message: could not decode ocamlmerlin response: response has no class
    metadata: false
    -- missing value --
    status: failed failed
    message: could not decode ocamlmerlin response: response has no value
    metadata: false
    -- duplicate value --
    status: failed failed
    message: could not decode ocamlmerlin response: response has duplicate value members
    metadata: false
    -- class type --
    status: failed failed
    message: could not decode ocamlmerlin response: response class is not a string
    metadata: false
    -- unknown class --
    status: failed failed
    message: could not decode ocamlmerlin response: unexpected response class wat
    metadata: false
    -- duplicate class --
    status: failed failed
    message: could not decode ocamlmerlin response: response has duplicate class members
    metadata: false
    -- failure --
    status: failed failed
    message: ocamlmerlin returned failure: boom
    metadata: false
    -- error --
    status: failed failed
    message: ocamlmerlin returned error: boom
    metadata: false
    -- exception --
    status: failed failed
    message: ocamlmerlin returned exception: boom
    metadata: false |}]

let%expect_test
    "source validation refuses missing nonregular escape and oversize" =
  run_filesystem_case "missing" ignore (fun _ -> "missing.ml");
  run_filesystem_case "directory" ignore (fun _ -> "logical");
  run_filesystem_case "symlink"
    (fun world ->
      Unix.symlink world.outside_dir (Filename.concat world.ws_dir "escape"))
    (fun _ -> "escape/outside.ml");
  run_filesystem_case "outside" ignore (fun world ->
      Filename.concat world.outside_dir "outside.ml");
  run_filesystem_case "oversize"
    (fun world ->
      let path = Filename.concat world.ws_dir "large.ml" in
      let channel = Unix.openfile path [ Unix.O_CREAT; Unix.O_WRONLY ] 0o644 in
      Fun.protect ~finally:(fun () -> Unix.close channel) @@ fun () ->
      Unix.LargeFile.ftruncate channel
        (Int64.of_int (Find.max_source_bytes + 1)))
    (fun _ -> "large.ml");
  [%expect
    {|
    -- missing --
    status: failed not_found
    message: missing.ml: path does not exist
    metadata: false
    invocations: 0
    -- directory --
    status: failed invalid_input
    message: logical: expected a regular file, found directory
    metadata: false
    invocations: 0
    -- symlink --
    status: failed invalid_input
    message: escape/outside.ml: path resolves outside workspace
    metadata: false
    invocations: 0
    -- outside --
    status: failed invalid_input
    message: path is outside workspace: <outside>/outside.ml
    metadata: false
    invocations: 0
    -- oversize --
    status: failed invalid_input
    message: large.ml: file is too large (8388609 bytes, max 8388608)
    metadata: false
    invocations: 0 |}]

let%expect_test
    "execution revalidates decoded paths and reads current complete bytes" =
  with_world "source-races" @@ fun world ->
  let provider_input = input ~path:"logical/main.ml" ~line:1 ~column:4 () in
  let fresh_call = decode_call world.tool provider_input in
  let source = Filename.concat world.ws_dir "logical/main.ml" in
  write_disk source "let changed = true\n";
  set_response world 1 (return_target ~line:1 ~column:4 ());
  print_endline "-- content replacement --";
  print_result world
    (Tool.Call.run fresh_call ~cancelled:(fun () -> false) |> finished);
  Printf.printf "current stdin exact: %b\n"
    (String.equal (invocation world "stdin" 1) "let changed = true\n");
  let escaped_call = decode_call world.tool provider_input in
  Unix.unlink source;
  Unix.symlink world.outside_dir source;
  print_endline "-- symlink replacement --";
  print_result world
    (Tool.Call.run escaped_call ~cancelled:(fun () -> false) |> finished);
  Printf.printf "invocations: %d\n" (invocation_count world);
  [%expect
    {|
    -- content replacement --
    status: completed
    text: "OCaml definitions: 1\n- logical/main.ml:1:4-1:4\nindex_status: unknown"
    truncated: false
    current stdin exact: true
    -- symlink replacement --
    status: failed invalid_input
    message: logical/main.ml: path resolves outside workspace
    metadata: false
    invocations: 1 |}]

let%expect_test
    "spawn exit signal output and incomplete-drain failures stay distinct" =
  with_world "missing-program" @@ fun world ->
  let clock = Eio_mock.Clock.Mono.make () in
  let tool =
    Find.make world.io ~clock
      ~program:[ Filename.concat world.ws_dir "missing-merlin" ]
  in
  let call =
    decode_call tool (input ~path:"logical/main.ml" ~line:1 ~column:4 ())
  in
  print_endline "-- spawn --";
  let spawn = Tool.Call.run call ~cancelled:(fun () -> false) |> finished in
  (match Tool.Result.status spawn with
  | Tool.Result.Failed { kind = `Unavailable; message; metadata } ->
      Printf.printf "unavailable=true diagnostic=%b metadata=%b output=%b\n"
        (not (String.is_empty message))
        (Option.is_some metadata)
        (Option.is_some (Tool.Result.output spawn))
  | _ ->
      failf "missing executable was not unavailable: %s"
        (json_string (encode result_codec spawn)));
  List.iter
    (fun behavior ->
      run_protocol_case behavior (fun world -> set_behavior world 1 behavior))
    [ "exit7"; "invalid_exit" ];
  with_world "signal" @@ fun signal_world ->
  set_behavior signal_world 1 "signal";
  let signal =
    run signal_world (input ~path:"logical/main.ml" ~line:1 ~column:4 ())
  in
  print_endline "-- signal --";
  (match Tool.Result.status signal with
  | Tool.Result.Failed { kind = `Failed; message; metadata } ->
      Printf.printf "signal-diagnostic=%b metadata=%b output=%b\n"
        (String.starts_with ~prefix:"ocamlmerlin was terminated by signal "
           message)
        (Option.is_some metadata)
        (Option.is_some (Tool.Result.output signal))
  | _ ->
      failf "signaled executable was not failed: %s"
        (json_string (encode result_codec signal)));
  run_protocol_case "overflow" (fun overflow ->
      set_behavior overflow 1 "overflow");
  run_protocol_case "stderr overflow" (fun overflow ->
      set_behavior overflow 1 "stderr_overflow");
  run_protocol_case "incomplete" (fun incomplete ->
      set_response incomplete 1 (return_target ~line:1 ~column:4 ());
      set_behavior incomplete 1 "incomplete");
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
    -- stderr overflow --
    status: failed failed
    message: ocamlmerlin stderr exceeded 1048576-byte output limit
    metadata: false
    -- incomplete --
    status: failed failed
    message: ocamlmerlin stdout was incomplete after the child exited
    metadata: false |}]

let%expect_test "timeout and cooperative cancellation stop the real child" =
  let mock_clock = Eio_mock.Clock.Mono.make () in
  Eio_mock.Clock.Mono.set_time mock_clock (Mtime.of_uint64_ns 0L);
  with_world ~clock:mock_clock "timeout" @@ fun world ->
  set_behavior world 1 "sleep";
  let rec advance_when_scheduled () =
    Eio.Fiber.yield ();
    if Eio_mock.Clock.Mono.try_advance mock_clock then `Stop_daemon
    else advance_when_scheduled ()
  in
  Eio.Fiber.fork_daemon ~sw:world.sw advance_when_scheduled;
  print_endline "-- timeout --";
  print_result world
    (run world (input ~path:"logical/main.ml" ~line:1 ~column:4 ()));
  with_world "cancel-running" @@ fun cancelled_world ->
  set_behavior cancelled_world 1 "sleep";
  let deadline = Unix.gettimeofday () +. 0.1 in
  print_endline "-- running cancellation --";
  print_result cancelled_world
    (run
       ~cancelled:(fun () -> Unix.gettimeofday () >= deadline)
       cancelled_world
       (input ~path:"logical/main.ml" ~line:1 ~column:4 ()));
  with_world "cancel-before" @@ fun before ->
  set_response before 1 (return_target ~line:1 ~column:4 ());
  print_endline "-- before observation --";
  print_result before
    (run
       ~cancelled:(fun () -> true)
       before
       (input ~path:"logical/main.ml" ~line:1 ~column:4 ()));
  Printf.printf "invocations: %d\n" (invocation_count before);
  with_world "cancel-after-read" @@ fun after_read ->
  set_response after_read 1 (return_target ~line:1 ~column:4 ());
  let polls = ref 0 in
  print_endline "-- after source read --";
  print_result after_read
    (run
       ~cancelled:(fun () ->
         incr polls;
         !polls >= 2)
       after_read
       (input ~path:"logical/main.ml" ~line:1 ~column:4 ()));
  Printf.printf "polls: %d invocations: %d\n" !polls
    (invocation_count after_read);
  [%expect
    {|
    +mock time is now 0
    -- timeout --
    +mock time is now 30
    status: failed timed_out
    message: ocamlmerlin timed out after 30000ms
    metadata: false
    -- running cancellation --
    status: interrupted cancelled=true
    reason: tool call cancelled
    -- before observation --
    status: interrupted cancelled=true
    reason: tool call cancelled
    invocations: 0
    -- after source read --
    status: interrupted cancelled=true
    reason: tool call cancelled
    polls: 2 invocations: 0 |}]

let%expect_test "cancellation after the one child response retains no output" =
  with_world "cancel-after" @@ fun world ->
  set_response world 1 (return_target ~line:4 ~column:2 ());
  write_disk (plan_file world "cancel-after" 1) "";
  let result =
    run
      ~cancelled:(fun () ->
        Sys.file_exists (Filename.concat world.plan_dir "cancel-now"))
      world
      (input ~path:"logical/main.ml" ~line:1 ~column:4 ())
  in
  print_result world result;
  Printf.printf "invocations: %d output=%b\n" (invocation_count world)
    (Option.is_some (Tool.Result.output result));
  [%expect
    {|
    status: interrupted cancelled=true
    reason: tool call cancelled
    invocations: 1 output=false |}]

let%expect_test
    "durable replay preserves authoritative JSON and excludes D2 names" =
  with_world "durable" @@ fun world ->
  set_response world 1
    (return_target ~file:"logical/main.ml" ~line:4 ~column:2 ());
  let result =
    run world
      (input ~path:"logical/main.ml" ~line:2 ~column:8 ~identifier:"answer"
         ~kind:"declaration" ())
  in
  let stored = encode result_codec result in
  let replayed = decode result_codec stored in
  let stored_again = encode result_codec replayed in
  Printf.printf "roundtrip equal: %b\n" (Json.equal stored stored_again);
  Printf.printf "output text equal: %b json equal: %b truncated equal: %b\n"
    (String.equal
       (Tool.Output.text (output_exn result))
       (Tool.Output.text (output_exn replayed)))
    (Option.equal Json.equal
       (Tool.Output.json (output_exn result))
       (Tool.Output.json (output_exn replayed)))
    (Bool.equal
       (Tool.Output.truncated (output_exn result))
       (Tool.Output.truncated (output_exn replayed)));
  let forbidden =
    [
      "value";
      "evidence";
      "mutation";
      "receipt";
      "revert";
      "sandbox";
      "permissions";
      "permission_requests";
      "claim";
      "settlement";
    ]
  in
  let names = json_member_names stored in
  Printf.printf "forbidden members: %s\n"
    (match List.filter (fun name -> List.mem name names) forbidden with
    | [] -> "<none>"
    | present -> String.concat "," present);
  let path, line = location_exn replayed in
  Printf.printf "definition=%s:%d invocations-after-replay=%d\n" path line
    (invocation_count world);
  [%expect
    {|
    roundtrip equal: true
    output text equal: true json equal: true truncated equal: true
    forbidden members: <none>
    definition=logical/main.ml:4 invocations-after-replay=1 |}]

let%expect_test "constructor rejects an empty immutable program prefix" =
  with_world "constructor" @@ fun world ->
  let clock = Eio_mock.Clock.Mono.make () in
  let raised =
    match Find.make world.io ~clock ~program:[] with
    | _ -> false
    | exception Invalid_argument diagnostic ->
        Printf.printf "diagnostic: %s\n" diagnostic;
        true
  in
  Printf.printf "raised: %b invocations: %d\n" raised (invocation_count world);
  [%expect
    {|
    diagnostic: Ocaml.Find_definitions.make: program prefix must not be empty
    raised: true invocations: 0 |}]

[%%run_tests "mentat.tools.ocaml_find_definitions"]
