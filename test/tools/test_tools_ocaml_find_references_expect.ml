(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for semantic reference tool.
   Every effectful case crosses provider JSON decoding, cached permissions, the
   erased result boundary, a real workspace capability, and a real child
   process. The scripted child is a protocol peer: it records physical cwd,
   argv, and exact stdin, then emits one response selected by invocation count. *)

open Windtrap
open Test_tools_support
module Json = Jsont.Json
module Tool = Mentat_tool
module Find = Mentat_tools.Ocaml.Find_references
module Wio = Mentat_workspace_io
module Workspace = Mentat_workspace
module Permission = Mentat_permission
module Access = Permission.Access

type cwd = Primary_root | Primary_logical

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

let input ?scope ?include_stale ?offset ?limit ~path ~line ~column () =
  [
    ("path", Json.string path);
    ("line", Json.int line);
    ("column", Json.int column);
  ]
  |> add_optional "scope" (fun value -> Json.string value) scope
  |> add_optional "include_stale" (fun value -> Json.bool value) include_stale
  |> add_optional "offset" (fun value -> Json.int value) offset
  |> add_optional "limit" (fun value -> Json.int value) limit
  |> List.rev |> json_object

let required_member name json =
  match json_member name json with
  | Some value -> value
  | None -> failf "JSON has no %S member: %s" name (json_string json)

let json_int_value = function
  | Jsont.Number (value, _) when Float.is_integer value -> int_of_float value
  | json -> failf "expected JSON integer, got %s" (json_string json)

let output_json result =
  match Tool.Output.json (output_exn result) with
  | Some json -> json
  | None -> fail "completed find-references output carried no JSON projection"

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
      "  overflow_stderr) exec /usr/bin/yes x >&2 ;;";
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
  let raw = Filename.temp_dir ("mentat-find-refs-" ^ name ^ "-") "" in
  Fun.protect ~finally:(fun () -> remove_tree raw) @@ fun () ->
  let base = Unix.realpath raw in
  let ws_dir = Filename.concat base "workspace" in
  let aux_dir = Filename.concat base "auxiliary" in
  let outside_dir = Filename.concat base "outside" in
  let plan_dir = Filename.concat ws_dir ".merlin-plan" in
  List.iter mkdir [ ws_dir; aux_dir; outside_dir; plan_dir ];
  write_disk
    (Filename.concat ws_dir "logical/main.ml")
    "let target = 42\nlet use = target\n";
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

let position ~line ~column = Printf.sprintf {|{"line":%d,"col":%d}|} line column

let occurrence ?file ~sl ~sc ~el ~ec ~stale () =
  let fields =
    [
      Printf.sprintf {|"start":%s|} (position ~line:sl ~column:sc);
      Printf.sprintf {|"end":%s|} (position ~line:el ~column:ec);
      Printf.sprintf {|"stale":%b|} stale;
    ]
  in
  let fields =
    match file with
    | None -> fields
    | Some file ->
        Printf.sprintf {|"file":%s|} (json_string (Json.string file)) :: fields
  in
  "{" ^ String.concat "," fields ^ "}"

let return value = Printf.sprintf {|{"class":"return","value":%s}|} value

let return_occurrences occurrences =
  return ("[" ^ String.concat "," occurrences ^ "]")

let standard_buffer_response =
  return_occurrences [ occurrence ~sl:1 ~sc:4 ~el:1 ~ec:10 ~stale:false () ]

let standard_project_response file =
  return_occurrences
    [ occurrence ~file ~sl:1 ~sc:4 ~el:1 ~ec:10 ~stale:false () ]

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

let%expect_test "declaration and provider input are exact bounded safe values" =
  with_world "schema" @@ fun world ->
  let declaration = Tool.declaration world.tool in
  Printf.printf "name: %s\n" (Mentat_llm.Tool.name declaration);
  Printf.printf "schema: %s\n"
    (json_string (Mentat_llm.Tool.input_schema declaration));
  decode_verdict world.tool "minimal"
    (input ~path:"logical/main.ml" ~line:1 ~column:4 ());
  decode_verdict world.tool "full"
    (input ~path:"logical/main.ml" ~line:2 ~column:8 ~scope:"renaming"
       ~include_stale:true ~offset:2 ~limit:1_000 ());
  decode_verdict world.tool "safe integer maxima"
    (input ~path:"logical/main.ml" ~line:9_007_199_254_740_991
       ~column:9_007_199_254_740_991 ~offset:9_007_199_254_740_991 ());
  let complete_members =
    [
      ("path", Json.string "logical/main.ml");
      ("line", Json.int 2);
      ("column", Json.int 8);
      ("scope", Json.string "renaming");
      ("include_stale", Json.bool true);
      ("offset", Json.int 2);
      ("limit", Json.int 1_000);
    ]
  in
  List.iter
    (fun ((name, _) as repeated) ->
      decode_verdict world.tool ("duplicate " ^ name)
        (json_object (complete_members @ [ repeated ])))
    complete_members;
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
            ("name", Json.string "target");
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
      ( "offset fraction",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("column", Json.int 0);
            ("offset", Json.number 1.5);
          ] );
      ( "offset unsafe",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("column", Json.int 0);
            ("offset", Json.number 9_007_199_254_740_992.);
          ] );
      ( "limit fraction",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("column", Json.int 0);
            ("limit", Json.number 1.5);
          ] );
      ( "bad scope",
        input ~path:"logical/main.ml" ~line:1 ~column:0 ~scope:"grep" () );
      ( "stale type",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("column", Json.int 0);
            ("include_stale", Json.string "yes");
          ] );
      ( "offset zero",
        input ~path:"logical/main.ml" ~line:1 ~column:0 ~offset:0 () );
      ("limit zero", input ~path:"logical/main.ml" ~line:1 ~column:0 ~limit:0 ());
      ( "limit high",
        input ~path:"logical/main.ml" ~line:1 ~column:0 ~limit:1_001 () );
    ];
  [%expect
    {|
    name: ocaml_find_references
    schema: {"type":"object","properties":{"path":{"type":"string","description":"Workspace-relative or workspace-contained absolute OCaml source file path.","minLength":1},"line":{"type":"integer","description":"One-based source line of the identifier cursor.","minimum":1,"maximum":9007199254740991},"column":{"type":"integer","description":"Zero-based byte column in the source line, matching OCaml and Merlin locations.","minimum":0,"maximum":9007199254740991},"scope":{"type":"string","enum":["buffer","project","renaming"],"description":"Merlin occurrence scope. buffer searches only the current source; project and renaming consult project occurrence indexes. Defaults to project."},"include_stale":{"type":"boolean","description":"Include occurrences Merlin marks stale. Defaults to false."},"offset":{"type":"integer","description":"One-based first reference after stale filtering and canonical deduplication. Defaults to 1.","minimum":1,"maximum":9007199254740991},"limit":{"type":"integer","description":"Maximum references returned. Defaults to 200.","minimum":1,"maximum":1000}},"required":["path","line","column"],"additionalProperties":false}
    minimal: accepted canonical={"column":4,"line":1,"path":"logical/main.ml"}
    full: accepted canonical={"column":8,"include_stale":true,"limit":1000,"line":2,"offset":2,"path":"logical/main.ml","scope":"renaming"}
    safe integer maxima: accepted canonical={"column":9007199254740991,"line":9007199254740991,"offset":9007199254740991,"path":"logical/main.ml"}
    duplicate path: rejected diagnostic=true
    duplicate line: rejected diagnostic=true
    duplicate column: rejected diagnostic=true
    duplicate scope: rejected diagnostic=true
    duplicate include_stale: rejected diagnostic=true
    duplicate offset: rejected diagnostic=true
    duplicate limit: rejected diagnostic=true
    missing path: rejected diagnostic=true
    unknown member: rejected diagnostic=true
    empty path: rejected diagnostic=true
    NUL path: rejected diagnostic=true
    line zero: rejected diagnostic=true
    column negative: rejected diagnostic=true
    line string: rejected diagnostic=true
    column fraction: rejected diagnostic=true
    line unsafe: rejected diagnostic=true
    column infinity: rejected diagnostic=true
    offset fraction: rejected diagnostic=true
    offset unsafe: rejected diagnostic=true
    limit fraction: rejected diagnostic=true
    bad scope: rejected diagnostic=true
    stale type: rejected diagnostic=true
    offset zero: rejected diagnostic=true
    limit zero: rejected diagnostic=true
    limit high: rejected diagnostic=true |}]

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
              failf "unexpected find-references permission: %a" Access.pp access)
        (Permission.Request.accesses request))
    requests

let%expect_test "permissions bind one read and one confinement fact once" =
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
  print_endline "-- missing but resolvable --";
  print_permissions
    (decode_call world.tool (input ~path:"missing.ml" ~line:1 ~column:4 ()));
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
    source: ocaml_find_references
    path: read key=main address=logical/main.ml
    custom: name=command.confinement subject=direct
    -- auxiliary --
    requests: 1
    source: ocaml_find_references
    path: read key=aux address=lib/shared.ml
    custom: name=command.confinement subject=direct
    -- missing but resolvable --
    requests: 1
    source: ocaml_find_references
    path: read key=main address=missing.ml
    custom: name=command.confinement subject=direct
    -- unresolved --
    requests: 0
    -- cached after deletion --
    requests: 1
    source: ocaml_find_references
    path: read key=main address=logical/main.ml
    custom: name=command.confinement subject=direct |}]

let%expect_test "the real command receives source owning cwd argv and stdin" =
  with_world ~cwd:Primary_logical "transport" @@ fun world ->
  set_response world 1 (standard_project_response "logical/main.ml");
  let result =
    run world (input ~path:"main.ml" ~line:1 ~column:4 ~scope:"project" ())
  in
  print_result ~json:true world result;
  Printf.printf "cwd: %s\n"
    (normalize world (String.trim (invocation world "cwd" 1)));
  Printf.printf "argv: %S\n" (normalize world (invocation world "argv" 1));
  Printf.printf "stdin exact: %b invocations: %d\n"
    (String.equal
       (invocation world "stdin" 1)
       "let target = 42\nlet use = target\n")
    (invocation_count world);
  [%expect
    {|
    status: completed
    text: "OCaml references for main.ml:1:4\nscope: project\nreferences: 1 returned of 1, offset 1, status complete\nindex_status: unknown\nbackend: ocamlmerlin\n- main.ml:1:4-1:10"
    truncated: false
    json: {"version":1,"references":1,"files":1}
    cwd: <workspace>
    argv: "single\noccurrences\n-identifier-at\n1:4\n-scope\nproject\n-filename\n<workspace>/logical/main.ml\n"
    stdin exact: true invocations: 1 |}]

let output_summary ?(normalize = Fun.id) result =
  let output = output_exn result in
  let json = output_json result in
  Printf.printf
    "text: %S\nsemantic: version=%d references=%d files=%d truncated=%b\n"
    (normalize (Tool.Output.text output))
    (json_int_value (required_member "version" json))
    (json_int_value (required_member "references" json))
    (json_int_value (required_member "files" json))
    (Tool.Output.truncated output)

let paging_response ?duplicate_file ?first_file file =
  let duplicate_file = Option.value duplicate_file ~default:file in
  let first_file = Option.value first_file ~default:file in
  return_occurrences
    [
      occurrence ~file ~sl:3 ~sc:4 ~el:3 ~ec:10 ~stale:false ();
      occurrence ~file ~sl:2 ~sc:4 ~el:2 ~ec:10 ~stale:true ();
      occurrence ~file:first_file ~sl:1 ~sc:4 ~el:1 ~ec:10 ~stale:false ();
      occurrence ~file:duplicate_file ~sl:3 ~sc:4 ~el:3 ~ec:10 ~stale:true ();
    ]

let%expect_test "scope controls exact protocol shape and index trust" =
  with_world "scopes" @@ fun world ->
  set_response world 1
    (return_occurrences
       [
         occurrence ~sl:1 ~sc:4 ~el:1 ~ec:10 ~stale:false ();
         occurrence ~sl:2 ~sc:10 ~el:2 ~ec:16 ~stale:true ();
       ]);
  let buffer =
    run world
      (input ~path:"logical/main.ml" ~line:1 ~column:4 ~scope:"buffer" ())
  in
  print_endline "-- buffer default stale policy --";
  print_status buffer;
  output_summary buffer;
  set_response world 2
    (return_occurrences
       [
         occurrence ~file:"logical/main.ml" ~sl:1 ~sc:4 ~el:1 ~ec:10
           ~stale:false ();
         occurrence ~file:"logical/main.ml" ~sl:2 ~sc:10 ~el:2 ~ec:16
           ~stale:true ();
       ]);
  let project =
    run world
      (input ~path:"logical/main.ml" ~line:1 ~column:4 ~scope:"project"
         ~include_stale:true ())
  in
  print_endline "-- project includes stale --";
  print_status project;
  output_summary project;
  set_response world 3
    (return_occurrences
       [
         occurrence ~file:"logical/main.ml" ~sl:1 ~sc:4 ~el:1 ~ec:10
           ~stale:false ();
       ]);
  let renaming =
    run world
      (input ~path:"logical/main.ml" ~line:1 ~column:4 ~scope:"renaming" ())
  in
  print_endline "-- renaming --";
  print_status renaming;
  output_summary renaming;
  let scopes =
    List.init 3 (fun index ->
        let argv = invocation world "argv" (index + 1) in
        let tokens = String.split_on_char '\n' argv in
        let rec find = function
          | "-scope" :: scope :: _ -> scope
          | _ :: rest -> find rest
          | [] -> "missing"
        in
        find tokens)
  in
  Printf.printf "argv scopes: %s\n" (String.concat "," scopes);
  [%expect
    {|
    -- buffer default stale policy --
    status: completed
    text: "OCaml references for logical/main.ml:1:4\nscope: buffer\nreferences: 1 returned of 2, 1 stale skipped, offset 1, status complete\nindex_status: not_applicable\nbackend: ocamlmerlin\n- logical/main.ml:1:4-1:10\nstale note: rebuild the project index, for Dune usually `dune build @ocaml-index`."
    semantic: version=1 references=1 files=1 truncated=false
    -- project includes stale --
    status: completed
    text: "OCaml references for logical/main.ml:1:4\nscope: project\nreferences: 2 returned of 2, offset 1, status complete\nindex_status: unknown\nbackend: ocamlmerlin\n- logical/main.ml:1:4-1:10\n- logical/main.ml:2:10-2:16 stale"
    semantic: version=1 references=2 files=1 truncated=false
    -- renaming --
    status: completed
    text: "OCaml references for logical/main.ml:1:4\nscope: renaming\nreferences: 1 returned of 1, offset 1, status complete\nindex_status: unknown\nbackend: ocamlmerlin\n- logical/main.ml:1:4-1:10"
    semantic: version=1 references=1 files=1 truncated=false
    argv scopes: buffer,project,renaming |}]

let%expect_test
    "nested cwd auxiliary roots and equal relative names stay unambiguous" =
  with_world ~cwd:Primary_logical "roots" @@ fun world ->
  let primary = Filename.concat world.ws_dir "lib/shared.ml" in
  let auxiliary = Filename.concat world.aux_dir "lib/shared.ml" in
  set_response world 1
    (return_occurrences
       [
         occurrence ~file:auxiliary ~sl:1 ~sc:4 ~el:1 ~ec:13 ~stale:false ();
         occurrence ~file:"logical/main.ml" ~sl:2 ~sc:10 ~el:2 ~ec:16
           ~stale:false ();
         occurrence ~file:primary ~sl:1 ~sc:4 ~el:1 ~ec:8 ~stale:false ();
       ]);
  let result =
    run world (input ~path:"main.ml" ~line:1 ~column:4 ~scope:"project" ())
  in
  print_endline "-- primary query --";
  print_status result;
  output_summary ~normalize:(normalize world) result;
  Printf.printf "cwd=%s\n"
    (normalize world (String.trim (invocation world "cwd" 1)));
  set_response world 2 (standard_project_response "lib/shared.ml");
  let auxiliary_query =
    run world (input ~path:auxiliary ~line:1 ~column:4 ~scope:"project" ())
  in
  print_endline "-- auxiliary query --";
  print_status auxiliary_query;
  output_summary ~normalize:(normalize world) auxiliary_query;
  Printf.printf "cwd=%s\n"
    (normalize world (String.trim (invocation world "cwd" 2)));
  [%expect
    {|
    -- primary query --
    status: completed
    text: "OCaml references for main.ml:1:4\nscope: project\nreferences: 3 returned of 3, offset 1, status complete\nindex_status: unknown\nbackend: ocamlmerlin\n- <auxiliary>/lib/shared.ml:1:4-1:13\n- <workspace>/lib/shared.ml:1:4-1:8\n- main.ml:2:10-2:16"
    semantic: version=1 references=3 files=3 truncated=false
    cwd=<workspace>
    -- auxiliary query --
    status: completed
    text: "OCaml references for <auxiliary>/lib/shared.ml:1:4\nscope: project\nreferences: 1 returned of 1, offset 1, status complete\nindex_status: unknown\nbackend: ocamlmerlin\n- <auxiliary>/lib/shared.ml:1:4-1:10"
    semantic: version=1 references=1 files=1 truncated=false
    cwd=<auxiliary> |}]

let run_occurrence_case ?(scope = "buffer") label response =
  with_world ("occurrence-" ^ label) @@ fun world ->
  set_response world 1 response;
  Printf.printf "-- %s --\n" label;
  let result =
    run world (input ~path:"logical/main.ml" ~line:1 ~column:4 ~scope ())
  in
  print_result world result;
  Printf.printf "invocations: %d\n" (invocation_count world)

let%expect_test "occurrence records enforce scope-specific exact shapes" =
  let buffer occurrence = return_occurrences [ occurrence ] in
  let project occurrence = return_occurrences [ occurrence ] in
  run_occurrence_case "buffer has file"
    (buffer
       (occurrence ~file:"logical/main.ml" ~sl:1 ~sc:0 ~el:1 ~ec:1 ~stale:false
          ()));
  run_occurrence_case "buffer missing stale"
    (buffer {|{"start":{"line":1,"col":0},"end":{"line":1,"col":1}}|});
  run_occurrence_case ~scope:"project" "project missing file"
    (project (occurrence ~sl:1 ~sc:0 ~el:1 ~ec:1 ~stale:false ()));
  run_occurrence_case ~scope:"project" "project missing stale"
    (project
       {|{"file":"logical/main.ml","start":{"line":1,"col":0},"end":{"line":1,"col":1}}|});
  run_occurrence_case ~scope:"project" "unknown member"
    (project
       {|{"file":"logical/main.ml","start":{"line":1,"col":0},"end":{"line":1,"col":1},"stale":false,"extra":0}|});
  run_occurrence_case ~scope:"project" "duplicate file"
    (project
       {|{"file":"logical/main.ml","file":"logical/main.ml","start":{"line":1,"col":0},"end":{"line":1,"col":1},"stale":false}|});
  run_occurrence_case ~scope:"project" "stale type"
    (project
       {|{"file":"logical/main.ml","start":{"line":1,"col":0},"end":{"line":1,"col":1},"stale":"no"}|});
  run_occurrence_case ~scope:"project" "file type"
    (project
       {|{"file":1,"start":{"line":1,"col":0},"end":{"line":1,"col":1},"stale":false}|});
  run_occurrence_case ~scope:"project" "position extra"
    (project
       {|{"file":"logical/main.ml","start":{"line":1,"col":0,"byte":0},"end":{"line":1,"col":1},"stale":false}|});
  run_occurrence_case ~scope:"project" "position duplicate"
    (project
       {|{"file":"logical/main.ml","start":{"line":1,"line":1,"col":0},"end":{"line":1,"col":1},"stale":false}|});
  run_occurrence_case ~scope:"project" "position unsafe"
    (project
       {|{"file":"logical/main.ml","start":{"line":9007199254740992,"col":0},"end":{"line":9007199254740992,"col":1},"stale":false}|});
  run_occurrence_case ~scope:"project" "position fraction"
    (project
       {|{"file":"logical/main.ml","start":{"line":1.5,"col":0},"end":{"line":2,"col":1},"stale":false}|});
  run_occurrence_case ~scope:"project" "position line zero"
    (project
       {|{"file":"logical/main.ml","start":{"line":0,"col":0},"end":{"line":1,"col":1},"stale":false}|});
  run_occurrence_case ~scope:"project" "position column negative"
    (project
       {|{"file":"logical/main.ml","start":{"line":1,"col":-1},"end":{"line":1,"col":1},"stale":false}|});
  run_occurrence_case ~scope:"project" "reversed range"
    (project
       (occurrence ~file:"logical/main.ml" ~sl:2 ~sc:0 ~el:1 ~ec:1 ~stale:false
          ()));
  run_occurrence_case ~scope:"project" "file NUL"
    (project
       {|{"file":"bad\u0000path","start":{"line":1,"col":0},"end":{"line":1,"col":1},"stale":false}|});
  run_occurrence_case ~scope:"project" "value not array"
    (return {|{"file":"logical/main.ml"}|});
  [%expect
    {|
    -- buffer has file --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: occurrence has unexpected or duplicate members
    metadata: false
    invocations: 1
    -- buffer missing stale --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: occurrence has unexpected or duplicate members
    metadata: false
    invocations: 1
    -- project missing file --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: occurrence has unexpected or duplicate members
    metadata: false
    invocations: 1
    -- project missing stale --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: occurrence has unexpected or duplicate members
    metadata: false
    invocations: 1
    -- unknown member --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: occurrence has unexpected or duplicate members
    metadata: false
    invocations: 1
    -- duplicate file --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: occurrence has unexpected or duplicate members
    metadata: false
    invocations: 1
    -- stale type --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: stale is not a boolean
    metadata: false
    invocations: 1
    -- file type --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: file is not a string
    metadata: false
    invocations: 1
    -- position extra --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: position has unexpected or duplicate members
    metadata: false
    invocations: 1
    -- position duplicate --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: position has unexpected or duplicate members
    metadata: false
    invocations: 1
    -- position unsafe --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: position members must be safe integers
    metadata: false
    invocations: 1
    -- position fraction --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: position members must be safe integers
    metadata: false
    invocations: 1
    -- position line zero --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: Mentat_ocaml.Position.make: line must be >= 1
    metadata: false
    invocations: 1
    -- position column negative --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: Mentat_ocaml.Position.make: column must be >= 0
    metadata: false
    invocations: 1
    -- reversed range --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: Mentat_ocaml.Range.make: end_ must not be before start
    metadata: false
    invocations: 1
    -- file NUL --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: file contains NUL
    metadata: false
    invocations: 1
    -- value not array --
    status: failed failed
    message: could not decode ocamlmerlin occurrences result: occurrences value is not an array
    metadata: false
    invocations: 1 |}]

let run_protocol_case label configure =
  with_world ("protocol-" ^ label) @@ fun world ->
  configure world;
  Printf.printf "-- %s --\n" label;
  print_result world
    (run world (input ~path:"logical/main.ml" ~line:1 ~column:4 ()))

let%expect_test "Merlin envelopes reject malformed duplicate and error classes"
    =
  run_protocol_case "not JSON" (fun world -> set_response world 1 "not-json");
  run_protocol_case "not object" (fun world -> set_response world 1 "[]");
  run_protocol_case "missing class" (fun world ->
      set_response world 1 {|{"value":[]}|});
  run_protocol_case "missing value" (fun world ->
      set_response world 1 {|{"class":"return"}|});
  run_protocol_case "class type" (fun world ->
      set_response world 1 {|{"class":1,"value":[]}|});
  run_protocol_case "unknown class" (fun world ->
      set_response world 1 {|{"class":"wat","value":[]}|});
  run_protocol_case "duplicate class" (fun world ->
      set_response world 1 {|{"class":"return","class":"return","value":[]}|});
  run_protocol_case "duplicate value" (fun world ->
      set_response world 1 {|{"class":"return","value":[],"value":[]}|});
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
    -- duplicate value --
    status: failed failed
    message: could not decode ocamlmerlin response: response has duplicate value members
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

let%expect_test "relative empty buffer and outside occurrence paths normalize" =
  with_world ~cwd:Primary_logical "path-protocol" @@ fun world ->
  let run_case label file =
    set_response world
      (invocation_count world + 1)
      (return_occurrences
         [ occurrence ~file ~sl:1 ~sc:4 ~el:1 ~ec:10 ~stale:false () ]);
    Printf.printf "-- %s --\n" label;
    let result =
      run world (input ~path:"main.ml" ~line:1 ~column:4 ~scope:"project" ())
    in
    print_result world result
  in
  run_case "relative" "logical/../logical/main.ml";
  run_case "empty" "";
  run_case "buffer sentinel" "*buffer*";
  run_case "outside relative" "../outside/outside.ml";
  run_case "outside absolute" (Filename.concat world.outside_dir "outside.ml");
  [%expect
    {|
    -- relative --
    status: completed
    text: "OCaml references for main.ml:1:4\nscope: project\nreferences: 1 returned of 1, offset 1, status complete\nindex_status: unknown\nbackend: ocamlmerlin\n- main.ml:1:4-1:10"
    truncated: false
    -- empty --
    status: completed
    text: "OCaml references for main.ml:1:4\nscope: project\nreferences: 1 returned of 1, offset 1, status complete\nindex_status: unknown\nbackend: ocamlmerlin\n- main.ml:1:4-1:10"
    truncated: false
    -- buffer sentinel --
    status: completed
    text: "OCaml references for main.ml:1:4\nscope: project\nreferences: 1 returned of 1, offset 1, status complete\nindex_status: unknown\nbackend: ocamlmerlin\n- main.ml:1:4-1:10"
    truncated: false
    -- outside relative --
    status: failed failed
    message: could not normalize ocamlmerlin occurrence: path is outside the workspace: <outside>/outside.ml
    metadata: false
    -- outside absolute --
    status: failed failed
    message: could not normalize ocamlmerlin occurrence: path is outside the workspace: <outside>/outside.ml
    metadata: false |}]

let run_filesystem_case label setup path =
  with_world ("fs-" ^ label) @@ fun world ->
  setup world;
  Printf.printf "-- %s --\n" label;
  let result = run world (input ~path:(path world) ~line:1 ~column:0 ()) in
  print_result world result;
  Printf.printf "invocations: %d\n" (invocation_count world)

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
  set_response world 1 (standard_project_response "logical/main.ml");
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
  Printf.printf "permissions still cached: %d invocations: %d\n"
    (List.length (Tool.Call.permissions escaped_call))
    (invocation_count world);
  [%expect
    {|
    -- content replacement --
    status: completed
    text: "OCaml references for logical/main.ml:1:4\nscope: project\nreferences: 1 returned of 1, offset 1, status complete\nindex_status: unknown\nbackend: ocamlmerlin\n- logical/main.ml:1:4-1:10"
    truncated: false
    current stdin exact: true
    -- symlink replacement --
    status: failed invalid_input
    message: logical/main.ml: path resolves outside workspace
    metadata: false
    permissions still cached: 1 invocations: 1 |}]

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
      run_protocol_case behavior (fun behavior_world ->
          set_behavior behavior_world 1 behavior))
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
  run_protocol_case "overflow_stderr" (fun overflow ->
      set_behavior overflow 1 "overflow_stderr");
  run_protocol_case "incomplete" (fun incomplete ->
      set_response incomplete 1 standard_buffer_response;
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
    -- overflow_stderr --
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
  with_world "cancel-after-read" @@ fun after_read ->
  set_response after_read 1 standard_buffer_response;
  let polls = ref 0 in
  print_endline "-- after complete source read --";
  print_result after_read
    (run
       ~cancelled:(fun () ->
         incr polls;
         !polls = 2)
       after_read
       (input ~path:"logical/main.ml" ~line:1 ~column:4 ()));
  Printf.printf "polls: %d invocations: %d\n" !polls
    (invocation_count after_read);
  with_world "cancel-before" @@ fun before ->
  set_response before 1 standard_buffer_response;
  print_endline "-- before observation --";
  print_result before
    (run
       ~cancelled:(fun () -> true)
       before
       (input ~path:"logical/main.ml" ~line:1 ~column:4 ()));
  Printf.printf "invocations: %d\n" (invocation_count before);
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
    -- after complete source read --
    status: interrupted cancelled=true
    reason: tool call cancelled
    polls: 2 invocations: 0
    -- before observation --
    status: interrupted cancelled=true
    reason: tool call cancelled
    invocations: 0 |}]

let%expect_test "cancellation after child exit discards the complete response" =
  with_world "cancel-after" @@ fun world ->
  set_response world 1 standard_buffer_response;
  write_disk (plan_file world "cancel-after" 1) "";
  let result =
    run
      ~cancelled:(fun () ->
        Sys.file_exists (Filename.concat world.plan_dir "cancel-now"))
      world
      (input ~path:"logical/main.ml" ~line:1 ~column:4 ())
  in
  print_result world result;
  Printf.printf "invocations: %d\n" (invocation_count world);
  [%expect
    {|
    status: interrupted cancelled=true
    reason: tool call cancelled
    invocations: 1 |}]

let print_page_contract label result =
  print_status result;
  let output = output_exn result in
  let lines = String.split_on_char '\n' (Tool.Output.text output) in
  let references = List.filter (String.starts_with ~prefix:"- ") lines in
  let first, last =
    match references with
    | [] -> ("<none>", "<none>")
    | first :: rest ->
        (first, List.fold_left (fun _ reference -> reference) first rest)
  in
  let continuation =
    List.find_opt (String.starts_with ~prefix:"next: ") lines
    |> Option.value ~default:"<none>"
  in
  let semantic = output_json result in
  Printf.printf
    "%s: listed=%d first=%s last=%s continuation=%s references=%d files=%d \
     truncated=%b\n"
    label (List.length references) first last continuation
    (json_int_value (required_member "references" semantic))
    (json_int_value (required_member "files" semantic))
    (Tool.Output.truncated output)

let%expect_test "the default page returns 200 of 201 references exactly" =
  with_world "default-page" @@ fun world ->
  let response =
    List.init 201 (fun index ->
        let line = index + 1 in
        occurrence ~file:"logical/main.ml" ~sl:line ~sc:0 ~el:line ~ec:1
          ~stale:false ())
    |> return_occurrences
  in
  set_response world 1 response;
  let first =
    run world
      (input ~path:"logical/main.ml" ~line:1 ~column:4 ~scope:"project" ())
  in
  print_page_contract "first" first;
  set_response world 2 response;
  let final =
    run world
      (input ~path:"logical/main.ml" ~line:1 ~column:4 ~scope:"project"
         ~offset:201 ())
  in
  print_page_contract "final" final;
  Printf.printf "default_limit=%d invocations=%d\n" Find.default_limit
    (invocation_count world);
  [%expect
    {|
    status: completed
    first: listed=200 first=- logical/main.ml:1:0-1:1 last=- logical/main.ml:200:0-200:1 continuation=next: ocaml_find_references {"path":"logical/main.ml","line":1,"column":4,"scope":"project","include_stale":false,"offset":201,"limit":200} references=201 files=1 truncated=true
    status: completed
    final: listed=1 first=- logical/main.ml:201:0-201:1 last=- logical/main.ml:201:0-201:1 continuation=<none> references=201 files=1 truncated=false
    default_limit=200 invocations=2 |}]

let%expect_test "durable text JSON pagination and D2 boundary survive replay" =
  with_world "durable" @@ fun world ->
  set_response world 1 (paging_response "logical/main.ml");
  let result =
    run world
      (input ~path:"logical/main.ml" ~line:1 ~column:4 ~scope:"project"
         ~include_stale:true ~limit:2 ())
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
      "source";
      "argv";
    ]
  in
  let names = json_member_names stored in
  Printf.printf "forbidden members: %s\n"
    (match List.filter (fun name -> List.mem name names) forbidden with
    | [] -> "<none>"
    | present -> String.concat "," present);
  let json = output_json replayed in
  Printf.printf
    "version=%d references=%d files=%d members=%s invocations-after-replay=%d\n"
    (json_int_value (required_member "version" json))
    (json_int_value (required_member "references" json))
    (json_int_value (required_member "files" json))
    (json_member_names json
    |> List.sort_uniq String.compare
    |> String.concat ",")
    (invocation_count world);
  [%expect
    {|
    roundtrip equal: true
    output text equal: true json equal: true truncated equal: true
    forbidden members: <none>
    version=1 references=3 files=1 members=files,references,version invocations-after-replay=1 |}]

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
    diagnostic: Ocaml.Find_references.make: program prefix must not be empty
    raised: true invocations: 0 |}]

let%expect_test
    "normalization dedup stale filtering ordering and continuation compose" =
  with_world "paging" @@ fun world ->
  let response =
    paging_response
      ~duplicate_file:(Filename.concat world.ws_dir "logical/main.ml")
      ~first_file:"lib/shared.ml" "logical/main.ml"
  in
  set_response world 1 response;
  let first =
    run world
      (input ~path:"logical/main.ml" ~line:1 ~column:4 ~scope:"project" ~limit:1
         ())
  in
  print_endline "-- first --";
  print_status first;
  output_summary first;
  set_response world 2 response;
  let second =
    run world
      (input ~path:"logical/main.ml" ~line:1 ~column:4 ~scope:"project"
         ~offset:2 ~limit:1 ())
  in
  print_endline "-- second --";
  print_status second;
  output_summary second;
  let beyond =
    input ~path:"logical/main.ml" ~line:1 ~column:4 ~scope:"project" ~offset:99
      ~limit:1 ()
  in
  set_response world 3 response;
  let beyond = run world beyond in
  print_endline "-- beyond --";
  print_status beyond;
  output_summary beyond;
  Printf.printf "invocations: %d\n" (invocation_count world);
  [%expect
    {|
    -- first --
    status: completed
    text: "OCaml references for logical/main.ml:1:4\nscope: project\nreferences: 1 returned of 4, 1 stale skipped, 1 duplicate skipped, offset 1, status partial\nindex_status: unknown\nbackend: ocamlmerlin\n- lib/shared.ml:1:4-1:10\nnext: ocaml_find_references {\"path\":\"logical/main.ml\",\"line\":1,\"column\":4,\"scope\":\"project\",\"include_stale\":false,\"offset\":2,\"limit\":1}\nstale note: rebuild the project index, for Dune usually `dune build @ocaml-index`."
    semantic: version=1 references=2 files=2 truncated=true
    -- second --
    status: completed
    text: "OCaml references for logical/main.ml:1:4\nscope: project\nreferences: 1 returned of 4, 1 stale skipped, 1 duplicate skipped, offset 2, status complete\nindex_status: unknown\nbackend: ocamlmerlin\n- logical/main.ml:3:4-3:10\nstale note: rebuild the project index, for Dune usually `dune build @ocaml-index`."
    semantic: version=1 references=2 files=2 truncated=false
    -- beyond --
    status: completed
    text: "OCaml references for logical/main.ml:1:4\nscope: project\nreferences: 0 returned of 4, 1 stale skipped, 1 duplicate skipped, offset 99, status complete\nindex_status: unknown\nbackend: ocamlmerlin\nstale note: rebuild the project index, for Dune usually `dune build @ocaml-index`."
    semantic: version=1 references=2 files=2 truncated=false
    invocations: 3 |}]

