(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for structural OCaml search.
   Every effectful scenario resolves a real workspace capability and drives
   provider JSON through [Call.decode], cached permissions, [Call.run], erased
   output, and (where relevant) durable replay. The matcher and Glob protocol
   implementation remain deliberately inaccessible. *)

open Windtrap
open Test_tools_support
module Json = Jsont.Json
module Tool = Mentat_tool
module Search = Mentat_tools.Ocaml.Search_expressions
module Workspace = Mentat_workspace
module Permission = Mentat_permission

type cwd = Primary_root | Primary_logical | Auxiliary_root

type world = {
  ws_dir : string;
  aux_dir : string;
  outside_dir : string;
  tool : Tool.t;
}

let add_optional name encode value fields =
  match value with
  | None -> fields
  | Some value -> (name, encode value) :: fields

let input ?paths ?offset ?limit pattern =
  [ ("pattern", Json.string pattern) ]
  |> add_optional "paths"
       (fun paths -> Json.list (List.map (fun path -> Json.string path) paths))
       paths
  |> add_optional "offset" (fun value -> Json.int value) offset
  |> add_optional "limit" (fun value -> Json.int value) limit
  |> List.rev |> json_object

let print_status ?(normalize = Fun.id) result =
  match Tool.Result.status result with
  | Tool.Result.Completed -> print_endline "status: completed"
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf "status: failed %s\nmessage: %s\nmetadata: %b\n"
        (failure_name kind) (normalize message) (Option.is_some metadata)
  | Tool.Result.Interrupted { reason; cancelled } ->
      Printf.printf "status: interrupted cancelled=%b\nreason: %s\n" cancelled
        reason

let decode_call tool provider_input =
  match Tool.Call.decode tool (model_call tool provider_input) with
  | Ok call -> call
  | Error error ->
      failf "call decode failed: %s" (Tool.Call.Decode_error.message error)

let run ?(cancelled = fun () -> false) world provider_input =
  Tool.Call.run (decode_call world.tool provider_input) ~cancelled |> finished

let normalize world text =
  text
  |> String.replace_all ~sub:world.aux_dir ~by:"<auxiliary>"
  |> String.replace_all ~sub:world.outside_dir ~by:"<outside>"
  |> String.replace_all ~sub:world.ws_dir ~by:"<workspace>"

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

let run_case ?(json = false) ?cancelled world name provider_input =
  print_case name;
  print_result ~json world (run ?cancelled world provider_input)

let abs path = Lpath.Abs.of_string_exn path

let write_disk path contents =
  mkdir_p (Filename.dirname path);
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel contents)

let structural_source =
  String.concat "\n"
    [
      "type item = { alpha : int; beta : int }";
      "module M = struct let map f xs = List.map f xs end";
      "let mapped = List.map succ numbers";
      "let filtered = (List.filter ready numbers : int list)";
      "let same = pair token token";
      "let different = pair token other";
      "let present = call ~label:1 ?optional:None value";
      "let missing = call ~label:1 value";
      "let choose = function Some x -> x | None -> fallback";
      "let classify value =";
      "  match value with";
      "  | None -> zero";
      "  | Some x -> x";
      "let record = { beta = two; alpha = one }";
      "let fields r = r.beta + r.alpha";
      "let nested = let first = compute () and second = load () in first";
      "let local x = let module N = struct let value = x end in N.value";
      "let multiline =";
      "  List.filter";
      "    (fun value -> ready value)";
      "    numbers";
      "";
    ]

let populate_main root =
  let file path contents = write_disk (Filename.concat root path) contents in
  file "structural.ml" structural_source;
  file "logical/cwd.ml" "let cwd_hit = needle\n";
  file "logical/deep/nested.ml" "let nested_hit = needle\n";
  file "lib/shared.ml" "let shared = needle\n";
  file "lib/only_main.ml" "let only_main = needle\n";
  file "direct.txt" "let direct = needle\n";
  file "ignored-direct.ml" "let direct_ignored = needle\n";
  file ".gitignore" "ignored-direct.ml\nignored/\nreopen/*\n!reopen/keep/\n";
  file "ignored/hidden.ml" "let hidden = needle\n";
  file "reopen/drop/hidden.ml" "let drop = needle\n";
  file "reopen/keep/visible.ml" "let visible = needle\n";
  file "local/.ignore" "ignored.ml\n";
  file "local/ignored.ml" "let local_ignored = needle\n";
  file "local/visible.ml" "let local_visible = needle\n";
  file "rg/.rgignore" "hidden.ml\n";
  file "rg/hidden.ml" "let rg_hidden = needle\n";
  file "rg/visible.ml" "let rg_visible = needle\n";
  file ".git/private.ml" "let vcs_private = needle\n";
  file ".hg/private.ml" "let hg_private = needle\n"

let populate_aux root =
  write_disk (Filename.concat root "lib/shared.ml") "let shared = needle\n";
  write_disk (Filename.concat root "lib/only_aux.ml") "let only_aux = needle\n";
  write_disk (Filename.concat root "logical/aux.ml") "let aux_cwd = needle\n"

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

let with_world ?(cwd = Primary_root) fn =
  Eio_main.run @@ fun stdenv ->
  let stdenv = (stdenv :> Eio_unix.Stdenv.base) in
  let raw = Filename.temp_dir "mentat-ocaml-search-" "" in
  Fun.protect ~finally:(fun () -> remove_tree raw) @@ fun () ->
  let base = Unix.realpath raw in
  let ws_dir = Filename.concat base "workspace" in
  let aux_dir = Filename.concat base "auxiliary" in
  let outside_dir = Filename.concat base "outside" in
  List.iter mkdir [ ws_dir; aux_dir; outside_dir ];
  populate_main ws_dir;
  populate_aux aux_dir;
  write_disk (Filename.concat outside_dir "outside.ml") "let outside = needle\n";
  let logical = workspace ~cwd ws_dir aux_dir in
  Eio.Switch.run @@ fun sw ->
  let io = resolve_exn ~sw ~stdenv ~logical () in
  fn { ws_dir; aux_dir; outside_dir; tool = Search.make io }

let decode_verdict tool label provider_input =
  match Tool.Call.decode tool (model_call tool provider_input) with
  | Ok call ->
      Printf.printf "%s: accepted canonical=%s\n" label
        (json_string (Tool.Call.input call))
  | Error error ->
      Printf.printf "%s: rejected diagnostic=%b\n" label
        (not (String.is_empty (Tool.Call.Decode_error.message error)))

let print_permissions call =
  let requests = Tool.Call.permissions call in
  Printf.printf "requests: %d\n" (List.length requests);
  List.iter
    (fun request ->
      Printf.printf "source: %s\n"
        (Option.value ~default:"none" (Permission.Request.source request));
      List.iter
        (fun item ->
          match Permission.Request.Item.access item with
          | Permission.Access.Path
              { op; scope = Permission.Access.Path_scope.Workspace path } ->
              Printf.printf "access: %s key=%s path=%s change=%b\n"
                (path_op_name op)
                (Workspace.Root.Key.to_string (Workspace.Path.root_key path))
                (Workspace.Path.display path)
                (Option.is_some (Permission.Request.Item.change item))
          | access ->
              failf "unexpected search permission: %a" Permission.Access.pp
                access)
        (Permission.Request.items request))
    requests

let output_json result =
  match Tool.Output.json (output_exn result) with
  | Some json -> json
  | None -> fail "completed search output has no JSON projection"

let print_frame result = print_endline (json_string (output_json result))

let print_finding_ranges result =
  Tool.Output.text (output_exn result)
  |> String.split_on_char '\n'
  |> List.filter (fun line ->
      (not (String.is_empty line))
      && (not (String.starts_with ~prefix:"  " line))
      && (not (String.starts_with ~prefix:"ocaml_search_expressions " line))
      && (not (String.starts_with ~prefix:"next: " line))
      && (not (String.equal line "No matches"))
      && not (String.equal line "skipped:"))
  |> List.iter print_endline

let%expect_test "declaration and provider input enforce strict safe JSON values"
    =
  with_world @@ fun world ->
  let declaration = Tool.declaration world.tool in
  Printf.printf "name: %s\n" (Mentat_llm.Tool.name declaration);
  Printf.printf "schema: %s\n"
    (json_string (Mentat_llm.Tool.input_schema declaration));
  decode_verdict world.tool "minimal" (input "List.map __ __");
  decode_verdict world.tool "full"
    (input ~paths:[ "lib" ] ~offset:2 ~limit:3 "needle");
  decode_verdict world.tool "limit maximum" (input ~limit:Search.max_limit "x");
  List.iter
    (fun (label, provider_input) ->
      decode_verdict world.tool label provider_input)
    [
      ("missing pattern", json_object []);
      ( "unknown member",
        json_object [ ("pattern", Json.string "x"); ("glob", Json.string "*") ]
      );
      ( "duplicate pattern",
        json_object
          [ ("pattern", Json.string "x"); ("pattern", Json.string "y") ] );
      ("pattern type", json_object [ ("pattern", Json.int 1) ]);
      ("empty pattern", input "");
      ("NUL pattern", input "bad\000pattern");
      ( "paths type",
        json_object
          [ ("pattern", Json.string "x"); ("paths", Json.string "lib") ] );
      ( "duplicate paths",
        json_object
          [
            ("pattern", Json.string "x");
            ("paths", Json.list [ Json.string "lib" ]);
            ("paths", Json.list [ Json.string "bin" ]);
          ] );
      ("paths empty", input ~paths:[] "x");
      ( "path element type",
        json_object
          [ ("pattern", Json.string "x"); ("paths", Json.list [ Json.int 1 ]) ]
      );
      ("path empty", input ~paths:[ "" ] "x");
      ("path NUL", input ~paths:[ "bad\000path" ] "x");
      ( "offset string",
        json_object
          [ ("pattern", Json.string "x"); ("offset", Json.string "1") ] );
      ( "duplicate offset",
        json_object
          [
            ("pattern", Json.string "x");
            ("offset", Json.int 1);
            ("offset", Json.int 2);
          ] );
      ( "offset fraction",
        json_object
          [ ("pattern", Json.string "x"); ("offset", Json.number 1.5) ] );
      ( "offset unsafe",
        json_object
          [
            ("pattern", Json.string "x");
            ("offset", Json.number 9_007_199_254_740_992.);
          ] );
      ( "offset safe maximum",
        json_object
          [
            ("pattern", Json.string "x");
            ("offset", Json.number 9_007_199_254_740_991.);
          ] );
      ("offset zero", input ~offset:0 "x");
      ("offset negative", input ~offset:(-1) "x");
      ( "limit string",
        json_object [ ("pattern", Json.string "x"); ("limit", Json.string "1") ]
      );
      ( "duplicate limit",
        json_object
          [
            ("pattern", Json.string "x");
            ("limit", Json.int 1);
            ("limit", Json.int 2);
          ] );
      ( "limit fraction",
        json_object [ ("pattern", Json.string "x"); ("limit", Json.number 1.5) ]
      );
      ( "limit unsafe",
        json_object
          [
            ("pattern", Json.string "x");
            ("limit", Json.number 9_007_199_254_740_992.);
          ] );
      ("limit zero", input ~limit:0 "x");
      ("limit over max", input ~limit:(Search.max_limit + 1) "x");
    ];
  [%expect
    {|
    name: ocaml_search_expressions
    schema: {"type":"object","properties":{"pattern":{"type":"string","description":"One complete OCaml expression pattern. __ replaces any expression but does not relax OCaml grammar; a match still needs complete PATTERN -> EXPR clauses. __1/__2 are unification metavariables, f ?arg:PRESENT / f ?arg:MISSING constrain optional arguments, and match/record clauses match as sets.","minLength":1},"paths":{"type":"array","description":"Workspace-relative or workspace-contained absolute file or directory roots. Defaults to the workspace current directory.","items":{"type":"string","minLength":1},"minItems":1},"offset":{"type":"integer","description":"One-based first finding. Defaults to 1.","minimum":1,"maximum":9007199254740991},"limit":{"type":"integer","description":"Maximum findings returned. Defaults to 100.","minimum":1,"maximum":1000}},"required":["pattern"],"additionalProperties":false}
    minimal: accepted canonical={"pattern":"List.map __ __"}
    full: accepted canonical={"limit":3,"offset":2,"paths":["lib"],"pattern":"needle"}
    limit maximum: accepted canonical={"limit":1000,"pattern":"x"}
    missing pattern: rejected diagnostic=true
    unknown member: rejected diagnostic=true
    duplicate pattern: rejected diagnostic=true
    pattern type: rejected diagnostic=true
    empty pattern: rejected diagnostic=true
    NUL pattern: rejected diagnostic=true
    paths type: rejected diagnostic=true
    duplicate paths: rejected diagnostic=true
    paths empty: rejected diagnostic=true
    path element type: rejected diagnostic=true
    path empty: rejected diagnostic=true
    path NUL: rejected diagnostic=true
    offset string: rejected diagnostic=true
    duplicate offset: rejected diagnostic=true
    offset fraction: rejected diagnostic=true
    offset unsafe: rejected diagnostic=true
    offset safe maximum: accepted canonical={"offset":9007199254740991,"pattern":"x"}
    offset zero: rejected diagnostic=true
    offset negative: rejected diagnostic=true
    limit string: rejected diagnostic=true
    duplicate limit: rejected diagnostic=true
    limit fraction: rejected diagnostic=true
    limit unsafe: rejected diagnostic=true
    limit zero: rejected diagnostic=true
    limit over max: rejected diagnostic=true
    |}]

let%expect_test
    "structural expression patterns preserve the legacy matcher contract" =
  with_world @@ fun world ->
  let cases =
    [
      ("application and target constraint erasure", "List.filter __ __");
      ("identifier suffix", "map __ __");
      ("numbered metavariable unifies", "pair __1 __1");
      ("optional present", "call ?optional:PRESENT");
      ("optional missing", "call ?optional:MISSING");
      ("clause set", "match __ with Some x -> x | None -> __");
      ("function clause set", "function None -> __ | Some x -> x");
      ("record fields as set", "{ alpha = __; beta = __ }");
      ("record field read", "__.alpha");
      ("let bindings as set", "let second = __ and first = __ in __");
      ("qualified suffix across nested item bodies", "N.value");
    ]
  in
  List.iter
    (fun (label, pattern) ->
      print_case label;
      let result = run world (input ~paths:[ "structural.ml" ] pattern) in
      print_status result;
      match Tool.Result.status result with
      | Tool.Result.Completed ->
          print_frame result;
          print_finding_ranges result
      | Tool.Result.Failed _ | Tool.Result.Interrupted _ -> ())
    cases;
  [%expect
    {|-- application and target constraint erasure --
status: completed
{"version":1,"shape":"matches","total":2,"files":1}
structural.ml:4:15-4:53
structural.ml:19:2-21:11
-- identifier suffix --
status: completed
{"version":1,"shape":"matches","total":2,"files":1}
structural.ml:2:33-2:46
structural.ml:3:13-3:34
-- numbered metavariable unifies --
status: completed
{"version":1,"shape":"matches","total":1,"files":1}
structural.ml:5:11-5:27
-- optional present --
status: completed
{"version":1,"shape":"matches","total":1,"files":1}
structural.ml:7:14-7:48
-- optional missing --
status: completed
{"version":1,"shape":"matches","total":1,"files":1}
structural.ml:8:14-8:33
-- clause set --
status: completed
{"version":1,"shape":"matches","total":1,"files":1}
structural.ml:11:2-13:15
-- function clause set --
status: completed
{"version":1,"shape":"matches","total":1,"files":1}
structural.ml:9:13-9:52
-- record fields as set --
status: completed
{"version":1,"shape":"matches","total":1,"files":1}
structural.ml:14:13-14:40
-- record field read --
status: completed
{"version":1,"shape":"matches","total":1,"files":1}
structural.ml:15:24-15:31
-- let bindings as set --
status: completed
{"version":1,"shape":"matches","total":1,"files":1}
structural.ml:16:13-16:65
-- qualified suffix across nested item bodies --
status: completed
{"version":1,"shape":"matches","total":5,"files":1}
structural.ml:7:43-7:48
structural.ml:8:28-8:33
structural.ml:11:8-11:13
structural.ml:17:57-17:64
structural.ml:20:24-20:29|}]

let%expect_test
    "invalid and unsupported complete-expression patterns fail loudly" =
  with_world @@ fun world ->
  List.iter
    (fun (label, pattern) -> run_case world label (input pattern))
    [
      ("syntax", "let x");
      ("incomplete clause", "match __ with Some x");
      ("expression constraint", "(__ : int)");
      ("coercion", "(__ :> int)");
      ("pattern constraint", "function (x : int) -> x");
      ("binding annotation", "let x : int = __ in x");
    ];
  [%expect
    {|
    -- syntax --
    status: failed invalid_input
    message: Syntax error at line 1, column 5. A pattern must be one complete OCaml expression; `__` replaces an expression but does not relax OCaml grammar. Match clauses require `PATTERN -> EXPR`, for example: `match __ with Some x -> __ | None -> __`.
    metadata: false
    -- incomplete clause --
    status: failed invalid_input
    message: Syntax error at line 1, column 20. A pattern must be one complete OCaml expression; `__` replaces an expression but does not relax OCaml grammar. Match clauses require `PATTERN -> EXPR`, for example: `match __ with Some x -> __ | None -> __`.
    metadata: false
    -- expression constraint --
    status: failed invalid_input
    message: type-constrained expression (e : t) is not supported: type-constrained patterns require a typed backend
    metadata: false
    -- coercion --
    status: failed invalid_input
    message: coercion (e :> t) is not supported: type-constrained patterns require a typed backend
    metadata: false
    -- pattern constraint --
    status: failed invalid_input
    message: type-constrained pattern (p : t) is not supported: type-constrained patterns require a typed backend
    metadata: false
    -- binding annotation --
    status: failed invalid_input
    message: type-annotated binding (let x : t = ...) is not supported: type-constrained patterns require a typed backend
    metadata: false
    |}]

let%expect_test
    "default nested absolute auxiliary and overlapping roots keep identity" =
  with_world @@ fun primary ->
  let aux_lib = Filename.concat primary.aux_dir "lib" in
  List.iter
    (fun (label, paths) ->
      print_case label;
      let result = run primary (input ?paths "needle") in
      print_status result;
      print_frame result;
      print_finding_ranges result)
    [
      ("default primary root", None);
      ("current relative root", Some [ "." ]);
      ("nested directory", Some [ "lib" ]);
      ("contained absolute", Some [ Filename.concat primary.ws_dir "lib" ]);
      ("regular non-ml file", Some [ "direct.txt" ]);
      ("overlapping roots deduplicate files", Some [ "."; "lib" ]);
      ("resolved duplicate roots", Some [ "lib"; "./lib" ]);
    ];
  write_disk (Filename.concat aux_lib "broken.ml") "let broken =\n";
  print_case "primary and auxiliary collision";
  let collision = run primary (input ~paths:[ "lib"; aux_lib ] "needle") in
  print_result ~json:true primary collision;
  let replayed = decode result_codec (encode result_codec collision) in
  Printf.printf "collision durable replay equal: %b\n"
    (Tool.Output.equal (output_exn collision) (output_exn replayed));
  with_world ~cwd:Primary_logical @@ fun logical ->
  run_case ~json:true logical "logical cwd default" (input "needle");
  run_case ~json:true logical "logical cwd absolute sibling"
    (input ~paths:[ Filename.concat logical.ws_dir "lib" ] "needle");
  with_world ~cwd:Auxiliary_root @@ fun auxiliary ->
  run_case ~json:true auxiliary "auxiliary cwd default" (input "needle");
  [%expect
    {|-- default primary root --
status: completed
{"version":1,"shape":"matches","total":7,"files":7}
lib/only_main.ml:1:16-1:22
lib/shared.ml:1:13-1:19
local/visible.ml:1:20-1:26
logical/cwd.ml:1:14-1:20
logical/deep/nested.ml:1:17-1:23
reopen/keep/visible.ml:1:14-1:20
rg/visible.ml:1:17-1:23
-- current relative root --
status: completed
{"version":1,"shape":"matches","total":7,"files":7}
lib/only_main.ml:1:16-1:22
lib/shared.ml:1:13-1:19
local/visible.ml:1:20-1:26
logical/cwd.ml:1:14-1:20
logical/deep/nested.ml:1:17-1:23
reopen/keep/visible.ml:1:14-1:20
rg/visible.ml:1:17-1:23
-- nested directory --
status: completed
{"version":1,"shape":"matches","total":2,"files":2}
lib/only_main.ml:1:16-1:22
lib/shared.ml:1:13-1:19
-- contained absolute --
status: completed
{"version":1,"shape":"matches","total":2,"files":2}
lib/only_main.ml:1:16-1:22
lib/shared.ml:1:13-1:19
-- regular non-ml file --
status: completed
{"version":1,"shape":"matches","total":1,"files":1}
direct.txt:1:13-1:19
-- overlapping roots deduplicate files --
status: completed
{"version":1,"shape":"matches","total":7,"files":7}
lib/only_main.ml:1:16-1:22
lib/shared.ml:1:13-1:19
local/visible.ml:1:20-1:26
logical/cwd.ml:1:14-1:20
logical/deep/nested.ml:1:17-1:23
reopen/keep/visible.ml:1:14-1:20
rg/visible.ml:1:17-1:23
-- resolved duplicate roots --
status: completed
{"version":1,"shape":"matches","total":2,"files":2}
lib/only_main.ml:1:16-1:22
lib/shared.ml:1:13-1:19
-- primary and auxiliary collision --
status: completed
text: "ocaml_search_expressions pattern=\"needle\" results=4/4 offset=1 limit=100 status=complete searched_files=4\n<auxiliary>/lib/only_aux.ml:1:15-1:21\n  1: let only_aux = needle\n<auxiliary>/lib/shared.ml:1:13-1:19\n  1: let shared = needle\nlib/only_main.ml:1:16-1:22\n  1: let only_main = needle\nlib/shared.ml:1:13-1:19\n  1: let shared = needle\nskipped:\n  <auxiliary>/lib/broken.ml reason=syntax_error syntax error at line 2, column 0\n"
truncated: true
json: {"version":1,"shape":"matches","total":4,"files":4}
collision durable replay equal: true
-- logical cwd default --
status: completed
text: "ocaml_search_expressions pattern=\"needle\" results=2/2 offset=1 limit=100 status=complete searched_files=2\ncwd.ml:1:14-1:20\n  1: let cwd_hit = needle\ndeep/nested.ml:1:17-1:23\n  1: let nested_hit = needle\n"
truncated: false
json: {"version":1,"shape":"matches","total":2,"files":2}
-- logical cwd absolute sibling --
status: completed
text: "ocaml_search_expressions pattern=\"needle\" results=2/2 offset=1 limit=100 status=complete searched_files=2\n<workspace>/lib/only_main.ml:1:16-1:22\n  1: let only_main = needle\n<workspace>/lib/shared.ml:1:13-1:19\n  1: let shared = needle\n"
truncated: false
json: {"version":1,"shape":"matches","total":2,"files":2}
-- auxiliary cwd default --
status: completed
text: "ocaml_search_expressions pattern=\"needle\" results=3/3 offset=1 limit=100 status=complete searched_files=3\nlib/only_aux.ml:1:15-1:21\n  1: let only_aux = needle\nlib/shared.ml:1:13-1:19\n  1: let shared = needle\nlogical/aux.ml:1:14-1:20\n  1: let aux_cwd = needle\n"
truncated: false
json: {"version":1,"shape":"matches","total":3,"files":3}|}]

let%expect_test
    "Glob ignore rules VCS pruning and direct file roots are observable" =
  with_world @@ fun world ->
  let result = run world (input "needle") in
  print_status result;
  print_frame result;
  print_finding_ranges result;
  run_case world "ignored file as an explicit regular root"
    (input ~paths:[ "ignored-direct.ml" ] "needle");
  run_case world "VCS metadata directory as root"
    (input ~paths:[ ".git" ] "needle");
  [%expect
    {|status: completed
{"version":1,"shape":"matches","total":7,"files":7}
lib/only_main.ml:1:16-1:22
lib/shared.ml:1:13-1:19
local/visible.ml:1:20-1:26
logical/cwd.ml:1:14-1:20
logical/deep/nested.ml:1:17-1:23
reopen/keep/visible.ml:1:14-1:20
rg/visible.ml:1:17-1:23
-- ignored file as an explicit regular root --
status: completed
text: "ocaml_search_expressions pattern=\"needle\" results=1/1 offset=1 limit=100 status=complete searched_files=1\nignored-direct.ml:1:21-1:27\n  1: let direct_ignored = needle\n"
truncated: false
-- VCS metadata directory as root --
status: completed
text: "ocaml_search_expressions pattern=\"needle\" results=0/0 offset=1 limit=100 status=complete searched_files=0\nNo matches\n"
truncated: false|}]

let%expect_test
    "missing outside symlink and unsupported roots are structured failures" =
  with_world @@ fun world ->
  Unix.symlink "lib" (Filename.concat world.ws_dir "link_dir");
  Unix.symlink "lib/shared.ml" (Filename.concat world.ws_dir "link_file.ml");
  Unix.mkfifo (Filename.concat world.ws_dir "pipe") 0o600;
  List.iter
    (fun (label, path) -> run_case world label (input ~paths:[ path ] "needle"))
    [
      ("missing", "missing");
      ("outside", Filename.concat world.outside_dir "outside.ml");
      ("directory symlink", "link_dir");
      ("file symlink", "link_file.ml");
      ("FIFO", "pipe");
    ];
  [%expect
    {|
    -- missing --
    status: failed not_found
    message: missing: path does not exist
    metadata: false
    -- outside --
    status: failed invalid_input
    message: path is outside workspace: <outside>/outside.ml
    metadata: false
    -- directory symlink --
    status: failed invalid_input
    message: link_dir: symlink search roots are not supported
    metadata: false
    -- file symlink --
    status: failed invalid_input
    message: link_file.ml: symlink search roots are not supported
    metadata: false
    -- FIFO --
    status: failed invalid_input
    message: pipe: expected a regular file or directory
    metadata: false
    |}]

let%expect_test
    "coverage records every legacy skip reason without hiding valid files" =
  (* [read_error] is one of the five reasons this coverage listing pins, and its
     only fixture is a mode-000 source the superuser reads anyway. *)
  if Unix.geteuid () = 0 then
    skip ~reason:"the superuser reads a mode-000 fixture file" ();
  with_world @@ fun world ->
  let coverage = Filename.concat world.ws_dir "coverage" in
  write_disk (Filename.concat coverage "binary.ml") "let x = \000bad\n";
  write_disk (Filename.concat coverage "invalid_utf8.ml") "let x = \255\n";
  write_disk (Filename.concat coverage "syntax.ml") "let x =\n";
  write_disk (Filename.concat coverage "valid.ml") "let found = needle\n";
  write_disk (Filename.concat coverage "unreadable.ml") "let hidden = needle\n";
  Unix.chmod (Filename.concat coverage "unreadable.ml") 0o000;
  let too_large = Filename.concat coverage "too_large.ml" in
  Out_channel.with_open_bin too_large (fun channel ->
      Out_channel.output_string channel "let huge = \"";
      Out_channel.output_string channel
        (String.make Search.max_source_bytes 'a');
      Out_channel.output_string channel "\"\n");
  Fun.protect
    ~finally:(fun () ->
      Unix.chmod (Filename.concat coverage "unreadable.ml") 0o600)
    (fun () ->
      let result = run world (input ~paths:[ "coverage" ] "needle") in
      print_status result;
      print_frame result;
      print_finding_ranges result;
      Tool.Output.text (output_exn result)
      |> String.split_on_char '\n'
      |> List.filter (String.starts_with ~prefix:"  coverage/")
      |> List.iter (fun line ->
          print_endline (String.sub line 2 (String.length line - 2))));
  [%expect
    {|status: completed
{"version":1,"shape":"matches","total":1,"files":1}
coverage/valid.ml:1:12-1:18
coverage/binary.ml reason=binary
coverage/invalid_utf8.ml reason=invalid_utf8
coverage/syntax.ml reason=syntax_error syntax error at line 2, column 0
coverage/too_large.ml reason=too_large
coverage/unreadable.ml reason=read_error coverage/unreadable.ml: filesystem I/O error|}]

let%expect_test "preview lines strip CR and truncate at a valid UTF-8 boundary"
    =
  with_world @@ fun world ->
  let previews = Filename.concat world.ws_dir "preview" in
  write_disk
    (Filename.concat previews "crlf.ml")
    "let selected =\r\n  List.filter\r\n    ready\r\n    values\r\n";
  let prefix = "let selected = String.length \"" in
  let padding = String.make (1_999 - String.length prefix) 'a' in
  write_disk
    (Filename.concat previews "utf8.ml")
    (prefix ^ padding ^ "é-tail\"\n");
  let crlf =
    run world (input ~paths:[ "preview/crlf.ml" ] "List.filter __ __")
  in
  print_case "CRLF multiline";
  print_result ~json:true world crlf;
  let utf8 =
    run world (input ~paths:[ "preview/utf8.ml" ] "String.length __")
  in
  print_case "UTF-8 boundary";
  print_status utf8;
  let line =
    Tool.Output.text (output_exn utf8)
    |> String.split_on_char '\n'
    |> List.find (String.starts_with ~prefix:"  1: ")
  in
  let prefix = "  1: " in
  let suffix = " [truncated]" in
  let rendered =
    String.sub line (String.length prefix)
      (String.length line - String.length prefix)
  in
  let text =
    String.sub rendered 0 (String.length rendered - String.length suffix)
  in
  Printf.printf "line bytes: %d\nvalid UTF-8: %b\ntruncated flag: %b\n"
    (String.length text)
    (String.is_valid_utf_8 text)
    (String.ends_with ~suffix rendered);
  Printf.printf "output truncated: %b\n"
    (Tool.Output.truncated (output_exn utf8));
  [%expect
    {|-- CRLF multiline --
status: completed
text: "ocaml_search_expressions pattern=\"List.filter __ __\" results=1/1 offset=1 limit=100 status=complete searched_files=1\npreview/crlf.ml:2:2-4:10\n  2:   List.filter\n  3:     ready\n  4:     values\n"
truncated: false
json: {"version":1,"shape":"matches","total":1,"files":1}
-- UTF-8 boundary --
status: completed
line bytes: 1999
valid UTF-8: true
truncated flag: true
output truncated: true|}]

let%expect_test "finding pagination advances within a file and beyond the end" =
  with_world @@ fun world ->
  write_disk
    (Filename.concat world.ws_dir "pages.ml")
    "let a = needle\nlet b = needle\nlet c = needle\nlet d = needle\n";
  let first = run world (input ~paths:[ "pages.ml" ] ~limit:2 "needle") in
  print_case "first";
  print_result ~json:true world first;
  let next = input ~paths:[ "pages.ml" ] ~offset:3 ~limit:2 "needle" in
  print_case "exact next replay";
  Printf.printf "next provider input: %s\n" (json_string next);
  let second = run world next in
  print_result ~json:true world second;
  run_case ~json:true world "beyond end"
    (input ~paths:[ "pages.ml" ] ~offset:99 ~limit:2 "needle");
  run_case ~json:true world "safe-integer offset beyond end"
    (input ~paths:[ "pages.ml" ] ~offset:9_007_199_254_740_991 ~limit:2 "needle");
  [%expect
    {|-- first --
status: completed
text: "ocaml_search_expressions pattern=\"needle\" results=2/4 offset=1 limit=2 status=partial searched_files=1\npages.ml:1:8-1:14\n  1: let a = needle\npages.ml:2:8-2:14\n  2: let b = needle\nnext: ocaml_search_expressions {\"pattern\":\"needle\",\"paths\":[\"pages.ml\"],\"offset\":3,\"limit\":2}\n"
truncated: true
json: {"version":1,"shape":"matches","total":4,"files":1}
-- exact next replay --
next provider input: {"pattern":"needle","paths":["pages.ml"],"offset":3,"limit":2}
status: completed
text: "ocaml_search_expressions pattern=\"needle\" results=2/4 offset=3 limit=2 status=complete searched_files=1\npages.ml:3:8-3:14\n  3: let c = needle\npages.ml:4:8-4:14\n  4: let d = needle\n"
truncated: false
json: {"version":1,"shape":"matches","total":4,"files":1}
-- beyond end --
status: completed
text: "ocaml_search_expressions pattern=\"needle\" results=0/4 offset=99 limit=2 status=complete searched_files=1\nNo matches\n"
truncated: false
json: {"version":1,"shape":"matches","total":4,"files":1}
-- safe-integer offset beyond end --
status: completed
text: "ocaml_search_expressions pattern=\"needle\" results=0/4 offset=9007199254740991 limit=2 status=complete searched_files=1\nNo matches\n"
truncated: false
json: {"version":1,"shape":"matches","total":4,"files":1}|}]

let%expect_test
    "nested-cwd consumes Glob provider addresses and preserves continuations" =
  with_world ~cwd:Primary_logical @@ fun logical ->
  write_disk
    (Filename.concat logical.ws_dir "logical/another.ml")
    "let another = needle\n";
  let first = run logical (input ~limit:1 "needle") in
  print_case "logical first";
  print_result ~json:true logical first;
  let logical_next = input ~paths:[ "." ] ~offset:2 ~limit:1 "needle" in
  print_case "logical exact continuation";
  Printf.printf "next: %s\n" (json_string logical_next);
  print_result ~json:true logical (run logical logical_next);
  with_world @@ fun primary ->
  let aux_lib = Filename.concat primary.aux_dir "lib" in
  let first = run primary (input ~paths:[ aux_lib ] ~limit:1 "needle") in
  print_case "auxiliary first";
  print_result ~json:true primary first;
  let auxiliary_next = input ~paths:[ aux_lib ] ~offset:2 ~limit:1 "needle" in
  print_case "auxiliary exact continuation";
  Printf.printf "next: %s\n" (normalize primary (json_string auxiliary_next));
  print_result ~json:true primary (run primary auxiliary_next);
  [%expect
    {|-- logical first --
status: completed
text: "ocaml_search_expressions pattern=\"needle\" results=1/3 offset=1 limit=1 status=partial searched_files=3\nanother.ml:1:14-1:20\n  1: let another = needle\nnext: ocaml_search_expressions {\"pattern\":\"needle\",\"paths\":[\".\"],\"offset\":2,\"limit\":1}\n"
truncated: true
json: {"version":1,"shape":"matches","total":3,"files":3}
-- logical exact continuation --
next: {"pattern":"needle","paths":["."],"offset":2,"limit":1}
status: completed
text: "ocaml_search_expressions pattern=\"needle\" results=1/3 offset=2 limit=1 status=partial searched_files=3\ncwd.ml:1:14-1:20\n  1: let cwd_hit = needle\nnext: ocaml_search_expressions {\"pattern\":\"needle\",\"paths\":[\".\"],\"offset\":3,\"limit\":1}\n"
truncated: true
json: {"version":1,"shape":"matches","total":3,"files":3}
-- auxiliary first --
status: completed
text: "ocaml_search_expressions pattern=\"needle\" results=1/2 offset=1 limit=1 status=partial searched_files=2\n<auxiliary>/lib/only_aux.ml:1:15-1:21\n  1: let only_aux = needle\nnext: ocaml_search_expressions {\"pattern\":\"needle\",\"paths\":[\"<auxiliary>/lib\"],\"offset\":2,\"limit\":1}\n"
truncated: true
json: {"version":1,"shape":"matches","total":2,"files":2}
-- auxiliary exact continuation --
next: {"pattern":"needle","paths":["<auxiliary>/lib"],"offset":2,"limit":1}
status: completed
text: "ocaml_search_expressions pattern=\"needle\" results=1/2 offset=2 limit=1 status=complete searched_files=2\n<auxiliary>/lib/shared.ml:1:13-1:19\n  1: let shared = needle\n"
truncated: false
json: {"version":1,"shape":"matches","total":2,"files":2}|}]

let%expect_test "enumeration handles large and concurrently growing directories"
    =
  with_world @@ fun world ->
  for index = 0 to 1_000 do
    write_disk
      (Filename.concat world.ws_dir (Printf.sprintf "bulk/f%04d.ml" index))
      "let hit = needle\n"
  done;
  let result =
    run world (input ~paths:[ "bulk" ] ~offset:1_000 ~limit:2 "needle")
  in
  print_status result;
  print_frame result;
  print_finding_ranges result;
  print_case "directory growth during typed enumeration";
  let polls = ref 0 in
  let mutated = ref false in
  let cancelled () =
    incr polls;
    if !polls = 100 then begin
      write_disk
        (Filename.concat world.ws_dir "bulk/f2000.ml")
        "let late = needle\n";
      mutated := true
    end;
    false
  in
  let growing =
    run ~cancelled world (input ~paths:[ "bulk" ] ~limit:1 "needle")
  in
  print_status growing;
  Printf.printf "mutated during enumeration: %b\noutput: %b\n" !mutated
    (Option.is_some (Tool.Result.output growing));
  [%expect
    {|status: completed
{"version":1,"shape":"matches","total":1001,"files":1001}
bulk/f0999.ml:1:10-1:16
bulk/f1000.ml:1:10-1:16
-- directory growth during typed enumeration --
status: completed
mutated during enumeration: true
output: true|}]

let%expect_test "permissions are exact cached reads from decoded roots only" =
  with_world ~cwd:Primary_logical @@ fun world ->
  let aux = Filename.concat world.aux_dir "lib" in
  List.iter
    (fun (label, provider_input) ->
      print_case label;
      let call = decode_call world.tool provider_input in
      print_permissions call;
      print_status ~normalize:(normalize world)
        (Tool.Call.run call ~cancelled:(fun () -> false) |> finished))
    [
      ("default logical cwd", input "needle");
      ("two roots", input ~paths:[ "../lib"; aux ] "needle");
      ("duplicate decoded roots", input ~paths:[ "../lib"; "../lib" ] "needle");
      ("missing", input ~paths:[ "missing" ] "needle");
      ("outside", input ~paths:[ world.outside_dir ] "needle");
    ];
  print_case "cached before observation";
  let call = decode_call world.tool (input ~paths:[ "../lib" ] "needle") in
  print_permissions call;
  Unix.rename
    (Filename.concat world.ws_dir "lib")
    (Filename.concat world.ws_dir "lib-moved");
  print_permissions call;
  print_status ~normalize:(normalize world)
    (Tool.Call.run call ~cancelled:(fun () -> false) |> finished);
  [%expect
    {|
    -- default logical cwd --
    requests: 1
    source: ocaml_search_expressions
    access: read key=main path=logical change=false
    status: completed
    -- two roots --
    requests: 2
    source: ocaml_search_expressions
    access: read key=main path=lib change=false
    source: ocaml_search_expressions
    access: read key=aux path=lib change=false
    status: completed
    -- duplicate decoded roots --
    requests: 2
    source: ocaml_search_expressions
    access: read key=main path=lib change=false
    source: ocaml_search_expressions
    access: read key=main path=lib change=false
    status: completed
    -- missing --
    requests: 1
    source: ocaml_search_expressions
    access: read key=main path=logical/missing change=false
    status: failed not_found
    message: logical/missing: path does not exist
    metadata: false
    -- outside --
    requests: 0
    status: failed invalid_input
    message: path is outside workspace: <outside>
    metadata: false
    -- cached before observation --
    requests: 1
    source: ocaml_search_expressions
    access: read key=main path=lib change=false
    requests: 1
    source: ocaml_search_expressions
    access: read key=main path=lib change=false
    status: failed not_found
    message: lib: path does not exist
    metadata: false
    |}]

let%expect_test
    "cancellation before during enumeration and between files publishes no page"
    =
  with_world @@ fun world ->
  run_case ~cancelled:(fun () -> true) world "before parse" (input "needle");
  for index = 0 to 99 do
    write_disk
      (Filename.concat world.ws_dir (Printf.sprintf "cancel/f%03d.ml" index))
      "let hit = needle\n"
  done;
  let polls = ref 0 in
  let cancelled () =
    incr polls;
    !polls > 12
  in
  let result =
    run ~cancelled world (input ~paths:[ "cancel" ] ~limit:1 "needle")
  in
  print_case "during real Glob traversal";
  print_status result;
  Printf.printf "polls exceeded threshold: %b\n" (!polls > 12);
  Printf.printf "partial output: %b\n"
    (Option.is_some (Tool.Result.output result));
  write_disk (Filename.concat world.ws_dir "scan-a.ml") "let a = needle\n";
  write_disk (Filename.concat world.ws_dir "scan-b.ml") "let b = needle\n";
  let polls = ref 0 in
  let cancelled () =
    incr polls;
    !polls = 3
  in
  let result =
    run ~cancelled world (input ~paths:[ "scan-a.ml"; "scan-b.ml" ] "needle")
  in
  print_case "between explicit files";
  print_status result;
  Printf.printf "polls: %d\npartial output: %b\n" !polls
    (Option.is_some (Tool.Result.output result));
  [%expect
    {|
    -- before parse --
    status: interrupted cancelled=true
    reason: tool call cancelled
    -- during real Glob traversal --
    status: interrupted cancelled=true
    reason: tool call cancelled
    polls exceeded threshold: true
    partial output: false
    -- between explicit files --
    status: interrupted cancelled=true
    reason: tool call cancelled
    polls: 3
    partial output: false
    |}]

let%expect_test "durable replay preserves text JSON truncation and no authority"
    =
  with_world @@ fun world ->
  write_disk
    (Filename.concat world.ws_dir "durable.ml")
    "let first = needle\nlet second = needle\n";
  let result = run world (input ~paths:[ "durable.ml" ] ~limit:1 "needle") in
  let output = output_exn result in
  let durable = encode result_codec result in
  let replayed = decode result_codec durable in
  let replayed_output = output_exn replayed in
  let projection = output_json result in
  let names = json_member_names projection in
  let forbidden =
    [
      "anchor"; "anchors"; "authority"; "claim"; "edit"; "permission"; "receipt";
    ]
  in
  Printf.printf "text: %S\n" (Tool.Output.text output);
  Printf.printf "json: %s\n" (json_string projection);
  Printf.printf "durable equal: %b\n" (Tool.Output.equal output replayed_output);
  Printf.printf "text parity: %b\n"
    (String.equal (Tool.Output.text output) (Tool.Output.text replayed_output));
  Printf.printf "JSON parity: %b\n"
    (Option.equal Json.equal (Tool.Output.json output)
       (Tool.Output.json replayed_output));
  Printf.printf "truncation parity: %b\n"
    (Bool.equal
       (Tool.Output.truncated output)
       (Tool.Output.truncated replayed_output));
  Printf.printf "no line anchors: %b\n"
    (not (String.contains (Tool.Output.text output) '#'));
  Printf.printf "forbidden durable members: %s\n"
    ( forbidden |> List.filter (fun name -> List.mem name names) |> function
      | [] -> "none"
      | names -> String.concat "," names );
  Printf.printf "durable envelope: %s\n" (json_string durable);
  [%expect
    {|text: "ocaml_search_expressions pattern=\"needle\" results=1/2 offset=1 limit=1 status=partial searched_files=1\ndurable.ml:1:12-1:18\n  1: let first = needle\nnext: ocaml_search_expressions {\"pattern\":\"needle\",\"paths\":[\"durable.ml\"],\"offset\":2,\"limit\":1}\n"
json: {"version":1,"shape":"matches","total":2,"files":1}
durable equal: true
text parity: true
JSON parity: true
truncation parity: true
no line anchors: true
forbidden durable members: none
durable envelope: {"status":"completed","output":{"text":"ocaml_search_expressions pattern=\"needle\" results=1/2 offset=1 limit=1 status=partial searched_files=1\ndurable.ml:1:12-1:18\n  1: let first = needle\nnext: ocaml_search_expressions {\"pattern\":\"needle\",\"paths\":[\"durable.ml\"],\"offset\":2,\"limit\":1}\n","json":{"version":1,"shape":"matches","total":2,"files":1},"truncated":true}}|}]

[%%run_tests "mentat.tools.ocaml_search_expressions"]
