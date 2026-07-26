(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for structural OCaml
   replacement tool. Every effectful case resolves a real workspace capability
   and sends provider JSON through [Call.decode], cached permission planning,
   [Call.run], erased output, and durable replay where relevant. The suite does
   not call typed or private matcher, renderer, Glob, or edit seams. *)

open Windtrap
open Test_tools_support
module Json = Jsont.Json
module Tool = Mentat_tool
module Replace = Mentat_tools.Ocaml.Replace_expressions
module Wio = Mentat_workspace_io
module Workspace = Mentat_workspace
module Permission = Mentat_permission

type cwd = Primary_root | Primary_logical

type world = {
  ws_dir : string;
  aux_dir : string;
  outside_dir : string;
  io : Wio.t;
  tool : Tool.t;
}

let add_optional name encode value fields =
  match value with
  | None -> fields
  | Some value -> (name, encode value) :: fields

let input ?paths ?max_sites ?dry_run pattern template =
  [ ("pattern", Json.string pattern); ("template", Json.string template) ]
  |> add_optional "paths"
       (fun paths -> Json.list (List.map (fun path -> Json.string path) paths))
       paths
  |> add_optional "max_sites" (fun value -> Json.int value) max_sites
  |> add_optional "dry_run" (fun value -> Json.bool value) dry_run
  |> List.rev |> json_object

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

let normalize world text =
  text
  |> String.replace_all ~sub:world.aux_dir ~by:"<auxiliary>"
  |> String.replace_all ~sub:world.outside_dir ~by:"<outside>"
  |> String.replace_all ~sub:world.ws_dir ~by:"<workspace>"

let print_status ?(normalize = Fun.id) result =
  match Tool.Result.status result with
  | Tool.Result.Completed -> print_endline "status: completed"
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf "status: failed %s\nmessage: %s\nmetadata: %b\n"
        (failure_name kind) (normalize message) (Option.is_some metadata)
  | Tool.Result.Interrupted { reason; cancelled } ->
      Printf.printf "status: interrupted cancelled=%b\nreason: %s\n" cancelled
        reason

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

let decode_call tool provider_input =
  match Tool.Call.decode tool (model_call tool provider_input) with
  | Ok call -> call
  | Error error ->
      failf "call decode failed: %s" (Tool.Call.Decode_error.message error)

let run ?(cancelled = fun () -> false) world provider_input =
  Tool.Call.run (decode_call world.tool provider_input) ~cancelled |> finished

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
      "let cons = x + 1 :: rest";
      "let signed = -1 :: tail";
      "let float = -1.5 :: tail";
      "let comments = f (alpha (* keep me *) + beta)";
      "let attributed = f (value [@opaque])";
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
      "let multiline =";
      "  List.filter";
      "    (fun value -> ready value)";
      "    numbers";
      "";
    ]

let populate_main root =
  let file path contents = write_disk (Filename.concat root path) contents in
  file "structural.ml" structural_source;
  file "logical/cwd.ml" "let cwd_hit = old value\n";
  file "logical/deep/nested.ml" "let nested_hit = old value\n";
  file "lib/shared.ml" "let shared = old value\n";
  file "lib/only_main.ml" "let only_main = old value\n";
  file "direct.txt" "let direct = old value\n";
  file "ignored-direct.ml" "let direct_ignored = old value\n";
  file ".gitignore" "ignored-direct.ml\nignored/\nreopen/*\n!reopen/keep/\n";
  file "ignored/hidden.ml" "let hidden = old value\n";
  file "reopen/drop/hidden.ml" "let drop = old value\n";
  file "reopen/keep/visible.ml" "let visible = old value\n";
  file "local/.ignore" "ignored.ml\n";
  file "local/ignored.ml" "let local_ignored = old value\n";
  file "local/visible.ml" "let local_visible = old value\n";
  file "rg/.rgignore" "hidden.ml\n";
  file "rg/hidden.ml" "let rg_hidden = old value\n";
  file "rg/visible.ml" "let rg_visible = old value\n";
  file ".git/private.ml" "let vcs_private = old value\n"

let populate_aux root =
  write_disk (Filename.concat root "lib/shared.ml") "let shared = old value\n";
  write_disk
    (Filename.concat root "lib/only_aux.ml")
    "let only_aux = old value\n";
  write_disk (Filename.concat root "logical/aux.ml") "let aux_cwd = old value\n"

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

let with_world ?(cwd = Primary_root)
    ?(mode = Mentat_config.Mode.Danger_full_access) fn =
  Eio_main.run @@ fun stdenv ->
  let stdenv = (stdenv :> Eio_unix.Stdenv.base) in
  let raw = Filename.temp_dir "mentat-ocaml-replace-" "" in
  Fun.protect ~finally:(fun () -> remove_tree raw) @@ fun () ->
  let base = Unix.realpath raw in
  let ws_dir = Filename.concat base "workspace" in
  let aux_dir = Filename.concat base "auxiliary" in
  let outside_dir = Filename.concat base "outside" in
  List.iter mkdir [ ws_dir; aux_dir; outside_dir ];
  populate_main ws_dir;
  populate_aux aux_dir;
  write_disk
    (Filename.concat outside_dir "outside.ml")
    "let outside = old value\n";
  let logical = workspace ~cwd ws_dir aux_dir in
  Eio.Switch.run @@ fun sw ->
  let io = resolve_exn ~sw ~stdenv ~logical ~mode () in
  fn { ws_dir; aux_dir; outside_dir; io; tool = Replace.make io }

let relative world path = Filename.concat world.ws_dir path

let print_disk world path =
  let absolute = relative world path in
  match Unix.lstat absolute with
  | { Unix.st_kind = Unix.S_REG; st_size; _ } ->
      if st_size <= 500 then Printf.printf "%s: %S\n" path (read_disk absolute)
      else Printf.printf "%s: regular bytes=%d\n" path st_size
  | { Unix.st_kind = Unix.S_DIR; _ } -> Printf.printf "%s: <directory>\n" path
  | { Unix.st_kind = Unix.S_LNK; _ } ->
      Printf.printf "%s: symlink->%s\n" path (Unix.readlink absolute)
  | _ -> Printf.printf "%s: <special>\n" path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      Printf.printf "%s: <missing>\n" path

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
          (match Permission.Request.Item.access item with
          | Permission.Access.Path
              { op; scope = Permission.Access.Path_scope.Workspace path } ->
              Printf.printf "access: %s key=%s path=%s\n" (path_op_name op)
                (Workspace.Root.Key.to_string (Workspace.Path.root_key path))
                (Workspace.Path.display path)
          | access ->
              failf "unexpected replace permission: %a" Permission.Access.pp
                access);
          match Permission.Request.Item.change item with
          | None -> print_endline "change: none"
          | Some change ->
              Printf.printf "change: additions=%s removals=%s diff=%b\n"
                (Option.fold ~none:"unknown" ~some:string_of_int
                   (Permission.Request.Change.additions change))
                (Option.fold ~none:"unknown" ~some:string_of_int
                   (Permission.Request.Change.removals change))
                (Option.is_some (Permission.Request.Change.diff change)))
        (Permission.Request.items request))
    requests

let close_scope scope =
  let evidence = Wio.Claim_scope.close scope in
  Printf.printf "claim applies: %d\n"
    (List.length evidence.Mentat_edit.Apply_evidence.applies);
  Printf.printf "claim observations: %d\n"
    (List.length evidence.Mentat_edit.Apply_evidence.observed)

let output_json result =
  match Tool.Output.json (output_exn result) with
  | Some json -> json
  | None -> fail "completed replacement output has no JSON projection"

let%expect_test "declaration and provider input enforce strict safe JSON" =
  with_world @@ fun world ->
  let declaration = Tool.declaration world.tool in
  Printf.printf "name: %s\n" (Mentat_llm.Tool.name declaration);
  Printf.printf "schema: %s\n"
    (json_string (Mentat_llm.Tool.input_schema declaration));
  decode_verdict world.tool "minimal" (input "f __1" "g __1");
  decode_verdict world.tool "full"
    (input ~paths:[ "lib" ] ~max_sites:3 ~dry_run:true "f __1" "g __1");
  decode_verdict world.tool "maximum"
    (input ~max_sites:Replace.max_max_sites "x" "y");
  List.iter
    (fun (label, provider_input) ->
      decode_verdict world.tool label provider_input)
    [
      ("missing pattern", json_object [ ("template", Json.string "y") ]);
      ("missing template", json_object [ ("pattern", Json.string "x") ]);
      ( "unknown member",
        json_object
          [
            ("pattern", Json.string "x");
            ("template", Json.string "y");
            ("glob", Json.string "*");
          ] );
      ( "duplicate pattern",
        json_object
          [
            ("pattern", Json.string "x");
            ("pattern", Json.string "shadowed");
            ("template", Json.string "y");
          ] );
      ( "pattern type",
        json_object [ ("pattern", Json.int 1); ("template", Json.string "y") ]
      );
      ("empty pattern", input "" "y");
      ("NUL template", input "x" "bad\000template");
      ("paths empty", input ~paths:[] "x" "y");
      ("path empty", input ~paths:[ "" ] "x" "y");
      ("path NUL", input ~paths:[ "bad\000path" ] "x" "y");
      ( "max string",
        json_object
          [
            ("pattern", Json.string "x");
            ("template", Json.string "y");
            ("max_sites", Json.string "1");
          ] );
      ( "max fraction",
        json_object
          [
            ("pattern", Json.string "x");
            ("template", Json.string "y");
            ("max_sites", Json.number 1.5);
          ] );
      ( "max unsafe",
        json_object
          [
            ("pattern", Json.string "x");
            ("template", Json.string "y");
            ("max_sites", Json.number 9_007_199_254_740_992.);
          ] );
      ("max zero", input ~max_sites:0 "x" "y");
      ("max over bound", input ~max_sites:(Replace.max_max_sites + 1) "x" "y");
      ( "dry type",
        json_object
          [
            ("pattern", Json.string "x");
            ("template", Json.string "y");
            ("dry_run", Json.string "false");
          ] );
    ];
  [%expect
    {|
    name: ocaml_replace_expressions
    schema: {"type":"object","properties":{"pattern":{"type":"string","description":"One complete OCaml expression pattern. __ is an anonymous expression wildcard; __1/__2 are unification metavariables; optional-argument markers and set-matched clauses follow ocaml_search_expressions.","minLength":1},"template":{"type":"string","description":"One OCaml expression whose numbered holes are bound by pattern. Captured source is spliced exactly and verified structurally; capture-prone binder scopes are rejected.","minLength":1},"paths":{"type":"array","description":"Workspace-relative or workspace-contained absolute regular file or directory roots. Defaults to the logical current directory.","items":{"type":"string","minLength":1},"minItems":1},"max_sites":{"type":"integer","description":"Maximum matched sites across all files. Exceeding it writes nothing. Defaults to 200.","minimum":1,"maximum":1000},"dry_run":{"type":"boolean","description":"Validate and summarize rewrites without applying them. Defaults to false."}},"required":["pattern","template"],"additionalProperties":false}
    minimal: accepted canonical={"pattern":"f __1","template":"g __1"}
    full: accepted canonical={"dry_run":true,"max_sites":3,"paths":["lib"],"pattern":"f __1","template":"g __1"}
    maximum: accepted canonical={"max_sites":1000,"pattern":"x","template":"y"}
    missing pattern: rejected diagnostic=true
    missing template: rejected diagnostic=true
    unknown member: rejected diagnostic=true
    duplicate pattern: rejected diagnostic=true
    pattern type: rejected diagnostic=true
    empty pattern: rejected diagnostic=true
    NUL template: rejected diagnostic=true
    paths empty: rejected diagnostic=true
    path empty: rejected diagnostic=true
    path NUL: rejected diagnostic=true
    max string: rejected diagnostic=true
    max fraction: rejected diagnostic=true
    max unsafe: rejected diagnostic=true
    max zero: rejected diagnostic=true
    max over bound: rejected diagnostic=true
    dry type: rejected diagnostic=true
    |}]

let%expect_test "patterns and templates preserve structural safety contracts" =
  with_world @@ fun world ->
  List.iter
    (fun (label, pattern, template) ->
      run_case world label
        (input ~paths:[ "structural.ml" ] ~dry_run:true pattern template))
    [
      ("numbered metavariable unifies", "pair __1 __1", "same __1");
      ("optional present", "call ?optional:PRESENT", "present ()");
      ("optional missing", "call ?optional:MISSING", "missing ()");
      ("clause set", "match __1 with Some x -> x | None -> __", "classify __1");
      ("record fields as set", "{ alpha = __1; beta = __2 }", "(__1, __2)");
    ];
  List.iter
    (fun (label, pattern, template) ->
      run_case world label (input ~paths:[ "structural.ml" ] pattern template))
    [
      ("pattern syntax", "match __ with Some x", "x");
      ("template syntax", "f __1", "let x =");
      ("missing template hole", "f __1", "g __2");
      ("anonymous wildcard", "f __1", "g __");
      ("optional marker", "f __1", "g PRESENT");
      ("metavariable outside expression hole", "old __1", "new __1");
      ("let capture", "f __1 __2", "let tmp = __1 in tmp + __2");
      ("let rec capture", "f __1", "let rec loop = __1 in loop");
      ("fun capture", "f __1", "fun x -> pair x __1");
      ("optional default capture", "f __1", "fun x ?(y = __1) -> pair x y");
      ( "function case capture",
        "f __1",
        "function Some x -> pair x __1 | None -> __1" );
      ("match guard capture", "f __1", "match x with y when __1 -> y | _ -> x");
      ("for capture", "f __1", "for i = 0 to 1 do ignore __1 done");
      ("binding operator capture", "f __1", "let* x = m in pair x __1");
      ("local definition capture", "f __1", "let open M in pair value __1");
    ];
  run_case world "nonrecursive rhs remains outside binder"
    (input ~paths:[ "structural.ml" ] ~dry_run:true "f __1" "let x = __1 in x");
  run_case world "first optional default remains outside binder"
    (input ~paths:[ "structural.ml" ] ~dry_run:true "f __1"
       "fun ?(x = __1) -> x");
  [%expect
    {|
    -- numbered metavariable unifies --
    status: completed
    text: "ocaml_replace_expressions status=previewed files=1 skipped=0\nmodify: structural.ml added=1 removed=1\n"
    truncated: false
    -- optional present --
    status: completed
    text: "ocaml_replace_expressions status=previewed files=1 skipped=0\nmodify: structural.ml added=1 removed=1\n"
    truncated: false
    -- optional missing --
    status: completed
    text: "ocaml_replace_expressions status=previewed files=1 skipped=0\nmodify: structural.ml added=1 removed=1\n"
    truncated: false
    -- clause set --
    status: completed
    text: "ocaml_replace_expressions status=previewed files=1 skipped=0\nmodify: structural.ml added=1 removed=1\n"
    truncated: false
    -- record fields as set --
    status: completed
    text: "ocaml_replace_expressions status=previewed files=1 skipped=0\nmodify: structural.ml added=1 removed=1\n"
    truncated: false
    -- pattern syntax --
    status: failed invalid_input
    message: Syntax error at line 1, column 20. A pattern must be one complete OCaml expression; `__` replaces an expression but does not relax OCaml grammar. Match clauses require `PATTERN -> EXPR`, for example: `match __ with Some x -> __ | None -> __`.
    metadata: false
    -- template syntax --
    status: failed invalid_input
    message: template is not a single OCaml expression at line 1, column 7
    metadata: false
    -- missing template hole --
    status: failed invalid_input
    message: template uses metavariable(s) __2 that the pattern does not bind
    metadata: false
    -- anonymous wildcard --
    status: failed invalid_input
    message: the anonymous wildcard __ is pattern-only; a template needs a numbered hole like __1
    metadata: false
    -- optional marker --
    status: failed invalid_input
    message: PRESENT is a pattern-only optional-argument marker and has no meaning in a template
    metadata: false
    -- metavariable outside expression hole --
    status: failed invalid_input
    message: template uses metavariable(s) __1 outside expression-hole positions
    metadata: false
    -- let capture --
    status: failed invalid_input
    message: template hole __2 is in the scope of a let binder (tmp), which risks variable capture; keep template holes outside any binder the template introduces
    metadata: false
    -- let rec capture --
    status: failed invalid_input
    message: template hole __1 is in the scope of a let rec binder (loop), which risks variable capture; keep template holes outside any binder the template introduces
    metadata: false
    -- fun capture --
    status: failed invalid_input
    message: template hole __1 is in the scope of a fun binder (x), which risks variable capture; keep template holes outside any binder the template introduces
    metadata: false
    -- optional default capture --
    status: failed invalid_input
    message: template hole __1 is in the scope of a fun parameter default binder (x), which risks variable capture; keep template holes outside any binder the template introduces
    metadata: false
    -- function case capture --
    status: failed invalid_input
    message: template hole __1 is in the scope of a function case binder (x), which risks variable capture; keep template holes outside any binder the template introduces
    metadata: false
    -- match guard capture --
    status: failed invalid_input
    message: template hole __1 is in the scope of a match/case binder (y), which risks variable capture; keep template holes outside any binder the template introduces
    metadata: false
    -- for capture --
    status: failed invalid_input
    message: template hole __1 is in the scope of a for binder (i), which risks variable capture; keep template holes outside any binder the template introduces
    metadata: false
    -- binding operator capture --
    status: failed invalid_input
    message: template hole __1 is in the scope of a binding operator binder (x), which risks variable capture; keep template holes outside any binder the template introduces
    metadata: false
    -- local definition capture --
    status: failed invalid_input
    message: template hole __1 is in the scope of a local definition binder (local binding), which risks variable capture; keep template holes outside any binder the template introduces
    metadata: false
    -- nonrecursive rhs remains outside binder --
    status: completed
    text: "ocaml_replace_expressions status=previewed files=1 skipped=0\nmodify: structural.ml added=1 removed=1\n"
    truncated: false
    -- first optional default remains outside binder --
    status: completed
    text: "ocaml_replace_expressions status=previewed files=1 skipped=0\nmodify: structural.ml added=1 removed=1\n"
    truncated: false
    |}]

let%expect_test
    "rendering preserves comments attributes and the intended expression tree" =
  with_world @@ fun world ->
  let run_one path pattern template =
    let result = run world (input ~paths:[ path ] pattern template) in
    print_status result;
    print_disk world path
  in
  run_one "structural.ml" "__1 :: __2" "List.cons __1 __2";
  run_one "structural.ml" "f __1" "wrap __1";
  read_disk (relative world "structural.ml")
  |> String.split_on_char '\n'
  |> List.filteri (fun index _ -> index < 5)
  |> List.iter print_endline;
  [%expect
    {|
    status: completed
    structural.ml: regular bytes=568
    status: completed
    structural.ml: regular bytes=576
    let cons = List.cons (x + 1) rest
    let signed = List.cons (-1) tail
    let float = List.cons (-1.5) tail
    let comments = wrap (alpha (* keep me *) + beta)
    let attributed = wrap ((value [@opaque]))
    |}]

let%expect_test "BOM refusal and CRLF Unicode preservation match legacy" =
  with_world @@ fun world ->
  let bom = "\239\187\191" in
  let bom_path = relative world "bom-crlf.ml" in
  let bom_before =
    bom ^ "let hit = old \"h\195\169llo\"\r\nlet keep = unchanged\r\n"
  in
  let crlf_path = relative world "crlf.ml" in
  write_disk bom_path bom_before;
  write_disk crlf_path
    "let hit = old \"h\195\169llo\"\r\nlet keep = unchanged\r\n";
  let result =
    run world (input ~paths:[ "bom-crlf.ml"; "crlf.ml" ] "old __1" "fresh __1")
  in
  print_result ~json:true world result;
  Printf.printf "BOM file byte-identical: %b\n"
    (String.equal bom_before (read_disk bom_path));
  let contents = read_disk crlf_path in
  let count needle =
    let text_length = String.length contents in
    let needle_length = String.length needle in
    let rec loop offset matches =
      if offset + needle_length > text_length then matches
      else if String.equal (String.sub contents offset needle_length) needle
      then loop (offset + needle_length) (matches + 1)
      else loop (offset + 1) matches
    in
    loop 0 0
  in
  Printf.printf "valid UTF-8: %b\n" (String.is_valid_utf_8 contents);
  Printf.printf "CRLF count: %d\n" (count "\r\n");
  Printf.printf "LF count: %d\n" (count "\n");
  Printf.printf "captured Unicode: %b\n"
    (contains_substring contents "fresh \"h\195\169llo\"");
  [%expect
    {|
    status: completed
    text: "ocaml_replace_expressions status=applied files=1 skipped=1\nmodify: crlf.ml added=1 removed=1\nskipped:\n  bom-crlf.ml reason=syntax_error\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":1}
    BOM file byte-identical: true
    valid UTF-8: true
    CRLF count: 2
    LF count: 2
    captured Unicode: true
    |}]

let%expect_test
    "dry run unchanged and mixed application use one real edit boundary" =
  with_world @@ fun world ->
  write_disk
    (relative world "apply/a.ml")
    "let a = List.rev xs @ ys\nlet same = keep value\n";
  write_disk (relative world "apply/b.ml") "let b = List.rev more @ tail\n";
  print_case "preview";
  let preview_scope = Wio.open_claim_scope world.io in
  let preview =
    run world
      (input ~paths:[ "apply" ] ~dry_run:true "List.rev __1 @ __2"
         "List.rev_append __1 __2")
  in
  print_result ~json:true world preview;
  close_scope preview_scope;
  print_disk world "apply/a.ml";
  print_case "apply two files";
  let apply_scope = Wio.open_claim_scope world.io in
  let applied =
    run world
      (input ~paths:[ "apply" ] "List.rev __1 @ __2" "List.rev_append __1 __2")
  in
  print_result ~json:true world applied;
  close_scope apply_scope;
  print_disk world "apply/a.ml";
  print_disk world "apply/b.ml";
  print_case "matched but unchanged";
  let unchanged_scope = Wio.open_claim_scope world.io in
  let unchanged =
    run world
      (input ~paths:[ "apply/a.ml" ] "List.rev_append __1 __2"
         "List.rev_append __1 __2")
  in
  print_result ~json:true world unchanged;
  close_scope unchanged_scope;
  [%expect
    {|-- preview --
status: completed
text: "ocaml_replace_expressions status=previewed files=2 skipped=0\nmodify: apply/a.ml added=1 removed=1\nmodify: apply/b.ml added=1 removed=1\n"
truncated: false
json: {"version":1,"disposition":"previewed","files":2,"additions":2,"deletions":2,"skipped":0}
claim applies: 0
claim observations: 0
apply/a.ml: "let a = List.rev xs @ ys\nlet same = keep value\n"
-- apply two files --
status: completed
text: "ocaml_replace_expressions status=applied files=2 skipped=0\nmodify: apply/a.ml added=1 removed=1\nmodify: apply/b.ml added=1 removed=1\n"
truncated: false
json: {"version":1,"disposition":"applied","files":2,"additions":2,"deletions":2,"skipped":0}
claim applies: 1
claim observations: 0
apply/a.ml: "let a = List.rev_append xs ys\nlet same = keep value\n"
apply/b.ml: "let b = List.rev_append more tail\n"
-- matched but unchanged --
status: completed
text: "ocaml_replace_expressions status=applied files=1 skipped=0\nunchanged: apply/a.ml added=0 removed=0\n"
truncated: false
json: {"version":1,"disposition":"applied","files":0,"additions":0,"deletions":0,"skipped":0}
claim applies: 0
claim observations: 0|}]

let%expect_test
    "recursive discovery honors ignores deduplicates overlaps and keeps roots \
     distinct" =
  with_world @@ fun world ->
  let aux_lib = Filename.concat world.aux_dir "lib" in
  let result =
    run world
      (input
         ~paths:[ "."; "lib"; "./lib"; aux_lib ]
         ~dry_run:true "old __1" "fresh __1")
  in
  print_result ~json:true world result;
  print_disk world "lib/shared.ml";
  Printf.printf "aux shared: %S\n"
    (read_disk (Filename.concat world.aux_dir "lib/shared.ml"));
  print_disk world "ignored-direct.ml";
  print_disk world "ignored/hidden.ml";
  print_disk world "reopen/keep/visible.ml";
  print_disk world "local/ignored.ml";
  print_disk world "rg/hidden.ml";
  [%expect
    {|status: completed
text: "ocaml_replace_expressions status=previewed files=9 skipped=0\nmodify: <auxiliary>/lib/only_aux.ml added=1 removed=1\nmodify: <auxiliary>/lib/shared.ml added=1 removed=1\nmodify: lib/only_main.ml added=1 removed=1\nmodify: lib/shared.ml added=1 removed=1\nmodify: local/visible.ml added=1 removed=1\nmodify: logical/cwd.ml added=1 removed=1\nmodify: logical/deep/nested.ml added=1 removed=1\nmodify: reopen/keep/visible.ml added=1 removed=1\nmodify: rg/visible.ml added=1 removed=1\n"
truncated: false
json: {"version":1,"disposition":"previewed","files":9,"additions":9,"deletions":9,"skipped":0}
lib/shared.ml: "let shared = old value\n"
aux shared: "let shared = old value\n"
ignored-direct.ml: "let direct_ignored = old value\n"
ignored/hidden.ml: "let hidden = old value\n"
reopen/keep/visible.ml: "let visible = old value\n"
local/ignored.ml: "let local_ignored = old value\n"
rg/hidden.ml: "let rg_hidden = old value\n"|}]

let%expect_test
    "direct roots nested cwd and absolute sibling addresses are durable" =
  with_world ~cwd:Primary_logical @@ fun world ->
  run_case ~json:true world "logical cwd default" (input "old __1" "fresh __1");
  run_case ~json:true world "explicit non-ml file"
    (input
       ~paths:[ Filename.concat world.ws_dir "direct.txt" ]
       "old __1" "fresh __1");
  run_case ~json:true world "absolute primary sibling"
    (input
       ~paths:[ Filename.concat world.ws_dir "lib/only_main.ml" ]
       "old __1" "fresh __1");
  [%expect
    {|-- logical cwd default --
status: completed
text: "ocaml_replace_expressions status=applied files=2 skipped=0\nmodify: cwd.ml added=1 removed=1\nmodify: deep/nested.ml added=1 removed=1\n"
truncated: false
json: {"version":1,"disposition":"applied","files":2,"additions":2,"deletions":2,"skipped":0}
-- explicit non-ml file --
status: completed
text: "ocaml_replace_expressions status=applied files=1 skipped=0\nmodify: <workspace>/direct.txt added=1 removed=1\n"
truncated: false
json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
-- absolute primary sibling --
status: completed
text: "ocaml_replace_expressions status=applied files=1 skipped=0\nmodify: <workspace>/lib/only_main.ml added=1 removed=1\n"
truncated: false
json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}|}]

let%expect_test "real Glob pagination reaches a match beyond the first page" =
  with_world @@ fun world ->
  for index = 0 to 1_000 do
    let contents =
      if index = 1_000 then "let boundary = old value\n" else "let quiet = 0\n"
    in
    write_disk (relative world (Printf.sprintf "pages/f%04d.ml" index)) contents
  done;
  let result = run world (input ~paths:[ "pages" ] "old __1" "fresh __1") in
  print_result ~json:true world result;
  print_disk world "pages/f1000.ml";
  [%expect
    {|status: completed
text: "ocaml_replace_expressions status=applied files=1 skipped=0\nmodify: pages/f1000.ml added=1 removed=1\n"
truncated: false
json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
pages/f1000.ml: "let boundary = fresh value\n"|}]

let%expect_test "skip taxonomy is complete and valid files still apply" =
  with_world @@ fun world ->
  write_disk (relative world "skips/good.ml") "let good = old value\n";
  write_disk (relative world "skips/binary.ml") "let x = \000payload\n";
  write_disk (relative world "skips/utf8.ml") "let x = \255\n";
  write_disk (relative world "skips/syntax.ml") "let x =\n";
  write_disk (relative world "skips/unreadable.ml") "let hidden = old value\n";
  Unix.chmod (relative world "skips/unreadable.ml") 0o000;
  write_disk
    (relative world "skips/large.ml")
    ("let padding = \"" ^ String.make Replace.max_source_bytes 'x' ^ "\"\n");
  let result = run world (input ~paths:[ "skips" ] "old __1" "fresh __1") in
  print_result ~json:true world result;
  print_disk world "skips/good.ml";
  [%expect
    {|status: completed
text: "ocaml_replace_expressions status=applied files=1 skipped=5\nmodify: skips/good.ml added=1 removed=1\nskipped:\n  skips/binary.ml reason=binary\n  skips/large.ml reason=too_large\n  skips/syntax.ml reason=syntax_error\n  skips/unreadable.ml reason=read_error\n  skips/utf8.ml reason=invalid_utf8\n"
truncated: false
json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":5}
skips/good.ml: "let good = fresh value\n"|}]

let%expect_test "max_sites is global and all-or-nothing across files" =
  with_world @@ fun world ->
  write_disk (relative world "limit/a.ml") "let a = old x\nlet b = old y\n";
  write_disk (relative world "limit/b.ml") "let c = old z\n";
  let before_a = read_disk (relative world "limit/a.ml") in
  let before_b = read_disk (relative world "limit/b.ml") in
  let scope = Wio.open_claim_scope world.io in
  run_case world "over limit"
    (input ~paths:[ "limit" ] ~max_sites:2 "old __1" "fresh __1");
  close_scope scope;
  Printf.printf "a unchanged: %b\n"
    (String.equal before_a (read_disk (relative world "limit/a.ml")));
  Printf.printf "b unchanged: %b\n"
    (String.equal before_b (read_disk (relative world "limit/b.ml")));
  [%expect
    {|
    -- over limit --
    status: failed failed
    message: found 3 matching site(s), which exceeds max_sites=2; narrow paths or raise max_sites (nothing was written)
    metadata: false
    claim applies: 0
    claim observations: 0
    a unchanged: true
    b unchanged: true
    |}]

let%expect_test "apply revalidates the complete observed source after a race" =
  with_world @@ fun world ->
  let path = relative world "race.ml" in
  write_disk path "let hit = old value\n";
  let polls = ref 0 in
  let raced = ref false in
  let poll () =
    incr polls;
    if !polls = 13 then begin
      write_disk path "let racer = changed value\n";
      raced := true
    end;
    false
  in
  let scope = Wio.open_claim_scope world.io in
  run_case ~cancelled:poll world "concurrent content change"
    (input ~paths:[ "race.ml" ] "old __1" "fresh __1");
  Printf.printf "race injected: %b\n" !raced;
  Printf.printf "polls: %d\n" !polls;
  close_scope scope;
  print_disk world "race.ml";
  [%expect
    {|
    -- concurrent content change --
    status: failed stale
    message: race.ml: stale write
    metadata: false
    race injected: true
    polls: 13
    claim applies: 0
    claim observations: 0
    race.ml: "let racer = changed value\n"
    |}]

let%expect_test "multi-file stale preflight commits none of the plan" =
  with_world @@ fun world ->
  let a = relative world "race-multi/a.ml" in
  let b = relative world "race-multi/b.ml" in
  let before_a = "let a = old first\n" in
  let before_b = "let b = old second\n" in
  write_disk a before_a;
  write_disk b before_b;
  let provider_input =
    input ~paths:[ "race-multi/a.ml"; "race-multi/b.ml" ] "old __1" "fresh __1"
  in
  let preview_polls = ref 0 in
  let preview =
    run
      ~cancelled:(fun () ->
        incr preview_polls;
        false)
      world
      (input
         ~paths:[ "race-multi/a.ml"; "race-multi/b.ml" ]
         ~dry_run:true "old __1" "fresh __1")
  in
  ignore (output_exn preview);
  let polls = ref 0 in
  let raced = ref false in
  let poll () =
    incr polls;
    if !polls = !preview_polls + 1 then begin
      write_disk b "let racer = external second\n";
      raced := true
    end;
    false
  in
  let scope = Wio.open_claim_scope world.io in
  run_case ~cancelled:poll world "second target changed before preflight"
    provider_input;
  Printf.printf "race injected: %b\n" !raced;
  Printf.printf "after preview boundary: %b\n" (!polls > !preview_polls);
  close_scope scope;
  Printf.printf "first target untouched: %b\n"
    (String.equal before_a (read_disk a));
  Printf.printf "second target is external write: %b\n"
    (String.equal "let racer = external second\n" (read_disk b));
  [%expect
    {|
    -- second target changed before preflight --
    status: failed stale
    message: race-multi/b.ml: stale write
    metadata: false
    race injected: true
    after preview boundary: true
    claim applies: 0
    claim observations: 0
    first target untouched: true
    second target is external write: true
    |}]

let%expect_test
    "permission plans are decoded input evidence and deduplicate paths" =
  with_world ~cwd:Primary_logical @@ fun world ->
  let aux_lib = Filename.concat world.aux_dir "lib" in
  print_case "preview default";
  print_permissions
    (decode_call world.tool (input ~dry_run:true "old __1" "fresh __1"));
  print_case "apply roots";
  print_permissions
    (decode_call world.tool
       (input ~paths:[ "."; "./"; aux_lib ] "old __1" "fresh __1"));
  print_case "outside omitted";
  print_permissions
    (decode_call world.tool
       (input ~paths:[ world.outside_dir ] "old __1" "fresh __1"));
  [%expect
    {|
    -- preview default --
    requests: 1
    source: ocaml_replace_expressions
    access: read key=main path=logical
    change: none
    -- apply roots --
    requests: 2
    source: ocaml_replace_expressions
    access: modify key=main path=logical
    change: additions=1 removals=1 diff=true
    source: ocaml_replace_expressions
    access: modify key=aux path=lib
    change: additions=1 removals=1 diff=true
    -- outside omitted --
    requests: 0
    |}]

let%expect_test "invalid roots and WIO write policy fail without residue" =
  with_world @@ fun world ->
  Unix.symlink "lib" (relative world "link_dir");
  Unix.symlink "lib/shared.ml" (relative world "link_file.ml");
  Unix.mkfifo (relative world "pipe") 0o600;
  List.iter
    (fun (label, path) ->
      run_case world label (input ~paths:[ path ] "old __1" "fresh __1"))
    [
      ("missing", "missing");
      ("outside", Filename.concat world.outside_dir "outside.ml");
      ("directory symlink", "link_dir");
      ("file symlink", "link_file.ml");
      ("FIFO", "pipe");
    ];
  run_case world "protected metadata"
    (input ~paths:[ ".git/private.ml" ] "old __1" "fresh __1");
  print_disk world ".git/private.ml";
  with_world ~mode:Mentat_config.Mode.Read_only (fun read_only ->
      run_case read_only "read-only mode"
        (input ~paths:[ "lib/shared.ml" ] "old __1" "fresh __1");
      print_disk read_only "lib/shared.ml");
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
    -- protected metadata --
    status: failed invalid_input
    message: .git/private.ml: .git is protected workspace metadata and cannot be modified by tools; change sandbox and workspace policy through configuration or the CLI instead
    metadata: false
    .git/private.ml: "let vcs_private = old value\n"
    -- read-only mode --
    status: failed invalid_input
    message: lib/shared.ml: edit target belongs to a read-only workspace root
    metadata: false
    lib/shared.ml: "let shared = old value\n"
    |}]

let%expect_test "apply-time one-MiB boundary does not narrow dry-run coverage" =
  with_world @@ fun world ->
  let suffix = String.make ((1024 * 1024) + 32) ' ' in
  write_disk (relative world "large-edit.ml") ("let hit = old value\n" ^ suffix);
  run_case world "preview"
    (input ~paths:[ "large-edit.ml" ] ~dry_run:true "old __1" "fresh __1");
  run_case world "apply"
    (input ~paths:[ "large-edit.ml" ] "old __1" "fresh __1");
  let prefix =
    let contents = read_disk (relative world "large-edit.ml") in
    String.sub contents 0 20
  in
  Printf.printf "disk prefix: %S\n" prefix;
  [%expect
    {|
    -- preview --
    status: completed
    text: "ocaml_replace_expressions status=previewed files=1 skipped=0\nmodify: large-edit.ml added=1 removed=1\n"
    truncated: false
    -- apply --
    status: failed invalid_input
    message: large-edit.ml: file is too large (1048628 bytes, max 1048576)
    metadata: false
    disk prefix: "let hit = old value\n"
    |}]

let%expect_test "cancellation publishes no summary mutation or claim evidence" =
  with_world @@ fun world ->
  let before = read_disk (relative world "lib/shared.ml") in
  let scope = Wio.open_claim_scope world.io in
  run_case
    ~cancelled:(fun () -> true)
    world "before validation"
    (input ~paths:[ "lib/shared.ml" ] "old __1" "fresh __1");
  close_scope scope;
  let polls = ref 0 in
  let during_scope = Wio.open_claim_scope world.io in
  let result =
    run
      ~cancelled:(fun () ->
        incr polls;
        !polls > 8)
      world
      (input ~paths:[ "lib/shared.ml" ] "old __1" "fresh __1")
  in
  print_case "during pipeline";
  print_status result;
  Printf.printf "partial output: %b\n"
    (Option.is_some (Tool.Result.output result));
  Printf.printf "poll threshold crossed: %b\n" (!polls > 8);
  close_scope during_scope;
  Printf.printf "disk unchanged: %b\n"
    (String.equal before (read_disk (relative world "lib/shared.ml")));
  [%expect
    {|
    -- before validation --
    status: interrupted cancelled=true
    reason: tool call cancelled
    claim applies: 0
    claim observations: 0
    -- during pipeline --
    status: interrupted cancelled=true
    reason: tool call cancelled
    partial output: false
    poll threshold crossed: true
    claim applies: 0
    claim observations: 0
    disk unchanged: true
    |}]

let%expect_test "durable output is a D2 summary and cannot replay authority" =
  with_world @@ fun world ->
  write_disk (relative world "durable.ml") "let hit = old secret_value\n";
  let scope = Wio.open_claim_scope world.io in
  let result =
    run world (input ~paths:[ "durable.ml" ] "old __1" "fresh __1")
  in
  let output = output_exn result in
  let durable = encode result_codec result in
  let replayed = decode result_codec durable in
  let replayed_output = output_exn replayed in
  let projection = output_json result in
  let names = json_member_names projection in
  let forbidden =
    [
      "pattern";
      "template";
      "roots";
      "sites";
      "before";
      "after";
      "diff";
      "identity";
      "receipt";
      "mutation";
      "evidence";
    ]
  in
  Printf.printf "text: %S\n" (Tool.Output.text output);
  Printf.printf "json: %s\n" (json_string projection);
  Printf.printf "durable equal: %b\n" (Tool.Output.equal output replayed_output);
  Printf.printf "forbidden durable members: %s\n"
    ( forbidden |> List.filter (fun name -> List.mem name names) |> function
      | [] -> "none"
      | names -> String.concat "," names );
  Printf.printf "secret leaked: %b\n"
    (contains_substring (json_string durable) "secret_value");
  Printf.printf "durable envelope: %s\n" (json_string durable);
  close_scope scope;
  print_disk world "durable.ml";
  [%expect
    {|text: "ocaml_replace_expressions status=applied files=1 skipped=0\nmodify: durable.ml added=1 removed=1\n"
json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
durable equal: true
forbidden durable members: none
secret leaked: false
durable envelope: {"status":"completed","output":{"text":"ocaml_replace_expressions status=applied files=1 skipped=0\nmodify: durable.ml added=1 removed=1\n","json":{"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0},"truncated":false}}
claim applies: 1
claim observations: 0
durable.ml: "let hit = fresh secret_value\n"|}]

[%%run_tests "mentat.tools.ocaml_replace_expressions"]
