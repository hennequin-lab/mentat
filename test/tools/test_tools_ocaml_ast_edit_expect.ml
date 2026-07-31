(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for [ocaml_ast_edit]. Every
   effectful case resolves a real workspace capability, sends provider JSON
   through [Call.decode], inspects the cached permission request, and executes
   through [Call.run] and the native [Edit.apply] boundary. No typed selector,
   planner, or output value is used by this suite.

   Compared with the legacy four-test suite, these cases retain all selector
   and rewrite examples while adding strict/safe decoding, both source
   grammars, every operation, original-tree multi-edit semantics, namespace and
   occurrence selection, root identity, stale interleavings, native-write
   protections, cancellation at both polls, durable replay, and D2 leakage and
   attribution checks. *)

open Windtrap
open Test_tools_support
module Ast_edit = Mentat_tools.Ocaml.Ocaml_ast_edit
module Json = Jsont.Json
module Permission = Mentat_permission
module Tool = Mentat_tool
module Wio = Mentat_workspace_io
module Workspace = Mentat_workspace

type cwd = Primary_root | Primary_logical | Auxiliary_root

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
  | Some value -> fields @ [ (name, encode value) ]

let item_selector ?item_kind ?occurrence path =
  [
    ("mode", Json.string "item");
    ("path", Json.list (List.map (fun value -> Json.string value) path));
  ]
  |> add_optional "item_kind" (fun value -> Json.string value) item_kind
  |> add_optional "occurrence" (fun value -> Json.int value) occurrence
  |> json_object

let enclosing_selector ?node_item_kind kind line column =
  [
    ("mode", Json.string "enclosing");
    ("kind", Json.string kind);
    ("line", Json.int line);
    ("column", Json.int column);
  ]
  |> add_optional "node_item_kind"
       (fun value -> Json.string value)
       node_item_kind
  |> json_object

let exact_selector ?node_item_kind kind start_line start_column end_line
    end_column =
  [
    ("mode", Json.string "exact");
    ("kind", Json.string kind);
    ("start_line", Json.int start_line);
    ("start_column", Json.int start_column);
    ("end_line", Json.int end_line);
    ("end_column", Json.int end_column);
  ]
  |> add_optional "node_item_kind"
       (fun value -> Json.string value)
       node_item_kind
  |> json_object

let edit ?text op selector =
  [ ("op", Json.string op); ("selector", selector) ]
  |> add_optional "text" (fun value -> Json.string value) text
  |> json_object

let input ?file_kind ?if_identity path edits =
  [ ("path", Json.string path); ("edits", Json.list edits) ]
  |> add_optional "file_kind" (fun value -> Json.string value) file_kind
  |> add_optional "if_identity" (fun value -> Json.string value) if_identity
  |> json_object

let identity contents =
  Mentat_digest.Content_ref.to_token
    (Mentat_digest.Content_ref.of_contents contents)

let decode_call_result tool provider_input =
  Tool.Call.decode tool (model_call tool provider_input)

let decode_call tool provider_input =
  match decode_call_result tool provider_input with
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

let print_status world result =
  match Tool.Result.status result with
  | Tool.Result.Completed -> print_endline "status: completed"
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf "status: failed %s\nmessage: %s\nmetadata: %b\n"
        (failure_name kind) (normalize world message) (Option.is_some metadata)
  | Tool.Result.Interrupted { reason; cancelled } ->
      Printf.printf "status: interrupted cancelled=%b\nreason: %s\n" cancelled
        reason

let print_failure_kind result =
  match Tool.Result.status result with
  | Tool.Result.Failed { kind; _ } ->
      Printf.printf "status: failed %s\n" (failure_name kind)
  | Tool.Result.Completed -> fail "expected tool failure, got completion"
  | Tool.Result.Interrupted _ -> fail "expected tool failure, got interruption"

let print_result ?(json = false) world result =
  print_status world result;
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

let implementation_source =
  String.concat "\n"
    [
      "module M = struct";
      "  type t = int";
      "  let add x = x + 1";
      "  let keep = 9";
      "end";
      "";
      "type same = A";
      "let same = 1";
      "let same = 2";
      "let calc : int = outer (inner 1)";
      "let pair = (10, 20)";
      "let (left, right) = (1, 2)";
      "";
    ]

let interface_source =
  String.concat "\n"
    [
      "module M : sig";
      "  type t";
      "  val answer : int";
      "end";
      "val top : string";
      "";
    ]

let size_bound_source =
  let prefix = "let boundary = 1\n(*" in
  let suffix = "*)" in
  let padding =
    Ast_edit.max_file_bytes - String.length prefix - String.length suffix
  in
  prefix ^ String.make padding 'x' ^ suffix

let populate_main root =
  let file path contents = write_disk (Filename.concat root path) contents in
  file "example.ml" implementation_source;
  file "same.ml" "let root = 1\n";
  file "api.mli" interface_source;
  file "api.source" interface_source;
  file "broken.ml" "let broken =\n";
  file "binary.ml" "let x = 1\000tail\n";
  file "invalid-utf8.ml" "let x = \255\254\n";
  file "utf8.ml" "let utf = (\"é\", target 1)\n";
  file "crlf.ml" "let first = 1\r\nlet target = 2\r\nlet last = 3\r\n";
  file "bom.ml" "\239\187\191let bom = 1\n";
  file "parent-file" "let parent = 1\n";
  file "oversized.ml" (String.make (Ast_edit.max_file_bytes + 1) ' ');
  file "logical/cwd.ml" "let cwd_value = 1\n";
  file ".git/protected.ml" "let protected_value = 1\n";
  file ".mentat/protected.ml" "let protected_value = 1\n";
  mkdir (Filename.concat root "dir");
  Unix.mkfifo (Filename.concat root "pipe") 0o600;
  Unix.symlink "example.ml" (Filename.concat root "link.ml");
  Unix.symlink "." (Filename.concat root "link_parent")

let populate_aux root =
  write_disk (Filename.concat root "same.ml") "let auxiliary = 1\n";
  write_disk (Filename.concat root "aux.ml") "let auxiliary = 1\n"

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

let with_world ?(cwd = Primary_root)
    ?(mode = Mentat_config.Mode.Danger_full_access) fn =
  Eio_main.run @@ fun stdenv ->
  let stdenv = (stdenv :> Eio_unix.Stdenv.base) in
  let raw = Filename.temp_dir "mentat-ocaml-ast-edit-" "" in
  Fun.protect ~finally:(fun () -> remove_tree raw) @@ fun () ->
  let base = Unix.realpath raw in
  let ws_dir = Filename.concat base "workspace" in
  let aux_dir = Filename.concat base "auxiliary" in
  let outside_dir = Filename.concat base "outside" in
  List.iter mkdir [ ws_dir; aux_dir; outside_dir ];
  populate_main ws_dir;
  populate_aux aux_dir;
  write_disk (Filename.concat outside_dir "outside.ml") "let outside = 1\n";
  let logical = workspace ~cwd ws_dir aux_dir in
  Eio.Switch.run @@ fun sw ->
  let io = resolve_exn ~sw ~stdenv ~logical ~mode () in
  fn { ws_dir; aux_dir; outside_dir; io; tool = Ast_edit.make io }

let primary world path = Filename.concat world.ws_dir path
let auxiliary world path = Filename.concat world.aux_dir path

let print_disk_at label path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_REG; st_size; _ } ->
      if st_size <= 500 then Printf.printf "%s: %S\n" label (read_disk path)
      else Printf.printf "%s: regular bytes=%d\n" label st_size
  | { Unix.st_kind = Unix.S_DIR; _ } -> Printf.printf "%s: <directory>\n" label
  | { Unix.st_kind = Unix.S_LNK; _ } ->
      Printf.printf "%s: symlink->%s\n" label (Unix.readlink path)
  | _ -> Printf.printf "%s: <special>\n" label
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      Printf.printf "%s: <missing>\n" label

let print_disk world path = print_disk_at "disk" (primary world path)

let decode_verdict tool label provider_input =
  match decode_call_result tool provider_input with
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
              failf "unexpected AST edit permission: %a" Permission.Access.pp
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

let%expect_test
    "declaration and provider input are strict and numerically exact" =
  with_world @@ fun world ->
  let declaration = Tool.declaration world.tool in
  Printf.printf "name: %s\n" (Mentat_llm.Tool.name declaration);
  Printf.printf "schema: %s\n"
    (json_string (Mentat_llm.Tool.input_schema declaration));
  let minimal =
    input "example.ml"
      [ edit ~text:"let same = 3" "replace" (item_selector [ "same" ]) ]
  in
  decode_verdict world.tool "minimal" minimal;
  decode_verdict world.tool "aliases"
    (input ~file_kind:"ml"
       ~if_identity:(identity implementation_source)
       "example.ml"
       [ edit ~text:"string" "replace" (exact_selector "type" 10 11 10 14) ]);
  decode_verdict world.tool "delete text ignored"
    (input "example.ml"
       [ edit ~text:"known but ignored" "delete" (item_selector [ "same" ]) ]);
  decode_verdict world.tool "irrelevant known selector members"
    (input "example.ml"
       [
         edit "delete"
           (json_object
              [
                ("mode", Json.string "item");
                ("path", Json.list [ Json.string "same" ]);
                ("line", Json.int 0);
                ("column", Json.int 0);
              ]);
       ]);
  decode_verdict world.tool "safe occurrence maximum"
    (input "example.ml"
       [
         edit "delete"
           (json_object
              [
                ("mode", Json.string "item");
                ("path", Json.list [ Json.string "same" ]);
                ("occurrence", Json.number 9_007_199_254_740_991.);
              ]);
       ]);
  let cases =
    [
      ("missing path", json_object [ ("edits", Json.list []) ]);
      ("empty path", input "" [ edit "delete" (item_selector [ "same" ]) ]);
      ("empty edits", input "example.ml" []);
      ( "outer unknown",
        json_object
          [
            ("path", Json.string "example.ml");
            ("edits", Json.list [ edit "delete" (item_selector [ "same" ]) ]);
            ("dry_run", Json.bool true);
          ] );
      ( "outer duplicate",
        json_object
          [
            ("path", Json.string "example.ml");
            ("path", Json.string "other.ml");
            ("edits", Json.list [ edit "delete" (item_selector [ "same" ]) ]);
          ] );
      ( "edit unknown",
        input "example.ml"
          [
            json_object
              [
                ("op", Json.string "delete");
                ("selector", item_selector [ "same" ]);
                ("extra", Json.bool true);
              ];
          ] );
      ( "edit duplicate",
        input "example.ml"
          [
            json_object
              [
                ("op", Json.string "delete");
                ("op", Json.string "replace");
                ("selector", item_selector [ "same" ]);
              ];
          ] );
      ( "selector unknown",
        input "example.ml"
          [
            edit "delete"
              (json_object
                 [
                   ("mode", Json.string "item");
                   ("path", Json.list [ Json.string "same" ]);
                   ("extra", Json.bool true);
                 ]);
          ] );
      ( "selector duplicate",
        input "example.ml"
          [
            edit "delete"
              (json_object
                 [
                   ("mode", Json.string "item");
                   ("mode", Json.string "exact");
                   ("path", Json.list [ Json.string "same" ]);
                 ]);
          ] );
      ("unknown file kind", input ~file_kind:"module" "example.ml" []);
      ( "empty identity",
        input ~if_identity:"" "example.ml"
          [ edit "delete" (item_selector [ "same" ]) ] );
      ( "malformed identity",
        input ~if_identity:"not-a-content-identity" "example.ml"
          [ edit "delete" (item_selector [ "same" ]) ] );
      ( "unknown suffix",
        input "example.source" [ edit "delete" (item_selector [ "x" ]) ] );
      ( "unknown op",
        input "example.ml" [ edit "move" (item_selector [ "same" ]) ] );
      ( "replace missing text",
        input "example.ml" [ edit "replace" (item_selector [ "same" ]) ] );
      ( "replace empty text",
        input "example.ml"
          [ edit ~text:"" "replace" (item_selector [ "same" ]) ] );
      ( "text invalid UTF-8",
        input "example.ml"
          [ edit ~text:"\255" "replace" (item_selector [ "same" ]) ] );
      ( "item path missing",
        input "example.ml"
          [ edit "delete" (json_object [ ("mode", Json.string "item") ]) ] );
      ( "item path empty",
        input "example.ml" [ edit "delete" (item_selector []) ] );
      ( "item component empty",
        input "example.ml" [ edit "delete" (item_selector [ "" ]) ] );
      ( "occurrence zero",
        input "example.ml"
          [ edit "delete" (item_selector ~occurrence:0 [ "same" ]) ] );
      ( "occurrence fraction",
        input "example.ml"
          [
            edit "delete"
              (json_object
                 [
                   ("mode", Json.string "item");
                   ("path", Json.list [ Json.string "same" ]);
                   ("occurrence", Json.number 1.5);
                 ]);
          ] );
      ( "occurrence unsafe",
        input "example.ml"
          [
            edit "delete"
              (json_object
                 [
                   ("mode", Json.string "item");
                   ("path", Json.list [ Json.string "same" ]);
                   ("occurrence", Json.number 9_007_199_254_740_992.);
                 ]);
          ] );
      ( "enclosing kind missing",
        input "example.ml"
          [
            edit "delete"
              (json_object
                 [
                   ("mode", Json.string "enclosing");
                   ("line", Json.int 1);
                   ("column", Json.int 0);
                 ]);
          ] );
      ( "enclosing position missing",
        input "example.ml"
          [
            edit "delete"
              (json_object
                 [
                   ("mode", Json.string "enclosing");
                   ("kind", Json.string "item");
                 ]);
          ] );
      ( "node item kind mismatch",
        input "example.ml"
          [
            edit "delete"
              (enclosing_selector ~node_item_kind:"value" "expression" 10 17);
          ] );
      ( "line zero",
        input "example.ml" [ edit "delete" (enclosing_selector "item" 0 0) ] );
      ( "column unsafe",
        input "example.ml"
          [
            edit "delete"
              (json_object
                 [
                   ("mode", Json.string "enclosing");
                   ("kind", Json.string "item");
                   ("line", Json.int 1);
                   ("column", Json.number 9_007_199_254_740_992.);
                 ]);
          ] );
      ( "exact coordinate missing",
        input "example.ml"
          [
            edit "delete"
              (json_object
                 [ ("mode", Json.string "exact"); ("kind", Json.string "type") ]);
          ] );
      ( "exact reversed",
        input "example.ml" [ edit "delete" (exact_selector "type" 2 14 2 11) ]
      );
    ]
  in
  List.iter
    (fun (label, provider_input) ->
      decode_verdict world.tool label provider_input)
    cases;
  [%expect
    {|
    name: ocaml_ast_edit
    schema: {"type":"object","properties":{"path":{"description":"Workspace OCaml source path to edit atomically.","type":"string","minLength":1},"file_kind":{"description":"Source grammar; defaults from a .ml or .mli suffix.","type":"string","enum":["implementation","interface","ml","mli"]},"if_identity":{"description":"Complete-file identity from a previous complete read.","type":"string","minLength":1},"edits":{"type":"array","items":{"type":"object","properties":{"op":{"type":"string","enum":["replace","insert_before","insert_after","delete"]},"selector":{"type":"object","properties":{"mode":{"type":"string","enum":["item","enclosing","exact"]},"path":{"description":"Qualified item path. Required when mode is item.","type":"array","items":{"type":"string"},"minItems":1},"item_kind":{"description":"Optional item namespace filter.","allOf":[{"type":"string","enum":["value","type","module","module_type","exception","external","open","include","class","class_type","extension","eval"]}]},"occurrence":{"description":"One-based matching declaration occurrence. Defaults to 1.","type":"integer","minimum":1,"maximum":9007199254740991},"kind":{"description":"Node kind for enclosing and exact selectors.","type":"string","enum":["item","expression","type"]},"node_item_kind":{"description":"Optional namespace filter when kind is item.","allOf":[{"type":"string","enum":["value","type","module","module_type","exception","external","open","include","class","class_type","extension","eval"]}]},"line":{"description":"One-based enclosing cursor line.","type":"integer","minimum":1,"maximum":9007199254740991},"column":{"description":"Zero-based byte cursor column.","type":"integer","minimum":0,"maximum":9007199254740991},"start_line":{"description":"One-based exact range start line.","type":"integer","minimum":1,"maximum":9007199254740991},"start_column":{"description":"Zero-based exact range start column.","type":"integer","minimum":0,"maximum":9007199254740991},"end_line":{"description":"One-based exact range end line.","type":"integer","minimum":1,"maximum":9007199254740991},"end_column":{"description":"Zero-based exact range end column.","type":"integer","minimum":0,"maximum":9007199254740991}},"required":["mode"],"additionalProperties":false},"text":{"description":"Non-empty replacement or insertion OCaml fragment.","type":"string","minLength":1}},"required":["op","selector"],"additionalProperties":false},"minItems":1}},"required":["path","edits"],"additionalProperties":false}
    minimal: accepted canonical={"edits":[{"op":"replace","selector":{"mode":"item","path":["same"]},"text":"let same = 3"}],"file_kind":"implementation","path":"example.ml"}
    aliases: accepted canonical={"edits":[{"op":"replace","selector":{"end_column":14,"end_line":10,"kind":"type","mode":"exact","start_column":11,"start_line":10},"text":"string"}],"file_kind":"implementation","if_identity":"sha256:b5808e4a3fdc581d25f29b0003aeecb279ef1b9d3a7274652e1bc76873f6a988:193","path":"example.ml"}
    delete text ignored: accepted canonical={"edits":[{"op":"delete","selector":{"mode":"item","path":["same"]}}],"file_kind":"implementation","path":"example.ml"}
    irrelevant known selector members: accepted canonical={"edits":[{"op":"delete","selector":{"mode":"item","path":["same"]}}],"file_kind":"implementation","path":"example.ml"}
    safe occurrence maximum: accepted canonical={"edits":[{"op":"delete","selector":{"mode":"item","occurrence":9007199254740991,"path":["same"]}}],"file_kind":"implementation","path":"example.ml"}
    missing path: rejected diagnostic=true
    empty path: rejected diagnostic=true
    empty edits: rejected diagnostic=true
    outer unknown: rejected diagnostic=true
    outer duplicate: rejected diagnostic=true
    edit unknown: rejected diagnostic=true
    edit duplicate: rejected diagnostic=true
    selector unknown: rejected diagnostic=true
    selector duplicate: rejected diagnostic=true
    unknown file kind: rejected diagnostic=true
    empty identity: rejected diagnostic=true
    malformed identity: rejected diagnostic=true
    unknown suffix: rejected diagnostic=true
    unknown op: rejected diagnostic=true
    replace missing text: rejected diagnostic=true
    replace empty text: rejected diagnostic=true
    text invalid UTF-8: rejected diagnostic=true
    item path missing: rejected diagnostic=true
    item path empty: rejected diagnostic=true
    item component empty: rejected diagnostic=true
    occurrence zero: rejected diagnostic=true
    occurrence fraction: rejected diagnostic=true
    occurrence unsafe: rejected diagnostic=true
    enclosing kind missing: rejected diagnostic=true
    enclosing position missing: rejected diagnostic=true
    node item kind mismatch: rejected diagnostic=true
    line zero: rejected diagnostic=true
    column unsafe: rejected diagnostic=true
    exact coordinate missing: rejected diagnostic=true
    exact reversed: rejected diagnostic=true
    |}]

let%expect_test "permissions are cached input-only plans with root identity" =
  with_world @@ fun world ->
  let before = read_disk (primary world "example.ml") in
  print_case "replacement and insertion";
  print_permissions
    (decode_call world.tool
       (input "example.ml"
          [
            edit ~text:"let same = 3" "replace" (item_selector [ "same" ]);
            edit ~text:"\nlet after = 4" "insert_after"
              (item_selector [ "same" ]);
          ]));
  print_case "insertions only";
  print_permissions
    (decode_call world.tool
       (input "example.ml"
          [
            edit ~text:"let before = 0\n" "insert_before"
              (item_selector [ "same" ]);
          ]));
  print_case "delete only";
  print_permissions
    (decode_call world.tool
       (input "example.ml" [ edit "delete" (item_selector [ "same" ]) ]));
  print_case "outside";
  print_permissions
    (decode_call world.tool
       (input
          (Filename.concat world.outside_dir "outside.ml")
          [ edit "delete" (item_selector [ "outside" ]) ]));
  print_case "auxiliary absolute";
  print_permissions
    (decode_call world.tool
       (input
          (auxiliary world "same.ml")
          [ edit "delete" (item_selector [ "auxiliary" ]) ]));
  Printf.printf "permission planning read source: %b\n"
    (not (String.equal before (read_disk (primary world "example.ml"))));
  [%expect
    {|
    -- replacement and insertion --
    requests: 1
    source: ocaml_ast_edit
    access: modify key=main path=example.ml
    change: additions=3 removals=unknown diff=false
    -- insertions only --
    requests: 1
    source: ocaml_ast_edit
    access: modify key=main path=example.ml
    change: additions=1 removals=0 diff=false
    -- delete only --
    requests: 1
    source: ocaml_ast_edit
    access: modify key=main path=example.ml
    change: additions=0 removals=unknown diff=false
    -- outside --
    requests: 0
    -- auxiliary absolute --
    requests: 1
    source: ocaml_ast_edit
    access: modify key=aux path=same.ml
    change: additions=0 removals=unknown diff=false
    permission planning read source: false
    |}]

let%expect_test "all four operations resolve once against the original tree" =
  with_world @@ fun world ->
  let scope = Wio.open_claim_scope world.io in
  run_case ~json:true world "atomic operations"
    (input "example.ml"
       [
         edit ~text:"let before = 0\n  " "insert_before"
           (item_selector ~item_kind:"type" [ "M"; "t" ]);
         edit ~text:"\n  let after = 0" "insert_after"
           (item_selector ~item_kind:"type" [ "M"; "t" ]);
         edit ~text:"let add x = x + 42" "replace"
           (item_selector ~item_kind:"value" [ "M"; "add" ]);
         edit "delete" (item_selector ~item_kind:"value" [ "M"; "keep" ]);
       ]);
  close_scope scope;
  print_disk world "example.ml";
  run_case ~json:true world "byte column after UTF-8"
    (input "utf8.ml"
       [
         edit ~text:"replacement 2" "replace"
           (exact_selector "expression" 1 17 1 25);
       ]);
  print_disk world "utf8.ml";
  run_case ~json:true world "CRLF preserved"
    (input "crlf.ml"
       [
         edit ~text:"let target = 20" "replace"
           (item_selector ~item_kind:"value" [ "target" ]);
       ]);
  print_disk world "crlf.ml";
  run_case ~json:true world "same-boundary insertion order"
    (input "same.ml"
       [
         edit ~text:"let first = 1\n" "insert_before"
           (item_selector ~item_kind:"value" [ "root" ]);
         edit ~text:"let second = 2\n" "insert_before"
           (item_selector ~item_kind:"value" [ "root" ]);
       ]);
  print_disk world "same.ml";
  [%expect
    {|
    -- atomic operations --
    status: completed
    text: "modify: example.ml added=5 removed=unknown\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":5,"skipped":0}
    claim applies: 1
    claim observations: 0
    disk: "module M = struct\n  let before = 0\n  type t = int\n  let after = 0\n  let add x = x + 42\n  \nend\n\ntype same = A\nlet same = 1\nlet same = 2\nlet calc : int = outer (inner 1)\nlet pair = (10, 20)\nlet (left, right) = (1, 2)\n"
    -- byte column after UTF-8 --
    status: completed
    text: "modify: utf8.ml added=1 removed=unknown\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"skipped":0}
    disk: "let utf = (\"\195\169\", replacement 2)\n"
    -- CRLF preserved --
    status: completed
    text: "modify: crlf.ml added=1 removed=unknown\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"skipped":0}
    disk: "let first = 1\r\nlet target = 20\r\nlet last = 3\r\n"
    -- same-boundary insertion order --
    status: completed
    text: "modify: same.ml added=2 removed=0\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":2,"deletions":0,"skipped":0}
    disk: "let second = 2\nlet first = 1\nlet root = 1\n"
    |}]

let%expect_test "exact and enclosing selectors choose expression and type nodes"
    =
  with_world @@ fun world ->
  let scope = Wio.open_claim_scope world.io in
  run_case ~json:true world "node edits"
    (input "example.ml"
       [
         edit ~text:"string" "replace" (exact_selector "type" 10 11 10 14);
         edit ~text:"double 2" "replace"
           (exact_selector "expression" 10 23 10 32);
         edit ~text:"(30, 40)" "replace" (enclosing_selector "expression" 11 11);
       ]);
  close_scope scope;
  print_disk world "example.ml";
  [%expect
    {|
    -- node edits --
    status: completed
    text: "modify: example.ml added=3 removed=unknown\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":3,"skipped":0}
    claim applies: 1
    claim observations: 0
    disk: "module M = struct\n  type t = int\n  let add x = x + 1\n  let keep = 9\nend\n\ntype same = A\nlet same = 1\nlet same = 2\nlet calc : string = outer double 2\nlet pair = (30, 40)\nlet (left, right) = (1, 2)\n"
    |}]

let%expect_test
    "item namespaces occurrences pattern names and signatures are exact" =
  with_world @@ fun world ->
  run_case ~json:true world "implementation namespaces"
    (input "example.ml"
       [
         edit ~text:"type same = B" "replace"
           (item_selector ~item_kind:"type" [ "same" ]);
         edit ~text:"let same = 22" "replace"
           (item_selector ~item_kind:"value" ~occurrence:2 [ "same" ]);
         edit ~text:"let left, right = (3, 4)" "replace"
           (item_selector ~item_kind:"value" [ "right" ]);
       ]);
  print_disk world "example.ml";
  run_case ~json:true world "nested interface declaration"
    (input "api.mli"
       [
         edit ~text:"val answer : string" "replace"
           (item_selector ~item_kind:"value" [ "M"; "answer" ]);
       ]);
  print_disk world "api.mli";
  run_case ~json:true world "explicit interface grammar without suffix"
    (input ~file_kind:"interface" "api.source"
       [
         edit ~text:"val top : bytes" "replace"
           (item_selector ~item_kind:"value" [ "top" ]);
       ]);
  print_disk world "api.source";
  [%expect
    {|
    -- implementation namespaces --
    status: completed
    text: "modify: example.ml added=3 removed=unknown\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":3,"skipped":0}
    disk: "module M = struct\n  type t = int\n  let add x = x + 1\n  let keep = 9\nend\n\ntype same = B\nlet same = 1\nlet same = 22\nlet calc : int = outer (inner 1)\nlet pair = (10, 20)\nlet left, right = (3, 4)\n"
    -- nested interface declaration --
    status: completed
    text: "modify: api.mli added=1 removed=unknown\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"skipped":0}
    disk: "module M : sig\n  type t\n  val answer : string\nend\nval top : string\n"
    -- explicit interface grammar without suffix --
    status: completed
    text: "modify: api.source added=1 removed=unknown\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"skipped":0}
    disk: "module M : sig\n  type t\n  val answer : int\nend\nval top : bytes\n"
    |}]

let%expect_test
    "selection fragment overlap and complete-file failures are atomic" =
  with_world @@ fun world ->
  let before = read_disk (primary world "example.ml") in
  let cases =
    [
      ( "missing item",
        input "example.ml" [ edit "delete" (item_selector [ "M"; "missing" ]) ]
      );
      ( "out-of-file cursor",
        input "example.ml" [ edit "delete" (enclosing_selector "item" 99 0) ] );
      ( "exact range absent",
        input "example.ml" [ edit "delete" (exact_selector "type" 2 10 2 14) ]
      );
      ( "invalid item fragment",
        input "example.ml"
          [
            edit ~text:"let =" "replace"
              (item_selector ~item_kind:"value" [ "M"; "add" ]);
          ] );
      ( "insertion around expression",
        input "example.ml"
          [
            edit ~text:"let impossible = 0" "insert_before"
              (exact_selector "expression" 10 23 10 32);
          ] );
      ( "overlap",
        input "example.ml"
          [
            edit ~text:"type t = string" "replace"
              (item_selector ~item_kind:"type" [ "M"; "t" ]);
            edit ~text:"float" "replace" (exact_selector "type" 2 11 2 14);
          ] );
      ( "insertion and replacement at one start",
        input "example.ml"
          [
            edit ~text:"let before = 0\n" "insert_before"
              (item_selector ~item_kind:"value" [ "same" ]);
            edit ~text:"let same = 3" "replace"
              (item_selector ~item_kind:"value" [ "same" ]);
          ] );
      ( "edited source does not parse",
        input "example.ml"
          [
            edit ~text:"let adjacent = 0" "insert_after"
              (item_selector ~item_kind:"type" [ "M"; "t" ]);
          ] );
      ( "wrong grammar",
        input ~file_kind:"implementation" "api.mli"
          [ edit "delete" (item_selector [ "top" ]) ] );
      ( "source syntax",
        input "broken.ml" [ edit "delete" (item_selector [ "broken" ]) ] );
    ]
  in
  List.iter
    (fun (label, provider_input) -> run_case world label provider_input)
    cases;
  Printf.printf "source unchanged: %b\n"
    (String.equal before (read_disk (primary world "example.ml")));
  [%expect
    {|
    -- missing item --
    status: failed invalid_input
    message: AST selection not found: item M.missing occurrence 1
    metadata: false
    -- out-of-file cursor --
    status: failed invalid_input
    message: invalid source range: line 99 is outside the file
    metadata: false
    -- exact range absent --
    status: failed invalid_input
    message: AST selection not found: exact type at 2:10-2:14
    metadata: false
    -- invalid item fragment --
    status: failed invalid_input
    message: replacement item parse error at 1:4-1:5: Syntax error
    metadata: false
    -- insertion around expression --
    status: failed invalid_input
    message: invalid AST edit operation: insert_before and insert_after are only valid around item selections
    metadata: false
    -- overlap --
    status: failed invalid_input
    message: AST edits overlap: 2:2-2:14 and 2:11-2:14
    metadata: false
    -- insertion and replacement at one start --
    status: failed invalid_input
    message: AST edits overlap: 8:0-8:12 and 8:0-8:12
    metadata: false
    -- edited source does not parse --
    status: failed invalid_input
    message: edited source parse error at 2:29-2:30: Syntax error
    metadata: false
    -- wrong grammar --
    status: failed invalid_input
    message: source parse error at 5:0-5:3: Syntax error
    metadata: false
    -- source syntax --
    status: failed invalid_input
    message: source parse error at 2:0-2:0: Syntax error
    metadata: false
    source unchanged: true
    |}]

let%expect_test "unchanged stale and concurrent writes preserve freshness" =
  with_world @@ fun world ->
  let unchanged = Wio.open_claim_scope world.io in
  run_case ~json:true world "unchanged"
    (input
       ~if_identity:(identity implementation_source)
       "example.ml"
       [
         edit ~text:"let same = 1" "replace"
           (item_selector ~item_kind:"value" [ "same" ]);
       ]);
  close_scope unchanged;
  let stale = Wio.open_claim_scope world.io in
  run_case world "stale precondition"
    (input ~if_identity:(identity "older source") "example.ml"
       [ edit "delete" (item_selector [ "same" ]) ]);
  close_scope stale;
  let polls = ref 0 in
  let concurrent () =
    incr polls;
    if !polls = 2 then
      write_disk (primary world "example.ml") "let external = 1\n";
    false
  in
  let raced = Wio.open_claim_scope world.io in
  run_case ~cancelled:concurrent world "changed before apply"
    (input "example.ml"
       [
         edit ~text:"let same = 3" "replace"
           (item_selector ~item_kind:"value" [ "same" ]);
       ]);
  close_scope raced;
  print_disk world "example.ml";
  [%expect
    {|
    -- unchanged --
    status: completed
    text: "unchanged: example.ml added=0 removed=0\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":0,"additions":0,"deletions":0,"skipped":0}
    claim applies: 0
    claim observations: 0
    -- stale precondition --
    status: failed stale
    message: example.ml: stale file identity
    metadata: false
    claim applies: 0
    claim observations: 0
    -- changed before apply --
    status: failed stale
    message: example.ml: stale write
    metadata: false
    claim applies: 0
    claim observations: 0
    disk: "let external = 1\n"
    |}]

let%expect_test "text size target and workspace protections prevent mutation" =
  with_world @@ fun world ->
  let huge =
    "\nlet huge = \"" ^ String.make Ast_edit.max_file_bytes 'x' ^ "\""
  in
  write_disk (primary world "size-bound.ml") size_bound_source;
  run_case world "source at exact size bound"
    (input "size-bound.ml"
       [
         edit ~text:"let boundary = 1" "replace"
           (item_selector ~item_kind:"value" [ "boundary" ]);
       ]);
  print_disk world "size-bound.ml";
  let growth_source = "let grow = 1\n" in
  let fragment_bytes = Ast_edit.max_file_bytes - String.length growth_source in
  let exact_fragment = "(*" ^ String.make (fragment_bytes - 4) 'x' ^ "*)" in
  write_disk (primary world "final-bound.ml") growth_source;
  run_case world "final source at exact size bound"
    (input "final-bound.ml"
       [
         edit ~text:exact_fragment "insert_after"
           (item_selector ~item_kind:"value" [ "grow" ]);
       ]);
  print_disk world "final-bound.ml";
  print_case "UTF-8 BOM is not OCaml syntax";
  print_failure_kind
    (run world
       (input "bom.ml"
          [
            edit ~text:"let bom = 2" "replace"
              (item_selector ~item_kind:"value" [ "bom" ]);
          ]));
  print_disk world "bom.ml";
  let cases =
    [
      ("missing", "missing.ml", item_selector [ "missing" ], None);
      ("directory", "dir", item_selector [ "x" ], Some "let x = 2");
      ("FIFO", "pipe", item_selector [ "x" ], Some "let x = 2");
      ("binary", "binary.ml", item_selector [ "x" ], Some "let x = 2");
      ( "invalid UTF-8",
        "invalid-utf8.ml",
        item_selector [ "x" ],
        Some "let x = 2" );
      ( "oversized source",
        "oversized.ml",
        item_selector [ "x" ],
        Some "let x = 2" );
      ( "file parent",
        "parent-file/child.ml",
        item_selector [ "x" ],
        Some "let x = 2" );
      ("final symlink", "link.ml", item_selector [ "same" ], Some "let same = 3");
      ( "intermediate symlink",
        "link_parent/example.ml",
        item_selector [ "same" ],
        Some "let same = 3" );
      ( "protected git",
        ".git/protected.ml",
        item_selector [ "protected_value" ],
        Some "let protected_value = 2" );
      ( "protected mentat",
        ".mentat/protected.ml",
        item_selector [ "protected_value" ],
        Some "let protected_value = 2" );
    ]
  in
  List.iter
    (fun (label, path, selector, replacement) ->
      let ast_edit =
        match replacement with
        | None -> edit "delete" selector
        | Some text -> edit ~text "replace" selector
      in
      run_case world label (input ~file_kind:"implementation" path [ ast_edit ]))
    cases;
  run_case world "final source too large"
    (input "example.ml"
       [
         edit ~text:huge "insert_after"
           (item_selector ~item_kind:"value" [ "same" ]);
       ]);
  run_case world "outside workspace"
    (input
       (Filename.concat world.outside_dir "outside.ml")
       [ edit "delete" (item_selector [ "outside" ]) ]);
  print_disk world "example.ml";
  print_disk world ".git/protected.ml";
  [%expect
    {|
    -- source at exact size bound --
    status: completed
    text: "unchanged: size-bound.ml added=0 removed=0\n"
    truncated: false
    disk: regular bytes=1048576
    -- final source at exact size bound --
    status: completed
    text: "modify: final-bound.ml added=1 removed=0\n"
    truncated: false
    disk: regular bytes=1048576
    -- UTF-8 BOM is not OCaml syntax --
    status: failed invalid_input
    disk: "\239\187\191let bom = 1\n"
    -- missing --
    status: failed not_found
    message: missing.ml: path does not exist
    metadata: false
    -- directory --
    status: failed invalid_input
    message: dir: expected a regular file, found directory
    metadata: false
    -- FIFO --
    status: failed invalid_input
    message: pipe: expected a regular file, found FIFO
    metadata: false
    -- binary --
    status: failed invalid_input
    message: invalid UTF-8 text: source contents must be UTF-8 text
    metadata: false
    -- invalid UTF-8 --
    status: failed invalid_input
    message: invalid UTF-8 text: source contents must be valid UTF-8
    metadata: false
    -- oversized source --
    status: failed invalid_input
    message: oversized.ml: file is too large (1048577 bytes, max 1048576)
    metadata: false
    -- file parent --
    status: failed not_found
    message: parent-file/child.ml: path does not exist
    metadata: false
    -- final symlink --
    status: failed invalid_input
    message: link.ml: expected text, found other
    metadata: false
    -- intermediate symlink --
    status: failed invalid_input
    message: link_parent: symlink targets are not supported
    metadata: false
    -- protected git --
    status: failed invalid_input
    message: .git/protected.ml: .git is protected workspace metadata and cannot be modified by tools; change sandbox and workspace policy through configuration or the CLI instead
    metadata: false
    -- protected mentat --
    status: failed invalid_input
    message: .mentat/protected.ml: .mentat is protected workspace metadata and cannot be modified by tools; change sandbox and workspace policy through configuration or the CLI instead
    metadata: false
    -- final source too large --
    status: failed invalid_input
    message: example.ml: file is too large (1048783 bytes, max 1048576)
    metadata: false
    -- outside workspace --
    status: failed invalid_input
    message: path is outside workspace: <outside>/outside.ml
    metadata: false
    disk: "module M = struct\n  type t = int\n  let add x = x + 1\n  let keep = 9\nend\n\ntype same = A\nlet same = 1\nlet same = 2\nlet calc : int = outer (inner 1)\nlet pair = (10, 20)\nlet (left, right) = (1, 2)\n"
    disk: "let protected_value = 1\n"
    |}]

let%expect_test "auxiliary and read-only modes reject otherwise valid writes" =
  with_world ~cwd:Auxiliary_root @@ fun auxiliary_cwd ->
  run_case auxiliary_cwd "relative auxiliary collision"
    (input "same.ml"
       [
         edit ~text:"let auxiliary = 2" "replace"
           (item_selector [ "auxiliary" ]);
       ]);
  print_disk_at "primary" (primary auxiliary_cwd "same.ml");
  print_disk_at "auxiliary" (auxiliary auxiliary_cwd "same.ml");
  run_case ~json:true auxiliary_cwd "absolute primary from auxiliary cwd"
    (input
       (primary auxiliary_cwd "same.ml")
       [ edit ~text:"let root = 2" "replace" (item_selector [ "root" ]) ]);
  print_disk_at "primary" (primary auxiliary_cwd "same.ml");
  with_world ~cwd:Primary_logical @@ fun logical ->
  run_case ~json:true logical "logical cwd"
    (input "cwd.ml"
       [
         edit ~text:"let cwd_value = 2" "replace"
           (item_selector [ "cwd_value" ]);
       ]);
  print_disk_at "logical" (primary logical "logical/cwd.ml");
  with_world ~mode:Mentat_config.Mode.Read_only @@ fun read_only ->
  run_case read_only "read-only mode"
    (input "example.ml"
       [ edit "delete" (item_selector ~item_kind:"value" [ "same" ]) ]);
  print_disk read_only "example.ml";
  [%expect
    {|
    -- relative auxiliary collision --
    status: failed invalid_input
    message: same.ml: edit target belongs to a read-only workspace root
    metadata: false
    primary: "let root = 1\n"
    auxiliary: "let auxiliary = 1\n"
    -- absolute primary from auxiliary cwd --
    status: completed
    text: "modify: <workspace>/same.ml added=1 removed=unknown\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"skipped":0}
    primary: "let root = 2\n"
    -- logical cwd --
    status: completed
    text: "modify: cwd.ml added=1 removed=unknown\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"skipped":0}
    logical: "let cwd_value = 2\n"
    -- read-only mode --
    status: failed invalid_input
    message: example.ml: edit target belongs to a read-only workspace root
    metadata: false
    disk: "module M = struct\n  type t = int\n  let add x = x + 1\n  let keep = 9\nend\n\ntype same = A\nlet same = 1\nlet same = 2\nlet calc : int = outer (inner 1)\nlet pair = (10, 20)\nlet (left, right) = (1, 2)\n"
    |}]

let%expect_test "cancellation at either poll never reaches the commit boundary"
    =
  with_world @@ fun world ->
  let first = Wio.open_claim_scope world.io in
  run_case
    ~cancelled:(fun () -> true)
    world "before resolution"
    (input "link.ml" [ edit "delete" (item_selector [ "same" ]) ]);
  close_scope first;
  let polls = ref 0 in
  let after_observation () =
    incr polls;
    !polls = 2
  in
  let second = Wio.open_claim_scope world.io in
  run_case ~cancelled:after_observation world "after planning"
    (input "example.ml"
       [
         edit ~text:"let same = 3" "replace"
           (item_selector ~item_kind:"value" [ "same" ]);
       ]);
  close_scope second;
  Printf.printf "polls: %d\n" !polls;
  print_disk world "example.ml";
  [%expect
    {|
    -- before resolution --
    status: interrupted cancelled=true
    reason: tool call cancelled
    claim applies: 0
    claim observations: 0
    -- after planning --
    status: interrupted cancelled=true
    reason: tool call cancelled
    claim applies: 0
    claim observations: 0
    polls: 2
    disk: "module M = struct\n  type t = int\n  let add x = x + 1\n  let keep = 9\nend\n\ntype same = A\nlet same = 1\nlet same = 2\nlet calc : int = outer (inner 1)\nlet pair = (10, 20)\nlet (left, right) = (1, 2)\n"
    |}]

let%expect_test "durable output is summary-only and mutation truth stays scoped"
    =
  with_world @@ fun world ->
  let scope = Wio.open_claim_scope world.io in
  let result =
    run world
      (input "example.ml"
         [
           edit ~text:"let same = 7\nlet companion = 8" "replace"
             (item_selector ~item_kind:"value" [ "same" ]);
         ])
  in
  let output = output_exn result in
  let frame = Option.get (Tool.Output.json output) in
  let durable = encode result_codec result in
  let replayed = decode result_codec durable in
  let replayed_output = output_exn replayed in
  Printf.printf "text: %S\n" (Tool.Output.text output);
  Printf.printf "json: %s\n" (json_string frame);
  Printf.printf "durable: %s\n" (json_string durable);
  Printf.printf "durable equal: %b\n" (Tool.Output.equal output replayed_output);
  let names = json_member_names frame in
  List.iter
    (fun forbidden ->
      Printf.printf "contains %s: %b\n" forbidden (List.mem forbidden names))
    [
      "contents";
      "selected_text";
      "range";
      "identity";
      "if_identity";
      "diff";
      "receipt";
      "mutation";
      "evidence";
      "before";
      "after";
      "path";
      "operation";
    ];
  close_scope scope;
  print_disk world "example.ml";
  [%expect
    {|
    text: "modify: example.ml added=2 removed=unknown\n"
    json: {"version":1,"disposition":"applied","files":1,"additions":2,"skipped":0}
    durable: {"status":"completed","output":{"text":"modify: example.ml added=2 removed=unknown\n","json":{"version":1,"disposition":"applied","files":1,"additions":2,"skipped":0},"truncated":false}}
    durable equal: true
    contains contents: false
    contains selected_text: false
    contains range: false
    contains identity: false
    contains if_identity: false
    contains diff: false
    contains receipt: false
    contains mutation: false
    contains evidence: false
    contains before: false
    contains after: false
    contains path: false
    contains operation: false
    claim applies: 1
    claim observations: 0
    disk: "module M = struct\n  type t = int\n  let add x = x + 1\n  let keep = 9\nend\n\ntype same = A\nlet same = 7\nlet companion = 8\nlet same = 2\nlet calc : int = outer (inner 1)\nlet pair = (10, 20)\nlet (left, right) = (1, 2)\n"
    |}]
