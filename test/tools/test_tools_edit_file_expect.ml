(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for [edit_file]. Every call is
   provider JSON decoded through [Call.decode], inspected through its cached
   permission request, and executed through [Call.run] over a real resolved
   workspace capability. The suite never calls private input, matching, or
   output helpers. Disk state and claim evidence prove the native edit seam's
   freshness, no-follow, one-apply, and attribution contracts. *)

open Windtrap
open Test_tools_support
module Edit_file = Mentat_tools.Fs.Edit_file
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

let input ?occurrence ?if_identity path old_string new_string =
  [
    ("path", Json.string path);
    ("old_string", Json.string old_string);
    ("new_string", Json.string new_string);
  ]
  |> add_optional "occurrence" (fun value -> Json.string value) occurrence
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

let print_result ?(json = false) world result =
  (match Tool.Result.status result with
  | Tool.Result.Completed -> print_endline "status: completed"
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf "status: failed %s\nmessage: %s\nmetadata: %s\n"
        (failure_name kind) (normalize world message)
        (match metadata with
        | None -> "none"
        | Some value -> normalize world (json_string value))
  | Tool.Result.Interrupted { reason; cancelled } ->
      Printf.printf "status: interrupted cancelled=%b\nreason: %s\n" cancelled
        reason);
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

let populate_main root =
  let file path contents = write_disk (Filename.concat root path) contents in
  file "note.txt" "alpha\nbeta\n";
  file "repeat.txt" "cat cat cat\n";
  file "overlap.txt" "aaaa";
  file "crlf.txt" "alpha\r\nbeta\r\n";
  file "mixed.txt" "alpha\r\nbeta\n";
  file "bom.txt" "\239\187\191alpha\n";
  file "unicode.txt" "café\n";
  file "same.txt" "primary\n";
  file "logical/same.txt" "logical\n";
  file "parent-file" "not a directory\n";
  file "bad.bin" "text\000payload\n";
  file "bad-utf8.txt" "\255\254\n";
  file "oversized.txt" (String.make (Edit_file.max_file_bytes + 1) 'o');
  file "maximum.txt" ("M" ^ String.make (Edit_file.max_file_bytes - 1) 'x');
  file ".git/config" "protected\n";
  file ".mentat/state" "protected\n";
  mkdir (Filename.concat root "dir");
  Unix.mkfifo (Filename.concat root "pipe") 0o600;
  Unix.symlink "note.txt" (Filename.concat root "link-note.txt");
  Unix.symlink "." (Filename.concat root "link-parent")

let populate_aux root =
  write_disk (Filename.concat root "same.txt") "auxiliary\n";
  write_disk (Filename.concat root "only-aux.txt") "auxiliary only\n"

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
  let raw = Filename.temp_dir "mentat-edit-file-" "" in
  Fun.protect ~finally:(fun () -> remove_tree raw) @@ fun () ->
  let base = Unix.realpath raw in
  let ws_dir = Filename.concat base "workspace" in
  let aux_dir = Filename.concat base "auxiliary" in
  let outside_dir = Filename.concat base "outside" in
  List.iter mkdir [ ws_dir; aux_dir; outside_dir ];
  populate_main ws_dir;
  populate_aux aux_dir;
  write_disk (Filename.concat outside_dir "outside.txt") "outside\n";
  let logical = workspace ~cwd ws_dir aux_dir in
  Eio.Switch.run @@ fun sw ->
  let io = resolve_exn ~sw ~stdenv ~logical ~mode () in
  fn { ws_dir; aux_dir; outside_dir; io; tool = Edit_file.make io }

let primary world path = Filename.concat world.ws_dir path
let auxiliary world path = Filename.concat world.aux_dir path

let print_disk_at label path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_REG; st_size; _ } ->
      if st_size <= 300 then Printf.printf "%s: %S\n" label (read_disk path)
      else Printf.printf "%s: regular bytes=%d\n" label st_size
  | { Unix.st_kind = Unix.S_DIR; _ } -> Printf.printf "%s: <directory>\n" label
  | { Unix.st_kind = Unix.S_LNK; _ } ->
      Printf.printf "%s: symlink->%s\n" label (Unix.readlink path)
  | _ -> Printf.printf "%s: <special>\n" label
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      Printf.printf "%s: <missing>\n" label

let print_disk world path = print_disk_at "disk" (primary world path)

let print_mode world path =
  let mode = (Unix.stat (primary world path)).Unix.st_perm land 0o777 in
  Printf.printf "mode: %03o\n" mode

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
      let items = Permission.Request.items request in
      Printf.printf "items: %d\n" (List.length items);
      List.iter
        (fun item ->
          (match Permission.Request.Item.access item with
          | Permission.Access.Path
              { op; scope = Permission.Access.Path_scope.Workspace path } ->
              Printf.printf "access: %s key=%s path=%s\n" (path_op_name op)
                (Workspace.Root.Key.to_string (Workspace.Path.root_key path))
                (Workspace.Path.display path)
          | access ->
              failf "unexpected edit permission: %a" Permission.Access.pp access);
          match Permission.Request.Item.change item with
          | None -> print_endline "change: none"
          | Some change ->
              Printf.printf "change: additions=%s removals=%s diff=%b\n"
                (Option.fold ~none:"unknown" ~some:string_of_int
                   (Permission.Request.Change.additions change))
                (Option.fold ~none:"unknown" ~some:string_of_int
                   (Permission.Request.Change.removals change))
                (Option.is_some (Permission.Request.Change.diff change)))
        items)
    requests

let close_scope scope =
  let evidence = Wio.Claim_scope.close scope in
  Printf.printf "claim applies: %d\n"
    (List.length evidence.Mentat_edit.Apply_evidence.applies);
  Printf.printf "claim observations: %d\n"
    (List.length evidence.Mentat_edit.Apply_evidence.observed)

let%expect_test "declaration and provider decoding are strict" =
  with_world @@ fun world ->
  let declaration = Tool.declaration world.tool in
  Printf.printf "name: %s\n" (Mentat_llm.Tool.name declaration);
  Printf.printf "schema: %s\n"
    (json_string (Mentat_llm.Tool.input_schema declaration));
  decode_verdict world.tool "minimal" (input "note.txt" "alpha" "bravo");
  decode_verdict world.tool "all and identity"
    (input ~occurrence:"all" ~if_identity:(identity "cat cat cat\n")
       "repeat.txt" "cat" "dog");
  decode_verdict world.tool "missing path"
    (json_object
       [ ("old_string", Json.string "a"); ("new_string", Json.string "b") ]);
  decode_verdict world.tool "missing old string"
    (json_object
       [ ("path", Json.string "note.txt"); ("new_string", Json.string "b") ]);
  decode_verdict world.tool "missing new string"
    (json_object
       [ ("path", Json.string "note.txt"); ("old_string", Json.string "a") ]);
  decode_verdict world.tool "unknown member"
    (json_object
       [
         ("path", Json.string "note.txt");
         ("old_string", Json.string "a");
         ("new_string", Json.string "b");
         ("replace_all", Json.bool true);
       ]);
  decode_verdict world.tool "duplicate path"
    (json_object
       [
         ("path", Json.string "note.txt");
         ("path", Json.string "other.txt");
         ("old_string", Json.string "a");
         ("new_string", Json.string "b");
       ]);
  decode_verdict world.tool "wrong path type"
    (json_object
       [
         ("path", Json.int 1);
         ("old_string", Json.string "a");
         ("new_string", Json.string "b");
       ]);
  decode_verdict world.tool "wrong old type"
    (json_object
       [
         ("path", Json.string "note.txt");
         ("old_string", Json.bool false);
         ("new_string", Json.string "b");
       ]);
  decode_verdict world.tool "wrong new type"
    (json_object
       [
         ("path", Json.string "note.txt");
         ("old_string", Json.string "a");
         ("new_string", Json.int 2);
       ]);
  decode_verdict world.tool "numeric occurrence"
    (json_object
       [
         ("path", Json.string "note.txt");
         ("old_string", Json.string "a");
         ("new_string", Json.string "b");
         ("occurrence", Json.int 1);
       ]);
  decode_verdict world.tool "unknown occurrence"
    (input ~occurrence:"first" "note.txt" "a" "b");
  decode_verdict world.tool "wrong identity type"
    (json_object
       [
         ("path", Json.string "note.txt");
         ("old_string", Json.string "a");
         ("new_string", Json.string "b");
         ("if_identity", Json.bool true);
       ]);
  decode_verdict world.tool "empty path" (input "" "a" "b");
  decode_verdict world.tool "empty old string" (input "note.txt" "" "b");
  decode_verdict world.tool "same strings" (input "note.txt" "a" "a");
  decode_verdict world.tool "empty identity"
    (input ~if_identity:"" "note.txt" "a" "b");
  decode_verdict world.tool "malformed identity"
    (input ~if_identity:"not-an-identity" "note.txt" "a" "b");
  decode_verdict world.tool "invalid UTF-8 old" (input "note.txt" "\255bad" "b");
  decode_verdict world.tool "invalid UTF-8 new" (input "note.txt" "a" "\255bad");
  decode_verdict world.tool "NUL old" (input "note.txt" "a\000b" "b");
  decode_verdict world.tool "control-dense new"
    (input "note.txt" "a" "\001\002\003\004text");
  [%expect
    {|
    name: edit_file
    schema: {"type":"object","properties":{"path":{"type":"string","description":"Workspace-relative or workspace-contained absolute existing file path to edit.","minLength":1},"old_string":{"type":"string","description":"Exact non-empty UTF-8 text to replace. By default it must occur exactly once.","minLength":1},"new_string":{"type":"string","description":"Replacement UTF-8 text. It must differ from old_string and may be empty."},"occurrence":{"type":"string","description":"Replacement multiplicity. Defaults to once; all replaces every non-overlapping occurrence.","enum":["once","all"]},"if_identity":{"type":"string","description":"Complete-file identity from a previous complete read_file observation.","minLength":1}},"required":["path","old_string","new_string"],"additionalProperties":false}
    minimal: accepted canonical={"new_string":"bravo","old_string":"alpha","path":"note.txt"}
    all and identity: accepted canonical={"if_identity":"sha256:7214ffa4ae5f8bd18ec11b1b6992ac56420e1aee0dada147ea2dbd169cca6c3a:12","new_string":"dog","occurrence":"all","old_string":"cat","path":"repeat.txt"}
    missing path: rejected diagnostic=true
    missing old string: rejected diagnostic=true
    missing new string: rejected diagnostic=true
    unknown member: rejected diagnostic=true
    duplicate path: rejected diagnostic=true
    wrong path type: rejected diagnostic=true
    wrong old type: rejected diagnostic=true
    wrong new type: rejected diagnostic=true
    numeric occurrence: rejected diagnostic=true
    unknown occurrence: rejected diagnostic=true
    wrong identity type: rejected diagnostic=true
    empty path: rejected diagnostic=true
    empty old string: rejected diagnostic=true
    same strings: rejected diagnostic=true
    empty identity: rejected diagnostic=true
    malformed identity: rejected diagnostic=true
    invalid UTF-8 old: rejected diagnostic=true
    invalid UTF-8 new: rejected diagnostic=true
    NUL old: rejected diagnostic=true
    control-dense new: rejected diagnostic=true |}]

let%expect_test "permissions are one input-only modify request" =
  with_world @@ fun world ->
  let before = read_disk (primary world "note.txt") in
  print_case "default";
  print_permissions
    (decode_call world.tool (input "note.txt" "alpha\nbeta" "one\ntwo"));
  print_case "all plus identity";
  print_permissions
    (decode_call world.tool
       (input ~occurrence:"all" ~if_identity:(identity "unobserved")
          "repeat.txt" "cat" "dog"));
  print_case "missing target still plans";
  print_permissions (decode_call world.tool (input "missing.txt" "old" "new"));
  print_case "outside";
  print_permissions
    (decode_call world.tool
       (input (Filename.concat world.outside_dir "outside.txt") "outside" "x"));
  Printf.printf "planning changed file: %b\n"
    (not (String.equal before (read_disk (primary world "note.txt"))));
  [%expect
    {|
    -- default --
    requests: 1
    source: edit_file
    items: 1
    access: modify key=main path=note.txt
    change: additions=2 removals=2 diff=true
    -- all plus identity --
    requests: 1
    source: edit_file
    items: 1
    access: modify key=main path=repeat.txt
    change: additions=1 removals=1 diff=true
    -- missing target still plans --
    requests: 1
    source: edit_file
    items: 1
    access: modify key=main path=missing.txt
    change: additions=1 removals=1 diff=true
    -- outside --
    requests: 0
    planning changed file: false |}]

let%expect_test
    "once requires uniqueness and all replaces non-overlapping matches" =
  with_world @@ fun world ->
  let ambiguous = Wio.open_claim_scope world.io in
  run_case world "ambiguous once" (input "repeat.txt" "cat" "dog");
  close_scope ambiguous;
  run_case world "all still requires a match"
    (input ~occurrence:"all" "note.txt" "missing" "replacement");
  let all = Wio.open_claim_scope world.io in
  run_case ~json:true world "replace all"
    (input ~occurrence:"all" "repeat.txt" "cat" "dog");
  close_scope all;
  print_disk world "repeat.txt";
  let overlap = Wio.open_claim_scope world.io in
  run_case ~json:true world "non-overlapping"
    (input ~occurrence:"all" "overlap.txt" "aa" "b");
  close_scope overlap;
  print_disk world "overlap.txt";
  let deletion = Wio.open_claim_scope world.io in
  run_case ~json:true world "delete exact text" (input "note.txt" "beta\n" "");
  close_scope deletion;
  print_disk world "note.txt";
  [%expect
    {|
    -- ambiguous once --
    status: failed stale
    message: repeat.txt: old_string matched 3 times; provide more context or set occurrence=all
    metadata: {"path":"repeat.txt","matches":3,"occurrence":"once"}
    claim applies: 0
    claim observations: 0
    -- all still requires a match --
    status: failed not_found
    message: note.txt: old_string was not found
    metadata: {"path":"note.txt","matches":0,"occurrence":"all"}
    -- replace all --
    status: completed
    text: "modify: repeat.txt added=1 removed=1\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
    claim applies: 1
    claim observations: 0
    disk: "dog dog dog\n"
    -- non-overlapping --
    status: completed
    text: "modify: overlap.txt added=1 removed=1\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
    claim applies: 1
    claim observations: 0
    disk: "bb"
    -- delete exact text --
    status: completed
    text: "modify: note.txt added=0 removed=1\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":0,"deletions":1,"skipped":0}
    claim applies: 1
    claim observations: 0
    disk: "alpha\n" |}]

let%expect_test
    "identity guard precedes matching and accepts exact observations" =
  with_world @@ fun world ->
  run_case world "stale identity before missing match"
    (input ~if_identity:(identity "older\n") "note.txt" "missing" "new");
  let scope = Wio.open_claim_scope world.io in
  run_case ~json:true world "matching identity"
    (input ~if_identity:(identity "alpha\nbeta\n") "note.txt" "alpha" "bravo");
  close_scope scope;
  print_disk world "note.txt";
  [%expect
    {|
    -- stale identity before missing match --
    status: failed stale
    message: note.txt: stale file identity
    metadata: none
    -- matching identity --
    status: completed
    text: "modify: note.txt added=1 removed=1\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
    claim applies: 1
    claim observations: 0
    disk: "bravo\nbeta\n" |}]

let%expect_test "not-found identity and normalized no-op failures stay distinct"
    =
  with_world @@ fun world ->
  run_case world "not found" (input "note.txt" "gamma" "delta");
  run_case world "stale identity"
    (input ~if_identity:(identity "older\n") "note.txt" "alpha" "bravo");
  let crlf = Wio.open_claim_scope world.io in
  run_case ~json:true world "newline-normalized no-op"
    (input "crlf.txt" "alpha\r\n" "alpha\n");
  close_scope crlf;
  let bom = Wio.open_claim_scope world.io in
  run_case ~json:true world "BOM-normalized no-op"
    (input "bom.txt" "\239\187\191alpha" "alpha");
  close_scope bom;
  print_disk world "crlf.txt";
  print_disk world "bom.txt";
  [%expect
    {|
    -- not found --
    status: failed not_found
    message: note.txt: old_string was not found
    metadata: {"path":"note.txt","matches":0,"occurrence":"once"}
    -- stale identity --
    status: failed stale
    message: note.txt: stale file identity
    metadata: none
    -- newline-normalized no-op --
    status: completed
    text: "unchanged: crlf.txt added=0 removed=0\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":0,"additions":0,"deletions":0,"skipped":0}
    claim applies: 0
    claim observations: 0
    -- BOM-normalized no-op --
    status: completed
    text: "unchanged: bom.txt added=0 removed=0\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":0,"additions":0,"deletions":0,"skipped":0}
    claim applies: 0
    claim observations: 0
    disk: "alpha\r\nbeta\r\n"
    disk: "\239\187\191alpha\n" |}]

let%expect_test "newline BOM and Unicode edits preserve target text conventions"
    =
  with_world @@ fun world ->
  run_case ~json:true world "CRLF target"
    (input "crlf.txt" "alpha\nbeta" "one\ntwo");
  print_disk world "crlf.txt";
  run_case world "mixed target remains byte-exact"
    (input "mixed.txt" "alpha\r\nbeta" "changed");
  print_disk world "mixed.txt";
  run_case ~json:true world "BOM preserved" (input "bom.txt" "alpha" "bravo");
  print_disk world "bom.txt";
  run_case ~json:true world "input BOM not duplicated"
    (input "bom.txt" "\239\187\191bravo" "\239\187\191charlie");
  print_disk world "bom.txt";
  run_case world "BOM-only pattern" (input "bom.txt" "\239\187\191" "prefix");
  run_case ~json:true world "Unicode" (input "unicode.txt" "café" "thé");
  print_disk world "unicode.txt";
  [%expect
    {|
    -- CRLF target --
    status: completed
    text: "modify: crlf.txt added=2 removed=2\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":2,"deletions":2,"skipped":0}
    disk: "one\r\ntwo\r\n"
    -- mixed target remains byte-exact --
    status: failed not_found
    message: mixed.txt: old_string was not found
    metadata: {"path":"mixed.txt","matches":0,"occurrence":"once"}
    disk: "alpha\r\nbeta\n"
    -- BOM preserved --
    status: completed
    text: "modify: bom.txt added=1 removed=1\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
    disk: "\239\187\191bravo\n"
    -- input BOM not duplicated --
    status: completed
    text: "modify: bom.txt added=1 removed=1\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
    disk: "\239\187\191charlie\n"
    -- BOM-only pattern --
    status: failed invalid_input
    message: bom.txt: old_string must contain text beyond the UTF-8 BOM
    metadata: none
    -- Unicode --
    status: completed
    text: "modify: unicode.txt added=1 removed=1\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
    disk: "th\195\169\n" |}]

let%expect_test "native observation rejects size and non-text targets" =
  with_world @@ fun world ->
  run_case world "binary target" (input "bad.bin" "text" "other");
  run_case world "invalid UTF-8 target" (input "bad-utf8.txt" "old" "new");
  run_case world "oversized target" (input "oversized.txt" "o" "x");
  run_case world "oversized result"
    (input "note.txt" "alpha" (String.make (Edit_file.max_file_bytes + 1) 'n'));
  run_case ~json:true world "exact maximum result" (input "maximum.txt" "M" "N");
  print_disk world "maximum.txt";
  [%expect
    {|
    -- binary target --
    status: failed invalid_input
    message: bad.bin: binary file
    metadata: none
    -- invalid UTF-8 target --
    status: failed invalid_input
    message: bad-utf8.txt: not valid UTF-8 text
    metadata: none
    -- oversized target --
    status: failed invalid_input
    message: oversized.txt: file is too large (1048577 bytes, max 1048576)
    metadata: none
    -- oversized result --
    status: failed invalid_input
    message: note.txt: file is too large (1048583 bytes, max 1048576)
    metadata: none
    -- exact maximum result --
    status: completed
    text: "modify: maximum.txt added=1 removed=1\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
    disk: regular bytes=1048576 |}]

let%expect_test "native no-follow and workspace protections classify targets" =
  with_world @@ fun world ->
  let cases =
    [
      ("missing", "missing.txt", "old", "new");
      ("directory", "dir", "old", "new");
      ("FIFO", "pipe", "old", "new");
      ("final symlink same", "link-note.txt", "alpha", "alpha\n");
      ("final symlink changed", "link-note.txt", "alpha", "bravo");
      ("intermediate symlink", "link-parent/note.txt", "alpha", "bravo");
      ("file parent", "parent-file/child.txt", "old", "new");
      ("protected git", ".git/config", "protected", "changed");
      ("protected mentat", ".mentat/state", "protected", "changed");
    ]
  in
  List.iter
    (fun (label, path, old_string, new_string) ->
      run_case world label (input path old_string new_string))
    cases;
  run_case world "outside workspace"
    (input (Filename.concat world.outside_dir "outside.txt") "outside" "x");
  print_disk world "note.txt";
  print_disk world ".git/config";
  [%expect
    {|
    -- missing --
    status: failed not_found
    message: missing.txt: path does not exist
    metadata: none
    -- directory --
    status: failed invalid_input
    message: dir: expected text, found other
    metadata: none
    -- FIFO --
    status: failed invalid_input
    message: pipe: expected text, found other
    metadata: none
    -- final symlink same --
    status: failed invalid_input
    message: link-note.txt: expected text, found other
    metadata: none
    -- final symlink changed --
    status: failed invalid_input
    message: link-note.txt: expected text, found other
    metadata: none
    -- intermediate symlink --
    status: failed invalid_input
    message: link-parent: symlink targets are not supported
    metadata: none
    -- file parent --
    status: failed invalid_input
    message: parent-file: not a directory
    metadata: none
    -- protected git --
    status: failed invalid_input
    message: .git/config: .git is protected workspace metadata and cannot be modified by tools; change sandbox and workspace policy through configuration or the CLI instead
    metadata: none
    -- protected mentat --
    status: failed invalid_input
    message: .mentat/state: .mentat is protected workspace metadata and cannot be modified by tools; change sandbox and workspace policy through configuration or the CLI instead
    metadata: none
    -- outside workspace --
    status: failed invalid_input
    message: path is outside workspace: <outside>/outside.txt
    metadata: none
    disk: "alpha\nbeta\n"
    disk: "protected\n" |}]

let%expect_test "root identity and fixed cwd determine the addressed target" =
  with_world ~cwd:Auxiliary_root @@ fun auxiliary_cwd ->
  run_case auxiliary_cwd "relative auxiliary collision"
    (input "same.txt" "auxiliary" "changed");
  print_disk_at "primary" (primary auxiliary_cwd "same.txt");
  print_disk_at "auxiliary" (auxiliary auxiliary_cwd "same.txt");
  run_case ~json:true auxiliary_cwd "absolute primary from auxiliary cwd"
    (input (primary auxiliary_cwd "same.txt") "primary" "changed");
  print_disk_at "primary" (primary auxiliary_cwd "same.txt");
  with_world ~cwd:Primary_logical @@ fun logical ->
  run_case ~json:true logical "logical cwd"
    (input "same.txt" "logical" "changed");
  print_disk_at "logical" (primary logical "logical/same.txt");
  with_world ~mode:Mentat_config.Mode.Read_only @@ fun read_only ->
  run_case read_only "read-only mode" (input "note.txt" "alpha" "changed");
  print_disk read_only "note.txt";
  [%expect
    {|
    -- relative auxiliary collision --
    status: failed invalid_input
    message: same.txt: edit target belongs to a read-only workspace root
    metadata: none
    primary: "primary\n"
    auxiliary: "auxiliary\n"
    -- absolute primary from auxiliary cwd --
    status: completed
    text: "modify: <workspace>/same.txt added=1 removed=1\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
    primary: "changed\n"
    -- logical cwd --
    status: completed
    text: "modify: same.txt added=1 removed=1\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
    logical: "changed\n"
    -- read-only mode --
    status: failed invalid_input
    message: note.txt: edit target belongs to a read-only workspace root
    metadata: none
    disk: "alpha\nbeta\n" |}]

let%expect_test "apply preserves mode and revalidates observation under races" =
  with_world @@ fun world ->
  Unix.chmod (primary world "note.txt") 0o755;
  let changed = Wio.open_claim_scope world.io in
  run_case ~json:true world "mode-preserving edit"
    (input "note.txt" "alpha" "bravo");
  close_scope changed;
  print_mode world "note.txt";
  let polls = ref 0 in
  let race () =
    incr polls;
    if !polls = 2 then write_disk (primary world "repeat.txt") "external\n";
    false
  in
  let raced = Wio.open_claim_scope world.io in
  run_case ~cancelled:race world "changed before apply"
    (input ~occurrence:"all" "repeat.txt" "cat" "dog");
  close_scope raced;
  Printf.printf "polls: %d\n" !polls;
  print_disk world "repeat.txt";
  [%expect
    {|
    -- mode-preserving edit --
    status: completed
    text: "modify: note.txt added=1 removed=1\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
    claim applies: 1
    claim observations: 0
    mode: 755
    -- changed before apply --
    status: failed stale
    message: repeat.txt: stale write
    metadata: none
    claim applies: 0
    claim observations: 0
    polls: 2
    disk: "external\n" |}]

let%expect_test "cancellation at either poll prevents application" =
  with_world @@ fun world ->
  let first = Wio.open_claim_scope world.io in
  run_case
    ~cancelled:(fun () -> true)
    world "before resolution"
    (input "link-note.txt" "alpha" "bravo");
  close_scope first;
  let polls = ref 0 in
  let after_observation () =
    incr polls;
    !polls = 2
  in
  let second = Wio.open_claim_scope world.io in
  run_case ~cancelled:after_observation world "after observation"
    (input "note.txt" "alpha" "bravo");
  close_scope second;
  Printf.printf "polls: %d\n" !polls;
  print_disk world "note.txt";
  [%expect
    {|
    -- before resolution --
    status: interrupted cancelled=true
    reason: tool call cancelled
    claim applies: 0
    claim observations: 0
    -- after observation --
    status: interrupted cancelled=true
    reason: tool call cancelled
    claim applies: 0
    claim observations: 0
    polls: 2
    disk: "alpha\nbeta\n" |}]

let%expect_test "durable output is summary-only and mutation truth stays scoped"
    =
  with_world @@ fun world ->
  let scope = Wio.open_claim_scope world.io in
  let result = run world (input ~occurrence:"all" "repeat.txt" "cat" "dog") in
  let output = output_exn result in
  let durable = encode result_codec result in
  let replayed = decode result_codec durable in
  let replayed_output = output_exn replayed in
  let frame = Option.get (Tool.Output.json output) in
  let names = json_member_names frame in
  Printf.printf "text: %S\n" (Tool.Output.text output);
  Printf.printf "json: %s\n" (json_string frame);
  Printf.printf "durable: %s\n" (json_string durable);
  Printf.printf "text replay equal: %b\n"
    (String.equal (Tool.Output.text output) (Tool.Output.text replayed_output));
  Printf.printf "json replay equal: %b\n"
    (Option.equal Jsont.Json.equal (Tool.Output.json output)
       (Tool.Output.json replayed_output));
  Printf.printf "truncated replay equal: %b\n"
    (Bool.equal
       (Tool.Output.truncated output)
       (Tool.Output.truncated replayed_output));
  List.iter
    (fun forbidden ->
      Printf.printf "contains %s: %b\n" forbidden (List.mem forbidden names))
    [
      "contents";
      "old_string";
      "new_string";
      "identity";
      "receipt";
      "mutation";
      "evidence";
      "before";
      "after";
      "result";
      "path";
      "operation";
    ];
  close_scope scope;
  print_disk world "repeat.txt";
  [%expect
    {|
    text: "modify: repeat.txt added=1 removed=1\n"
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
    durable: {"status":"completed","output":{"text":"modify: repeat.txt added=1 removed=1\n","json":{"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0},"truncated":false}}
    text replay equal: true
    json replay equal: true
    truncated replay equal: true
    contains contents: false
    contains old_string: false
    contains new_string: false
    contains identity: false
    contains receipt: false
    contains mutation: false
    contains evidence: false
    contains before: false
    contains after: false
    contains result: false
    contains path: false
    contains operation: false
    claim applies: 1
    claim observations: 0
    disk: "dog dog dog\n" |}]

[%%run_tests "mentat.tools.edit_file"]
