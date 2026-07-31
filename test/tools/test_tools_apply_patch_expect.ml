(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for [apply_patch]. Every call
   sends provider JSON through the declaration's decoder, cached permission
   plan, erased run callback, native workspace edit capability, and durable
   result codec. The private patch parser and typed planner are never called
   directly: grammar and planning behavior are observed only through the tool
   contract that models and providers use. *)

open Windtrap
open Test_tools_support
module Apply_patch = Mentat_tools.Fs.Apply_patch
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

let input patch = json_object [ ("patch", Json.string patch) ]

let document lines =
  String.concat "\n" (("*** Begin Patch" :: lines) @ [ "*** End Patch" ])

let normalize world text =
  text
  |> String.replace_all ~sub:world.aux_dir ~by:"<auxiliary>"
  |> String.replace_all ~sub:world.outside_dir ~by:"<outside>"
  |> String.replace_all ~sub:world.ws_dir ~by:"<workspace>"

let print_result ?(json = false) world result =
  (match Tool.Result.status result with
  | Tool.Result.Completed -> print_endline "status: completed"
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf "status: failed %s\nmessage: %s\nmetadata: %b\n"
        (failure_name kind) (normalize world message) (Option.is_some metadata)
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

let decode_call_result tool provider_input =
  Tool.Call.decode tool (model_call tool provider_input)

let decode_call tool provider_input =
  match decode_call_result tool provider_input with
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

let with_env name value fn =
  let saved = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect
    ~finally:(fun () ->
      match saved with
      | Some value -> Unix.putenv name value
      | None -> Unix.unsetenv name)
    fn

let populate_main root =
  let file path contents = write_disk (Filename.concat root path) contents in
  file "source.txt" "line1\nline2\nline3\n";
  file "repeat.txt" "marker\nvalue\nmarker\nvalue\n";
  file "markers.txt"
    "*** End Patch\n*** End of File\n*** Update File: other.txt\n-old\n";
  file "delete.txt" "remove me\n";
  file "move.txt" "old\nkeep\n";
  file "same.txt" "primary\n";
  file "logical/cwd.txt" "logical\n";
  file "bom-crlf.txt" "\239\187\191alpha\r\nbravo\r\n";
  file "mixed-newlines.txt" "alpha\r\nbravo\n";
  file "empty.txt" "";
  file "no-final.txt" "old";
  file "eof.txt" "head\ntail\n";
  file "bad.bin" "text\000payload\n";
  file "bad-utf8.txt" "\255\254\n";
  file "oversized.txt" (String.make (Apply_patch.max_file_bytes + 1) 'o');
  file "parent-file" "not a directory\n";
  file "real_parent/child.txt" "child\n";
  file "commit-ok.txt" "old\n";
  file "readonly-parent/commit-fail.txt" "old\n";
  file ".git/config" "protected\n";
  file ".mentat/state" "protected\n";
  mkdir (Filename.concat root "dir");
  Unix.mkfifo (Filename.concat root "pipe") 0o600;
  Unix.symlink "source.txt" (Filename.concat root "link_source.txt");
  Unix.symlink "real_parent" (Filename.concat root "link_parent")

let populate_aux root =
  write_disk (Filename.concat root "same.txt") "auxiliary\n"

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
  let raw = Filename.temp_dir "mentat-apply-patch-" "" in
  Fun.protect ~finally:(fun () -> remove_tree raw) @@ fun () ->
  let base = Unix.realpath raw in
  let ws_dir = Filename.concat base "workspace" in
  let aux_dir = Filename.concat base "auxiliary" in
  let outside_dir = Filename.concat base "outside" in
  let tmp_dir = Filename.concat base "tmp" in
  List.iter mkdir [ ws_dir; aux_dir; outside_dir; tmp_dir ];
  populate_main ws_dir;
  populate_aux aux_dir;
  write_disk (Filename.concat outside_dir "secret.txt") "outside\n";
  let logical = workspace ~cwd ws_dir aux_dir in
  with_env "TMPDIR" tmp_dir @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let io = resolve_exn ~sw ~stdenv ~logical ~mode () in
  fn { ws_dir; aux_dir; outside_dir; io; tool = Apply_patch.make io }

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

let kind_name = function
  | Unix.S_REG -> "file"
  | Unix.S_DIR -> "dir"
  | Unix.S_LNK -> "symlink"
  | Unix.S_FIFO -> "fifo"
  | Unix.S_CHR -> "char"
  | Unix.S_BLK -> "block"
  | Unix.S_SOCK -> "socket"

let rec tree_snapshot root relative =
  let path =
    if String.is_empty relative then root else Filename.concat root relative
  in
  let stat = Unix.lstat path in
  let head =
    Printf.sprintf "%s|%s|%o|%d"
      (if String.is_empty relative then "." else relative)
      (kind_name stat.Unix.st_kind)
      stat.Unix.st_perm stat.Unix.st_size
  in
  match stat.Unix.st_kind with
  | Unix.S_DIR ->
      let children =
        Sys.readdir path |> Array.to_list |> List.sort String.compare
      in
      head
      :: List.concat_map
           (fun name ->
             tree_snapshot root
               (if String.is_empty relative then name
                else Filename.concat relative name))
           children
  | Unix.S_REG -> [ head ^ "|" ^ Digest.to_hex (Digest.file path) ]
  | Unix.S_LNK -> [ head ^ "|" ^ Unix.readlink path ]
  | Unix.S_FIFO | Unix.S_CHR | Unix.S_BLK | Unix.S_SOCK -> [ head ]

let decode_verdict tool label provider_input =
  match decode_call_result tool provider_input with
  | Ok call ->
      Printf.printf "%s: accepted canonical=%s\n" label
        (json_string (Tool.Call.input call))
  | Error error ->
      Printf.printf "%s: rejected %s\n" label
        (Tool.Call.Decode_error.message error)

let print_permissions call =
  let requests = Tool.Call.permissions call in
  Printf.printf "requests: %d\n" (List.length requests);
  List.iter
    (fun request ->
      Printf.printf "source: %s\n"
        (Option.value ~default:"none" (Permission.Request.source request));
      Printf.printf "items: %d normalized-accesses: %d\n"
        (List.length (Permission.Request.items request))
        (List.length (Permission.Request.normalized_accesses request));
      List.iter
        (fun item ->
          (match Permission.Request.Item.access item with
          | Permission.Access.Path
              { op; scope = Permission.Access.Path_scope.Workspace path } ->
              Printf.printf "access: %s key=%s path=%s\n" (path_op_name op)
                (Workspace.Root.Key.to_string (Workspace.Path.root_key path))
                (Workspace.Path.display path)
          | access ->
              failf "unexpected apply-patch permission: %a" Permission.Access.pp
                access);
          match Permission.Request.Item.change item with
          | None -> print_endline "change: none"
          | Some change ->
              let diff = Permission.Request.Change.diff change in
              Printf.printf
                "change: additions=%s removals=%s diff=%b leaks-secret=%b\n"
                (Option.fold ~none:"unknown" ~some:string_of_int
                   (Permission.Request.Change.additions change))
                (Option.fold ~none:"unknown" ~some:string_of_int
                   (Permission.Request.Change.removals change))
                (Option.is_some diff)
                (Option.exists
                   (fun text -> String.includes ~affix:"disk-only-secret" text)
                   diff))
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

let decode_class tool label provider_input =
  match decode_call_result tool provider_input with
  | Ok _ -> Printf.printf "%s: accepted\n" label
  | Error error ->
      Printf.printf "%s: rejected %s\n" label
        (Tool.Call.Decode_error.message error)

let%expect_test
    "declaration and provider decoding expose one strict patch field" =
  with_world @@ fun world ->
  let declaration = Tool.declaration world.tool in
  Printf.printf "name: %s\n" (Mentat_llm.Tool.name declaration);
  Printf.printf "schema: %s\n"
    (json_string (Mentat_llm.Tool.input_schema declaration));
  let valid = document [ "*** Add File: new.txt"; "+hello" ] in
  decode_verdict world.tool "valid" (input valid);
  decode_class world.tool "missing patch" (json_object []);
  decode_class world.tool "wrong type" (json_object [ ("patch", Json.int 1) ]);
  decode_class world.tool "unknown member"
    (json_object [ ("patch", Json.string valid); ("dry_run", Json.bool true) ]);
  decode_class world.tool "duplicate patch"
    (json_object [ ("patch", Json.string valid); ("patch", Json.string valid) ]);
  decode_class world.tool "empty string" (input "");
  decode_class world.tool "invalid UTF-8 patch" (input "\255");
  [%expect
    {|
    name: apply_patch
    schema: {"type":"object","properties":{"patch":{"type":"string","description":"Complete Codex-style patch text. Paths are relative to the root containing the workspace current directory.","minLength":1}},"required":["patch"],"additionalProperties":false}
    valid: accepted canonical={"patch":"*** Begin Patch\n*** Add File: new.txt\n+hello\n*** End Patch"}
    missing patch: rejected tool "apply_patch" rejected its input: Missing member patch in apply_patch input object
    wrong type: rejected tool "apply_patch" rejected its input: Expected string but found number
    in member patch of
    apply_patch input object
    unknown member: rejected tool "apply_patch" rejected its input: Unexpected member dry_run for apply_patch input object
    duplicate patch: rejected tool "apply_patch" rejected its input: duplicate input member patch
    empty string: rejected tool "apply_patch" rejected its input: patch must not be empty
    invalid UTF-8 patch: rejected tool "apply_patch" rejected its input: line 1: first line must be '*** Begin Patch' |}]

let%expect_test "malformed documents and unsafe paths fail during decoding" =
  with_world @@ fun world ->
  let cases =
    [
      ("missing begin", "not a patch");
      ("missing end", "*** Begin Patch\n*** Delete File: source.txt");
      ("empty document", "*** Begin Patch\n*** End Patch");
      ( "trailing material",
        "*** Begin Patch\n*** Delete File: source.txt\n*** End Patch\nextra" );
      ("unknown operation", document [ "*** Rename File: source.txt" ]);
      ("empty add", document [ "*** Add File: empty.txt" ]);
      ("empty update", document [ "*** Update File: source.txt" ]);
      ( "EOF-only update",
        document [ "*** Update File: source.txt"; "@@"; "*** End of File" ] );
      ("empty update line", document [ "*** Update File: source.txt"; "@@"; "" ]);
      ( "later chunk lacks header",
        document
          [
            "*** Update File: source.txt";
            "@@";
            "-line1";
            "+LINE1";
            "";
            "-line2";
            "+LINE2";
          ] );
      ("absolute path", document [ "*** Add File: /tmp/outside"; "+bad" ]);
      ("escaping path", document [ "*** Delete File: ../outside" ]);
      ( "escaping move",
        document
          [
            "*** Update File: move.txt";
            "*** Move to: ../outside";
            "@@";
            "-old";
            "+new";
          ] );
      ( "leading whitespace before patch",
        " \n*** Begin Patch\n*** Delete File: source.txt\n*** End Patch" );
      ( "trailing whitespace after patch",
        "*** Begin Patch\n*** Delete File: source.txt\n*** End Patch\n " );
    ]
  in
  List.iter
    (fun (label, patch) -> decode_class world.tool label (input patch))
    cases;
  [%expect
    {|
    missing begin: rejected tool "apply_patch" rejected its input: line 1: first line must be '*** Begin Patch'
    missing end: rejected tool "apply_patch" rejected its input: last line must be '*** End Patch'
    empty document: rejected tool "apply_patch" rejected its input: line 2: patch contains no operations
    trailing material: rejected tool "apply_patch" rejected its input: line 4: last line must be '*** End Patch'
    unknown operation: rejected tool "apply_patch" rejected its input: line 2: expected add, delete, or update file hunk
    empty add: rejected tool "apply_patch" rejected its input: line 2: add file hunk must contain at least one line
    empty update: rejected tool "apply_patch" rejected its input: line 2: update file hunk must not be empty
    EOF-only update: rejected tool "apply_patch" rejected its input: line 4: update file hunk must not be empty
    empty update line: rejected tool "apply_patch" rejected its input: line 4: unexpected line in update hunk: ""; expected space, +, or -
    later chunk lacks header: rejected tool "apply_patch" rejected its input: line 6: unexpected line in update hunk: ""; expected space, +, or -
    absolute path: rejected tool "apply_patch" rejected its input: line 2: invalid patch path "/tmp/outside": path must be relative
    escaping path: rejected tool "apply_patch" rejected its input: line 2: invalid patch path "../outside": path escapes root
    escaping move: rejected tool "apply_patch" rejected its input: line 3: invalid patch path "../outside": path escapes root
    leading whitespace before patch: rejected tool "apply_patch" rejected its input: line 1: first line must be '*** Begin Patch'
    trailing whitespace after patch: rejected tool "apply_patch" rejected its input: line 4: last line must be '*** End Patch' |}]

let%expect_test
    "permissions preserve operation evidence while policy accesses deduplicate"
    =
  with_world @@ fun world ->
  write_disk (primary world "disk-secret.txt") "disk-only-secret\n";
  let patch =
    document
      [
        "*** Add File: made.txt";
        "+one";
        "+two";
        "*** Update File: source.txt";
        "@@";
        "-line2";
        "+LINE2";
        "*** Update File: source.txt";
        "@@";
        "-LINE2";
        "+final";
        "*** Delete File: disk-secret.txt";
        "*** Update File: move.txt";
        "*** Move to: destination.txt";
        "@@";
        "-old";
        "+new";
      ]
  in
  let before = tree_snapshot world.ws_dir "" in
  let call = decode_call world.tool (input patch) in
  print_permissions call;
  let after_permissions = tree_snapshot world.ws_dir "" in
  Printf.printf "permission planning changed tree: %b\n"
    (not (List.equal String.equal before after_permissions));
  write_disk (primary world "source.txt") "changed after decode\n";
  print_permissions call;
  [%expect
    {|
    requests: 1
    source: apply_patch
    items: 6 normalized-accesses: 5
    access: create key=main path=made.txt
    change: additions=2 removals=0 diff=true leaks-secret=false
    access: modify key=main path=source.txt
    change: additions=1 removals=1 diff=true leaks-secret=false
    access: modify key=main path=source.txt
    change: additions=1 removals=1 diff=true leaks-secret=false
    access: delete key=main path=disk-secret.txt
    change: none
    access: delete key=main path=move.txt
    change: none
    access: create key=main path=destination.txt
    change: additions=1 removals=1 diff=true leaks-secret=false
    permission planning changed tree: false
    requests: 1
    source: apply_patch
    items: 6 normalized-accesses: 5
    access: create key=main path=made.txt
    change: additions=2 removals=0 diff=true leaks-secret=false
    access: modify key=main path=source.txt
    change: additions=1 removals=1 diff=true leaks-secret=false
    access: modify key=main path=source.txt
    change: additions=1 removals=1 diff=true leaks-secret=false
    access: delete key=main path=disk-secret.txt
    change: none
    access: delete key=main path=move.txt
    change: none
    access: create key=main path=destination.txt
    change: additions=1 removals=1 diff=true leaks-secret=false |}]

let%expect_test
    "one public call atomically creates updates deletes and moves text files" =
  with_world @@ fun world ->
  let patch =
    document
      [
        "*** Add File: new dir/created.txt";
        "+hello";
        "+";
        "++literal plus";
        "*** Update File: source.txt";
        "@@";
        "-line2";
        "+LINE2";
        "*** Delete File: delete.txt";
        "*** Update File: move.txt";
        "*** Move to: moved path/name.txt";
        "@@";
        "-old";
        "+new";
      ]
  in
  let scope = Wio.open_claim_scope world.io in
  run_case ~json:true world "mixed patch" (input patch);
  close_scope scope;
  print_disk world "new dir/created.txt";
  print_disk world "source.txt";
  print_disk world "delete.txt";
  print_disk world "move.txt";
  print_disk world "moved path/name.txt";
  [%expect
    {|
    -- mixed patch --
    status: completed
    text: "Success. Updated the following files:\ncreate new dir/created.txt\nmodify source.txt\ndelete delete.txt\nmove move.txt -> moved path/name.txt\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":4,"additions":5,"skipped":0}
    claim applies: 1
    claim apply 1: applied entries=5
    claim observations: 0
    disk: "hello\n\n+literal plus\n"
    disk: "line1\nLINE2\nline3\n"
    disk: <missing>
    disk: <missing>
    disk: "new\nkeep\n" |}]

let%expect_test
    "ordered chunks support context insertion deletion repetition and EOF" =
  with_world @@ fun world ->
  run_case ~json:true world "evolving repeated context"
    (input
       (document
          [
            "*** Update File: repeat.txt";
            "@@ marker";
            "-value";
            "+first";
            "@@ marker";
            "-value";
            "+second";
          ]));
  print_disk world "repeat.txt";
  run_case ~json:true world "context insertion"
    (input
       (document [ "*** Update File: repeat.txt"; "@@ first"; "+inserted" ]));
  print_disk world "repeat.txt";
  run_case ~json:true world "deletion-only chunk"
    (input (document [ "*** Update File: source.txt"; "@@"; "-line2" ]));
  print_disk world "source.txt";
  run_case ~json:true world "EOF replacement and insertion"
    (input
       (document
          [
            "*** Update File: eof.txt";
            "@@";
            "-tail";
            "+TAIL";
            "+last";
            "*** End of File";
          ]));
  print_disk world "eof.txt";
  run_case ~json:true world "contextless EOF insertion"
    (input
       (document
          [ "*** Update File: eof.txt"; "@@"; "+appended"; "*** End of File" ]));
  print_disk world "eof.txt";
  run_case ~json:true world "missing final newline stays missing"
    (input (document [ "*** Update File: no-final.txt"; "@@"; "-old"; "+new" ]));
  print_disk world "no-final.txt";
  run_case ~json:true world "empty-file insertion has no final newline"
    (input (document [ "*** Update File: empty.txt"; "@@"; "+tail" ]));
  print_disk world "empty.txt";
  run_case ~json:true world "marker-looking and grammar-prefixed content"
    (input
       (document
          [
            "*** Update File: markers.txt";
            "@@";
            " *** End Patch";
            " *** End of File";
            " *** Update File: other.txt";
            "--old";
            "+new";
          ]));
  print_disk world "markers.txt";
  [%expect
    {|
    -- evolving repeated context --
    status: completed
    text: "Success. Updated the following files:\nmodify repeat.txt\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":2,"deletions":2,"skipped":0}
    disk: "marker\nfirst\nmarker\nsecond\n"
    -- context insertion --
    status: completed
    text: "Success. Updated the following files:\nmodify repeat.txt\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":0,"skipped":0}
    disk: "marker\nfirst\ninserted\nmarker\nsecond\n"
    -- deletion-only chunk --
    status: completed
    text: "Success. Updated the following files:\nmodify source.txt\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":0,"deletions":1,"skipped":0}
    disk: "line1\nline3\n"
    -- EOF replacement and insertion --
    status: completed
    text: "Success. Updated the following files:\nmodify eof.txt\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":2,"deletions":1,"skipped":0}
    disk: "head\nTAIL\nlast\n"
    -- contextless EOF insertion --
    status: completed
    text: "Success. Updated the following files:\nmodify eof.txt\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":0,"skipped":0}
    disk: "head\nTAIL\nlast\nappended\n"
    -- missing final newline stays missing --
    status: completed
    text: "Success. Updated the following files:\nmodify no-final.txt\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
    disk: "new"
    -- empty-file insertion has no final newline --
    status: completed
    text: "Success. Updated the following files:\nmodify empty.txt\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":0,"skipped":0}
    disk: "tail"
    -- marker-looking and grammar-prefixed content --
    status: completed
    text: "Success. Updated the following files:\nmodify markers.txt\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
    disk: "*** End Patch\n*** End of File\n*** Update File: other.txt\nnew\n" |}]

let%expect_test "updates preserve BOM and unambiguous CRLF style" =
  with_world @@ fun world ->
  let crlf_patch =
    document [ "*** Update File: bom-crlf.txt"; "@@"; "-alpha"; "+ALPHA" ]
    |> String.replace_all ~sub:"\n" ~by:"\r\n"
  in
  run_case ~json:true world "BOM and CRLF" (input crlf_patch);
  print_disk world "bom-crlf.txt";
  run_case ~json:true world "mixed target selects LF"
    (input
       (document
          [ "*** Update File: mixed-newlines.txt"; "@@"; "-alpha"; "+ALPHA" ]));
  print_disk world "mixed-newlines.txt";
  let crlf_add =
    document [ "*** Add File: crlf-add.txt"; "+first"; "+second" ]
    |> String.replace_all ~sub:"\n" ~by:"\r\n"
  in
  run_case ~json:true world "CRLF patch add is parser-normalized"
    (input crlf_add);
  print_disk world "crlf-add.txt";
  [%expect
    {|
    -- BOM and CRLF --
    status: completed
    text: "Success. Updated the following files:\nmodify bom-crlf.txt\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
    disk: "\239\187\191ALPHA\r\nbravo\r\n"
    -- mixed target selects LF --
    status: completed
    text: "Success. Updated the following files:\nmodify mixed-newlines.txt\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
    disk: "ALPHA\nbravo\n"
    -- CRLF patch add is parser-normalized --
    status: completed
    text: "Success. Updated the following files:\ncreate crlf-add.txt\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":2,"deletions":0,"skipped":0}
    disk: "first\nsecond\n" |}]

let%expect_test
    "virtual planning collapses replacements repeated updates and move chains" =
  with_world @@ fun world ->
  let scope = Wio.open_claim_scope world.io in
  run_case ~json:true world "sequential plan"
    (input
       (document
          [
            "*** Delete File: delete.txt";
            "*** Add File: delete.txt";
            "+replacement";
            "*** Update File: source.txt";
            "@@";
            "-line2";
            "+LINE2";
            "*** Update File: source.txt";
            "@@";
            "-LINE2";
            "+final";
            "*** Update File: move.txt";
            "*** Move to: intermediate.txt";
            "@@";
            " old";
            "*** Update File: intermediate.txt";
            "*** Move to: chained/path.txt";
            "@@";
            " keep";
          ]));
  close_scope scope;
  print_disk world "delete.txt";
  print_disk world "source.txt";
  print_disk world "move.txt";
  print_disk world "intermediate.txt";
  print_disk world "chained/path.txt";
  [%expect
    {|
    -- sequential plan --
    status: completed
    text: "Success. Updated the following files:\nmodify delete.txt\nmodify source.txt\nmove move.txt -> chained/path.txt\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":3,"additions":3,"skipped":0}
    claim applies: 1
    claim apply 1: applied entries=4
    claim observations: 0
    disk: "replacement\n"
    disk: "line1\nfinal\nline3\n"
    disk: <missing>
    disk: <missing>
    disk: "old\nkeep\n" |}]

let%expect_test "a net-empty document performs no apply and mints no evidence" =
  with_world @@ fun world ->
  let call =
    input
      (document
         [
           "*** Add File: temporary.txt";
           "+temporary";
           "*** Delete File: temporary.txt";
         ])
  in
  let scope = Wio.open_claim_scope world.io in
  run_case ~json:true world "cancelled transition" call;
  close_scope scope;
  print_disk world "temporary.txt";
  [%expect
    {|
    -- cancelled transition --
    status: completed
    text: "Success. Updated the following files:\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":0,"additions":0,"deletions":0,"skipped":0}
    claim applies: 0
    claim observations: 0
    disk: <missing> |}]

let%expect_test
    "conflicting sequential and nested operations fail before any mutation" =
  with_world @@ fun world ->
  let before = tree_snapshot world.ws_dir "" in
  let cases =
    [
      ( "add then add",
        document
          [
            "*** Add File: duplicate.txt";
            "+one";
            "*** Add File: duplicate.txt";
            "+two";
          ] );
      ( "add occupies move output",
        document
          [
            "*** Add File: occupied.txt";
            "+occupied";
            "*** Update File: move.txt";
            "*** Move to: occupied.txt";
            "@@";
            "-old";
            "+new";
          ] );
      ( "move then update old source",
        document
          [
            "*** Update File: move.txt";
            "*** Move to: moved.txt";
            "@@";
            "-old";
            "+new";
            "*** Update File: move.txt";
            "@@";
            "-keep";
            "+kept";
          ] );
      ( "same-path move",
        document
          [
            "*** Update File: move.txt";
            "*** Move to: move.txt";
            "@@";
            "-old";
            "+new";
          ] );
      ( "nested target",
        document
          [
            "*** Add File: nested";
            "+parent";
            "*** Add File: nested/child";
            "+child";
          ] );
    ]
  in
  List.iter (fun (label, patch) -> run_case world label (input patch)) cases;
  let after = tree_snapshot world.ws_dir "" in
  Printf.printf "tree unchanged: %b\n" (List.equal String.equal before after);
  [%expect
    {|
    -- add then add --
    status: failed invalid_input
    message: duplicate.txt: operation 2 (Add) conflicts with operation 1 (Add): expected missing, found text
    metadata: false
    -- add occupies move output --
    status: failed invalid_input
    message: occupied.txt: operation 2 (Move) conflicts with operation 1 (Add): expected missing, found text
    metadata: false
    -- move then update old source --
    status: failed invalid_input
    message: move.txt: operation 2 (Update) conflicts with operation 1 (Move): expected text, found missing
    metadata: false
    -- same-path move --
    status: failed invalid_input
    message: move.txt: operation 1 (Move) has the same source and output
    metadata: false
    -- nested target --
    status: failed invalid_input
    message: nested: patch path conflicts with nested path nested/child
    metadata: false
    tree unchanged: true |}]

let%expect_test "context and target-state failures are stable and non-mutating"
    =
  with_world @@ fun world ->
  let before = tree_snapshot world.ws_dir "" in
  let cases =
    [
      ( "missing context",
        document
          [ "*** Update File: source.txt"; "@@ anchor"; "-line2"; "+LINE2" ] );
      ( "missing lines",
        document [ "*** Update File: source.txt"; "@@"; "-absent"; "+present" ]
      );
      ( "EOF mismatch",
        document
          [
            "*** Update File: eof.txt";
            "@@";
            "-head";
            "+HEAD";
            "*** End of File";
          ] );
      ( "EOF context insertion mismatch",
        document
          [
            "*** Update File: eof.txt";
            "@@ head";
            "+inserted";
            "*** End of File";
          ] );
      ( "second chunk mismatch",
        document
          [
            "*** Update File: source.txt";
            "@@";
            "-line1";
            "+LINE1";
            "@@";
            "-absent";
            "+present";
          ] );
      ( "missing update source",
        document [ "*** Update File: missing.txt"; "@@"; "-old"; "+new" ] );
      ("missing delete source", document [ "*** Delete File: missing.txt" ]);
      ( "existing add destination",
        document [ "*** Add File: source.txt"; "+replacement" ] );
      ( "existing move destination",
        document
          [
            "*** Update File: move.txt";
            "*** Move to: source.txt";
            "@@";
            "-old";
            "+new";
          ] );
    ]
  in
  List.iter (fun (label, patch) -> run_case world label (input patch)) cases;
  let after = tree_snapshot world.ws_dir "" in
  Printf.printf "tree unchanged: %b\n" (List.equal String.equal before after);
  [%expect
    {|
    -- missing context --
    status: failed invalid_input
    message: source.txt: patch chunk 0 failed: missing context "anchor"
    metadata: false
    -- missing lines --
    status: failed invalid_input
    message: source.txt: patch chunk 0 failed: missing lines: "absent"
    metadata: false
    -- EOF mismatch --
    status: failed invalid_input
    message: eof.txt: patch chunk 0 failed: missing lines at end of file: "head"
    metadata: false
    -- EOF context insertion mismatch --
    status: failed invalid_input
    message: eof.txt: patch chunk 0 failed: missing insertion point at end of file
    metadata: false
    -- second chunk mismatch --
    status: failed invalid_input
    message: source.txt: patch chunk 1 failed: missing lines: "absent"
    metadata: false
    -- missing update source --
    status: failed not_found
    message: missing.txt: path does not exist
    metadata: false
    -- missing delete source --
    status: failed not_found
    message: missing.txt: path does not exist
    metadata: false
    -- existing add destination --
    status: failed invalid_input
    message: source.txt: expected missing, found text
    metadata: false
    -- existing move destination --
    status: failed invalid_input
    message: source.txt: expected missing, found text
    metadata: false
    tree unchanged: true |}]

let%expect_test
    "invalid text sizes filesystem kinds and write paths fail without residue" =
  with_world @@ fun world ->
  let before = tree_snapshot world.ws_dir "" in
  let too_long = String.make 300 'x' in
  let cases =
    [
      ("NUL add", document [ "*** Add File: nul.txt"; "+text\000tail" ]);
      ("invalid UTF-8 add", document [ "*** Add File: invalid.txt"; "+\255" ]);
      ( "control-dense add",
        document [ "*** Add File: controls.txt"; "+\001\002\003\004text" ] );
      ( "oversized add",
        document
          [
            "*** Add File: too-big.txt";
            "+" ^ String.make Apply_patch.max_file_bytes 'x';
          ] );
      ( "oversized existing",
        document [ "*** Update File: oversized.txt"; "@@"; "-old"; "+new" ] );
      ( "binary existing",
        document [ "*** Update File: bad.bin"; "@@"; "-text"; "+TEXT" ] );
      ( "invalid UTF-8 existing",
        document [ "*** Update File: bad-utf8.txt"; "@@"; "-old"; "+new" ] );
      ("directory", document [ "*** Delete File: dir" ]);
      ("FIFO", document [ "*** Delete File: pipe" ]);
      ( "final symlink",
        document
          [ "*** Update File: link_source.txt"; "@@"; "-line2"; "+LINE2" ] );
      ( "intermediate symlink",
        document
          [ "*** Update File: link_parent/child.txt"; "@@"; "-child"; "+CHILD" ]
      );
      ( "file parent",
        document [ "*** Add File: parent-file/child.txt"; "+child" ] );
      ( "protected git",
        document
          [ "*** Update File: .git/config"; "@@"; "-protected"; "+changed" ] );
      ( "protected mentat",
        document
          [ "*** Update File: .mentat/state"; "@@"; "-protected"; "+changed" ]
      );
      ( "failed nested create",
        document [ "*** Add File: rollback/valid/" ^ too_long; "+new" ] );
    ]
  in
  List.iter (fun (label, patch) -> run_case world label (input patch)) cases;
  print_disk world "rollback";
  let after = tree_snapshot world.ws_dir "" in
  Printf.printf "pre-existing tree unchanged: %b\n"
    (List.equal String.equal before after);
  [%expect
    {|
    -- NUL add --
    status: failed invalid_input
    message: nul.txt: binary file
    metadata: false
    -- invalid UTF-8 add --
    status: failed invalid_input
    message: invalid.txt: invalid UTF-8
    metadata: false
    -- control-dense add --
    status: failed invalid_input
    message: controls.txt: binary file
    metadata: false
    -- oversized add --
    status: failed invalid_input
    message: too-big.txt: file is too large (1048577 bytes, max 1048576)
    metadata: false
    -- oversized existing --
    status: failed invalid_input
    message: oversized.txt: file is too large (1048577 bytes, max 1048576)
    metadata: false
    -- binary existing --
    status: failed invalid_input
    message: bad.bin: binary file
    metadata: false
    -- invalid UTF-8 existing --
    status: failed invalid_input
    message: bad-utf8.txt: not valid UTF-8 text
    metadata: false
    -- directory --
    status: failed invalid_input
    message: dir: expected text, found other
    metadata: false
    -- FIFO --
    status: failed invalid_input
    message: pipe: expected text, found other
    metadata: false
    -- final symlink --
    status: failed invalid_input
    message: link_source.txt: expected text, found other
    metadata: false
    -- intermediate symlink --
    status: failed invalid_input
    message: link_parent: symlink targets are not supported
    metadata: false
    -- file parent --
    status: failed invalid_input
    message: parent-file: not a directory
    metadata: false
    -- protected git --
    status: failed invalid_input
    message: .git/config: .git is protected workspace metadata and cannot be modified by tools; change sandbox and workspace policy through configuration or the CLI instead
    metadata: false
    -- protected mentat --
    status: failed invalid_input
    message: .mentat/state: .mentat is protected workspace metadata and cannot be modified by tools; change sandbox and workspace policy through configuration or the CLI instead
    metadata: false
    -- failed nested create --
    status: failed failed
    message: rollback/valid/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx: filesystem I/O error
    metadata: false
    disk: <missing>
    pre-existing tree unchanged: true |}]

let%expect_test
    "relative patch paths use the cwd root and retain root identity on \
     collision" =
  with_world ~cwd:Primary_logical @@ fun logical ->
  let logical_call =
    decode_call logical.tool
      (input
         (document
            [ "*** Update File: same.txt"; "@@"; "-primary"; "+PRIMARY" ]))
  in
  print_case "logical cwd permission";
  print_permissions logical_call;
  print_result ~json:true logical
    (Tool.Call.run logical_call ~cancelled:(fun () -> false) |> finished);
  print_disk_at "primary" (primary logical "same.txt");
  print_disk_at "logical child" (primary logical "logical/same.txt");
  print_disk_at "auxiliary" (auxiliary logical "same.txt");
  with_world ~cwd:Auxiliary_root @@ fun auxiliary_cwd ->
  let aux_call =
    decode_call auxiliary_cwd.tool
      (input
         (document
            [ "*** Update File: same.txt"; "@@"; "-auxiliary"; "+AUXILIARY" ]))
  in
  print_case "auxiliary collision permission";
  print_permissions aux_call;
  print_result auxiliary_cwd
    (Tool.Call.run aux_call ~cancelled:(fun () -> false) |> finished);
  print_disk_at "primary" (primary auxiliary_cwd "same.txt");
  print_disk_at "auxiliary" (auxiliary auxiliary_cwd "same.txt");
  [%expect
    {|
    -- logical cwd permission --
    requests: 1
    source: apply_patch
    items: 1 normalized-accesses: 1
    access: modify key=main path=same.txt
    change: additions=1 removals=1 diff=true leaks-secret=false
    status: completed
    text: "Success. Updated the following files:\nmodify same.txt\n"
    truncated: false
    json: {"version":1,"disposition":"applied","files":1,"additions":1,"deletions":1,"skipped":0}
    primary: "PRIMARY\n"
    logical child: <missing>
    auxiliary: "auxiliary\n"
    -- auxiliary collision permission --
    requests: 1
    source: apply_patch
    items: 1 normalized-accesses: 1
    access: modify key=aux path=same.txt
    change: additions=1 removals=1 diff=true leaks-secret=false
    status: failed invalid_input
    message: same.txt: edit target belongs to a read-only workspace root
    metadata: false
    primary: "primary\n"
    auxiliary: "auxiliary\n" |}]

let%expect_test "read-only mode refuses a valid plan before observation" =
  with_world ~mode:Mentat_config.Mode.Read_only @@ fun world ->
  let before = tree_snapshot world.ws_dir "" in
  let scope = Wio.open_claim_scope world.io in
  run_case world "read-only"
    (input
       (document [ "*** Update File: source.txt"; "@@"; "-line2"; "+LINE2" ]));
  close_scope scope;
  let after = tree_snapshot world.ws_dir "" in
  Printf.printf "tree unchanged: %b\n" (List.equal String.equal before after);
  [%expect
    {|
    -- read-only --
    status: failed invalid_input
    message: source.txt: edit target belongs to a read-only workspace root
    metadata: false
    claim applies: 0
    claim observations: 0
    tree unchanged: true |}]

let%expect_test
    "stale multi-path preflight applies neither source removal nor destination"
    =
  with_world @@ fun world ->
  let polls = ref 0 in
  let raced = ref false in
  (* The cancellation callback is the public run seam's deterministic
     interleaving hook. For this one move, poll ten follows both observations
     and precedes [Edit.apply]; returning [false] keeps the call live. *)
  let concurrent () =
    incr polls;
    if !polls = 10 then begin
      raced := true;
      write_disk (primary world "move.txt") "external\n"
    end;
    false
  in
  let scope = Wio.open_claim_scope world.io in
  run_case ~cancelled:concurrent world "stale move"
    (input
       (document
          [
            "*** Update File: move.txt";
            "*** Move to: raced/destination.txt";
            "@@";
            "-old";
            "+new";
          ]));
  close_scope scope;
  Printf.printf "polls: %d raced: %b\n" !polls !raced;
  print_disk world "move.txt";
  print_disk world "raced/destination.txt";
  print_disk world "raced";
  [%expect
    {|
    -- stale move --
    status: failed stale
    message: move.txt: stale write
    metadata: false
    claim applies: 0
    claim observations: 0
    polls: 10 raced: true
    disk: "external\n"
    disk: <missing>
    disk: <missing> |}]

let%expect_test
    "a create destination appearing after planning preserves state-mismatch \
     classification" =
  with_world @@ fun world ->
  let polls = ref 0 in
  let raced = ref false in
  (* For a single add, poll seven is the public checkpoint after the missing
     observation and immediately before [Edit.apply]. *)
  let concurrent () =
    incr polls;
    if !polls = 7 then begin
      raced := true;
      write_disk (primary world "raced-create.txt") "external\n"
    end;
    false
  in
  let scope = Wio.open_claim_scope world.io in
  run_case ~cancelled:concurrent world "create destination appeared"
    (input (document [ "*** Add File: raced-create.txt"; "+planned" ]));
  close_scope scope;
  Printf.printf "polls: %d raced: %b\n" !polls !raced;
  print_disk world "raced-create.txt";
  [%expect
    {|
    -- create destination appeared --
    status: failed invalid_input
    message: raced-create.txt: expected missing, found text
    metadata: false
    claim applies: 0
    claim observations: 0
    polls: 7 raced: true
    disk: "external\n" |}]

let%expect_test
    "a commit failure reports its applied prefix only through claim evidence" =
  (* The commit failure is provoked by a mode-0555 parent directory, and uid 0
     writes below it regardless of its mode bits. *)
  if Unix.geteuid () = 0 then
    skip ~reason:"the superuser writes below a mode-0555 directory" ();
  with_world @@ fun world ->
  let protected_parent = primary world "readonly-parent" in
  let scope = Wio.open_claim_scope world.io in
  Unix.chmod protected_parent 0o555;
  Fun.protect
    ~finally:(fun () -> Unix.chmod protected_parent 0o755)
    (fun () ->
      run_case world "second commit fails"
        (input
           (document
              [
                "*** Update File: commit-ok.txt";
                "@@";
                "-old";
                "+new";
                "*** Update File: readonly-parent/commit-fail.txt";
                "@@";
                "-old";
                "+new";
              ])));
  close_scope scope;
  print_disk world "commit-ok.txt";
  print_disk world "readonly-parent/commit-fail.txt";
  [%expect
    {|
    -- second commit fails --
    status: failed failed
    message: readonly-parent/commit-fail.txt: filesystem I/O error
    metadata: false
    claim applies: 1
    claim apply 1: attempted applied=1 uncertain=true
    claim observations: 0
    disk: "new\n"
    disk: "old\n" |}]

let%expect_test "cancellation during public planning checkpoints never applies"
    =
  with_world @@ fun world ->
  let immediate = Wio.open_claim_scope world.io in
  run_case
    ~cancelled:(fun () -> true)
    world "before planning"
    (input
       (document
          [ "*** Update File: link_source.txt"; "@@"; "-line2"; "+LINE2" ]));
  close_scope immediate;
  let polls = ref 0 in
  let after_planning () =
    incr polls;
    !polls = 6
  in
  let planned = Wio.open_claim_scope world.io in
  run_case ~cancelled:after_planning world "after observation"
    (input
       (document [ "*** Update File: source.txt"; "@@"; "-line2"; "+LINE2" ]));
  close_scope planned;
  Printf.printf "polls: %d\n" !polls;
  print_disk world "source.txt";
  let preflight_polls = ref 0 in
  let during_conflict_scan () =
    incr preflight_polls;
    !preflight_polls = 10
  in
  let many_adds =
    List.init 8 (fun index ->
        [ Printf.sprintf "*** Add File: cancelled-%d.txt" index; "+contents" ])
    |> List.flatten |> document |> input
  in
  let scanning = Wio.open_claim_scope world.io in
  run_case ~cancelled:during_conflict_scan world "during path-conflict scan"
    many_adds;
  close_scope scanning;
  Printf.printf "conflict scan reached cancellation: %b\n"
    (!preflight_polls >= 10);
  print_disk world "cancelled-0.txt";
  [%expect
    {|
    -- before planning --
    status: interrupted cancelled=true
    reason: tool call cancelled
    claim applies: 0
    claim observations: 0
    -- after observation --
    status: interrupted cancelled=true
    reason: tool call cancelled
    claim applies: 0
    claim observations: 0
    polls: 6
    disk: "line1\nline2\nline3\n"
    -- during path-conflict scan --
    status: interrupted cancelled=true
    reason: tool call cancelled
    claim applies: 0
    claim observations: 0
    conflict scan reached cancellation: true
    disk: <missing> |}]

let%expect_test
    "durable replay retains only D2 summaries while claim scope owns mutation" =
  with_world @@ fun world ->
  let scope = Wio.open_claim_scope world.io in
  let result =
    run world
      (input
         (document
            [
              "*** Update File: source.txt";
              "@@";
              "-line2";
              "+LINE2";
              "+companion";
              "*** Delete File: delete.txt";
            ]))
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
      "patch";
      "diff";
      "receipt";
      "mutation";
      "evidence";
      "before";
      "after";
      "created_directories";
      "path";
      "operation";
      "source_path";
    ];
  close_scope scope;
  print_disk world "source.txt";
  print_disk world "delete.txt";
  [%expect
    {|
    text: "Success. Updated the following files:\nmodify source.txt\ndelete delete.txt\n"
    json: {"version":1,"disposition":"applied","files":2,"additions":2,"skipped":0}
    durable: {"status":"completed","output":{"text":"Success. Updated the following files:\nmodify source.txt\ndelete delete.txt\n","json":{"version":1,"disposition":"applied","files":2,"additions":2,"skipped":0},"truncated":false}}
    durable equal: true
    contains contents: false
    contains patch: false
    contains diff: false
    contains receipt: false
    contains mutation: false
    contains evidence: false
    contains before: false
    contains after: false
    contains created_directories: false
    contains path: false
    contains operation: false
    contains source_path: false
    claim applies: 1
    claim apply 1: applied entries=2
    claim observations: 0
    disk: "line1\nLINE2\ncompanion\nline3\n"
    disk: <missing> |}]
