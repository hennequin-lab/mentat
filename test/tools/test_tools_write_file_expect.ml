(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for [write_file]. Each case
   resolves a real workspace capability and drives provider JSON through
   [Call.decode], cached permissions, [Call.run], and durable result replay.
   The suite never calls typed/private tool helpers. Disk checks and claim-scope
   evidence jointly prove that the native edit boundary, rather than model
   output, owns mutation truth. *)

open Windtrap
open Test_tools_support
module Json = Jsont.Json
module Tool = Mentat_tool
module Write_file = Mentat_tools.Fs.Write_file
module Wio = Mentat_workspace_io
module Workspace = Mentat_workspace
module Permission = Mentat_permission

type world = { ws_dir : string; io : Wio.t; tool : Tool.t }

let input ?if_identity path contents =
  let fields =
    [ ("path", Json.string path); ("contents", Json.string contents) ]
  in
  let fields =
    match if_identity with
    | None -> fields
    | Some identity -> fields @ [ ("if_identity", Json.string identity) ]
  in
  json_object fields

let identity contents =
  Mentat_digest.Content_ref.to_token
    (Mentat_digest.Content_ref.of_contents contents)

let print_result ?(json = false) result =
  (match Tool.Result.status result with
  | Tool.Result.Completed -> print_endline "status: completed"
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf "status: failed %s\nmessage: %s\nmetadata: %b\n"
        (failure_name kind) message (Option.is_some metadata)
  | Tool.Result.Interrupted { reason; cancelled } ->
      Printf.printf "status: interrupted cancelled=%b\nreason: %s\n" cancelled
        reason);
  match Tool.Result.output result with
  | None -> ()
  | Some output ->
      Printf.printf "text: %S\ntruncated: %b\n" (Tool.Output.text output)
        (Tool.Output.truncated output);
      if json then
        Printf.printf "json: %s\n"
          (match Tool.Output.json output with
          | None -> "none"
          | Some value -> json_string value)

let decode_call tool provider_input =
  match Tool.Call.decode tool (model_call tool provider_input) with
  | Ok call -> call
  | Error error ->
      failf "call decode failed: %s" (Tool.Call.Decode_error.message error)

let run ?(cancelled = fun () -> false) world provider_input =
  Tool.Call.run (decode_call world.tool provider_input) ~cancelled |> finished

let run_case ?(json = false) ?cancelled world name provider_input =
  print_case name;
  print_result ~json (run ?cancelled world provider_input)

let abs path = Lpath.Abs.of_string_exn path

let write_disk path contents =
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

let populate_fixture ws_dir out_dir =
  write_disk (Filename.concat ws_dir "note.txt") "alpha\n";
  write_disk (Filename.concat ws_dir "bom.txt") "\239\187\191alpha\n";
  write_disk (Filename.concat ws_dir "bad.bin") "text\000payload\n";
  write_disk (Filename.concat ws_dir "bad-utf8.txt") "\255\254\n";
  write_disk (Filename.concat ws_dir "parent-file") "not a directory\n";
  write_disk
    (Filename.concat ws_dir "oversized.txt")
    (String.make (Write_file.max_file_bytes + 1) 'o');
  write_disk (Filename.concat out_dir "secret.txt") "outside\n";
  let dir = Filename.concat ws_dir "dir" in
  let real_parent = Filename.concat ws_dir "real_parent" in
  let git = Filename.concat ws_dir ".git" in
  let mentat = Filename.concat ws_dir ".mentat" in
  List.iter mkdir [ dir; real_parent; git; mentat ];
  write_disk (Filename.concat real_parent "child.txt") "alpha\n";
  write_disk (Filename.concat git "config") "protected\n";
  write_disk (Filename.concat mentat "state") "protected\n";
  Unix.mkfifo (Filename.concat ws_dir "pipe") 0o600;
  Unix.symlink "note.txt" (Filename.concat ws_dir "link_note.txt");
  Unix.symlink "real_parent" (Filename.concat ws_dir "link_parent")

let with_world ?(mode = Mentat_config.Mode.Danger_full_access) fn =
  Eio_main.run @@ fun stdenv ->
  let stdenv = (stdenv :> Eio_unix.Stdenv.base) in
  let raw = Filename.temp_dir "mentat-write-file-" "" in
  Fun.protect ~finally:(fun () -> remove_tree raw) @@ fun () ->
  let base = Unix.realpath raw in
  let ws_dir = Filename.concat base "workspace" in
  let out_dir = Filename.concat base "outside" in
  let tmp_dir = Filename.concat base "tmp" in
  List.iter mkdir [ ws_dir; out_dir; tmp_dir ];
  populate_fixture ws_dir out_dir;
  let logical = Workspace.single (Workspace.Root.of_dir (abs ws_dir)) in
  with_env "TMPDIR" tmp_dir @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let io = resolve_exn ~sw ~stdenv ~logical ~mode () in
  fn { ws_dir; io; tool = Write_file.make io }

let relative world path = Filename.concat world.ws_dir path

let print_disk world path =
  let disk_path = relative world path in
  match Unix.lstat disk_path with
  | { Unix.st_kind = Unix.S_REG; st_size; _ } ->
      if st_size <= 200 then Printf.printf "disk: %S\n" (read_disk disk_path)
      else Printf.printf "disk: regular bytes=%d\n" st_size
  | { Unix.st_kind = Unix.S_DIR; _ } -> print_endline "disk: <directory>"
  | { Unix.st_kind = Unix.S_LNK; _ } ->
      Printf.printf "disk: symlink->%s\n" (Unix.readlink disk_path)
  | _ -> print_endline "disk: <special>"
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      print_endline "disk: <missing>"

let print_mode world path =
  let mode = (Unix.stat (relative world path)).Unix.st_perm land 0o777 in
  Printf.printf "mode: %03o\n" mode

let kind_name = function
  | Unix.S_REG -> "file"
  | Unix.S_DIR -> "dir"
  | Unix.S_LNK -> "symlink"
  | Unix.S_FIFO -> "fifo"
  | Unix.S_CHR -> "char"
  | Unix.S_BLK -> "block"
  | Unix.S_SOCK -> "socket"

let rec tree_snapshot root rel =
  let path = if String.is_empty rel then root else Filename.concat root rel in
  let stat = Unix.lstat path in
  let head =
    Printf.sprintf "%s|%s|%o|%d"
      (if String.is_empty rel then "." else rel)
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
               (if String.is_empty rel then name else Filename.concat rel name))
           children
  | Unix.S_REG ->
      let contents = read_disk path in
      [ head ^ "|" ^ Digest.to_hex (Digest.string contents) ]
  | Unix.S_LNK -> [ head ^ "|" ^ Unix.readlink path ]
  | Unix.S_FIFO | Unix.S_CHR | Unix.S_BLK | Unix.S_SOCK -> [ head ]

let decode_verdict tool label provider_input =
  match Tool.Call.decode tool (model_call tool provider_input) with
  | Ok call ->
      Printf.printf "%s: accepted canonical=%s\n" label
        (json_string (Tool.Call.input call))
  | Error error ->
      Printf.printf "%s: rejected diagnostic=%b\n" label
        (not (String.is_empty (Tool.Call.Decode_error.message error)))

let decode_only tool label provider_input =
  match Tool.Call.decode tool (model_call tool provider_input) with
  | Ok _ -> Printf.printf "%s: accepted\n" label
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
          let change = Permission.Request.Item.change item in
          (match Permission.Request.Item.access item with
          | Permission.Access.Path
              { op; scope = Permission.Access.Path_scope.Workspace path } ->
              Printf.printf "access: %s %s\n" (path_op_name op)
                (Workspace.Path.display path)
          | access ->
              failf "unexpected write permission: %a" Permission.Access.pp
                access);
          match change with
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

let%expect_test "declaration schema and provider decoding are strict" =
  with_world @@ fun world ->
  let declaration = Tool.declaration world.tool in
  Printf.printf "name: %s\n" (Mentat_llm.Tool.name declaration);
  Printf.printf "schema: %s\n"
    (json_string (Mentat_llm.Tool.input_schema declaration));
  decode_verdict world.tool "create" (input "new.txt" "hello\n");
  decode_verdict world.tool "identity"
    (input ~if_identity:(identity "alpha\n") "note.txt" "bravo\n");
  decode_verdict world.tool "missing path"
    (json_object [ ("contents", Json.string "hello\n") ]);
  decode_verdict world.tool "missing contents"
    (json_object [ ("path", Json.string "new.txt") ]);
  decode_verdict world.tool "unknown member"
    (json_object
       [
         ("path", Json.string "new.txt");
         ("contents", Json.string "hello\n");
         ("extra", Json.bool true);
       ]);
  decode_verdict world.tool "duplicate path"
    (json_object
       [
         ("path", Json.string "first.txt");
         ("path", Json.string "second.txt");
         ("contents", Json.string "hello\n");
       ]);
  decode_verdict world.tool "duplicate contents"
    (json_object
       [
         ("path", Json.string "new.txt");
         ("contents", Json.string "first\n");
         ("contents", Json.string "second\n");
       ]);
  decode_verdict world.tool "duplicate identity"
    (json_object
       [
         ("path", Json.string "note.txt");
         ("contents", Json.string "hello\n");
         ("if_identity", Json.string (identity "alpha\n"));
         ("if_identity", Json.string (identity "bravo\n"));
       ]);
  decode_verdict world.tool "wrong path type"
    (json_object [ ("path", Json.int 1); ("contents", Json.string "hello\n") ]);
  decode_verdict world.tool "wrong contents type"
    (json_object
       [ ("path", Json.string "new.txt"); ("contents", Json.bool true) ]);
  decode_verdict world.tool "wrong identity type"
    (json_object
       [
         ("path", Json.string "new.txt");
         ("contents", Json.string "hello\n");
         ("if_identity", Json.int 1);
       ]);
  decode_verdict world.tool "empty path" (input "" "hello\n");
  decode_verdict world.tool "malformed identity"
    (input ~if_identity:"not-an-identity" "note.txt" "hello\n");
  decode_verdict world.tool "invalid UTF-8" (input "new.txt" "\255bad");
  decode_verdict world.tool "NUL binary" (input "new.txt" "hello\000tail");
  decode_verdict world.tool "control-dense binary"
    (input "new.txt" "\001\002\003\004text");
  decode_verdict world.tool "empty contents" (input "empty.txt" "");
  decode_only world.tool "oversized contents decode"
    (input "too-big.txt" (String.make (Write_file.max_file_bytes + 1) 'x'));
  [%expect
    {|
    name: write_file
    schema: {"type":"object","properties":{"path":{"type":"string","description":"Workspace-relative or workspace-contained absolute file path to write.","minLength":1},"contents":{"type":"string","description":"Complete UTF-8 file contents to write."},"if_identity":{"type":"string","description":"Complete-file identity from a previous complete read. Supply it to overwrite an existing file; if the target is missing, the file is created."}},"required":["path","contents"],"additionalProperties":false}
    create: accepted canonical={"contents":"hello\n","path":"new.txt"}
    identity: accepted canonical={"contents":"bravo\n","if_identity":"sha256:b6a98d9ce9a2d9149288fa3df42d377c3e42737afdcdaf714e33c0a100b51060:6","path":"note.txt"}
    missing path: rejected diagnostic=true
    missing contents: rejected diagnostic=true
    unknown member: rejected diagnostic=true
    duplicate path: rejected diagnostic=true
    duplicate contents: rejected diagnostic=true
    duplicate identity: rejected diagnostic=true
    wrong path type: rejected diagnostic=true
    wrong contents type: rejected diagnostic=true
    wrong identity type: rejected diagnostic=true
    empty path: rejected diagnostic=true
    malformed identity: rejected diagnostic=true
    invalid UTF-8: rejected diagnostic=true
    NUL binary: rejected diagnostic=true
    control-dense binary: rejected diagnostic=true
    empty contents: accepted canonical={"contents":"","path":"empty.txt"}
    oversized contents decode: accepted |}]

let%expect_test
    "permissions are input-only and identity atomically requests create and \
     modify" =
  with_world @@ fun world ->
  let before = tree_snapshot world.ws_dir "" in
  print_case "missing precondition";
  print_permissions (decode_call world.tool (input "new.txt" "one\ntwo\n"));
  print_case "identity precondition";
  print_permissions
    (decode_call world.tool
       (input ~if_identity:(identity "alpha\n") "note.txt" "one\ntwo\n"));
  print_case "identity and missing target has same authority";
  print_permissions
    (decode_call world.tool
       (input ~if_identity:(identity "unseen") "missing.txt" "one\ntwo\n"));
  print_case "outside path";
  let outside = decode_call world.tool (input "/outside-write" "new\n") in
  print_permissions outside;
  print_result (Tool.Call.run outside ~cancelled:(fun () -> false) |> finished);
  let after = tree_snapshot world.ws_dir "" in
  Printf.printf "permission planning changed tree: %b\n"
    (not (List.equal String.equal before after));
  [%expect
    {|
    -- missing precondition --
    requests: 1
    source: write_file
    access: create new.txt
    change: additions=2 removals=0 diff=true
    -- identity precondition --
    requests: 1
    source: write_file
    access: create note.txt
    change: additions=2 removals=0 diff=true
    access: modify note.txt
    change: additions=2 removals=unknown diff=true
    -- identity and missing target has same authority --
    requests: 1
    source: write_file
    access: create missing.txt
    change: additions=2 removals=0 diff=true
    access: modify missing.txt
    change: additions=2 removals=unknown diff=true
    -- outside path --
    requests: 0
    status: failed invalid_input
    message: path is outside workspace: /outside-write
    metadata: false
    permission planning changed tree: false |}]

let%expect_test "writes nested absolute empty and maximum-sized files" =
  with_world @@ fun world ->
  let scope = Wio.open_claim_scope world.io in
  run_case ~json:true world "nested create"
    (input "new/dir/file.txt" "hello\nworld\n");
  close_scope scope;
  print_disk world "new/dir/file.txt";
  run_case ~json:true world "empty create" (input "empty.txt" "");
  print_disk world "empty.txt";
  let absolute = relative world "absolute.txt" in
  run_case world "contained absolute create" (input absolute "absolute");
  print_disk world "absolute.txt";
  run_case world "exact maximum input"
    (input "maximum.txt" (String.make Write_file.max_file_bytes 'm'));
  print_disk world "maximum.txt";
  run_case world "exact maximum after BOM preservation"
    (input
       ~if_identity:(identity "\239\187\191alpha\n")
       "bom.txt"
       (String.make (Write_file.max_file_bytes - 3) 'b'));
  print_disk world "bom.txt";
  [%expect
    {|-- nested create --
status: completed
text: "create: new/dir/file.txt added=2 removed=0\n"
truncated: false
json: {"version":1,"shape":"wrote","lines":2}
claim applies: 1
claim observations: 0
disk: "hello\nworld\n"
-- empty create --
status: completed
text: "create: empty.txt added=0 removed=0\n"
truncated: false
json: {"version":1,"shape":"wrote","lines":0}
disk: ""
-- contained absolute create --
status: completed
text: "create: absolute.txt added=1 removed=0\n"
truncated: false
disk: "absolute"
-- exact maximum input --
status: completed
text: "create: maximum.txt added=1 removed=0\n"
truncated: false
disk: regular bytes=1048576
-- exact maximum after BOM preservation --
status: completed
text: "modify: bom.txt added=1 removed=unknown\n"
truncated: false
disk: regular bytes=1048576|}]

let%expect_test
    "identity on a missing target creates and records authoritative evidence" =
  with_world @@ fun world ->
  let scope = Wio.open_claim_scope world.io in
  run_case ~json:true world "missing with identity"
    (input ~if_identity:(identity "previously seen") "missing.txt" "new\n");
  close_scope scope;
  print_disk world "missing.txt";
  [%expect
    {|-- missing with identity --
status: completed
text: "create: missing.txt added=1 removed=0\n"
truncated: false
json: {"version":1,"shape":"wrote","lines":1}
claim applies: 1
claim observations: 0
disk: "new\n"|}]

let%expect_test "fresh unchanged and stale replacement preserve no-op semantics"
    =
  with_world @@ fun world ->
  let first = Wio.open_claim_scope world.io in
  run_case ~json:true world "fresh replacement"
    (input ~if_identity:(identity "alpha\n") "note.txt" "bravo!\n");
  close_scope first;
  print_disk world "note.txt";
  let unchanged = Wio.open_claim_scope world.io in
  run_case ~json:true world "unchanged replacement"
    (input ~if_identity:(identity "bravo!\n") "note.txt" "bravo!\n");
  close_scope unchanged;
  print_disk world "note.txt";
  write_disk (relative world "note.txt") "external\n";
  let stale = Wio.open_claim_scope world.io in
  run_case world "stale replacement"
    (input ~if_identity:(identity "bravo!\n") "note.txt" "agent\n");
  close_scope stale;
  print_disk world "note.txt";
  [%expect
    {|-- fresh replacement --
status: completed
text: "modify: note.txt added=1 removed=unknown\n"
truncated: false
json: {"version":1,"shape":"wrote","lines":1}
claim applies: 1
claim observations: 0
disk: "bravo!\n"
-- unchanged replacement --
status: completed
text: "unchanged: note.txt added=0 removed=0\n"
truncated: false
json: {"version":1,"shape":"unchanged"}
claim applies: 0
claim observations: 0
disk: "bravo!\n"
-- stale replacement --
status: failed stale
message: note.txt: stale file identity
metadata: false
claim applies: 0
claim observations: 0
disk: "external\n"|}]

let%expect_test "replacement preserves UTF-8 BOM and executable mode" =
  with_world @@ fun world ->
  run_case ~json:true world "BOM unchanged"
    (input ~if_identity:(identity "\239\187\191alpha\n") "bom.txt" "alpha\n");
  print_disk world "bom.txt";
  run_case ~json:true world "BOM"
    (input ~if_identity:(identity "\239\187\191alpha\n") "bom.txt" "bravo\n");
  print_disk world "bom.txt";
  Unix.chmod (relative world "note.txt") 0o755;
  run_case world "mode"
    (input ~if_identity:(identity "alpha\n") "note.txt" "#!/bin/sh\necho ok\n");
  print_mode world "note.txt";
  [%expect
    {|-- BOM unchanged --
status: completed
text: "unchanged: bom.txt added=0 removed=0\n"
truncated: false
json: {"version":1,"shape":"unchanged"}
disk: "\239\187\191alpha\n"
-- BOM --
status: completed
text: "modify: bom.txt added=1 removed=unknown\n"
truncated: false
json: {"version":1,"shape":"wrote","lines":1}
disk: "\239\187\191bravo\n"
-- mode --
status: completed
text: "modify: note.txt added=2 removed=unknown\n"
truncated: false
mode: 755|}]

let%expect_test
    "replacement breaks hard-link aliases instead of mutating another path" =
  with_world @@ fun world ->
  let outside =
    Filename.concat (Filename.dirname world.ws_dir) "outside/secret.txt"
  in
  let hard_link = relative world "hard-link.txt" in
  Unix.link outside hard_link;
  let before = (Unix.stat hard_link).Unix.st_ino in
  let scope = Wio.open_claim_scope world.io in
  run_case ~json:true world "hard-link replacement"
    (input ~if_identity:(identity "outside\n") "hard-link.txt" "inside\n");
  close_scope scope;
  let after = (Unix.stat hard_link).Unix.st_ino in
  print_disk world "hard-link.txt";
  Printf.printf "outside disk: %S\n" (read_disk outside);
  Printf.printf "workspace inode replaced: %b\n" (before <> after);
  [%expect
    {|-- hard-link replacement --
status: completed
text: "modify: hard-link.txt added=1 removed=unknown\n"
truncated: false
json: {"version":1,"shape":"wrote","lines":1}
claim applies: 1
claim observations: 0
disk: "inside\n"
outside disk: "outside\n"
workspace inode replaced: true|}]

let%expect_test
    "final and intermediate symlinks reveal no same-versus-changed oracle" =
  with_world @@ fun world ->
  let cases =
    [
      ("final same", "link_note.txt", "alpha\n");
      ("final changed", "link_note.txt", "changed\n");
      ("intermediate same", "link_parent/child.txt", "alpha\n");
      ("intermediate changed", "link_parent/child.txt", "changed\n");
    ]
  in
  List.iter
    (fun (label, path, contents) ->
      run_case world label
        (input ~if_identity:(identity "alpha\n") path contents))
    cases;
  print_disk world "note.txt";
  print_disk world "real_parent/child.txt";
  [%expect
    {|
    -- final same --
    status: failed invalid_input
    message: link_note.txt: expected text, found other
    metadata: false
    -- final changed --
    status: failed invalid_input
    message: link_note.txt: expected text, found other
    metadata: false
    -- intermediate same --
    status: failed invalid_input
    message: link_parent: symlink targets are not supported
    metadata: false
    -- intermediate changed --
    status: failed invalid_input
    message: link_parent: symlink targets are not supported
    metadata: false
    disk: "alpha\n"
    disk: "alpha\n" |}]

let%expect_test
    "oversized input existing oversized and non-text targets fail before \
     mutation" =
  with_world @@ fun world ->
  let before = tree_snapshot world.ws_dir "" in
  run_case world "oversized input precedes target observation"
    (input ~if_identity:(identity "alpha\n") "link_note.txt"
       (String.make (Write_file.max_file_bytes + 1) 'x'));
  let bom_scope = Wio.open_claim_scope world.io in
  run_case world "preserved BOM exceeds final size bound"
    (input
       ~if_identity:(identity "\239\187\191alpha\n")
       "bom.txt"
       (String.make Write_file.max_file_bytes 'x'));
  close_scope bom_scope;
  print_disk world "bom.txt";
  run_case world "existing oversized target"
    (input ~if_identity:(identity "anything") "oversized.txt" "new\n");
  run_case world "binary target matching identity"
    (input ~if_identity:(identity "text\000payload\n") "bad.bin" "new\n");
  run_case world "binary target different identity"
    (input ~if_identity:(identity "anything") "bad.bin" "new\n");
  run_case world "invalid UTF-8 target matching identity"
    (input ~if_identity:(identity "\255\254\n") "bad-utf8.txt" "new\n");
  run_case world "invalid UTF-8 target different identity"
    (input ~if_identity:(identity "anything") "bad-utf8.txt" "new\n");
  run_case world "special target"
    (input ~if_identity:(identity "anything") "pipe" "new\n");
  let after = tree_snapshot world.ws_dir "" in
  Printf.printf "tree unchanged: %b\n" (List.equal String.equal before after);
  [%expect
    {|
    -- oversized input precedes target observation --
    status: failed invalid_input
    message: link_note.txt: file is too large (1048577 bytes, max 1048576)
    metadata: false
    -- preserved BOM exceeds final size bound --
    status: failed invalid_input
    message: bom.txt: file is too large (1048579 bytes, max 1048576)
    metadata: false
    claim applies: 0
    claim observations: 0
    disk: "\239\187\191alpha\n"
    -- existing oversized target --
    status: failed invalid_input
    message: oversized.txt: file is too large (1048577 bytes, max 1048576)
    metadata: false
    -- binary target matching identity --
    status: failed invalid_input
    message: bad.bin: binary file
    metadata: false
    -- binary target different identity --
    status: failed invalid_input
    message: bad.bin: binary file
    metadata: false
    -- invalid UTF-8 target matching identity --
    status: failed invalid_input
    message: bad-utf8.txt: not valid UTF-8 text
    metadata: false
    -- invalid UTF-8 target different identity --
    status: failed invalid_input
    message: bad-utf8.txt: not valid UTF-8 text
    metadata: false
    -- special target --
    status: failed invalid_input
    message: pipe: expected text, found other
    metadata: false
    tree unchanged: true |}]

let%expect_test "unsafe protected and read-only targets fail without residue" =
  with_world (fun world ->
      let before = tree_snapshot world.ws_dir "" in
      run_case world "create over existing file" (input "note.txt" "new\n");
      run_case world "directory target" (input "dir" "new\n");
      run_case world "file parent" (input "parent-file/child.txt" "new\n");
      run_case world "outside workspace" (input "/outside-write-root" "new\n");
      run_case world "protected git" (input ".git/new" "new\n");
      run_case world "protected mentat" (input ".mentat/new" "new\n");
      let too_long = String.make 300 'x' in
      run_case world "failed create rolls back parents"
        (input ("rollback/" ^ too_long) "new\n");
      let after = tree_snapshot world.ws_dir "" in
      Printf.printf "pre-existing tree unchanged: %b\n"
        (List.equal String.equal before after);
      print_disk world "rollback");
  with_world ~mode:Mentat_config.Mode.Read_only (fun read_only ->
      run_case read_only "read-only workspace" (input "new.txt" "new\n");
      print_disk read_only "new.txt");
  [%expect
    {|
    -- create over existing file --
    status: failed invalid_input
    message: note.txt: expected missing, found text
    metadata: false
    -- directory target --
    status: failed invalid_input
    message: dir: expected missing, found other
    metadata: false
    -- file parent --
    status: failed invalid_input
    message: parent-file: not a directory
    metadata: false
    -- outside workspace --
    status: failed invalid_input
    message: path is outside workspace: /outside-write-root
    metadata: false
    -- protected git --
    status: failed invalid_input
    message: .git/new: .git is protected workspace metadata and cannot be modified by tools; change sandbox and workspace policy through configuration or the CLI instead
    metadata: false
    -- protected mentat --
    status: failed invalid_input
    message: .mentat/new: .mentat is protected workspace metadata and cannot be modified by tools; change sandbox and workspace policy through configuration or the CLI instead
    metadata: false
    -- failed create rolls back parents --
    status: failed failed
    message: rollback/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx: filesystem I/O error
    metadata: false
    pre-existing tree unchanged: true
    disk: <missing>
    -- read-only workspace --
    status: failed invalid_input
    message: new.txt: edit target belongs to a read-only workspace root
    metadata: false
    disk: <missing> |}]

let%expect_test
    "cancellation refuses before observation and leaves no claim evidence" =
  with_world @@ fun world ->
  let before = tree_snapshot world.ws_dir "" in
  let scope = Wio.open_claim_scope world.io in
  run_case
    ~cancelled:(fun () -> true)
    world "cancelled"
    (input ~if_identity:(identity "alpha\n") "link_note.txt" "changed\n");
  close_scope scope;
  let after = tree_snapshot world.ws_dir "" in
  Printf.printf "tree unchanged: %b\n" (List.equal String.equal before after);
  print_disk world "note.txt";
  [%expect
    {|
    -- cancelled --
    status: interrupted cancelled=true
    reason: tool call cancelled
    claim applies: 0
    claim observations: 0
    tree unchanged: true
    disk: "alpha\n" |}]

let%expect_test "durable replay keeps only the presentation summary" =
  with_world @@ fun world ->
  let scope = Wio.open_claim_scope world.io in
  let result = run world (input "durable.txt" "one\r\ntwo") in
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
      "identity";
      "receipt";
      "mutation";
      "evidence";
      "before";
      "after";
    ];
  close_scope scope;
  print_disk world "durable.txt";
  [%expect
    {|text: "create: durable.txt added=2 removed=0\n"
json: {"version":1,"shape":"wrote","lines":2}
durable: {"status":"completed","output":{"text":"create: durable.txt added=2 removed=0\n","json":{"version":1,"shape":"wrote","lines":2},"truncated":false}}
text replay equal: true
json replay equal: true
truncated replay equal: true
contains contents: false
contains identity: false
contains receipt: false
contains mutation: false
contains evidence: false
contains before: false
contains after: false
claim applies: 1
claim observations: 0
disk: "one\r\ntwo"|}]
