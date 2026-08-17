(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for [read_file]. Every effect
   case resolves a real [Mentat_workspace_io.t] over a fresh workspace and
   drives the erased production path:

   provider JSON -> [Mentat_tool.Call.decode] -> cached permission requests
   -> [Call.run] -> erased [Output.t] -> durable [Result.jsont Output.jsont].

   There are deliberately no mocks and no calls to the tool's typed/private
   implementation. Besides preserving the legacy contract, this catches wiring
   regressions at the two important seams: raw paths must become the exact
   workspace paths used for both permission and IO, and durable JSON must keep
   only compact semantic facts while [Output.text] remains the canonical
   presentation. *)

open Windtrap
open Test_tools_support
module Json = Jsont.Json
module Tool = Mentat_tool
module Read_file = Mentat_tools.Fs.Read_file
module Output_facts = Mentat_tools_output
module Wio = Mentat_workspace_io
module Workspace = Mentat_workspace
module Permission = Mentat_permission

type world = {
  ws_dir : string;
  io : Wio.t;
  tool : Tool.t;
  conditional_tool : Tool.t;
}

let input ?offset ?limit ?max_bytes ?if_identity path =
  let add name make value fields =
    match value with
    | None -> fields
    | Some value -> (name, make value) :: fields
  in
  [ ("path", Json.string path) ]
  |> add "offset" (fun value -> Json.int value) offset
  |> add "limit" (fun value -> Json.int value) limit
  |> add "max_bytes" (fun value -> Json.int value) max_bytes
  |> add "if_identity" (fun value -> Json.string value) if_identity
  |> List.rev |> json_object

let required_json_member name json =
  match json_member name json with
  | Some value -> value
  | None ->
      failf "missing top-level JSON member %S in %s" name (json_string json)

let json_int = function
  | Jsont.Number (value, _) when Float.is_integer value -> int_of_float value
  | json -> failf "expected a JSON integer, got %s" (json_string json)

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

let decode_call tool json =
  match Tool.Call.decode tool (model_call tool json) with
  | Ok call -> call
  | Error error ->
      failf "call decode failed: %s" (Tool.Call.Decode_error.message error)

let run ?(cancelled = fun () -> false) world json =
  Tool.Call.run (decode_call world.tool json) ~cancelled |> finished

let run_with ?(cancelled = fun () -> false) tool json =
  Tool.Call.run (decode_call tool json) ~cancelled |> finished

let run_case ?(json = false) ?cancelled world name provider_json =
  print_case name;
  print_result ~json (run ?cancelled world provider_json)

let run_case_with ?(json = false) ?cancelled tool name provider_json =
  print_case name;
  print_result ~json (run_with ?cancelled tool provider_json)

let abs path = Lpath.Abs.of_string_exn path

let write_file path contents =
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel contents)

let populate_fixture ws_dir out_dir =
  write_file
    (Filename.concat ws_dir "note.txt")
    "alpha\nbravo\ncharlie\ndelta\n";
  write_file (Filename.concat ws_dir "no-final-newline.txt") "sole line";
  write_file (Filename.concat ws_dir "empty.txt") "";
  write_file (Filename.concat ws_dir "crlf.txt") "one\r\ntwo\r\n";
  write_file (Filename.concat ws_dir "utf8-2.txt") "\195\169x\n";
  write_file (Filename.concat ws_dir "utf8-3.txt") "\226\130\172x\n";
  write_file (Filename.concat ws_dir "utf8-4.txt") "\240\159\152\128x\n";
  write_file (Filename.concat ws_dir "binary.bin") "text\000tail";
  write_file (Filename.concat ws_dir "invalid-utf8.txt") "good\n\255bad\n";
  write_file (Filename.concat ws_dir "invalid-byte-at-cap.txt") "ok\255tail";
  write_file
    (Filename.concat ws_dir "invalid-sequence-at-cap.txt")
    "ok\224\128tail";
  write_file
    (Filename.concat ws_dir "stray-continuation-at-cap.txt")
    "ok\128tail";
  write_file
    (Filename.concat ws_dir "late-invalid-utf8.txt")
    (String.make 17_000 'a' ^ "\255\n");
  write_file
    (Filename.concat ws_dir "late-nul.txt")
    (String.make 17_000 'a' ^ "\000tail\n");
  write_file
    (Filename.concat ws_dir "long-line.txt")
    (String.make 2_005 'x' ^ "\n");
  write_file
    (Filename.concat ws_dir "long-utf8-line.txt")
    (String.make 1_999 'x' ^ "\240\159\152\128\n");
  write_file
    (Filename.concat ws_dir "displayed-long-line.txt")
    "short [line truncated]\n";
  write_file (Filename.concat ws_dir "unreadable.txt") "permission denied\n";
  write_file
    (Filename.concat ws_dir "control-dense.bin")
    "\001\002\003\004plain-text";
  write_file (Filename.concat ws_dir "odd\n\".txt") "one\ntwo\nthree\n";
  let suggestion_dir = Filename.concat ws_dir "suggestions" in
  mkdir suggestion_dir;
  List.iter
    (fun name ->
      write_file (Filename.concat suggestion_dir name) "suggestion\n")
    [ "catb.txt"; "catc.txt"; "cats.txt"; "data.txt" ];
  let far = Buffer.create 440_000 in
  for line = 1 to 40_000 do
    Buffer.add_string far (Printf.sprintf "line-%05d\n" line)
  done;
  write_file (Filename.concat ws_dir "far.txt") (Buffer.contents far);
  write_file (Filename.concat out_dir "secret.txt") "outside secret\n";
  mkdir (Filename.concat out_dir "outside-dir");
  Unix.mkfifo (Filename.concat ws_dir "fifo") 0o600;
  Unix.symlink "note.txt" (Filename.concat ws_dir "link-note.txt");
  Unix.symlink
    (Filename.concat out_dir "secret.txt")
    (Filename.concat ws_dir "link-outside.txt");
  Unix.symlink "missing-target" (Filename.concat ws_dir "link-broken.txt");
  Unix.symlink "link-loop.txt" (Filename.concat ws_dir "link-loop.txt");
  let subject = Filename.concat ws_dir "subject" in
  mkdir subject;
  List.iter
    (fun name -> mkdir (Filename.concat subject name))
    [
      ".config";
      "alpha_dir";
      "beta_dir";
      ".git";
      ".svn";
      ".hg";
      ".bzr";
      ".jj";
      ".sl";
    ];
  mkdir (Filename.concat subject ".git/hooks");
  write_file (Filename.concat subject ".git/config") "repo metadata\n";
  write_file (Filename.concat subject ".env") "A=1\n";
  write_file (Filename.concat subject "alpha.txt") "alpha\n";
  write_file (Filename.concat subject "beta.txt") "beta\n";
  Unix.mkfifo (Filename.concat subject "pipe") 0o600;
  Unix.symlink "alpha_dir" (Filename.concat subject "dir_link");
  Unix.symlink "alpha.txt" (Filename.concat subject "file_link");
  mkdir (Filename.concat ws_dir "empty_dir");
  Unix.symlink "subject" (Filename.concat ws_dir "subject_link");
  Unix.symlink
    (Filename.concat out_dir "outside-dir")
    (Filename.concat ws_dir "link_outside_dir");
  let symlink_children = Filename.concat ws_dir "symlink_children" in
  mkdir symlink_children;
  Unix.symlink
    (Filename.concat out_dir "secret.txt")
    (Filename.concat symlink_children "escape_link");
  Unix.symlink "missing-target" (Filename.concat symlink_children "broken_link");
  Unix.symlink "loop_link" (Filename.concat symlink_children "loop_link");
  let big = Filename.concat ws_dir "big_dir" in
  mkdir big;
  for index = 1 to 205 do
    write_file (Filename.concat big (Printf.sprintf "f%03d.txt" index)) "x\n"
  done

let with_world fn =
  Eio_main.run @@ fun stdenv ->
  let stdenv = (stdenv :> Eio_unix.Stdenv.base) in
  let base = Unix.realpath (temp_dir ~prefix:"mentat-read-file" ()) in
  let ws_dir = Filename.concat base "workspace" in
  let out_dir = Filename.concat base "outside" in
  let tmp_dir = Filename.concat base "tmp" in
  List.iter mkdir [ ws_dir; out_dir; tmp_dir ];
  populate_fixture ws_dir out_dir;
  let primary = Workspace.Root.of_dir (abs ws_dir) in
  let logical = Workspace.single primary in
  setenv "TMPDIR" (Some tmp_dir);
  Eio.Switch.run @@ fun sw ->
  let io = resolve_exn ~sw ~stdenv ~logical () in
  let tool = Read_file.make io in
  let conditional_tool = Read_file.make ~conditional_read:true io in
  fn { ws_dir; io; tool; conditional_tool }

let relative world path = Filename.concat world.ws_dir path

let kind_name = function
  | Unix.S_REG -> "file"
  | Unix.S_DIR -> "dir"
  | Unix.S_LNK -> "symlink"
  | Unix.S_FIFO -> "fifo"
  | Unix.S_CHR -> "char"
  | Unix.S_BLK -> "block"
  | Unix.S_SOCK -> "socket"

(* Mutation evidence excludes access time: reading is allowed to update it.
   The remaining stat fields and bytes are sufficient evidence that no file,
   directory, symlink, or special entry was created, removed, or rewritten. *)
let rec tree_snapshot root rel =
  let path = if String.is_empty rel then root else Filename.concat root rel in
  let stat = Unix.lstat path in
  let head =
    Printf.sprintf "%s|%s|%o|%d|%.0f|%.0f"
      (if String.is_empty rel then "." else rel)
      (kind_name stat.Unix.st_kind)
      stat.Unix.st_perm stat.Unix.st_size stat.Unix.st_mtime stat.Unix.st_ctime
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
      let contents = In_channel.with_open_bin path In_channel.input_all in
      [ head ^ "|" ^ Digest.to_hex (Digest.string contents) ]
  | Unix.S_LNK -> [ head ^ "|" ^ Unix.readlink path ]
  | Unix.S_FIFO | Unix.S_CHR | Unix.S_BLK | Unix.S_SOCK -> [ head ]

let decode_verdict tool label json =
  match Tool.Call.decode tool (model_call tool json) with
  | Ok call ->
      Printf.printf "%s: accepted canonical=%s\n" label
        (json_string (Tool.Call.input call))
  | Error error ->
      let diagnostic = Tool.Call.Decode_error.message error in
      Printf.printf "%s: rejected diagnostic=%b\n" label
        (not (String.is_empty diagnostic))

let print_permissions call =
  match Tool.Call.permissions call with
  | [ request ] -> (
      Printf.printf "request source: %s\n"
        (Option.value ~default:"none" (Permission.Request.source request));
      match Permission.Request.items request with
      | [ item ] -> (
          Printf.printf "request display: %s\n"
            (Option.value ~default:"none" (Permission.Request.display request));
          Printf.printf "item display: %s\n"
            (Option.value ~default:"none"
               (Permission.Request.Item.display item));
          Printf.printf "item change: %b\n"
            (Option.is_some (Permission.Request.Item.change item));
          match Permission.Request.Item.access item with
          | Permission.Access.Path
              {
                op = `Read;
                scope = Permission.Access.Path_scope.Workspace path;
              } ->
              Printf.printf "access: read %s\n" (Workspace.Path.display path)
          | access ->
              failf "expected a workspace read access, got %a"
                Permission.Access.pp access)
      | items -> failf "expected one request item, got %d" (List.length items))
  | requests ->
      failf "expected one permission request, got %d" (List.length requests)

let identity_of_output output =
  let header =
    match String.split_on_char '\n' (Tool.Output.text output) with
    | header :: _ -> header
    | [] -> fail "read output has no header"
  in
  let prefix = "identity=" in
  match
    header |> String.split_on_char ' '
    |> List.find_opt (String.starts_with ~prefix)
  with
  | None -> fail "complete read output has no identity"
  | Some field ->
      String.sub field (String.length prefix)
        (String.length field - String.length prefix)

let typed_output_summary output =
  match Output_facts.Codec.decode Output_facts.Read.jsont output with
  | None -> "none"
  | Some (Output_facts.Read.File { lines }) ->
      Printf.sprintf "file lines=%d" lines
  | Some Output_facts.Read.Unchanged -> "unchanged"
  | Some Output_facts.Read.Image -> "image"
  | Some (Output_facts.Read.Listing { entries }) ->
      Printf.sprintf "listing entries=%d" entries

let replay_output output =
  output |> encode Tool.Output.jsont |> decode Tool.Output.jsont

let test_identity =
  Mentat_digest.Content_ref.to_token
    (Mentat_digest.Content_ref.of_contents "seen")

let%expect_test "compact read semantics survive durable replay" =
  with_world @@ fun world ->
  let first = run world (input "note.txt") |> output_exn in
  let identity = identity_of_output first in
  let cases =
    [
      ("file", first);
      ("listing", run world (input ~limit:4 "subject") |> output_exn);
      ( "unchanged",
        run_with world.conditional_tool (input ~if_identity:identity "note.txt")
        |> output_exn );
    ]
  in
  List.iter
    (fun (label, live) ->
      let replay = replay_output live in
      Printf.printf "%s live semantic: %s\n" label (typed_output_summary live);
      Printf.printf "%s replay semantic: %s\n" label
        (typed_output_summary replay);
      Printf.printf "%s durable output settled: %b\n" label
        ((not (String.is_empty (Tool.Output.text replay)))
        && Tool.Output.equal live replay))
    cases;
  [%expect
    {|file live semantic: file lines=4
file replay semantic: file lines=4
file durable output settled: true
listing live semantic: listing entries=4
listing replay semantic: listing entries=4
listing durable output settled: true
unchanged live semantic: unchanged
unchanged replay semantic: unchanged
unchanged durable output settled: true|}]

let%expect_test "declaration schema and provider decoding are strict" =
  with_world @@ fun world ->
  let declaration = Tool.declaration world.tool in
  Printf.printf "name: %s\n" (Mentat_llm.Tool.name declaration);
  Printf.printf "hard maxima: file_bytes=%d directory_entries=%d\n"
    Read_file.max_file_bytes Read_file.max_directory_limit;
  Printf.printf "default schema: %s\n"
    (json_string (Mentat_llm.Tool.input_schema declaration));
  Printf.printf "conditional schema: %s\n"
    (json_string
       (Mentat_llm.Tool.input_schema (Tool.declaration world.conditional_tool)));
  decode_verdict world.tool "minimal" (input "note.txt");
  decode_verdict world.tool "all fields"
    (input ~offset:2 ~limit:3 ~max_bytes:20 "note.txt");
  decode_verdict world.tool "hard maxima accepted"
    (input ~limit:Read_file.max_directory_limit
       ~max_bytes:Read_file.max_file_bytes "note.txt");
  decode_verdict world.tool "missing path" (json_object []);
  decode_verdict world.tool "unknown member"
    (json_object
       [ ("path", Json.string "note.txt"); ("extra", Json.bool true) ]);
  decode_verdict world.tool "duplicate path"
    (json_object
       [ ("path", Json.string "note.txt"); ("path", Json.string "other.txt") ]);
  decode_verdict world.tool "path wrong type"
    (json_object [ ("path", Json.int 1) ]);
  decode_verdict world.tool "empty path" (input "");
  decode_verdict world.tool "offset zero" (input ~offset:0 "note.txt");
  decode_verdict world.tool "offset negative" (input ~offset:(-1) "note.txt");
  decode_verdict world.tool "limit zero" (input ~limit:0 "note.txt");
  decode_verdict world.tool "limit negative" (input ~limit:(-1) "note.txt");
  decode_verdict world.tool "limit over hard maximum"
    (input ~limit:(Read_file.max_directory_limit + 1) "note.txt");
  decode_verdict world.tool "max_bytes negative"
    (input ~max_bytes:(-1) "note.txt");
  decode_verdict world.tool "max_bytes over hard maximum"
    (input ~max_bytes:(Read_file.max_file_bytes + 1) "note.txt");
  decode_verdict world.tool "fractional offset"
    (json_object
       [ ("path", Json.string "note.txt"); ("offset", Json.number 1.5) ]);
  decode_verdict world.tool "numeric-string offset"
    (json_object
       [ ("path", Json.string "note.txt"); ("offset", Json.string "2") ]);
  decode_verdict world.tool "offset above JSON safe integer"
    (json_object
       [
         ("path", Json.string "note.txt");
         ("offset", Json.number 9_007_199_254_740_992.);
       ]);
  decode_verdict world.tool "infinite limit"
    (json_object
       [
         ("path", Json.string "note.txt"); ("limit", Json.number Float.infinity);
       ]);
  decode_verdict world.tool "NaN max_bytes"
    (json_object
       [
         ("path", Json.string "note.txt"); ("max_bytes", Json.number Float.nan);
       ]);
  decode_verdict world.tool "default rejects valid identity"
    (input ~if_identity:test_identity "note.txt");
  decode_verdict world.conditional_tool "conditional malformed identity"
    (input ~if_identity:"not-a-content-identity" "note.txt");
  decode_verdict world.conditional_tool "conditional identity with range"
    (input ~offset:1 ~if_identity:test_identity "note.txt");
  decode_verdict world.conditional_tool "conditional identity with byte cap"
    (input ~max_bytes:1 ~if_identity:test_identity "note.txt");
  [%expect
    {|
      name: read_file
      hard maxima: file_bytes=1048576 directory_entries=1000
      default schema: {"type":"object","properties":{"path":{"type":"string","description":"Workspace-relative or workspace-contained absolute path to read."},"offset":{"type":"integer","description":"One-based first file line or directory entry. Defaults to 1.","minimum":1,"maximum":9007199254740991},"limit":{"type":"integer","description":"Maximum file lines or directory entries to return.","minimum":1,"maximum":1000},"max_bytes":{"type":"integer","description":"Maximum UTF-8 bytes returned for a file.","minimum":0,"maximum":1048576}},"required":["path"],"additionalProperties":false}
      conditional schema: {"type":"object","properties":{"path":{"type":"string","description":"Workspace-relative or workspace-contained absolute path to read."},"offset":{"type":"integer","description":"One-based first file line or directory entry. Defaults to 1.","minimum":1,"maximum":9007199254740991},"limit":{"type":"integer","description":"Maximum file lines or directory entries to return.","minimum":1,"maximum":1000},"max_bytes":{"type":"integer","description":"Maximum UTF-8 bytes returned for a file.","minimum":0,"maximum":1048576},"if_identity":{"type":"string","description":"Complete-file identity from a previous read. Valid only without offset, limit, or max_bytes."}},"required":["path"],"additionalProperties":false}
      minimal: accepted canonical={"path":"note.txt"}
      all fields: accepted canonical={"limit":3,"max_bytes":20,"offset":2,"path":"note.txt"}
      hard maxima accepted: accepted canonical={"limit":1000,"max_bytes":1048576,"offset":1,"path":"note.txt"}
      missing path: rejected diagnostic=true
      unknown member: rejected diagnostic=true
      duplicate path: rejected diagnostic=true
      path wrong type: rejected diagnostic=true
      empty path: rejected diagnostic=true
      offset zero: rejected diagnostic=true
      offset negative: rejected diagnostic=true
      limit zero: rejected diagnostic=true
      limit negative: rejected diagnostic=true
      limit over hard maximum: rejected diagnostic=true
      max_bytes negative: rejected diagnostic=true
      max_bytes over hard maximum: rejected diagnostic=true
      fractional offset: rejected diagnostic=true
      numeric-string offset: rejected diagnostic=true
      offset above JSON safe integer: rejected diagnostic=true
      infinite limit: rejected diagnostic=true
      NaN max_bytes: rejected diagnostic=true
      default rejects valid identity: rejected diagnostic=true
      conditional malformed identity: rejected diagnostic=true
      conditional identity with range: rejected diagnostic=true
      conditional identity with byte cap: rejected diagnostic=true |}]

let%expect_test "permissions derive once from the exact decoded path" =
  with_world @@ fun world ->
  print_case "relative path";
  let relative_call = decode_call world.tool (input "subject/alpha.txt") in
  print_permissions relative_call;
  print_case "workspace-contained absolute path";
  let absolute =
    decode_call world.tool (input (relative world "subject/alpha.txt"))
  in
  print_permissions absolute;
  print_case "outside path";
  let outside =
    decode_call world.tool (input "/outside-mentat-read-file-test")
  in
  Printf.printf "requests: %d\n" (List.length (Tool.Call.permissions outside));
  print_result (Tool.Call.run outside ~cancelled:(fun () -> false) |> finished);
  [%expect
    {|
      -- relative path --
      request source: read_file
      request display: none
      item display: none
      item change: false
      access: read subject/alpha.txt
      -- workspace-contained absolute path --
      request source: read_file
      request display: none
      item display: none
      item change: false
      access: read subject/alpha.txt
      -- outside path --
      requests: 0
      status: failed invalid_input
      message: path is outside workspace: /outside-mentat-read-file-test
      metadata: false |}]

let%expect_test "malformed and traversing paths acquire no read authority" =
  with_world @@ fun world ->
  List.iter
    (fun (label, path) ->
      print_case label;
      let call = decode_call world.tool (input path) in
      Printf.printf "requests: %d\n" (List.length (Tool.Call.permissions call));
      let result =
        Tool.Call.run call ~cancelled:(fun () -> false) |> finished
      in
      match Tool.Result.status result with
      | Tool.Result.Failed { kind; message; metadata } ->
          Printf.printf "status: failed %s\ndiagnostic: %b\nmetadata: %b\n"
            (failure_name kind)
            (not (String.is_empty message))
            (Option.is_some metadata)
      | Tool.Result.Completed | Tool.Result.Interrupted _ ->
          fail "malformed path did not fail")
    [ ("NUL component", "bad\000name"); ("parent traversal", "../secret.txt") ];
  [%expect
    {|
      -- NUL component --
      requests: 0
      status: failed invalid_input
      diagnostic: true
      metadata: false
      -- parent traversal --
      requests: 0
      status: failed invalid_input
      diagnostic: true
      metadata: false |}]

let%expect_test "complete reads cover text edge cases without mutation" =
  with_world @@ fun world ->
  let before = tree_snapshot world.ws_dir "" in
  let scope = Wio.open_claim_scope world.io in
  run_case ~json:true world "ordinary LF text" (input "note.txt");
  run_case ~json:true world "empty file" (input "empty.txt");
  run_case ~json:true world "no final newline" (input "no-final-newline.txt");
  run_case ~json:true world "CRLF display and durable bytes" (input "crlf.txt");
  let crlf_identity =
    identity_of_output (output_exn (run world (input "crlf.txt")))
  in
  let expected_crlf_identity =
    Mentat_digest.Content_ref.to_token
      (Mentat_digest.Content_ref.of_contents "one\r\ntwo\r\n")
  in
  Printf.printf "CRLF identity uses raw bytes: %b\n"
    (String.equal expected_crlf_identity crlf_identity);
  let evidence = Wio.Claim_scope.close scope in
  let after = tree_snapshot world.ws_dir "" in
  Printf.printf "tree unchanged: %b\n" (List.equal String.equal before after);
  Printf.printf "mutation applies: %d\n"
    (List.length evidence.Mentat_edit.Apply_evidence.applies);
  Printf.printf "attributed opaque observations: %d\n"
    (List.length evidence.Mentat_edit.Apply_evidence.observed);
  [%expect
    {|-- ordinary LF text --
status: completed
text: "note.txt lines=1-4 returned=4/4 status=complete identity=sha256:833940e53452e86ad3cf12deb4054606301b43cec7607677dab4625777c7cee3:26\n1\talpha\n2\tbravo\n3\tcharlie\n4\tdelta\n"
truncated: false
json: {"version":1,"shape":"file","count":4}
-- empty file --
status: completed
text: "empty.txt lines=empty at 1 (past EOF; file has 0 lines) returned=0/0 status=complete identity=sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855:0\n"
truncated: false
json: {"version":1,"shape":"file","count":0}
-- no final newline --
status: completed
text: "no-final-newline.txt lines=1-1 returned=1/1 status=complete identity=sha256:c65f95ea759c79081ff523898b1f29c05e532470abdef1ad612bf524c1bdf948:9\n1\tsole line\n"
truncated: false
json: {"version":1,"shape":"file","count":1}
-- CRLF display and durable bytes --
status: completed
text: "crlf.txt lines=1-2 returned=2/2 status=complete identity=sha256:6f4792b265fe72790b344fd3ef5294701d9d087bed9fce815c0f4bbad6d2ed87:10\n1\tone\n2\ttwo\n"
truncated: false
json: {"version":1,"shape":"file","count":2}
CRLF identity uses raw bytes: true
tree unchanged: true
mutation applies: 0
attributed opaque observations: 0|}]

let%expect_test "conditional reads distinguish unchanged from stale" =
  with_world @@ fun world ->
  let first = run world (input "note.txt") in
  let identity = identity_of_output (output_exn first) in
  run_case_with ~json:true world.conditional_tool "matching identity"
    (input ~if_identity:identity "note.txt");
  write_file (relative world "note.txt") "new contents\n";
  run_case_with ~json:true world.conditional_tool
    "stale identity returns current contents"
    (input ~if_identity:identity "note.txt");
  run_case_with world.conditional_tool "identity is invalid for a directory"
    (input ~if_identity:identity "subject");
  [%expect
    {|-- matching identity --
status: completed
text: "note.txt unchanged identity=sha256:833940e53452e86ad3cf12deb4054606301b43cec7607677dab4625777c7cee3:26\n"
truncated: false
json: {"version":1,"shape":"unchanged"}
-- stale identity returns current contents --
status: completed
text: "note.txt lines=1-1 returned=1/1 status=complete identity=sha256:128ad17d8fa462647ec5c88f46dc706f05bae1625adca9e00bbd2ff97e02d279:13\n1\tnew contents\n"
truncated: false
json: {"version":1,"shape":"file","count":1}
-- identity is invalid for a directory --
status: failed invalid_input
message: subject: if_identity cannot be used with a directory
metadata: false|}]

let%expect_test "line ranges paginate without gaps or re-read loops" =
  with_world @@ fun world ->
  run_case ~json:true world "middle page" (input ~offset:2 ~limit:2 "note.txt");
  run_case world "generated next preserves explicit byte cap"
    (input ~offset:1 ~limit:2 ~max_bytes:20 "note.txt");
  let next_json = input ~offset:4 ~limit:2 "note.txt" in
  let next_call = decode_call world.tool next_json in
  Printf.printf "decoded next: %s\n" (json_string (Tool.Call.input next_call));
  print_result (Tool.Call.run next_call ~cancelled:(fun () -> false) |> finished);
  run_case ~json:true world "range wider than file"
    (input ~offset:1 ~limit:99 "note.txt");
  run_case ~json:true world "range exactly covers file"
    (input ~offset:1 ~limit:4 "note.txt");
  run_case ~json:true world "offset past EOF"
    (input ~offset:99 ~limit:2 "note.txt");
  run_case ~json:true world "limit without offset" (input ~limit:2 "note.txt");
  run_case ~json:true world "offset through EOF" (input ~offset:3 "note.txt");
  run_case ~json:true world "range and byte cap"
    (input ~offset:2 ~limit:2 ~max_bytes:7 "note.txt");
  run_case world "far range in a file larger than the default output budget"
    (input ~offset:39_999 ~limit:2 "far.txt");
  run_case world "continuation escapes a path with JSON metacharacters"
    (input ~offset:1 ~limit:1 "odd\n\".txt");
  let odd_next = input ~offset:2 ~limit:1 "odd\n\".txt" in
  Printf.printf "special-path next: %s\n" (json_string odd_next);
  print_result (run world odd_next);
  [%expect
    {|-- middle page --
status: completed
text: "note.txt lines=2-3 returned=2/>=3 status=partial reason=ranged\n2\tbravo\n3\tcharlie\nnext: read_file {\"path\":\"note.txt\",\"offset\":4,\"limit\":2}\n"
truncated: false
json: {"version":1,"shape":"file","count":2}
-- generated next preserves explicit byte cap --
status: completed
text: "note.txt lines=1-2 returned=2/>=2 status=partial reason=ranged\n1\talpha\n2\tbravo\nnext: read_file {\"path\":\"note.txt\",\"offset\":3,\"limit\":2,\"max_bytes\":20}\n"
truncated: false
decoded next: {"limit":2,"offset":4,"path":"note.txt"}
status: completed
text: "note.txt lines=4-4 returned=1/4 status=partial reason=ranged\n4\tdelta\n"
truncated: false
-- range wider than file --
status: completed
text: "note.txt lines=1-4 returned=4/4 status=complete identity=sha256:833940e53452e86ad3cf12deb4054606301b43cec7607677dab4625777c7cee3:26\n1\talpha\n2\tbravo\n3\tcharlie\n4\tdelta\n"
truncated: false
json: {"version":1,"shape":"file","count":4}
-- range exactly covers file --
status: completed
text: "note.txt lines=1-4 returned=4/4 status=complete identity=sha256:833940e53452e86ad3cf12deb4054606301b43cec7607677dab4625777c7cee3:26\n1\talpha\n2\tbravo\n3\tcharlie\n4\tdelta\n"
truncated: false
json: {"version":1,"shape":"file","count":4}
-- offset past EOF --
status: completed
text: "note.txt lines=empty at 99 (past EOF; file has 4 lines) returned=0/4 status=partial reason=ranged\n"
truncated: false
json: {"version":1,"shape":"file","count":0}
-- limit without offset --
status: completed
text: "note.txt lines=1-2 returned=2/>=2 status=partial reason=ranged\n1\talpha\n2\tbravo\nnext: read_file {\"path\":\"note.txt\",\"offset\":3,\"limit\":2}\n"
truncated: false
json: {"version":1,"shape":"file","count":2}
-- offset through EOF --
status: completed
text: "note.txt lines=3-4 returned=2/4 status=partial reason=ranged\n3\tcharlie\n4\tdelta\n"
truncated: false
json: {"version":1,"shape":"file","count":2}
-- range and byte cap --
status: completed
text: "note.txt lines=2-3 returned=2/>=3 status=partial reason=ranged_and_byte_capped\n2\tbravo\n3\tc\n"
truncated: true
json: {"version":1,"shape":"file","count":2}
-- far range in a file larger than the default output budget --
status: completed
text: "far.txt lines=39999-40000 returned=2/40000 status=partial reason=ranged\n39999\tline-39999\n40000\tline-40000\n"
truncated: false
-- continuation escapes a path with JSON metacharacters --
status: completed
text: "odd\n\".txt lines=1-1 returned=1/>=1 status=partial reason=ranged\n1\tone\nnext: read_file {\"path\":\"odd\\n\\\".txt\",\"offset\":2,\"limit\":1}\n"
truncated: false
special-path next: {"path":"odd\n\".txt","offset":2,"limit":1}
status: completed
text: "odd\n\".txt lines=2-2 returned=1/>=2 status=partial reason=ranged\n2\ttwo\nnext: read_file {\"path\":\"odd\\n\\\".txt\",\"offset\":3,\"limit\":1}\n"
truncated: false|}]

let%expect_test "byte caps repair only incomplete UTF-8 suffixes" =
  with_world @@ fun world ->
  List.iter
    (fun (file, caps) ->
      List.iter
        (fun cap ->
          run_case world
            (Printf.sprintf "%s max_bytes=%d" file cap)
            (input ~max_bytes:cap file))
        caps)
    [
      ("utf8-2.txt", [ 0; 1; 2; 3; 4 ]);
      ("utf8-3.txt", [ 1; 2; 3; 4; 5 ]);
      ("utf8-4.txt", [ 1; 2; 3; 4; 5; 6 ]);
    ];
  [%expect
    {|
      -- utf8-2.txt max_bytes=0 --
      status: completed
      text: "utf8-2.txt lines=empty at 1 returned=0/>=1 status=partial reason=byte_capped\n"
      truncated: true
      -- utf8-2.txt max_bytes=1 --
      status: completed
      text: "utf8-2.txt lines=empty at 1 returned=0/>=1 status=partial reason=byte_capped\n"
      truncated: true
      -- utf8-2.txt max_bytes=2 --
      status: completed
      text: "utf8-2.txt lines=1-1 returned=1/>=1 status=partial reason=byte_capped\n1\t\195\169\n"
      truncated: true
      -- utf8-2.txt max_bytes=3 --
      status: completed
      text: "utf8-2.txt lines=1-1 returned=1/>=1 status=partial reason=byte_capped\n1\t\195\169x\n"
      truncated: true
      -- utf8-2.txt max_bytes=4 --
      status: completed
      text: "utf8-2.txt lines=1-1 returned=1/1 status=complete identity=sha256:1f284e412e503818e2938201afee2306d7d6df2d499294dd4b686e549b86c87a:4\n1\t\195\169x\n"
      truncated: false
      -- utf8-3.txt max_bytes=1 --
      status: completed
      text: "utf8-3.txt lines=empty at 1 returned=0/>=1 status=partial reason=byte_capped\n"
      truncated: true
      -- utf8-3.txt max_bytes=2 --
      status: completed
      text: "utf8-3.txt lines=empty at 1 returned=0/>=1 status=partial reason=byte_capped\n"
      truncated: true
      -- utf8-3.txt max_bytes=3 --
      status: completed
      text: "utf8-3.txt lines=1-1 returned=1/>=1 status=partial reason=byte_capped\n1\t\226\130\172\n"
      truncated: true
      -- utf8-3.txt max_bytes=4 --
      status: completed
      text: "utf8-3.txt lines=1-1 returned=1/>=1 status=partial reason=byte_capped\n1\t\226\130\172x\n"
      truncated: true
      -- utf8-3.txt max_bytes=5 --
      status: completed
      text: "utf8-3.txt lines=1-1 returned=1/1 status=complete identity=sha256:7187a056a447242ee351823d269a0d6d408641b0ad3241dfebeb686d91043b44:5\n1\t\226\130\172x\n"
      truncated: false
      -- utf8-4.txt max_bytes=1 --
      status: completed
      text: "utf8-4.txt lines=empty at 1 returned=0/>=1 status=partial reason=byte_capped\n"
      truncated: true
      -- utf8-4.txt max_bytes=2 --
      status: completed
      text: "utf8-4.txt lines=empty at 1 returned=0/>=1 status=partial reason=byte_capped\n"
      truncated: true
      -- utf8-4.txt max_bytes=3 --
      status: completed
      text: "utf8-4.txt lines=empty at 1 returned=0/>=1 status=partial reason=byte_capped\n"
      truncated: true
      -- utf8-4.txt max_bytes=4 --
      status: completed
      text: "utf8-4.txt lines=1-1 returned=1/>=1 status=partial reason=byte_capped\n1\t\240\159\152\128\n"
      truncated: true
      -- utf8-4.txt max_bytes=5 --
      status: completed
      text: "utf8-4.txt lines=1-1 returned=1/>=1 status=partial reason=byte_capped\n1\t\240\159\152\128x\n"
      truncated: true
      -- utf8-4.txt max_bytes=6 --
      status: completed
      text: "utf8-4.txt lines=1-1 returned=1/1 status=complete identity=sha256:60c8918bb3137dbc7c528a1bda35b4ffc24e83350c618a762df0e3b795d5bb3c:6\n1\t\240\159\152\128x\n"
      truncated: false |}]

let%expect_test "byte caps do not hide malformed UTF-8 suffixes" =
  with_world @@ fun world ->
  run_case world "invalid leading byte"
    (input ~max_bytes:3 "invalid-byte-at-cap.txt");
  run_case world "invalid constrained continuation"
    (input ~max_bytes:4 "invalid-sequence-at-cap.txt");
  run_case world "stray continuation byte"
    (input ~max_bytes:3 "stray-continuation-at-cap.txt");
  [%expect
    {|
      -- invalid leading byte --
      status: failed invalid_input
      message: invalid-byte-at-cap.txt: not valid UTF-8 text
      metadata: false
      -- invalid constrained continuation --
      status: failed invalid_input
      message: invalid-sequence-at-cap.txt: not valid UTF-8 text
      metadata: false
      -- stray continuation byte --
      status: failed invalid_input
      message: stray-continuation-at-cap.txt: not valid UTF-8 text
      metadata: false |}]

let%expect_test "display truncation never changes the durable file bytes" =
  with_world @@ fun world ->
  let result = run world (input "long-line.txt") in
  let output = output_exn result in
  let expected_identity =
    Mentat_digest.Content_ref.of_contents (String.make 2_005 'x' ^ "\n")
    |> Mentat_digest.Content_ref.to_token
  in
  Printf.printf "identity includes undisplayed bytes: %b\n"
    (String.equal expected_identity (identity_of_output output));
  let lines = String.split_on_char '\n' (Tool.Output.text output) in
  let displayed =
    match lines with
    | header :: body :: rest ->
        Printf.printf "header has complete status: %b\n"
          (String.includes ~affix:"status=complete" header);
        if List.is_empty rest then fail "long-line rendering has no terminator";
        body
    | _ -> fail "long-line rendering has no body"
  in
  Printf.printf "displayed body bytes: %d\n" (String.length displayed);
  Printf.printf "display sentinel: %b\n"
    (String.ends_with ~suffix:" [line truncated]" displayed);
  Printf.printf "output truncated for display omission: %b\n"
    (Tool.Output.truncated output);
  let json, names =
    match Tool.Output.json output with
    | Some json -> (json, json_member_names json)
    | None -> fail "long-line output has no JSON frame"
  in
  Printf.printf "versioned JSON: %b\n"
    (Option.is_some (json_member "version" json));
  Printf.printf "frame: %s\n"
    (json_string
       (json_object
          [
            ("shape", required_json_member "shape" json);
            ("count", required_json_member "count" json);
          ]));
  Printf.printf "durable JSON duplicates contents: %b\n"
    (List.mem "contents" names);
  let literal = output_exn (run world (input "displayed-long-line.txt")) in
  let literal_lines = String.split_on_char '\n' (Tool.Output.text literal) in
  let literal_body =
    match literal_lines with
    | header :: body :: rest ->
        if String.is_empty header || List.is_empty rest then
          fail "literal-suffix rendering is malformed";
        body
    | _ -> fail "literal-suffix rendering has no body"
  in
  Printf.printf "literal suffix preserved: %b\n"
    (String.ends_with ~suffix:" [line truncated]" literal_body);
  Printf.printf "literal suffix marked truncated: %b\n"
    (Tool.Output.truncated literal);
  let utf8 = output_exn (run world (input "long-utf8-line.txt")) in
  Printf.printf "multibyte boundary remains valid UTF-8: %b\n"
    (String.is_valid_utf_8 (Tool.Output.text utf8));
  Printf.printf "multibyte boundary is marked truncated: %b\n"
    (Tool.Output.truncated utf8);
  [%expect
    {|identity includes undisplayed bytes: true
header has complete status: true
displayed body bytes: 2019
display sentinel: true
output truncated for display omission: true
versioned JSON: true
frame: {"shape":"file","count":1}
durable JSON duplicates contents: false
literal suffix preserved: true
literal suffix marked truncated: false
multibyte boundary remains valid UTF-8: true
multibyte boundary is marked truncated: true|}]

let%expect_test "binary invalid UTF-8 and special files fail structurally" =
  with_world @@ fun world ->
  run_case world "NUL-bearing binary" (input "binary.bin");
  run_case world "control-density binary without NUL"
    (input "control-dense.bin");
  run_case world "invalid UTF-8" (input "invalid-utf8.txt");
  run_case world "invalid UTF-8 after the first read chunk"
    (input "late-invalid-utf8.txt");
  print_case "binary heuristic samples only the first chunk";
  let late_nul = run world (input "late-nul.txt") in
  (match Tool.Result.status late_nul with
  | Tool.Result.Completed -> print_endline "status: completed"
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf "status: failed %s message=%s metadata=%b\n"
        (failure_name kind) message (Option.is_some metadata)
  | Tool.Result.Interrupted { reason; cancelled } ->
      Printf.printf "status: interrupted reason=%s cancelled=%b\n" reason
        cancelled);
  let late_nul_output = output_exn late_nul in
  Printf.printf "model text omits late NUL behind display cap: %b\n"
    (not (String.contains (Tool.Output.text late_nul_output) '\000'));
  Printf.printf "truncated: %b\n" (Tool.Output.truncated late_nul_output);
  run_case world "FIFO" (input "fifo");
  [%expect
    {|
      -- NUL-bearing binary --
      status: failed invalid_input
      message: binary.bin: binary file
      metadata: false
      -- control-density binary without NUL --
      status: failed invalid_input
      message: control-dense.bin: binary file
      metadata: false
      -- invalid UTF-8 --
      status: failed invalid_input
      message: invalid-utf8.txt: not valid UTF-8 text
      metadata: false
      -- invalid UTF-8 after the first read chunk --
      status: failed invalid_input
      message: late-invalid-utf8.txt: not valid UTF-8 text
      metadata: false
      -- binary heuristic samples only the first chunk --
      status: completed
      model text omits late NUL behind display cap: true
      truncated: true
      -- FIFO --
      status: failed invalid_input
      message: fifo: not a readable file or directory
      metadata: false |}]

(* The unreadable-file classification stands apart from the structural
   failures above because its precondition is a mode bit the kernel must
   honour, and uid 0 reads a mode-000 file regardless. *)
let%expect_test "an unreadable regular file fails as filesystem I/O" =
  if Unix.geteuid () = 0 then
    skip ~reason:"the superuser reads a mode-000 fixture file" ();
  with_world @@ fun world ->
  let unreadable = relative world "unreadable.txt" in
  Unix.chmod unreadable 0o000;
  Fun.protect
    ~finally:(fun () -> Unix.chmod unreadable 0o600)
    (fun () ->
      run_case world "unreadable regular file" (input "unreadable.txt"));
  [%expect
    {|
      -- unreadable regular file --
      status: failed failed
      message: unreadable.txt: filesystem I/O error
      metadata: false |}]

let%expect_test "directory listing preserves kinds filtering and order" =
  with_world @@ fun world ->
  run_case ~json:true world "sorted entries with dotfiles and VCS filtering"
    (input "subject");
  run_case ~json:true world "explicit VCS directory remains readable"
    (input "subject/.git");
  run_case ~json:true world "empty directory" (input "empty_dir");
  run_case ~json:true world
    "child symlinks are classified without following broken or escaping targets"
    (input "symlink_children");
  run_case world "max_bytes does not page a directory"
    (input ~max_bytes:1 "subject");
  [%expect
    {|-- sorted entries with dotfiles and VCS filtering --
status: completed
text: "subject entries=9/9 offset=1 limit=200 status=complete\nsubject/.config/\nsubject/alpha_dir/\nsubject/beta_dir/\nsubject/.env\nsubject/alpha.txt\nsubject/beta.txt\nsubject/dir_link@\nsubject/file_link@\nsubject/pipe?\n"
truncated: false
json: {"version":1,"shape":"listing","count":9}
-- explicit VCS directory remains readable --
status: completed
text: "subject/.git entries=2/2 offset=1 limit=200 status=complete\nsubject/.git/hooks/\nsubject/.git/config\n"
truncated: false
json: {"version":1,"shape":"listing","count":2}
-- empty directory --
status: completed
text: "empty_dir entries=0/0 offset=1 limit=200 status=complete\n"
truncated: false
json: {"version":1,"shape":"listing","count":0}
-- child symlinks are classified without following broken or escaping targets --
status: completed
text: "symlink_children entries=3/3 offset=1 limit=200 status=complete\nsymlink_children/broken_link@\nsymlink_children/escape_link@\nsymlink_children/loop_link@\n"
truncated: false
json: {"version":1,"shape":"listing","count":3}
-- max_bytes does not page a directory --
status: completed
text: "subject entries=9/9 offset=1 limit=200 status=complete\nsubject/.config/\nsubject/alpha_dir/\nsubject/beta_dir/\nsubject/.env\nsubject/alpha.txt\nsubject/beta.txt\nsubject/dir_link@\nsubject/file_link@\nsubject/pipe?\n"
truncated: false|}]

let%expect_test "directory pages have progress-making continuations" =
  with_world @@ fun world ->
  run_case ~json:true world "first page" (input ~limit:4 "subject");
  run_case ~json:true world "final page" (input ~offset:7 ~limit:4 "subject");
  run_case ~json:true world "past end" (input ~offset:99 ~limit:4 "subject");
  let result = run world (input "big_dir") in
  let output = output_exn result in
  Printf.printf
    "-- default 200-entry budget --\ntext lines: %d\ntruncated: %b\n"
    (List.length (String.split_on_char '\n' (Tool.Output.text output)) - 1)
    (Tool.Output.truncated output);
  (match Tool.Output.json output with
  | None -> fail "directory output has no JSON"
  | Some json -> Printf.printf "json: %s\n" (json_string json));
  [%expect
    {|-- first page --
status: completed
text: "subject entries=4/9 offset=1 limit=4 status=partial\nsubject/.config/\nsubject/alpha_dir/\nsubject/beta_dir/\nsubject/.env\nnext: read_file {\"path\":\"subject\",\"offset\":5,\"limit\":4}\n"
truncated: true
json: {"version":1,"shape":"listing","count":4}
-- final page --
status: completed
text: "subject entries=3/9 offset=7 limit=4 status=complete\nsubject/dir_link@\nsubject/file_link@\nsubject/pipe?\n"
truncated: false
json: {"version":1,"shape":"listing","count":3}
-- past end --
status: completed
text: "subject entries=0/9 offset=99 limit=4 status=complete\n"
truncated: false
json: {"version":1,"shape":"listing","count":0}
-- default 200-entry budget --
text lines: 202
truncated: true
json: {"version":1,"shape":"listing","count":200}|}]

let returned_file_bytes output =
  match String.split_on_char '\n' (Tool.Output.text output) with
  | [] -> fail "file output has no header"
  | _header :: rendered_lines ->
      List.fold_left
        (fun bytes line ->
          if String.is_empty line then bytes
          else
            match String.index_opt line '\t' with
            | Some separator -> bytes + (String.length line - separator - 1) + 1
            | None -> failf "unexpected rendered file line: %S" line)
        0 rendered_lines

let%expect_test "hard output budgets cap files and directory pages" =
  with_world @@ fun world ->
  Printf.printf "defaults within maxima: %b\n"
    (Read_file.default_max_bytes <= Read_file.max_file_bytes
    && Read_file.default_directory_limit <= Read_file.max_directory_limit);
  let line_bytes = 1_024 in
  let content_line = String.make (line_bytes - 1) 'x' ^ "\n" in
  let selected_lines = Read_file.max_file_bytes / line_bytes in
  let oversized_contents =
    String.concat "" (List.init (selected_lines + 1) (fun _ -> content_line))
  in
  write_file (relative world "hard-cap.txt") oversized_contents;
  let file_result =
    run world (input ~max_bytes:Read_file.max_file_bytes "hard-cap.txt")
  in
  let file_output = output_exn file_result in
  Printf.printf "file result completed: %b\n"
    (match Tool.Result.status file_result with
    | Tool.Result.Completed -> true
    | Tool.Result.Failed _ | Tool.Result.Interrupted _ -> false);
  Printf.printf "file source bytes: %d\n" (String.length oversized_contents);
  Printf.printf "file returned bytes: %d/%d\n"
    (returned_file_bytes file_output)
    Read_file.max_file_bytes;
  Printf.printf "file truncated: %b\n" (Tool.Output.truncated file_output);
  let file_next =
    input ~offset:3 ~limit:2 ~max_bytes:Read_file.max_file_bytes "note.txt"
  in
  Printf.printf "file next: %s\n" (json_string file_next);
  Printf.printf "file next advances and preserves cap: %b\n"
    (json_int (required_json_member "offset" file_next) = 3
    && json_int (required_json_member "max_bytes" file_next)
       = Read_file.max_file_bytes);
  let file_next_output = output_exn (run world file_next) in
  Printf.printf "file next starts at line 3: %b\n"
    (String.starts_with ~prefix:"note.txt lines=3-4"
       (Tool.Output.text file_next_output));
  let directory = relative world "hard-cap-dir" in
  mkdir directory;
  for index = 1 to Read_file.max_directory_limit + 1 do
    write_file
      (Filename.concat directory (Printf.sprintf "f%04d.txt" index))
      "x\n"
  done;
  let listing_output =
    output_exn
      (run world (input ~limit:Read_file.max_directory_limit "hard-cap-dir"))
  in
  let returned_entries =
    match Output_facts.Codec.decode Output_facts.Read.jsont listing_output with
    | Some (Output_facts.Read.Listing { entries }) -> entries
    | Some
        ( Output_facts.Read.File _ | Output_facts.Read.Image
        | Output_facts.Read.Unchanged )
    | None ->
        fail "hard-cap directory returned no listing fact"
  in
  Printf.printf "directory returned entries: %d/%d\n" returned_entries
    Read_file.max_directory_limit;
  Printf.printf "directory continuation preserves hard limit: %b\n"
    (String.includes
       ~affix:
         (Printf.sprintf
            "next: read_file \
             {\"path\":\"hard-cap-dir\",\"offset\":1001,\"limit\":%d}"
            Read_file.max_directory_limit)
       (Tool.Output.text listing_output));
  let directory_next =
    input
      ~offset:(Read_file.max_directory_limit + 1)
      ~limit:Read_file.max_directory_limit "hard-cap-dir"
  in
  Printf.printf "directory next: %s\n" (json_string directory_next);
  Printf.printf "directory next advances and preserves cap: %b\n"
    (json_int (required_json_member "offset" directory_next)
     = Read_file.max_directory_limit + 1
    && json_int (required_json_member "limit" directory_next)
       = Read_file.max_directory_limit);
  let final_listing = output_exn (run world directory_next) in
  let final_entries =
    match Output_facts.Codec.decode Output_facts.Read.jsont final_listing with
    | Some (Output_facts.Read.Listing { entries }) -> entries
    | Some
        ( Output_facts.Read.File _ | Output_facts.Read.Image
        | Output_facts.Read.Unchanged )
    | None ->
        fail "final hard-cap page returned no listing fact"
  in
  Printf.printf "directory final page: entries=%d complete=%s\n" final_entries
    (if
       String.includes ~affix:"status=complete" (Tool.Output.text final_listing)
     then "complete"
     else "partial");
  [%expect
    {|defaults within maxima: true
file result completed: true
file source bytes: 1049600
file returned bytes: 1048576/1048576
file truncated: true
file next: {"path":"note.txt","offset":3,"limit":2,"max_bytes":1048576}
file next advances and preserves cap: true
file next starts at line 3: true
directory returned entries: 1000/1000
directory continuation preserves hard limit: true
directory next: {"path":"hard-cap-dir","offset":1001,"limit":1000}
directory next advances and preserves cap: true
directory final page: entries=1 complete=complete|}]

let%expect_test "symlinks follow only while contained and report child kind" =
  with_world @@ fun world ->
  run_case world "in-workspace file symlink" (input "link-note.txt");
  run_case world "escaping file symlink" (input "link-outside.txt");
  run_case world "broken file symlink" (input "link-broken.txt");
  run_case world "symlink loop" (input "link-loop.txt");
  run_case world "in-workspace directory symlink" (input "subject_link");
  run_case world "escaping directory symlink" (input "link_outside_dir");
  [%expect
    {|
      -- in-workspace file symlink --
      status: completed
      text: "link-note.txt lines=1-4 returned=4/4 status=complete identity=sha256:833940e53452e86ad3cf12deb4054606301b43cec7607677dab4625777c7cee3:26\n1\talpha\n2\tbravo\n3\tcharlie\n4\tdelta\n"
      truncated: false
      -- escaping file symlink --
      status: failed invalid_input
      message: link-outside.txt: path resolves outside workspace
      metadata: false
      -- broken file symlink --
      status: failed not_found
      message: link-broken.txt: path does not exist
      metadata: false
      -- symlink loop --
      status: failed failed
      message: link-loop.txt: filesystem I/O error
      metadata: false
      -- in-workspace directory symlink --
      status: completed
      text: "subject_link entries=9/9 offset=1 limit=200 status=complete\nsubject_link/.config/\nsubject_link/alpha_dir/\nsubject_link/beta_dir/\nsubject_link/.env\nsubject_link/alpha.txt\nsubject_link/beta.txt\nsubject_link/dir_link@\nsubject_link/file_link@\nsubject_link/pipe?\n"
      truncated: false
      -- escaping directory symlink --
      status: failed invalid_input
      message: link_outside_dir: path resolves outside workspace
      metadata: false |}]

let%expect_test "not-found diagnostics suggest siblings deterministically" =
  with_world @@ fun world ->
  run_case world "missing without close sibling" (input "missing.txt");
  run_case world "misspelling with close sibling" (input "nate.txt");
  run_case world "missing directory sibling" (input "subjectt");
  run_case world
    "equal-distance suggestions are name-ordered and capped at three"
    (input "suggestions/cata.txt");
  [%expect
    {|
      -- missing without close sibling --
      status: failed not_found
      message: missing.txt: path does not exist
      metadata: false
      -- misspelling with close sibling --
      status: failed not_found
      message: nate.txt: path does not exist. Did you mean: note.txt?
      metadata: false
      -- missing directory sibling --
      status: failed not_found
      message: subjectt: path does not exist. Did you mean: subject?
      metadata: false
      -- equal-distance suggestions are name-ordered and capped at three --
      status: failed not_found
      message: suggestions/cata.txt: path does not exist. Did you mean: suggestions/catb.txt, suggestions/catc.txt, suggestions/cats.txt?
      metadata: false |}]

let%expect_test "cooperative cancellation is observable before and during IO" =
  with_world @@ fun world ->
  run_case world "cancelled before IO"
    ~cancelled:(fun () -> true)
    (input "note.txt");
  let file_polls = ref 0 in
  let cancel_file () =
    incr file_polls;
    !file_polls > 2
  in
  run_case world "cancelled during a file read" ~cancelled:cancel_file
    (input "far.txt");
  Printf.printf "file polled during work: %b\n" (!file_polls > 2);
  let directory_polls = ref 0 in
  let cancel_directory () =
    incr directory_polls;
    !directory_polls > 2
  in
  run_case world "cancelled during a directory listing"
    ~cancelled:cancel_directory (input "big_dir");
  Printf.printf "directory polled during work: %b\n" (!directory_polls > 2);
  [%expect
    {|
      -- cancelled before IO --
      status: interrupted cancelled=true
      reason: tool call cancelled
      -- cancelled during a file read --
      status: interrupted cancelled=true
      reason: tool call cancelled
      file polled during work: true
      -- cancelled during a directory listing --
      status: interrupted cancelled=true
      reason: tool call cancelled
      directory polled during work: true |}]

let%expect_test "durable replay preserves presentation and carries no receipt" =
  with_world @@ fun world ->
  List.iter
    (fun (label, provider_json) ->
      print_case label;
      let live = run world provider_json in
      let stored = encode result_codec live in
      let replay = decode result_codec stored in
      Printf.printf "durable result equal: %b\n"
        (Tool.Result.equal Tool.Output.equal live replay);
      let live_output = output_exn live in
      let replay_output = output_exn replay in
      Printf.printf "text equal: %b\njson equal: %b\ntruncated equal: %b\n"
        (String.equal
           (Tool.Output.text live_output)
           (Tool.Output.text replay_output))
        (Option.equal Json.equal
           (Tool.Output.json live_output)
           (Tool.Output.json replay_output))
        (Bool.equal
           (Tool.Output.truncated live_output)
           (Tool.Output.truncated replay_output));
      let forbidden =
        [
          "contents";
          "fingerprint";
          "anchors";
          "receipt";
          "mutation";
          "before";
          "after";
          "diff";
        ]
      in
      let names =
        match Tool.Output.json replay_output with
        | None -> []
        | Some json -> json_member_names json
      in
      let replay_json =
        match Tool.Output.json replay_output with
        | Some json -> json
        | None -> fail "rich read_file output has no durable JSON"
      in
      Printf.printf "versioned: %b\nsemantic shape: %b\n"
        (Option.is_some (json_member "version" replay_json))
        (Option.is_some (json_member "shape" replay_json));
      Printf.printf "retained value absent at top level: %b\n"
        (Option.is_none (json_member "value" replay_json));
      Printf.printf "non-presentation fields absent: %b\nanchors absent: %b\n"
        (List.for_all (fun field -> not (List.mem field names)) forbidden)
        (not
           (String.includes ~affix:"anchors=" (Tool.Output.text replay_output))))
    [
      ("complete file", input "note.txt");
      ("partial file", input ~offset:2 ~limit:2 "note.txt");
      ("directory page", input ~limit:4 "subject");
    ];
  [%expect
    {|-- complete file --
durable result equal: true
text equal: true
json equal: true
truncated equal: true
versioned: true
semantic shape: true
retained value absent at top level: true
non-presentation fields absent: true
anchors absent: true
-- partial file --
durable result equal: true
text equal: true
json equal: true
truncated equal: true
versioned: true
semantic shape: true
retained value absent at top level: true
non-presentation fields absent: true
anchors absent: true
-- directory page --
durable result equal: true
text equal: true
json equal: true
truncated equal: true
versioned: true
semantic shape: true
retained value absent at top level: true
non-presentation fields absent: true
anchors absent: true|}]
