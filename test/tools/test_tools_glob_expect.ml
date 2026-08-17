(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for [glob]. Every scenario
   resolves a real workspace capability and drives provider JSON through
   [Call.decode], cached permissions, [Call.run], erased output, and durable
   replay. Matcher and walker internals remain deliberately inaccessible. *)

open Windtrap
open Test_tools_support
module Json = Jsont.Json
module Tool = Mentat_tool
module Glob = Mentat_tools.Fs.Glob
module Wio = Mentat_workspace_io
module Workspace = Mentat_workspace
module Permission = Mentat_permission

type cwd = Primary_root | Primary_logical | Auxiliary_root
type world = { ws_dir : string; aux_dir : string; io : Wio.t; tool : Tool.t }

let add_optional name encode value fields =
  match value with
  | None -> fields
  | Some value -> (name, encode value) :: fields

let input ?path ?offset ?limit ?sort pattern =
  [ ("pattern", Json.string pattern) ]
  |> add_optional "path" (fun value -> Json.string value) path
  |> add_optional "offset" (fun value -> Json.int value) offset
  |> add_optional "limit" (fun value -> Json.int value) limit
  |> add_optional "sort" (fun value -> Json.string value) sort
  |> List.rev |> json_object

let print_status result =
  match Tool.Result.status result with
  | Tool.Result.Completed -> print_endline "status: completed"
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf "status: failed %s\nmessage: %s\nmetadata: %b\n"
        (failure_name kind) message (Option.is_some metadata)
  | Tool.Result.Interrupted { reason; cancelled } ->
      Printf.printf "status: interrupted cancelled=%b\nreason: %s\n" cancelled
        reason

let print_result ?(json = false) ?(normalize = Fun.id) result =
  print_status result;
  match Tool.Result.output result with
  | None -> ()
  | Some output ->
      Printf.printf "text: %S\ntruncated: %b\n"
        (normalize (Tool.Output.text output))
        (Tool.Output.truncated output);
      if json then
        Printf.printf "json: %s\n"
          (match Tool.Output.json output with
          | None -> "none"
          | Some value -> normalize (json_string value))

let decode_call tool provider_input =
  match Tool.Call.decode tool (model_call tool provider_input) with
  | Ok call -> call
  | Error error ->
      failf "call decode failed: %s" (Tool.Call.Decode_error.message error)

let run ?(cancelled = fun () -> false) world provider_input =
  Tool.Call.run (decode_call world.tool provider_input) ~cancelled |> finished

let run_case ?(json = false) ?cancelled ?(normalize = Fun.id) world name
    provider_input =
  print_case name;
  print_result ~json ~normalize (run ?cancelled world provider_input)

let abs path = Lpath.Abs.of_string_exn path

let write_disk ?mtime path contents =
  mkdir_p (Filename.dirname path);
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel contents);
  Option.iter (fun time -> Unix.utimes path time time) mtime

let root_ignore =
  String.concat ""
    [
      "ignored.txt\n";
      "ignored_dir/\n";
      "ancestor/deep/*.ml\n";
      "!ancestor/deep/root-keep.ml\n";
      "ignored-root/\n";
      "blocked/\n";
      "!blocked/reinclude.ml\n";
      "reopen/*\n";
      "!reopen/keep/\n";
    ]

let local_ignore =
  String.concat ""
    [
      "# comment\n";
      "commented.txt\n";
      "\\#literal\n";
      "\\!literal\n";
      "glob.ml \n";
      "escaped.ml\\ \n";
      "double.ml";
      String.make 2 '\\';
      " \n";
      "triple.ml";
      String.make 3 '\\';
      " \n";
    ]

let populate_main root outside =
  let file ?mtime path contents =
    write_disk ?mtime (Filename.concat root path) contents
  in
  file ".gitignore" root_ignore;
  file ".env" "hidden\n";
  file "root.ml" "let root = ()\n";
  file "notes.txt" "notes\n";
  file "file-root.txt" "not a directory\n";
  file "ignored.txt" "ignored\n";
  file "ignored_dir/secret.ml" "ignored\n";
  file "ignored-root/hidden.ml" "ignored root\n";
  file "blocked/reinclude.ml" "still pruned\n";
  file "reopen/drop/hidden.ml" "pruned\n";
  file "reopen/keep/visible.ml" "visible\n";
  file "logical/cwd.ml" "cwd\n";
  file "logical/deep/nested.ml" "nested\n";
  file "lib/aux.ml" "primary collision\n";
  file "lib/aux.mli" "primary collision signature\n";
  file "next/lib/model.ml" "model\n";
  file "next/lib/tools/glob.ml" "glob\n";
  file "next/lib/tools/read_file.ml" "read\n";
  file "next/test/test.ml" "test\n";
  file "wild/a.ml" "a\n";
  file "wild/a1.ml" "a1\n";
  file "wild/ab.ml" "ab\n";
  file "wild/abc.ml" "abc\n";
  file "wild/b.ml" "b\n";
  file "match/a/b.ml" "a b\n";
  file "match/a/x/b.ml" "a x b\n";
  file "match/a/x/y/b.ml" "a x y b\n";
  file "match/a/xxb/b.ml" "a xxb b\n";
  file "match/axx/b.ml" "axx b\n";
  file "match/b.ml" "b\n";
  file "match/x/b.ml" "x b\n";
  file "match/y/b.ml" "y b\n";
  file "match/foo" "foo\n";
  file "match/nested/foo" "nested foo\n";
  file "match/abc" "abc\n";
  file "match/ac" "ac\n";
  file "match/acc" "acc\n";
  file "match/adc" "adc\n";
  file "syntax/!literal" "bang\n";
  file "syntax/[literal]" "brackets\n";
  file "syntax/{literal}" "braces\n";
  file "syntax/a,b" "comma\n";
  file "syntax/*.ml" "star\n";
  file "match/chars/a/b.txt" "slash\n";
  file "match/chars/a-b.txt" "dash\n";
  file "match/chars/axb.txt" "x\n";
  file "match/chars/acb.txt" "c\n";
  file "match/chars/a1b.txt" "one\n";
  file "prune/!" "bang\n";
  file "prune/fs/a.ml" "fs\n";
  file "prune/fs/nested/b.txt" "fs nested\n";
  file "prune/tools/a.ml" "tools\n";
  file "prune/tools/nested/b.txt" "tools nested\n";
  file ".hidden/h.ml" "hidden ml\n";
  List.iter
    (fun dir -> file (dir ^ "/secret-vcs.ml") "vcs\n")
    [ ".git"; ".svn"; ".hg"; ".bzr"; ".jj"; ".sl"; "nested/.git" ];
  file "code/lib/a.ml" "a\n";
  file "code/lib/b.mli" "b\n";
  file "code/test/c.ml" "c\n";
  file "code/readme.md" "readme\n";
  file ~mtime:1_700_000_010. "sort/old.ml" "old\n";
  file ~mtime:1_700_000_030. "sort/new.ml" "new\n";
  file ~mtime:1_700_000_020. "sort/same-a.ml" "same a\n";
  file ~mtime:1_700_000_020. "sort/same-b.ml" "same b\n";
  file "order/.gitignore" "winner.ml\n";
  file "order/.ignore" "!winner.ml\nsecond.ml\n";
  file "order/.rgignore" "winner.ml\n!second.ml\n";
  file "order/winner.ml" "winner\n";
  file "order/second.ml" "second\n";
  file "ancestor/.ignore" "!deep/ancestor-keep.ml\n";
  file "ancestor/deep/.gitignore" "!child-keep.ml\nroot-keep.ml\n";
  file "ancestor/deep/drop.ml" "drop\n";
  file "ancestor/deep/root-keep.ml" "root keep\n";
  file "ancestor/deep/ancestor-keep.ml" "ancestor keep\n";
  file "ancestor/deep/child-keep.ml" "child keep\n";
  file "ignore/.gitignore" local_ignore;
  file "ignore/commented.txt" "commented\n";
  file "ignore/#literal" "hash\n";
  file "ignore/!literal" "bang\n";
  file "ignore/glob.ml" "glob\n";
  file "ignore/escaped.ml" "plain\n";
  file "ignore/escaped.ml " "space\n";
  file "ignore/double.ml" "plain\n";
  file "ignore/double.ml\\" "slash\n";
  file "ignore/triple.ml\\" "slash\n";
  file "ignore/triple.ml\\ " "slash space\n";
  file "scoped/.gitignore"
    "/root-only.ml\r\nbasename.ml\r\nnested/path-only.ml\r\n";
  file "scoped/root-only.ml" "ignored at ignore root\n";
  file "scoped/basename.ml" "ignored by basename\n";
  file "scoped/nested/root-only.ml" "not anchored here\n";
  file "scoped/nested/basename.ml" "ignored by basename\n";
  file "scoped/nested/path-only.ml" "ignored by relative path\n";
  Unix.mkfifo (Filename.concat root "pipe") 0o600;
  Unix.symlink "code" (Filename.concat root "code_link");
  Unix.symlink "root.ml" (Filename.concat root "link_file.ml");
  Unix.symlink outside (Filename.concat root "outside_link")

let populate_aux root =
  write_disk (Filename.concat root "lib/aux.ml") "aux\n";
  write_disk (Filename.concat root "lib/aux.mli") "aux sig\n";
  write_disk (Filename.concat root ".hidden-aux.ml") "hidden aux\n"

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
  let base = Unix.realpath (temp_dir ~prefix:"mentat-glob" ()) in
  let ws_dir = Filename.concat base "workspace" in
  let aux_dir = Filename.concat base "auxiliary" in
  let out_dir = Filename.concat base "outside" in
  List.iter mkdir [ ws_dir; aux_dir; out_dir ];
  write_disk (Filename.concat out_dir "secret.ml") "outside\n";
  populate_main ws_dir out_dir;
  populate_aux aux_dir;
  let logical = workspace ~cwd ws_dir aux_dir in
  Eio.Switch.run @@ fun sw ->
  let io = resolve_exn ~sw ~stdenv ~logical () in
  fn { ws_dir; aux_dir; io; tool = Glob.make io }

let paths_of_result result =
  match Tool.Result.status result with
  | Tool.Result.Completed -> (
      let lines =
        Tool.Output.text (output_exn result) |> String.split_on_char '\n'
      in
      match lines with
      | _header :: paths ->
          paths
          |> List.filter (fun path -> not (String.is_empty path))
          |> List.take_while (fun path ->
              not (String.starts_with ~prefix:"next: " path))
      | [] -> fail "completed glob output has no header")
  | Tool.Result.Failed { kind; message; _ } ->
      failf "glob failed %s: %s" (failure_name kind) message
  | Tool.Result.Interrupted { reason; _ } -> failf "glob interrupted: %s" reason

let paths_json paths =
  Json.list (List.map (fun path -> Json.string path) paths) |> json_string

let print_paths ?(normalize = Fun.id) world label provider_input =
  let paths = run world provider_input |> paths_of_result in
  Printf.printf "%s: %s\n" label (normalize (paths_json paths))

let normalize_roots world text =
  text
  |> String.replace_all ~sub:world.ws_dir ~by:"<workspace>"
  |> String.replace_all ~sub:world.aux_dir ~by:"<auxiliary>"

let root_of_output output =
  let header =
    match String.split_on_char '\n' (Tool.Output.text output) with
    | header :: _ -> header
    | [] -> fail "glob output has no header"
  in
  let prefix = "root=" in
  match
    header |> String.split_on_char ' '
    |> List.find_opt (String.starts_with ~prefix)
  with
  | None -> fail "glob output header has no root"
  | Some field ->
      String.sub field (String.length prefix)
        (String.length field - String.length prefix)

let print_address_resolution world label result =
  let output = output_exn result in
  let addresses =
    ("root", root_of_output output)
    :: List.map (fun path -> ("path", path)) (paths_of_result result)
  in
  print_case label;
  List.iter
    (fun (kind, address) ->
      match Wio.resolve_path world.io address with
      | Ok path ->
          Printf.printf "%s %s -> key=%s path=%s\n" kind
            (normalize_roots world address)
            (Workspace.Root.Key.to_string (Workspace.Path.root_key path))
            (Workspace.Path.display path)
      | Error error ->
          failf "%s address %S does not resolve: %s" kind address
            (Workspace.Resolve_error.message error))
    addresses

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
              failf "unexpected glob permission: %a" Permission.Access.pp access)
        (Permission.Request.items request))
    requests

(* Resolve a program against PATH the way [execvp] does, so a missing external
   oracle is a declared precondition instead of an ENOENT deep inside a spawn. *)
let executable_in_path name =
  String.split_on_char ':' (Option.value ~default:"" (Sys.getenv_opt "PATH"))
  |> List.filter (fun dir -> dir <> "")
  |> List.find_opt (fun dir ->
      let candidate = Filename.concat dir name in
      match Unix.access candidate [ Unix.X_OK ] with
      | () -> true
      | exception Unix.Unix_error _ -> false)

let with_cwd dir fn =
  let previous = Sys.getcwd () in
  Unix.chdir dir;
  Fun.protect ~finally:(fun () -> Unix.chdir previous) fn

(* A child reaped under an installed SIGCHLD handler interrupts the wait before
   the handler runs: its own exit is the signal that arrives. *)
let rec waitpid_nointr pid =
  match Unix.waitpid [] pid with
  | result -> result
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_nointr pid

let ensure_git_repository root =
  let argv = [| "git"; "-C"; root; "init"; "--quiet" |] in
  let pid = Unix.create_process "git" argv Unix.stdin Unix.stdout Unix.stderr in
  match snd (waitpid_nointr pid) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED code -> failf "git init for rg oracle exited %d" code
  | Unix.WSIGNALED signal -> failf "git init for rg oracle signalled %d" signal
  | Unix.WSTOPPED signal -> failf "git init for rg oracle stopped %d" signal

let rg_paths world pattern =
  ensure_git_repository world.ws_dir;
  with_cwd world.ws_dir @@ fun () ->
  let base_args =
    ([
       "rg";
       "--files";
       "--hidden";
       "--no-config";
       "--no-require-git";
       "--no-ignore-global";
       "--no-ignore-exclude";
       "--no-ignore-parent";
     ]
      : string list)
  in
  (* Exit 2 is ripgrep declining the request, which for these inputs means its
     globset refused to parse the pattern. That is an answer about the oracle,
     not about the enumeration, so it is reported rather than raised: the
     comparison below then records that ripgrep has no reference match set for
     the pattern instead of aborting the whole high-risk sweep. Its stderr goes
     to the void for the same reason — the refusal is reported below in one
     deterministic line rather than leaking ripgrep's wording into the golden. *)
  let run args =
    let argv = Array.of_list args in
    let read, write = Unix.pipe ~cloexec:false () in
    let devnull = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
    let pid = Unix.create_process "rg" argv Unix.stdin write devnull in
    Unix.close write;
    Unix.close devnull;
    let channel = Unix.in_channel_of_descr read in
    let rec lines found =
      match In_channel.input_line channel with
      | Some line -> lines (line :: found)
      | None -> List.rev found
    in
    let paths = lines [] in
    In_channel.close channel;
    match snd (waitpid_nointr pid) with
    | Unix.WEXITED 0 | Unix.WEXITED 1 -> Ok paths
    | Unix.WEXITED 2 -> Error "rejected by the ripgrep globset"
    | Unix.WEXITED code -> failf "rg oracle exited %d" code
    | Unix.WSIGNALED signal -> failf "rg oracle signalled %d" signal
    | Unix.WSTOPPED signal -> failf "rg oracle stopped %d" signal
  in
  let ( let* ) = Result.bind in
  (* Ripgrep's positive [--glob] can re-include a path excluded by an ignore
     file. The legacy tool deliberately intersected a globbed pass with an
     unglobbed visible-files pass, so this oracle must compare the same final
     match set rather than the globbed pass in isolation. The capability also
     exposes only names representable by [Lpath.Rel], so OS entries with a
     backslash component are outside both the tool result and this oracle. *)
  let* matching = run (base_args @ [ "--glob"; pattern ]) in
  let* visible = run base_args in
  Ok
    (List.filter (fun path -> List.mem path visible) matching
    |> List.filter (fun path ->
        match Lpath.Rel.of_string path with Ok _ -> true | Error _ -> false)
    |> List.sort String.compare)

let compare_rg world pattern =
  let tool_paths = run world (input pattern) |> paths_of_result in
  match rg_paths world pattern with
  | Ok rg_paths ->
      Printf.printf "%s\nequal: %b\ntool: %s\nrg:   %s\n" pattern
        (List.equal String.equal tool_paths rg_paths)
        (paths_json tool_paths) (paths_json rg_paths)
  | Error reason ->
      Printf.printf "%s\nequal: no oracle (%s)\ntool: %s\n" pattern reason
        (paths_json tool_paths)

let resolve world path =
  match Wio.resolve_path world.io path with
  | Ok path -> path
  | Error error ->
      failf "could not resolve %S: %s" path
        (Workspace.Resolve_error.message error)

let print_enumeration world label ~root ~pattern ~cancelled =
  Printf.printf "%s: " label;
  match Glob.Enumeration.paths world.io ~cancelled ~root ~pattern with
  | Ok paths ->
      paths |> List.map Workspace.Path.display |> paths_json |> print_endline
  | Error `Cancelled -> print_endline "cancelled"
  | Error (`Invalid_pattern message) ->
      Printf.printf "invalid pattern: %s\n" message
  | Error (`File_error error) ->
      Format.printf "file error: %a@." Wio.File_error.pp error

let%expect_test
    "declaration and provider input enforce exact JSON types and bounds" =
  with_world @@ fun world ->
  let declaration = Tool.declaration world.tool in
  Printf.printf "name: %s\n" (Mentat_llm.Tool.name declaration);
  Printf.printf "schema: %s\n"
    (json_string (Mentat_llm.Tool.input_schema declaration));
  decode_verdict world.tool "minimal" (input "**/*.ml");
  decode_verdict world.tool "full"
    (input ~path:"code" ~offset:2 ~limit:3 ~sort:"modified" "**/*.ml");
  List.iter
    (fun (label, provider_input) ->
      decode_verdict world.tool label provider_input)
    [
      ("missing pattern", json_object []);
      ( "unknown member",
        json_object [ ("pattern", Json.string "*"); ("x", Json.int 1) ] );
      ( "duplicate pattern",
        json_object
          [ ("pattern", Json.string "*.ml"); ("pattern", Json.string "*") ] );
      ("pattern type", json_object [ ("pattern", Json.int 1) ]);
      ( "path type",
        json_object [ ("pattern", Json.string "*"); ("path", Json.bool true) ]
      );
      ( "offset string",
        json_object
          [ ("pattern", Json.string "*"); ("offset", Json.string "2") ] );
      ( "offset fraction",
        json_object
          [ ("pattern", Json.string "*"); ("offset", Json.number 1.5) ] );
      ( "offset unsafe",
        json_object
          [
            ("pattern", Json.string "*");
            ("offset", Json.number 9_007_199_254_740_992.);
          ] );
      ( "limit fraction",
        json_object [ ("pattern", Json.string "*"); ("limit", Json.number 1.5) ]
      );
      ( "sort type",
        json_object [ ("pattern", Json.string "*"); ("sort", Json.int 1) ] );
      ("empty pattern", input "");
      ("NUL pattern", input "bad\000glob");
      ("empty path", input ~path:"" "*");
      ("NUL path", input ~path:"bad\000path" "*");
      ("offset zero", input ~offset:0 "*");
      ("limit zero", input ~limit:0 "*");
      ("limit over max", input ~limit:(Glob.max_limit + 1) "*");
      ("bad sort", input ~sort:"size" "*");
      ("malformed glob deferred", input "[");
    ];
  [%expect
    {|
    name: glob
    schema: {"type":"object","properties":{"pattern":{"type":"string","description":"Glob for workspace-relative file paths, for example **/*.ml or **/*.{ts,tsx}.","minLength":1},"path":{"type":"string","description":"Workspace-relative or workspace-contained absolute directory root. Defaults to the logical workspace current directory.","minLength":1},"offset":{"type":"integer","description":"One-based first matching file. Defaults to 1.","minimum":1,"maximum":9007199254740991},"limit":{"type":"integer","description":"Maximum matching files returned. Defaults to 100.","minimum":1,"maximum":1000},"sort":{"type":"string","description":"Ordering: path, or newest modification time with path as tie-breaker. Defaults to path.","enum":["path","modified"]}},"required":["pattern"],"additionalProperties":false}
    minimal: accepted canonical={"pattern":"**/*.ml","sort":"path"}
    full: accepted canonical={"limit":3,"offset":2,"path":"code","pattern":"**/*.ml","sort":"modified"}
    missing pattern: rejected diagnostic=true
    unknown member: rejected diagnostic=true
    duplicate pattern: rejected diagnostic=true
    pattern type: rejected diagnostic=true
    path type: rejected diagnostic=true
    offset string: rejected diagnostic=true
    offset fraction: rejected diagnostic=true
    offset unsafe: rejected diagnostic=true
    limit fraction: rejected diagnostic=true
    sort type: rejected diagnostic=true
    empty pattern: rejected diagnostic=true
    NUL pattern: rejected diagnostic=true
    empty path: rejected diagnostic=true
    NUL path: rejected diagnostic=true
    offset zero: rejected diagnostic=true
    limit zero: rejected diagnostic=true
    limit over max: rejected diagnostic=true
    bad sort: rejected diagnostic=true
    malformed glob deferred: accepted canonical={"pattern":"[","sort":"path"}
    |}]

let%expect_test "permissions cache the exact resolved root without observing it"
    =
  with_world @@ fun world ->
  let before =
    Sys.readdir world.ws_dir |> Array.to_list |> List.sort String.compare
  in
  List.iter
    (fun (label, provider_input) ->
      print_case label;
      let call = decode_call world.tool provider_input in
      print_permissions call;
      print_status (Tool.Call.run call ~cancelled:(fun () -> false) |> finished))
    [
      ("primary nested", input ~path:"code" "*.ml");
      ("missing", input ~path:"missing" "*.ml");
      ("file root", input ~path:"file-root.txt" "*.ml");
      ("outside", input ~path:"/outside-glob-root" "*.ml");
      ("auxiliary absolute", input ~path:world.aux_dir "*.ml");
    ];
  let after =
    Sys.readdir world.ws_dir |> Array.to_list |> List.sort String.compare
  in
  Printf.printf "tree names unchanged: %b\n"
    (List.equal String.equal before after);
  [%expect
    {|
    -- primary nested --
    requests: 1
    source: glob
    access: read key=main path=code change=false
    status: completed
    -- missing --
    requests: 1
    source: glob
    access: read key=main path=missing change=false
    status: failed not_found
    message: missing: path does not exist
    metadata: false
    -- file root --
    requests: 1
    source: glob
    access: read key=main path=file-root.txt change=false
    status: failed invalid_input
    message: file-root.txt: not a directory
    metadata: false
    -- outside --
    requests: 0
    status: failed invalid_input
    message: path is outside workspace: /outside-glob-root
    metadata: false
    -- auxiliary absolute --
    requests: 1
    source: glob
    access: read key=aux path=. change=false
    status: completed
    tree names unchanged: true
    |}]

let%expect_test
    "logical cwd keeps matching root-relative but emits resolvable addresses" =
  with_world ~cwd:Primary_logical @@ fun world ->
  print_paths world "default slash" (input "logical/**/*.ml");
  print_paths world "default basename" (input "*.ml");
  print_paths ~normalize:(normalize_roots world) world "nested root full path"
    (input ~path:"../next/lib" "next/lib/tools/*.ml");
  print_paths world "nested root short path"
    (input ~path:"../next/lib" "tools/*.ml");
  print_paths ~normalize:(normalize_roots world) world "nested root basename"
    (input ~path:"../next/lib" "*.ml");
  print_paths ~normalize:(normalize_roots world) world "contained absolute"
    (input
       ~path:(Filename.concat world.ws_dir "next/lib")
       "next/lib/tools/*.ml");
  let sibling_first = run world (input ~path:"../next/lib" ~limit:1 "*.ml") in
  print_case "same-root sibling first page";
  print_result ~json:true ~normalize:(normalize_roots world) sibling_first;
  print_address_resolution world "same-root sibling addresses" sibling_first;
  let sibling_next = input ~path:"../next/lib" ~offset:2 ~limit:1 "*.ml" in
  run_case ~json:true ~normalize:(normalize_roots world) world
    "same-root sibling continuation" sibling_next;
  let first = run world (input ~limit:1 "*.ml") in
  print_case "nested cwd first page";
  print_result ~json:true first;
  print_address_resolution world "nested cwd addresses" first;
  let replayed = decode result_codec (encode result_codec first) in
  Printf.printf "nested cwd durable replay equal: %b\n"
    (Jsont.Json.equal
       (encode result_codec first)
       (encode result_codec replayed));
  let next = input ~path:"." ~offset:2 ~limit:1 "*.ml" in
  run_case ~json:true world "nested cwd continuation" next;
  [%expect
    {|
    default slash: ["cwd.ml","deep/nested.ml"]
    default basename: ["cwd.ml","deep/nested.ml"]
    nested root full path: ["<workspace>/next/lib/tools/glob.ml","<workspace>/next/lib/tools/read_file.ml"]
    nested root short path: ["No files"]
    nested root basename: ["<workspace>/next/lib/model.ml","<workspace>/next/lib/tools/glob.ml","<workspace>/next/lib/tools/read_file.ml"]
    contained absolute: ["<workspace>/next/lib/tools/glob.ml","<workspace>/next/lib/tools/read_file.ml"]
    -- same-root sibling first page --
    status: completed
    text: "pattern=\"*.ml\" root=<workspace>/next/lib files=1/3 offset=1 limit=1 sort=path status=partial\n<workspace>/next/lib/model.ml\nnext: glob {\"pattern\":\"*.ml\",\"path\":\"../next/lib\",\"offset\":2,\"limit\":1,\"sort\":\"path\"}\n"
    truncated: true
    json: {"version":1,"shape":"files","total":3}
    -- same-root sibling addresses --
    root <workspace>/next/lib -> key=main path=next/lib
    path <workspace>/next/lib/model.ml -> key=main path=next/lib/model.ml
    -- same-root sibling continuation --
    status: completed
    text: "pattern=\"*.ml\" root=<workspace>/next/lib files=1/3 offset=2 limit=1 sort=path status=partial\n<workspace>/next/lib/tools/glob.ml\nnext: glob {\"pattern\":\"*.ml\",\"path\":\"../next/lib\",\"offset\":3,\"limit\":1,\"sort\":\"path\"}\n"
    truncated: true
    json: {"version":1,"shape":"files","total":3}
    -- nested cwd first page --
    status: completed
    text: "pattern=\"*.ml\" root=. files=1/2 offset=1 limit=1 sort=path status=partial\ncwd.ml\nnext: glob {\"pattern\":\"*.ml\",\"path\":\".\",\"offset\":2,\"limit\":1,\"sort\":\"path\"}\n"
    truncated: true
    json: {"version":1,"shape":"files","total":2}
    -- nested cwd addresses --
    root . -> key=main path=logical
    path cwd.ml -> key=main path=logical/cwd.ml
    nested cwd durable replay equal: true
    -- nested cwd continuation --
    status: completed
    text: "pattern=\"*.ml\" root=. files=1/2 offset=2 limit=1 sort=path status=complete\ndeep/nested.ml\n"
    truncated: false
    json: {"version":1,"shape":"files","total":2}
    |}]

let%expect_test
    "wildcards classes ranges complements and slash classes match globset" =
  with_world @@ fun world ->
  List.iter
    (fun (label, pattern) -> print_paths world label (input pattern))
    [
      ("star", "wild/a*.ml");
      ("question", "wild/a?.ml");
      ("range", "wild/a[0-9].ml");
      ("bang complement", "wild/a[!0-9].ml");
      ("caret complement", "wild/a[^b].ml");
      ("slash or dash", "match/chars/a[/-]b.txt");
      ("slash", "match/chars/a[/]b.txt");
      ("slash or x", "match/chars/a[/x]b.txt");
    ];
  [%expect
    {|
    star: ["wild/a.ml","wild/a1.ml","wild/ab.ml","wild/abc.ml"]
    question: ["wild/a1.ml","wild/ab.ml"]
    range: ["wild/a1.ml"]
    bang complement: ["wild/ab.ml"]
    caret complement: ["wild/a1.ml"]
    slash or dash: ["match/chars/a-b.txt","match/chars/a/b.txt"]
    slash: ["match/chars/a/b.txt"]
    slash or x: ["match/chars/a/b.txt","match/chars/axb.txt"]
    |}]

let%expect_test
    "globstar placement and nested braces preserve globset boundaries" =
  with_world @@ fun world ->
  List.iter
    (fun pattern -> print_paths world pattern (input pattern))
    [
      "match/a/**/b.ml";
      "match/a/***/b.ml";
      "match/a**/b.ml";
      "match/a/**b/b.ml";
      "match/a/{**,x}/b.ml";
      "match/a/{**}/b.ml";
      "match/{**,x}/b.ml";
      "match/{**/,}foo";
      "match/a{b,}c";
      "match/a{b,{c,d}}c";
      "match/a{}c";
      "match/a{,}c";
    ];
  [%expect
    {|
    match/a/**/b.ml: ["match/a/b.ml","match/a/x/b.ml","match/a/x/y/b.ml","match/a/xxb/b.ml"]
    match/a/***/b.ml: ["match/a/x/b.ml","match/a/xxb/b.ml"]
    match/a**/b.ml: ["match/a/b.ml","match/axx/b.ml"]
    match/a/**b/b.ml: ["match/a/xxb/b.ml"]
    match/a/{**,x}/b.ml: ["match/a/x/b.ml","match/a/xxb/b.ml"]
    match/a/{**}/b.ml: ["match/a/x/b.ml","match/a/xxb/b.ml"]
    match/{**,x}/b.ml: ["match/a/b.ml","match/axx/b.ml","match/x/b.ml","match/y/b.ml"]
    match/{**/,}foo: ["match/foo","match/nested/foo"]
    match/a{b,}c: ["match/abc"]
    match/a{b,{c,d}}c: ["match/abc","match/acc","match/adc"]
    match/a{}c: ["match/ac"]
    match/a{,}c: ["match/ac"]
    |}]

let%expect_test "anchoring and escaped metacharacters preserve globset syntax" =
  with_world @@ fun world ->
  List.iter
    (fun pattern -> print_paths world pattern (input pattern))
    [
      "/root.ml";
      "root.ml";
      "\\!literal";
      "syntax/\\[literal\\]";
      "syntax/\\{literal\\}";
      "syntax/a,b";
      "syntax/\\*.ml";
    ];
  [%expect
    {|
    /root.ml: ["root.ml"]
    root.ml: ["root.ml"]
    \!literal: ["syntax/!literal"]
    syntax/\[literal\]: ["syntax/[literal]"]
    syntax/\{literal\}: ["syntax/{literal}"]
    syntax/a,b: ["syntax/a,b"]
    syntax/\*.ml: ["syntax/*.ml"]
    |}]

let%expect_test "high-risk match sets agree with real ripgrep globset" =
  (* This is the one case that reaches outside the workspace for a reference
     implementation; without ripgrep on PATH there is nothing to agree with. *)
  if Option.is_none (executable_in_path "rg") then
    skip ~reason:"ripgrep is not installed on this host" ();
  with_world @@ fun world ->
  List.iter (compare_rg world)
    [
      "match/a/**/b.ml";
      "match/a/***/b.ml";
      "match/a/{**,x}/b.ml";
      "match/a/{**}/b.ml";
      "match/{**,x}/b.ml";
      "match/chars/a[/-]b.txt";
      "match/a{b,}c";
      "match/a{b,{c,d}}c";
      "match/{**/,}foo";
      "ignore/*.ml*";
      "/root.ml";
      "\\!literal";
      "syntax/\\[literal\\]";
      "syntax/\\{literal\\}";
      "syntax/a,b";
      "syntax/\\*.ml";
    ];
  [%expect
    {|
    match/a/**/b.ml
    equal: true
    tool: ["match/a/b.ml","match/a/x/b.ml","match/a/x/y/b.ml","match/a/xxb/b.ml"]
    rg:   ["match/a/b.ml","match/a/x/b.ml","match/a/x/y/b.ml","match/a/xxb/b.ml"]
    match/a/***/b.ml
    equal: true
    tool: ["match/a/x/b.ml","match/a/xxb/b.ml"]
    rg:   ["match/a/x/b.ml","match/a/xxb/b.ml"]
    match/a/{**,x}/b.ml
    equal: true
    tool: ["match/a/x/b.ml","match/a/xxb/b.ml"]
    rg:   ["match/a/x/b.ml","match/a/xxb/b.ml"]
    match/a/{**}/b.ml
    equal: true
    tool: ["match/a/x/b.ml","match/a/xxb/b.ml"]
    rg:   ["match/a/x/b.ml","match/a/xxb/b.ml"]
    match/{**,x}/b.ml
    equal: true
    tool: ["match/a/b.ml","match/axx/b.ml","match/x/b.ml","match/y/b.ml"]
    rg:   ["match/a/b.ml","match/axx/b.ml","match/x/b.ml","match/y/b.ml"]
    match/chars/a[/-]b.txt
    equal: true
    tool: ["match/chars/a-b.txt","match/chars/a/b.txt"]
    rg:   ["match/chars/a-b.txt","match/chars/a/b.txt"]
    match/a{b,}c
    equal: true
    tool: ["match/abc"]
    rg:   ["match/abc"]
    match/a{b,{c,d}}c
    equal: true
    tool: ["match/abc","match/acc","match/adc"]
    rg:   ["match/abc","match/acc","match/adc"]
    match/{**/,}foo
    equal: true
    tool: ["match/foo","match/nested/foo"]
    rg:   ["match/foo","match/nested/foo"]
    ignore/*.ml*
    equal: true
    tool: ["ignore/double.ml","ignore/escaped.ml"]
    rg:   ["ignore/double.ml","ignore/escaped.ml"]
    /root.ml
    equal: true
    tool: ["root.ml"]
    rg:   ["root.ml"]
    \!literal
    equal: true
    tool: ["syntax/!literal"]
    rg:   ["syntax/!literal"]
    syntax/\[literal\]
    equal: true
    tool: ["syntax/[literal]"]
    rg:   ["syntax/[literal]"]
    syntax/\{literal\}
    equal: true
    tool: ["syntax/{literal}"]
    rg:   ["syntax/{literal}"]
    syntax/a,b
    equal: true
    tool: ["syntax/a,b"]
    rg:   ["syntax/a,b"]
    syntax/\*.ml
    equal: true
    tool: ["syntax/*.ml"]
    rg:   ["syntax/*.ml"]
    |}]

let%expect_test
    "leading negation prunes child directories but never the explicit root" =
  with_world @@ fun world ->
  List.iter
    (fun (label, pattern) ->
      print_paths world label (input ~path:"prune" pattern))
    [
      ("basename directory", "!fs");
      ("slashed directory", "!prune/fs");
      ("explicit root", "!prune");
      ("positive traversal", "prune/tools/*.ml");
      ("lone bang", "!");
      ("double bang", "!!");
    ];
  [%expect
    {|
    basename directory: ["prune/!","prune/tools/a.ml","prune/tools/nested/b.txt"]
    slashed directory: ["prune/!","prune/tools/a.ml","prune/tools/nested/b.txt"]
    explicit root: ["prune/!","prune/fs/a.ml","prune/fs/nested/b.txt","prune/tools/a.ml","prune/tools/nested/b.txt"]
    positive traversal: ["prune/tools/a.ml"]
    lone bang: ["No files"]
    double bang: ["prune/fs/a.ml","prune/fs/nested/b.txt","prune/tools/a.ml","prune/tools/nested/b.txt"]
    |}]

let%expect_test "hidden VCS symlink and special entries keep traversal confined"
    =
  with_world @@ fun world ->
  print_paths world "hidden" (input ".hidden/**");
  print_paths world "all protected VCS" (input "**/secret-vcs.ml");
  print_paths world "symlink directory child" (input "code_link/**");
  print_paths world "symlink file child" (input "link_file.ml");
  print_paths world "special child" (input "pipe");
  print_paths world "explicit git root" (input ~path:".git" "**");
  print_paths world "explicit nested git root" (input ~path:"nested/.git" "**");
  run_case world "explicit in-root symlink" (input ~path:"code_link" "**");
  run_case world "explicit escape symlink" (input ~path:"outside_link" "**");
  run_case world "escape through symlink"
    (input ~path:"outside_link/deeper" "**");
  [%expect
    {|
    hidden: [".hidden/h.ml"]
    all protected VCS: ["No files"]
    symlink directory child: ["No files"]
    symlink file child: ["No files"]
    special child: ["No files"]
    explicit git root: ["No files"]
    explicit nested git root: ["No files"]
    -- explicit in-root symlink --
    status: failed invalid_input
    message: code_link: symlink search roots are not supported
    metadata: false
    -- explicit escape symlink --
    status: failed invalid_input
    message: outside_link: symlink search roots are not supported
    metadata: false
    -- escape through symlink --
    status: failed invalid_input
    message: outside_link/deeper: path resolves outside workspace
    metadata: false
    |}]

let%expect_test
    "ignore files compose by source ancestor child negation and spaces" =
  with_world @@ fun world ->
  print_paths world "ignored file" (input "ignored.txt");
  print_paths world "ignored directory" (input "ignored_dir/**");
  print_paths world "pruned re-inclusion" (input "blocked/**");
  print_paths world "reopened parent" (input "reopen/**/*.ml");
  print_paths world "source order" (input "order/*.ml");
  print_paths world "ancestor and child precedence"
    (input ~path:"ancestor/deep" "ancestor/deep/*.ml");
  print_paths world "ignored requested root"
    (input ~path:"ignored-root" "**/*.ml");
  print_paths world "comments escapes and trailing spaces"
    (input "ignore/*.ml*");
  print_paths world "anchored ignore rule" (input "scoped/**/root-only.ml");
  print_paths world "basename ignore rule" (input "scoped/**/basename.ml");
  print_paths world "relative ignore rule" (input "scoped/**/path-only.ml");
  [%expect
    {|
    ignored file: ["No files"]
    ignored directory: ["No files"]
    pruned re-inclusion: ["No files"]
    reopened parent: ["reopen/keep/visible.ml"]
    source order: ["order/second.ml"]
    ancestor and child precedence: ["ancestor/deep/ancestor-keep.ml","ancestor/deep/child-keep.ml"]
    ignored requested root: ["No files"]
    comments escapes and trailing spaces: ["ignore/double.ml","ignore/escaped.ml"]
    anchored ignore rule: ["scoped/nested/root-only.ml"]
    basename ignore rule: ["No files"]
    relative ignore rule: ["No files"]
    |}]

let%expect_test "typed enumeration shares ignore ordering and failure semantics"
    =
  with_world @@ fun world ->
  let root = resolve world "ancestor/deep" in
  print_enumeration world "ordered paths" ~root ~pattern:"**/*.ml"
    ~cancelled:(fun () -> false);
  print_enumeration world "invalid pattern" ~root ~pattern:"["
    ~cancelled:(fun () -> false);
  print_enumeration world "cancelled" ~root ~pattern:"**/*.ml"
    ~cancelled:(fun () -> true);
  [%expect
    {|
    ordered paths: ["ancestor/deep/ancestor-keep.ml","ancestor/deep/child-keep.ml"]
    invalid pattern: invalid pattern: unclosed character class at byte 2
    cancelled: cancelled
    |}]

let%expect_test "ignore-file I/O errors fail instead of changing the match set"
    =
  if Unix.geteuid () = 0 then
    skip ~reason:"the superuser can read mode-000 fixture files" ();
  with_world @@ fun world ->
  let directory = Filename.concat world.ws_dir "unreadable" in
  let ignore = Filename.concat directory ".gitignore" in
  write_disk ignore "visible.ml\n";
  write_disk (Filename.concat directory "visible.ml") "visible\n";
  Unix.chmod ignore 0o000;
  Fun.protect ~finally:(fun () -> Unix.chmod ignore 0o644) @@ fun () ->
  run_case world "unreadable ignore" (input "unreadable/*.ml");
  [%expect
    {|
    -- unreadable ignore --
    status: failed failed
    message: unreadable/.gitignore: filesystem I/O error
    metadata: false
    |}]

let%expect_test "oversized ignore files fail loudly instead of changing matches"
    =
  with_world @@ fun world ->
  write_disk
    (Filename.concat world.ws_dir ".gitignore")
    (String.make ((16 * 1024 * 1024) + 1) 'x');
  run_case world "oversized ignore" (input "**/*.ml");
  [%expect
    {|
    -- oversized ignore --
    status: failed invalid_input
    message: .gitignore: file is too large (16777217 bytes, max 16777216)
    metadata: false
    |}]

let%expect_test
    "malformed patterns are structured run failures after permission planning" =
  with_world @@ fun world ->
  List.iter
    (fun pattern ->
      print_case (Printf.sprintf "%S" pattern);
      let call = decode_call world.tool (input pattern) in
      print_permissions call;
      print_result (Tool.Call.run call ~cancelled:(fun () -> false) |> finished))
    [ "["; "[]"; "[z-a]"; "abc\\"; "{a,b" ];
  [%expect
    {|
    -- "[" --
    requests: 1
    source: glob
    access: read key=main path=. change=false
    status: failed invalid_input
    message: invalid glob pattern: unclosed character class at byte 2
    metadata: false
    -- "[]" --
    requests: 1
    source: glob
    access: read key=main path=. change=false
    status: failed invalid_input
    message: invalid glob pattern: unclosed character class at byte 3
    metadata: false
    -- "[z-a]" --
    requests: 1
    source: glob
    access: read key=main path=. change=false
    status: failed invalid_input
    message: invalid glob pattern: descending character-class range at byte 5
    metadata: false
    -- "abc\\" --
    requests: 1
    source: glob
    access: read key=main path=. change=false
    status: failed invalid_input
    message: invalid glob pattern: trailing escape at byte 5
    metadata: false
    -- "{a,b" --
    requests: 1
    source: glob
    access: read key=main path=. change=false
    status: failed invalid_input
    message: invalid glob pattern: unclosed brace expansion at byte 5
    metadata: false
    |}]

let%expect_test "path and modified ordering are stable before exact pagination"
    =
  with_world @@ fun world ->
  print_paths world "path" (input ~path:"sort" "*.ml");
  print_paths world "modified" (input ~path:"sort" ~sort:"modified" "*.ml");
  let first = run world (input ~path:"sort" ~limit:2 "*.ml") in
  print_case "first";
  print_result ~json:true first;
  let next = input ~path:"sort" ~offset:3 ~limit:2 "*.ml" in
  run_case ~json:true world "continuation" next;
  let modified_first =
    run world (input ~path:"sort" ~limit:2 ~sort:"modified" "*.ml")
  in
  print_case "modified first";
  print_result ~json:true modified_first;
  run_case ~json:true world "modified continuation"
    (input ~path:"sort" ~offset:3 ~limit:2 ~sort:"modified" "*.ml");
  run_case ~json:true world "beyond end"
    (input ~path:"sort" ~offset:99 ~limit:2 "*.ml");
  [%expect
    {|
    path: ["sort/new.ml","sort/old.ml","sort/same-a.ml","sort/same-b.ml"]
    modified: ["sort/new.ml","sort/same-a.ml","sort/same-b.ml","sort/old.ml"]
    -- first --
    status: completed
    text: "pattern=\"*.ml\" root=sort files=2/4 offset=1 limit=2 sort=path status=partial\nsort/new.ml\nsort/old.ml\nnext: glob {\"pattern\":\"*.ml\",\"path\":\"sort\",\"offset\":3,\"limit\":2,\"sort\":\"path\"}\n"
    truncated: true
    json: {"version":1,"shape":"files","total":4}
    -- continuation --
    status: completed
    text: "pattern=\"*.ml\" root=sort files=2/4 offset=3 limit=2 sort=path status=complete\nsort/same-a.ml\nsort/same-b.ml\n"
    truncated: false
    json: {"version":1,"shape":"files","total":4}
    -- modified first --
    status: completed
    text: "pattern=\"*.ml\" root=sort files=2/4 offset=1 limit=2 sort=modified status=partial\nsort/new.ml\nsort/same-a.ml\nnext: glob {\"pattern\":\"*.ml\",\"path\":\"sort\",\"offset\":3,\"limit\":2,\"sort\":\"modified\"}\n"
    truncated: true
    json: {"version":1,"shape":"files","total":4}
    -- modified continuation --
    status: completed
    text: "pattern=\"*.ml\" root=sort files=2/4 offset=3 limit=2 sort=modified status=complete\nsort/same-b.ml\nsort/old.ml\n"
    truncated: false
    json: {"version":1,"shape":"files","total":4}
    -- beyond end --
    status: completed
    text: "pattern=\"*.ml\" root=sort files=0/4 offset=99 limit=2 sort=path status=complete\nNo files\n"
    truncated: false
    json: {"version":1,"shape":"files","total":4}
    |}]

let%expect_test "the omitted limit uses the bounded one-hundred-file page" =
  with_world @@ fun world ->
  List.init 105 (fun index -> Printf.sprintf "many/file-%03d.ml" index)
  |> List.iter (fun path ->
      write_disk (Filename.concat world.ws_dir path) "page\n");
  let print_page label provider_input =
    let result = run world provider_input in
    let paths = paths_of_result result in
    let endpoint = function [] -> "none" | path :: _ -> path in
    Printf.printf "%s: returned=%d first=%s last=%s truncated=%b root=%s\n"
      label (List.length paths) (endpoint paths)
      (endpoint (List.rev paths))
      (Tool.Output.truncated (output_exn result))
      (root_of_output (output_exn result))
  in
  print_page "default page" (input ~path:"many" "*.ml");
  print_page "default continuation" (input ~path:"many" ~offset:101 "*.ml");
  [%expect
    {|
    default page: returned=100 first=many/file-000.ml last=many/file-099.ml truncated=true root=many
    default continuation: returned=5 first=many/file-100.ml last=many/file-104.ml truncated=false root=many
    |}]

let%expect_test
    "cancellation before and during traversal never returns a partial page" =
  with_world @@ fun world ->
  List.iter
    (fun stop ->
      let polls = ref 0 in
      let cancelled () =
        incr polls;
        !polls >= stop
      in
      Printf.printf "-- poll %d --\n" stop;
      let result = run ~cancelled world (input "**/*") in
      print_status result;
      Printf.printf "output: %b polls: %d\n"
        (Option.is_some (Tool.Result.output result))
        !polls)
    [ 1; 2; 5; 12 ];
  [%expect
    {|
    -- poll 1 --
    status: interrupted cancelled=true
    reason: tool call cancelled
    output: false polls: 1
    -- poll 2 --
    status: interrupted cancelled=true
    reason: tool call cancelled
    output: false polls: 2
    -- poll 5 --
    status: interrupted cancelled=true
    reason: tool call cancelled
    output: false polls: 5
    -- poll 12 --
    status: interrupted cancelled=true
    reason: tool call cancelled
    output: false polls: 12
    |}]

let%expect_test
    "auxiliary roots retain permission identity and durable addressing" =
  with_world @@ fun world ->
  run_case ~json:true world "primary collision"
    (input ~limit:1 "lib/*.{ml,mli}");
  let normalize text =
    String.replace_all ~sub:world.aux_dir ~by:"<auxiliary>" text
  in
  let noncanonical_auxiliary = Filename.concat world.aux_dir "." in
  let call =
    decode_call world.tool
      (input ~path:noncanonical_auxiliary ~limit:1 "lib/*.{ml,mli}")
  in
  print_case "auxiliary absolute canonicalized";
  print_permissions call;
  let first = Tool.Call.run call ~cancelled:(fun () -> false) |> finished in
  print_result ~json:true ~normalize first;
  print_address_resolution world "auxiliary addresses" first;
  let replayed = decode result_codec (encode result_codec first) in
  let first_output = output_exn first in
  let replayed_output = output_exn replayed in
  Printf.printf "auxiliary durable replay equal: %b\n"
    (String.equal
       (Tool.Output.text first_output)
       (Tool.Output.text replayed_output)
    && Option.equal Jsont.Json.equal
         (Tool.Output.json first_output)
         (Tool.Output.json replayed_output)
    && Bool.equal
         (Tool.Output.truncated first_output)
         (Tool.Output.truncated replayed_output));
  let next = input ~path:world.aux_dir ~offset:2 ~limit:1 "lib/*.{ml,mli}" in
  let continuation = decode_call world.tool next in
  print_case "auxiliary continuation";
  print_permissions continuation;
  print_result ~json:true ~normalize
    (Tool.Call.run continuation ~cancelled:(fun () -> false) |> finished);
  with_world ~cwd:Auxiliary_root @@ fun auxiliary_cwd ->
  run_case ~json:true auxiliary_cwd "auxiliary cwd" (input "lib/*.{ml,mli}");
  [%expect
    {|
    -- primary collision --
    status: completed
    text: "pattern=\"lib/*.{ml,mli}\" root=. files=1/2 offset=1 limit=1 sort=path status=partial\nlib/aux.ml\nnext: glob {\"pattern\":\"lib/*.{ml,mli}\",\"path\":\".\",\"offset\":2,\"limit\":1,\"sort\":\"path\"}\n"
    truncated: true
    json: {"version":1,"shape":"files","total":2}
    -- auxiliary absolute canonicalized --
    requests: 1
    source: glob
    access: read key=aux path=. change=false
    status: completed
    text: "pattern=\"lib/*.{ml,mli}\" root=<auxiliary> files=1/2 offset=1 limit=1 sort=path status=partial\n<auxiliary>/lib/aux.ml\nnext: glob {\"pattern\":\"lib/*.{ml,mli}\",\"path\":\"<auxiliary>\",\"offset\":2,\"limit\":1,\"sort\":\"path\"}\n"
    truncated: true
    json: {"version":1,"shape":"files","total":2}
    -- auxiliary addresses --
    root <auxiliary> -> key=aux path=.
    path <auxiliary>/lib/aux.ml -> key=aux path=lib/aux.ml
    auxiliary durable replay equal: true
    -- auxiliary continuation --
    requests: 1
    source: glob
    access: read key=aux path=. change=false
    status: completed
    text: "pattern=\"lib/*.{ml,mli}\" root=<auxiliary> files=1/2 offset=2 limit=1 sort=path status=complete\n<auxiliary>/lib/aux.mli\n"
    truncated: false
    json: {"version":1,"shape":"files","total":2}
    -- auxiliary cwd --
    status: completed
    text: "pattern=\"lib/*.{ml,mli}\" root=. files=2/2 offset=1 limit=100 sort=path status=complete\nlib/aux.ml\nlib/aux.mli\n"
    truncated: false
    json: {"version":1,"shape":"files","total":2}
    |}]

let%expect_test
    "durable replay preserves the complete presentation and no authority" =
  with_world @@ fun world ->
  let result = run world (input ~path:"sort" ~limit:2 "*.ml") in
  let output = output_exn result in
  let durable = encode result_codec result in
  let replayed = decode result_codec durable in
  let replayed_output = output_exn replayed in
  let names =
    Tool.Output.json output
    |> Option.value ~default:(Json.null ())
    |> json_member_names
  in
  Printf.printf "text: %S\n" (Tool.Output.text output);
  Printf.printf "json: %s\n"
    (Tool.Output.json output |> Option.map json_string
    |> Option.value ~default:"none");
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
    [ "receipt"; "mutation"; "evidence"; "fingerprint"; "anchor"; "command" ];
  [%expect
    {|
    text: "pattern=\"*.ml\" root=sort files=2/4 offset=1 limit=2 sort=path status=partial\nsort/new.ml\nsort/old.ml\nnext: glob {\"pattern\":\"*.ml\",\"path\":\"sort\",\"offset\":3,\"limit\":2,\"sort\":\"path\"}\n"
    json: {"version":1,"shape":"files","total":4}
    durable: {"status":"completed","output":{"text":"pattern=\"*.ml\" root=sort files=2/4 offset=1 limit=2 sort=path status=partial\nsort/new.ml\nsort/old.ml\nnext: glob {\"pattern\":\"*.ml\",\"path\":\"sort\",\"offset\":3,\"limit\":2,\"sort\":\"path\"}\n","json":{"version":1,"shape":"files","total":4},"truncated":true}}
    text replay equal: true
    json replay equal: true
    truncated replay equal: true
    contains receipt: false
    contains mutation: false
    contains evidence: false
    contains fingerprint: false
    contains anchor: false
    contains command: false
    |}]

let%expect_test "root ignore rules prune with gitignore semantics" =
  let rules =
    Glob.Ignore.parse
      "# artifacts\n_ref/\n/dist/\n_*/\n*.log\n!keep.log\nnode_modules\n"
  in
  let check text =
    Printf.printf "%s -> %b\n" text
      (Glob.Ignore.prunes rules (Lpath.Rel.of_string_exn text))
  in
  List.iter check
    [
      ".";
      "_ref";
      "_reference";
      "sub/_ref";
      "dist";
      "sub/dist";
      "_autoresearch";
      "x.log";
      "keep.log";
      "deep/a/node_modules";
    ];
  [%expect
    {|
    . -> false
    _ref -> true
    _reference -> true
    sub/_ref -> true
    dist -> true
    sub/dist -> false
    _autoresearch -> true
    x.log -> true
    keep.log -> false
    deep/a/node_modules -> true
    |}]

let%expect_test "later joined ignore rules have the final say" =
  let earlier = Glob.Ignore.parse "vendor/\n" in
  let later = Glob.Ignore.parse "!vendor/\n" in
  let check rules text =
    Printf.printf "%s -> %b\n" text
      (Glob.Ignore.prunes rules (Lpath.Rel.of_string_exn text))
  in
  check earlier "vendor";
  check (Glob.Ignore.join earlier later) "vendor";
  check Glob.Ignore.empty "vendor";
  [%expect {|
    vendor -> true
    vendor -> false
    vendor -> false
    |}]
