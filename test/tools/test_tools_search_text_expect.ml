(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for [search_text]. Effects run
   through a real resolved workspace capability and the erased production path:

   provider JSON -> [Mentat_tool.Call.decode] -> cached permission requests
   -> [Call.run] -> erased [Output.t] -> durable [Result.jsont Output.jsont].

   Real ripgrep covers search semantics. Small executable fixtures are reserved
   for process outcomes that real ripgrep cannot deterministically produce on a
   tiny tree: cooperative cancellation, timeout, malformed output, output
   overflow, and a missing executable. No private tool helper or IO mock is
   called. *)

open Windtrap
open Test_tools_support
module Json = Jsont.Json
module Tool = Mentat_tool
module Search_text = Mentat_tools.Fs.Search_text
module Wio = Mentat_workspace_io
module Workspace = Mentat_workspace
module Permission = Mentat_permission

type world = { ws_dir : string; io : Wio.t; tool : Tool.t; sw : Eio.Switch.t }
type rg = Ambient | Missing | Script of string

let add_optional name encode value fields =
  match value with
  | None -> fields
  | Some value -> (name, encode value) :: fields

let input ?paths ?glob ?mode ?case_insensitive ?context_lines ?offset ?limit
    pattern =
  [ ("pattern", Json.string pattern) ]
  |> add_optional "paths"
       (fun paths ->
         Json.list (List.map (fun value -> Json.string value) paths))
       paths
  |> add_optional "glob" (fun value -> Json.string value) glob
  |> add_optional "mode" (fun value -> Json.string value) mode
  |> add_optional "case_insensitive"
       (fun value -> Json.bool value)
       case_insensitive
  |> add_optional "context_lines" (fun value -> Json.int value) context_lines
  |> add_optional "offset" (fun value -> Json.int value) offset
  |> add_optional "limit" (fun value -> Json.int value) limit
  |> List.rev |> json_object

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

let write_file path contents =
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

let with_ripgrep_config contents fn =
  let path = Filename.temp_file "mentat-rg-config-" ".conf" in
  Fun.protect ~finally:(fun () -> remove_tree path) @@ fun () ->
  write_file path contents;
  with_env "RIPGREP_CONFIG_PATH" path fn

let populate_fixture ws_dir out_dir =
  write_file (Filename.concat ws_dir ".env") "alpha env\n";
  write_file (Filename.concat ws_dir ".gitignore") "ignored.txt\n";
  write_file (Filename.concat ws_dir "bad.bin") "alpha\000payload\n";
  write_file (Filename.concat ws_dir "bad-utf8.txt") "\255alpha\n";
  write_file (Filename.concat ws_dir "ignored.txt") "alpha ignored\n";
  write_file (Filename.concat ws_dir "notes.txt") "nothing here\n";
  write_file (Filename.concat ws_dir "-root") "alpha option-like root\n";
  write_file
    (Filename.concat ws_dir "unicode.txt")
    "caf\195\169 alpha \240\159\152\128\n";
  write_file (Filename.concat ws_dir "columns.txt") "xx alpha yy alpha\n";
  write_file
    (Filename.concat ws_dir "long.txt")
    (String.make 2_100 'x' ^ " alpha\n");
  write_file
    (Filename.concat ws_dir "utf8-cutoff.txt")
    (String.make 1_999 'x' ^ "\195\169 alpha\n");
  write_file (Filename.concat ws_dir "odd\n\".txt") "needle one\nneedle two\n";
  let docs = Filename.concat ws_dir "docs" in
  let lib = Filename.concat ws_dir "lib" in
  let src = Filename.concat ws_dir "src" in
  let git = Filename.concat ws_dir ".git" in
  List.iter mkdir [ docs; lib; src; git ];
  write_file (Filename.concat docs "readme.md") "Alpha heading\nplain text\n";
  write_file
    (Filename.concat lib "util.ml")
    "let helper = \"alpha\"\nlet second = \"alpha\"\n";
  write_file
    (Filename.concat src "app.ml")
    "let alpha = 1\nlet beta = alpha + 1\nlet gamma = beta\n";
  write_file (Filename.concat git "config") "alpha secret\n";
  write_file (Filename.concat out_dir "secret.txt") "alpha outside\n";
  Unix.mkfifo (Filename.concat ws_dir "pipe") 0o600;
  Unix.symlink "src" (Filename.concat ws_dir "src_link");
  Unix.symlink
    (Filename.concat out_dir "secret.txt")
    (Filename.concat ws_dir "outside_link")

let with_world ?(rg = Ambient) ?clock
    ?(mode = Mentat_config.Mode.Danger_full_access) fn =
  Eio_main.run @@ fun stdenv ->
  let stdenv = (stdenv :> Eio_unix.Stdenv.base) in
  let raw = Filename.temp_dir "mentat-search-text-" "" in
  Fun.protect ~finally:(fun () -> remove_tree raw) @@ fun () ->
  let base = Unix.realpath raw in
  let ws_dir = Filename.concat base "workspace" in
  let out_dir = Filename.concat base "outside" in
  let tmp_dir = Filename.concat base "tmp" in
  let bin_dir = Filename.concat base "bin" in
  List.iter mkdir [ ws_dir; out_dir; tmp_dir; bin_dir ];
  populate_fixture ws_dir out_dir;
  let path =
    match rg with
    | Ambient -> Sys.getenv "PATH"
    | Missing -> ""
    | Script script ->
        let executable = Filename.concat bin_dir "rg" in
        install_executable executable ("#!/bin/sh\n" ^ script ^ "\n");
        bin_dir
  in
  let logical = Workspace.single (Workspace.Root.of_dir (abs ws_dir)) in
  with_env "TMPDIR" tmp_dir @@ fun () ->
  with_env "PATH" path @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let io = resolve_exn ~sw ~stdenv ~logical ~mode () in
  let tool =
    match clock with
    | None -> Search_text.make io ~clock:(Eio.Stdenv.mono_clock stdenv)
    | Some clock -> Search_text.make io ~clock
  in
  fn { ws_dir; io; tool; sw }

let relative world path = Filename.concat world.ws_dir path

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
      let items = Permission.Request.items request in
      Printf.printf "items: %d\n" (List.length items);
      List.iter
        (fun item ->
          Printf.printf "change: %b\n"
            (Option.is_some (Permission.Request.Item.change item));
          match Permission.Request.Item.access item with
          | Permission.Access.Path
              {
                op = `Read;
                scope = Permission.Access.Path_scope.Workspace path;
              } ->
              Printf.printf "read: %s\n" (Workspace.Path.display path)
          | access ->
              failf "unexpected search permission: %a" Permission.Access.pp
                access)
        items)
    requests

let failure_summary result =
  match Tool.Result.status result with
  | Tool.Result.Failed { kind; message; _ } ->
      Printf.printf "failure: %s regex=%b glob=%b\n" (failure_name kind)
        (String.includes ~affix:"regex parse error" message)
        (String.includes ~affix:"error parsing glob" message)
  | Tool.Result.Completed -> print_endline "unexpected completion"
  | Tool.Result.Interrupted _ -> print_endline "unexpected interruption"

let print_bounded_failure result =
  match Tool.Result.status result with
  | Tool.Result.Failed { kind; message; _ } ->
      Printf.printf
        "failure=%s bytes=%d bounded=%b utf8=%b replacement=%b marker=%b\n"
        (failure_name kind) (String.length message)
        (String.length message <= 65_600)
        (String.is_valid_utf_8 message)
        (String.includes ~affix:"\239\191\189" message)
        (String.includes ~affix:"... ripgrep stderr truncated ..." message)
  | Tool.Result.Completed -> print_endline "unexpected completion"
  | Tool.Result.Interrupted _ -> print_endline "unexpected interruption"

let matching_preview output =
  let prefix = "  1: " in
  let suffix = " [truncated]" in
  match
    Tool.Output.text output |> String.split_on_char '\n'
    |> List.find_opt (String.starts_with ~prefix)
  with
  | None -> fail "matching result has no rendered line"
  | Some line ->
      let text =
        String.sub line (String.length prefix)
          (String.length line - String.length prefix)
      in
      if String.ends_with ~suffix text then
        String.sub text 0 (String.length text - String.length suffix)
      else fail "matching result is not marked truncated"

let%expect_test "declaration schema and provider decoding are strict" =
  with_world @@ fun world ->
  let declaration = Tool.declaration world.tool in
  Printf.printf "name: %s\n" (Mentat_llm.Tool.name declaration);
  Printf.printf "schema: %s\n"
    (json_string (Mentat_llm.Tool.input_schema declaration));
  decode_verdict world.tool "minimal" (input "alpha");
  decode_verdict world.tool "all fields"
    (input ~paths:[ "src"; "lib" ] ~glob:"*.ml" ~mode:"matches"
       ~case_insensitive:true ~context_lines:2 ~offset:3 ~limit:4 "Alpha");
  decode_verdict world.tool "missing pattern" (json_object []);
  decode_verdict world.tool "unknown member"
    (json_object
       [ ("pattern", Json.string "alpha"); ("extra", Json.bool true) ]);
  decode_verdict world.tool "duplicate pattern"
    (json_object
       [ ("pattern", Json.string "alpha"); ("pattern", Json.string "beta") ]);
  decode_verdict world.tool "duplicate optional member"
    (json_object
       [
         ("pattern", Json.string "alpha");
         ("limit", Json.int 1);
         ("limit", Json.int 2);
       ]);
  decode_verdict world.tool "empty pattern" (input "");
  decode_verdict world.tool "pattern NUL" (input "alpha\000beta");
  decode_verdict world.tool "empty paths" (input ~paths:[] "alpha");
  decode_verdict world.tool "empty path" (input ~paths:[ "" ] "alpha");
  decode_verdict world.tool "path NUL"
    (input ~paths:[ "src\000app.ml" ] "alpha");
  decode_verdict world.tool "empty glob" (input ~glob:"" "alpha");
  decode_verdict world.tool "glob NUL" (input ~glob:"*.ml\000*.mli" "alpha");
  decode_verdict world.tool "bad mode" (input ~mode:"lines" "alpha");
  decode_verdict world.tool "context outside matches"
    (input ~context_lines:1 "alpha");
  decode_verdict world.tool "context too large"
    (input ~mode:"matches" ~context_lines:6 "alpha");
  decode_verdict world.tool "offset zero" (input ~offset:0 "alpha");
  decode_verdict world.tool "limit zero" (input ~limit:0 "alpha");
  decode_verdict world.tool "limit too large" (input ~limit:1001 "alpha");
  decode_verdict world.tool "fractional offset"
    (json_object
       [ ("pattern", Json.string "alpha"); ("offset", Json.number 1.5) ]);
  decode_verdict world.tool "numeric-string limit"
    (json_object
       [ ("pattern", Json.string "alpha"); ("limit", Json.string "2") ]);
  decode_verdict world.tool "unsafe integer"
    (json_object
       [
         ("pattern", Json.string "alpha");
         ("offset", Json.number 9_007_199_254_740_992.);
       ]);
  [%expect
    {|
    name: search_text
    schema: {"type":"object","properties":{"pattern":{"type":"string","description":"Ripgrep/Rust regular expression to search for in UTF-8 text files.","minLength":1},"paths":{"type":"array","description":"Workspace-relative or workspace-contained absolute file or directory roots. Defaults to the workspace current directory.","items":{"type":"string","minLength":1},"minItems":1},"glob":{"type":"string","description":"Optional file glob, for example *.ml or **/*.ts.","minLength":1},"mode":{"type":"string","description":"Result mode. Defaults to files.","enum":["files","count","matches"]},"case_insensitive":{"type":"boolean","description":"Use case-insensitive regular-expression matching."},"context_lines":{"type":"integer","description":"Symmetric context lines around matches; valid only in matches mode.","minimum":0,"maximum":5},"offset":{"type":"integer","description":"One-based first result entry. Defaults to 1.","minimum":1,"maximum":9007199254740991},"limit":{"type":"integer","description":"Maximum result entries. Defaults to 100.","minimum":1,"maximum":1000}},"required":["pattern"],"additionalProperties":false}
    minimal: accepted canonical={"case_insensitive":false,"mode":"files","pattern":"alpha"}
    all fields: accepted canonical={"case_insensitive":true,"context_lines":2,"glob":"*.ml","limit":4,"mode":"matches","offset":3,"paths":["src","lib"],"pattern":"Alpha"}
    missing pattern: rejected diagnostic=true
    unknown member: rejected diagnostic=true
    duplicate pattern: rejected diagnostic=true
    duplicate optional member: rejected diagnostic=true
    empty pattern: rejected diagnostic=true
    pattern NUL: rejected diagnostic=true
    empty paths: rejected diagnostic=true
    empty path: rejected diagnostic=true
    path NUL: rejected diagnostic=true
    empty glob: rejected diagnostic=true
    glob NUL: rejected diagnostic=true
    bad mode: rejected diagnostic=true
    context outside matches: rejected diagnostic=true
    context too large: rejected diagnostic=true
    offset zero: rejected diagnostic=true
    limit zero: rejected diagnostic=true
    limit too large: rejected diagnostic=true
    fractional offset: rejected diagnostic=true
    numeric-string limit: rejected diagnostic=true
    unsafe integer: rejected diagnostic=true |}]

let%expect_test "permissions are exact distinct read scopes cached at decode" =
  with_world @@ fun world ->
  print_case "default root";
  print_permissions (decode_call world.tool (input "alpha"));
  print_case "plural roots preserve first distinct order";
  print_permissions
    (decode_call world.tool
       (input ~paths:[ "src/app.ml"; "lib"; "src/app.ml" ] "alpha"));
  print_case "contained absolute root";
  print_permissions
    (decode_call world.tool (input ~paths:[ relative world "src" ] "alpha"));
  print_case "lexical aliases collapse to one scope";
  print_permissions
    (decode_call world.tool (input ~paths:[ "src"; "./src" ] "alpha"));
  print_case "outside root";
  let outside =
    decode_call world.tool (input ~paths:[ "/outside-search" ] "alpha")
  in
  print_permissions outside;
  print_result (Tool.Call.run outside ~cancelled:(fun () -> false) |> finished);
  [%expect
    {|
    -- default root --
    requests: 1
    source: search_text
    items: 1
    change: false
    read: .
    -- plural roots preserve first distinct order --
    requests: 2
    source: search_text
    items: 1
    change: false
    read: src/app.ml
    source: search_text
    items: 1
    change: false
    read: lib
    -- contained absolute root --
    requests: 1
    source: search_text
    items: 1
    change: false
    read: src
    -- lexical aliases collapse to one scope --
    requests: 1
    source: search_text
    items: 1
    change: false
    read: src
    -- outside root --
    requests: 0
    status: failed invalid_input
    message: path is outside workspace: /outside-search
    metadata: false |}]

let%expect_test "files count matches context columns and case are durable" =
  with_world @@ fun world ->
  run_case ~json:true world "files"
    (input ~paths:[ ".env"; "lib"; "src/app.ml" ] "alpha");
  run_case ~json:true world "count"
    (input ~paths:[ "lib"; "src/app.ml" ] ~mode:"count" "alpha");
  run_case world "overlapping roots do not duplicate counts"
    (input ~paths:[ "src"; "src/app.ml" ] ~mode:"count" "alpha");
  run_case ~json:true world "matches with context and first column"
    (input ~paths:[ "src/app.ml" ] ~mode:"matches" ~context_lines:1 "alpha");
  run_case world "overlapping roots do not duplicate matches"
    (input ~paths:[ "src"; "src/app.ml" ] ~mode:"matches" "alpha");
  run_case ~json:true world "column is one based"
    (input ~paths:[ "columns.txt" ] ~mode:"matches" "alpha");
  run_case world "case sensitive"
    (input ~paths:[ "docs/readme.md"; "src/app.ml" ] "Alpha");
  run_case world "case insensitive"
    (input
       ~paths:[ "docs/readme.md"; "src/app.ml" ]
       ~case_insensitive:true "Alpha");
  [%expect
    {|-- files --
status: completed
text: "pattern=\"alpha\" mode=files results=3/3 offset=1 limit=100 status=complete\n.env\nlib/util.ml\nsrc/app.ml\n"
truncated: false
json: {"version":1,"shape":"files","total":3}
-- count --
status: completed
text: "pattern=\"alpha\" mode=count results=2/2 offset=1 limit=100 status=complete\nlib/util.ml matching_lines=2\nsrc/app.ml matching_lines=2\n"
truncated: false
json: {"version":1,"shape":"matching_lines","total":4}
-- overlapping roots do not duplicate counts --
status: completed
text: "pattern=\"alpha\" mode=count results=1/1 offset=1 limit=100 status=complete\nsrc/app.ml matching_lines=2\n"
truncated: false
-- matches with context and first column --
status: completed
text: "pattern=\"alpha\" mode=matches results=2/2 offset=1 limit=100 status=complete\nsrc/app.ml\n  1: let alpha = 1\n  2: let beta = alpha + 1\n  3- let gamma = beta\n"
truncated: false
json: {"version":1,"shape":"matches","total":2,"files":1}
-- overlapping roots do not duplicate matches --
status: completed
text: "pattern=\"alpha\" mode=matches results=2/2 offset=1 limit=100 status=complete\nsrc/app.ml\n  1: let alpha = 1\n  2: let beta = alpha + 1\n"
truncated: false
-- column is one based --
status: completed
text: "pattern=\"alpha\" mode=matches results=1/1 offset=1 limit=100 status=complete\ncolumns.txt\n  1: xx alpha yy alpha\n"
truncated: false
json: {"version":1,"shape":"matches","total":1,"files":1}
-- case sensitive --
status: completed
text: "pattern=\"Alpha\" mode=files results=1/1 offset=1 limit=100 status=complete\ndocs/readme.md\n"
truncated: false
-- case insensitive --
status: completed
text: "pattern=\"Alpha\" mode=files results=2/2 offset=1 limit=100 status=complete\ndocs/readme.md\nsrc/app.ml\n"
truncated: false|}]

let%expect_test "roots globs ignores dotfiles unicode and VCS metadata" =
  with_world @@ fun world ->
  run_case world "plural file and directory roots"
    (input ~paths:[ "lib"; "src/app.ml" ] "alpha");
  run_case world "contained absolute root"
    (input ~paths:[ relative world "src/app.ml" ] "alpha");
  run_case world "lexical aliases are deduplicated"
    (input ~paths:[ "src/app.ml"; "./src/app.ml" ] "alpha");
  run_case world "ml glob" (input ~glob:"*.ml" "alpha");
  run_case world "ordinary dotfile glob" (input ~glob:".*" "alpha");
  run_case world "ignored file stays ignored"
    (input ~paths:[ "." ] "alpha ignored");
  run_case world "protected metadata stays hidden" (input "secret");
  run_case world "explicit protected metadata stays hidden"
    (input ~paths:[ ".git" ] "secret");
  run_case world "user glob cannot re-include protected metadata"
    (input ~paths:[ "." ] ~glob:".git/**" "secret");
  run_case ~json:true world "valid UTF-8 preview"
    (input ~paths:[ "unicode.txt" ] ~mode:"matches" "alpha");
  [%expect
    {|-- plural file and directory roots --
status: completed
text: "pattern=\"alpha\" mode=files results=2/2 offset=1 limit=100 status=complete\nlib/util.ml\nsrc/app.ml\n"
truncated: false
-- contained absolute root --
status: completed
text: "pattern=\"alpha\" mode=files results=1/1 offset=1 limit=100 status=complete\nsrc/app.ml\n"
truncated: false
-- lexical aliases are deduplicated --
status: completed
text: "pattern=\"alpha\" mode=files results=1/1 offset=1 limit=100 status=complete\nsrc/app.ml\n"
truncated: false
-- ml glob --
status: completed
text: "pattern=\"alpha\" mode=files results=2/2 offset=1 limit=100 status=complete\nlib/util.ml\nsrc/app.ml\n"
truncated: false
-- ordinary dotfile glob --
status: completed
text: "pattern=\"alpha\" mode=files results=1/1 offset=1 limit=100 status=complete\n.env\n"
truncated: false
-- ignored file stays ignored --
status: completed
text: "pattern=\"alpha ignored\" mode=files results=0/0 offset=1 limit=100 status=complete\nNo matches\n"
truncated: false
-- protected metadata stays hidden --
status: completed
text: "pattern=\"secret\" mode=files results=0/0 offset=1 limit=100 status=complete\nNo matches\n"
truncated: false
-- explicit protected metadata stays hidden --
status: completed
text: "pattern=\"secret\" mode=files results=0/0 offset=1 limit=100 status=complete\nNo matches\n"
truncated: false
-- user glob cannot re-include protected metadata --
status: completed
text: "pattern=\"secret\" mode=files results=0/0 offset=1 limit=100 status=complete\nNo matches\n"
truncated: false
-- valid UTF-8 preview --
status: completed
text: "pattern=\"alpha\" mode=matches results=1/1 offset=1 limit=100 status=complete\nunicode.txt\n  1: caf\195\169 alpha \240\159\152\128\n"
truncated: false
json: {"version":1,"shape":"matches","total":1,"files":1}|}]

let%expect_test "pagination continuations advance in every result mode" =
  with_world @@ fun world ->
  let first =
    run world (input ~paths:[ ".env"; "lib"; "src" ] ~limit:2 "alpha")
  in
  print_case "files first page";
  print_result ~json:true first;
  let next = input ~paths:[ ".env"; "lib"; "src" ] ~offset:3 ~limit:2 "alpha" in
  Printf.printf "canonical next: %s\n"
    (json_string (Tool.Call.input (decode_call world.tool next)));
  print_case "files final page";
  print_result (run world next);
  run_case ~json:true world "past end"
    (input ~paths:[ ".env"; "lib"; "src" ] ~offset:99 ~limit:2 "alpha");
  run_case ~json:true world "count first page"
    (input ~paths:[ ".env"; "lib"; "src" ] ~mode:"count" ~limit:1 "alpha");
  run_case ~json:true world "matches first page keeps adjacent context"
    (input ~paths:[ "src/app.ml" ] ~mode:"matches" ~context_lines:1 ~limit:1
       "alpha");
  run_case ~json:true world "matches second page"
    (input ~paths:[ "src/app.ml" ] ~mode:"matches" ~context_lines:1 ~offset:2
       ~limit:1 "alpha");
  [%expect
    {|-- files first page --
status: completed
text: "pattern=\"alpha\" mode=files results=2/3 offset=1 limit=2 status=partial\n.env\nlib/util.ml\nnext: search_text {\"pattern\":\"alpha\",\"paths\":[\".env\",\"lib\",\"src\"],\"mode\":\"files\",\"case_insensitive\":false,\"offset\":3,\"limit\":2}\n"
truncated: true
json: {"version":1,"shape":"files","total":3}
canonical next: {"case_insensitive":false,"limit":2,"mode":"files","offset":3,"paths":[".env","lib","src"],"pattern":"alpha"}
-- files final page --
status: completed
text: "pattern=\"alpha\" mode=files results=1/3 offset=3 limit=2 status=complete\nsrc/app.ml\n"
truncated: false
-- past end --
status: completed
text: "pattern=\"alpha\" mode=files results=0/3 offset=99 limit=2 status=complete\nNo matches\n"
truncated: false
json: {"version":1,"shape":"files","total":3}
-- count first page --
status: completed
text: "pattern=\"alpha\" mode=count results=1/3 offset=1 limit=1 status=partial\n.env matching_lines=1\nnext: search_text {\"pattern\":\"alpha\",\"paths\":[\".env\",\"lib\",\"src\"],\"mode\":\"count\",\"case_insensitive\":false,\"offset\":2,\"limit\":1}\n"
truncated: true
json: {"version":1,"shape":"matching_lines","total":5}
-- matches first page keeps adjacent context --
status: completed
text: "pattern=\"alpha\" mode=matches results=1/2 offset=1 limit=1 status=partial\nsrc/app.ml\n  1: let alpha = 1\nnext: search_text {\"pattern\":\"alpha\",\"paths\":[\"src/app.ml\"],\"mode\":\"matches\",\"case_insensitive\":false,\"context_lines\":1,\"offset\":2,\"limit\":1}\n"
truncated: true
json: {"version":1,"shape":"matches","total":2,"files":1}
-- matches second page --
status: completed
text: "pattern=\"alpha\" mode=matches results=1/2 offset=2 limit=1 status=complete\nsrc/app.ml\n  2: let beta = alpha + 1\n  3- let gamma = beta\n"
truncated: false
json: {"version":1,"shape":"matches","total":2,"files":1}|}]

let%expect_test "continuation text escapes regex and path metacharacters" =
  with_world @@ fun world ->
  run_case world "special continuation"
    (input ~paths:[ "odd\n\".txt" ] ~mode:"matches" ~limit:1 "needle|\"");
  [%expect
    {|
    -- special continuation --
    status: completed
    text: "pattern=\"needle|\\\"\" mode=matches results=1/2 offset=1 limit=1 status=partial\nodd\n\".txt\n  1: needle one\nnext: search_text {\"pattern\":\"needle|\\\"\",\"paths\":[\"odd\\n\\\".txt\"],\"mode\":\"matches\",\"case_insensitive\":false,\"context_lines\":0,\"offset\":2,\"limit\":1}\n"
    truncated: true |}]

let%expect_test "binary invalid UTF-8 and long previews report omission once" =
  with_world @@ fun world ->
  run_case ~json:true world "skipped unsafe evidence"
    (input ~paths:[ "bad.bin"; "bad-utf8.txt"; "src/app.ml" ] "alpha");
  print_case "long matching line";
  let long = run world (input ~paths:[ "long.txt" ] ~mode:"matches" "alpha") in
  let output = output_exn long in
  let text = matching_preview output in
  Printf.printf "status completed: %b\n"
    (match Tool.Result.status long with
    | Tool.Result.Completed -> true
    | _ -> false);
  Printf.printf "preview bytes: %d\n" (String.length text);
  Printf.printf "preview contains omitted match: %b\n"
    (String.includes ~affix:"alpha" text);
  let names =
    Tool.Output.json output
    |> Option.value ~default:(Json.null ())
    |> json_member_names
  in
  Printf.printf "semantic JSON duplicates preview: %b\n" (List.mem "text" names);
  Printf.printf "output truncated: %b\n" (Tool.Output.truncated output);
  print_case "UTF-8 cutoff";
  let utf8 =
    run world (input ~paths:[ "utf8-cutoff.txt" ] ~mode:"matches" "alpha")
    |> output_exn |> matching_preview
  in
  Printf.printf "preview bytes: %d\n" (String.length utf8);
  Printf.printf "preview valid UTF-8: %b\n" (String.is_valid_utf_8 utf8);
  [%expect
    {|-- skipped unsafe evidence --
status: completed
text: "pattern=\"alpha\" mode=files results=1/1 offset=1 limit=100 status=complete\nsrc/app.ml\nskipped:\n  bad-utf8.txt reason=invalid_utf8\n  bad.bin reason=binary\n"
truncated: true
json: {"version":1,"shape":"files","total":1}
-- long matching line --
status completed: true
preview bytes: 2000
preview contains omitted match: false
semantic JSON duplicates preview: false
output truncated: true
-- UTF-8 cutoff --
preview bytes: 1999
preview valid UTF-8: true|}]

let%expect_test
    "root confinement rejects missing symlink special and outside paths" =
  with_world @@ fun world ->
  run_case world "missing" (input ~paths:[ "missing" ] "alpha");
  run_case world "in-workspace symlink" (input ~paths:[ "src_link" ] "alpha");
  run_case world "escaping symlink" (input ~paths:[ "outside_link" ] "alpha");
  run_case world "special file" (input ~paths:[ "pipe" ] "alpha");
  run_case world "outside absolute path"
    (input ~paths:[ "/outside-search-root" ] "alpha");
  [%expect
    {|
    -- missing --
    status: failed not_found
    message: missing: path does not exist
    metadata: false
    -- in-workspace symlink --
    status: failed invalid_input
    message: src_link: symlink search roots are not supported
    metadata: false
    -- escaping symlink --
    status: failed invalid_input
    message: outside_link: symlink search roots are not supported
    metadata: false
    -- special file --
    status: failed invalid_input
    message: pipe: expected a regular file or directory
    metadata: false
    -- outside absolute path --
    status: failed invalid_input
    message: path is outside workspace: /outside-search-root
    metadata: false |}]

let%expect_test
    "ripgrep validation config isolation and missing executable are explicit" =
  with_world (fun world ->
      print_case "bad regex";
      failure_summary (run world (input "["));
      print_case "bad glob";
      failure_summary (run world (input ~glob:"[" "alpha")));
  with_ripgrep_config "--ignore-case\n" (fun () ->
      with_world (fun configured ->
          run_case configured "config disabled"
            (input ~paths:[ "docs/readme.md"; "src/app.ml" ] "Alpha")));
  with_world ~rg:Missing (fun missing ->
      run_case missing "missing ripgrep" (input "alpha"));
  with_world ~rg:(Script "exit 1") (fun no_matches ->
      run_case no_matches "rg exit 1 means no matches" (input "alpha"));
  with_world ~rg:(Script "printf 'rejected request\\n' >&2; exit 2")
    (fun rejected ->
      run_case rejected "rg exit 2 is invalid input" (input "alpha"));
  with_world ~rg:(Script "printf 'engine failed\\n' >&2; exit 7") (fun failed ->
      run_case failed "other rg exits fail" (input "alpha"));
  [%expect
    {|
    -- bad regex --
    failure: invalid_input regex=true glob=false
    -- bad glob --
    failure: invalid_input regex=false glob=true
    -- config disabled --
    status: completed
    text: "pattern=\"Alpha\" mode=files results=1/1 offset=1 limit=100 status=complete\ndocs/readme.md\n"
    truncated: false
    -- missing ripgrep --
    status: failed failed
    message: ripgrep executable not found; search_text requires rg in PATH
    metadata: false
    -- rg exit 1 means no matches --
    status: completed
    text: "pattern=\"alpha\" mode=files results=0/0 offset=1 limit=100 status=complete\nNo matches\n"
    truncated: false
    -- rg exit 2 is invalid input --
    status: failed invalid_input
    message: rejected request
    metadata: false
    -- other rg exits fail --
    status: failed failed
    message: ripgrep exited with status 7: engine failed
    metadata: false |}]

let%expect_test "ripgrep receives injection-shaped values as exact argv tokens"
    =
  let script =
    {|
printf 'cwd=<%s>\n' "${PWD##*/}" >&2
for argument do
  printf 'arg=<%s>\n' "$argument" >&2
done
exit 2|}
  in
  with_world ~rg:(Script script) (fun world ->
      let before = tree_snapshot world.ws_dir "" in
      run_case world "argv"
        (input ~paths:[ "-root" ] ~glob:"*.ml; echo glob-injected"
           "alpha;$(touch pattern-injected)");
      let after = tree_snapshot world.ws_dir "" in
      Printf.printf "tree unchanged: %b\n"
        (List.equal String.equal before after));
  [%expect
    {|
    -- argv --
    status: failed invalid_input
    message: cwd=<workspace>
    arg=<--json>
    arg=<--hidden>
    arg=<--no-config>
    arg=<--no-require-git>
    arg=<--no-messages>
    arg=<--color>
    arg=<never>
    arg=<--line-number>
    arg=<--with-filename>
    arg=<--glob>
    arg=<*.ml; echo glob-injected>
    arg=<--glob>
    arg=<!.git/**>
    arg=<--glob>
    arg=<!**/.git/**>
    arg=<--glob>
    arg=<!.svn/**>
    arg=<--glob>
    arg=<!**/.svn/**>
    arg=<--glob>
    arg=<!.hg/**>
    arg=<--glob>
    arg=<!**/.hg/**>
    arg=<--glob>
    arg=<!.bzr/**>
    arg=<--glob>
    arg=<!**/.bzr/**>
    arg=<--glob>
    arg=<!.jj/**>
    arg=<--glob>
    arg=<!**/.jj/**>
    arg=<--glob>
    arg=<!.sl/**>
    arg=<--glob>
    arg=<!**/.sl/**>
    arg=<-->
    arg=<alpha;$(touch pattern-injected)>
    arg=<-root>
    metadata: false
    tree unchanged: true |}]

let%expect_test "ripgrep runs through the workspace command confinement" =
  with_world ~mode:Mentat_config.Mode.Read_only
    ~rg:(Script "printf 'changed\\n' > src/app.ml 2>/dev/null; exit 1")
  @@ fun world ->
  let path = relative world "src/app.ml" in
  let before = In_channel.with_open_bin path In_channel.input_all in
  run_case world "read-only search" (input ~paths:[ "src/app.ml" ] "alpha");
  let after = In_channel.with_open_bin path In_channel.input_all in
  Printf.printf "file unchanged: %b\n" (String.equal before after);
  [%expect
    {|
    -- read-only search --
    status: completed
    text: "pattern=\"alpha\" mode=files results=0/0 offset=1 limit=100 status=complete\nNo matches\n"
    truncated: false
    file unchanged: true |}]

let%expect_test "ripgrep byte payloads are distinct from malformed payloads" =
  with_world
    ~rg:
      (Script
         "printf '%s\\n' \
          '{\"type\":\"match\",\"data\":{\"path\":{\"text\":\"src/app.ml\"},\"lines\":{\"bytes\":\"/2FscGhhCg==\"},\"line_number\":1}}'")
    (fun byte_lines ->
      run_case byte_lines "byte-encoded lines are skipped" (input "alpha"));
  with_world
    ~rg:
      (Script
         "printf '%s\\n' \
          '{\"type\":\"match\",\"data\":{\"path\":{\"bytes\":\"c3JjL2FwcC5tbA==\"},\"lines\":{\"text\":\"alpha\"},\"line_number\":1}}'")
    (fun byte_path ->
      run_case byte_path "byte-encoded path is unrepresentable" (input "alpha"));
  [%expect
    {|
    -- byte-encoded lines are skipped --
    status: completed
    text: "pattern=\"alpha\" mode=files results=0/0 offset=1 limit=100 status=complete\nNo matches\nskipped:\n  src/app.ml reason=invalid_utf8\n"
    truncated: true
    -- byte-encoded path is unrepresentable --
    status: completed
    text: "pattern=\"alpha\" mode=files results=0/0 offset=1 limit=100 status=complete\nNo matches\n"
    truncated: false |}]

let%expect_test
    "command cancellation timeout malformed output and caps are observable" =
  with_world (fun world ->
      run_case
        ~cancelled:(fun () -> true)
        world "cancelled before observation" (input "alpha");
      let calls = ref 0 in
      let cancelled_during_roots () =
        incr calls;
        !calls >= 3
      in
      run_case ~cancelled:cancelled_during_roots world "cancelled between roots"
        (input ~paths:[ "src"; "lib" ] "alpha"));
  with_world ~rg:(Script "exec /bin/sleep 5") (fun sleeping ->
      let polls = ref 0 in
      let cancelled_during_command () =
        incr polls;
        !polls >= 3
      in
      run_case ~cancelled:cancelled_during_command sleeping "cancelled command"
        (input "alpha"));
  with_world ~rg:(Script "printf 'not-json\\n'") (fun malformed ->
      run_case malformed "malformed rg JSON" (input "alpha"));
  with_world ~rg:(Script "printf '%s\\n' '{\"type\":\"match\",\"data\":{}}'")
    (fun malformed ->
      run_case malformed "malformed match event" (input "alpha"));
  with_world
    ~rg:
      (Script
         "printf '%s\\n' \
          '{\"type\":\"match\",\"data\":{\"path\":\"src/app.ml\",\"lines\":{\"text\":\"alpha\"},\"line_number\":1}}'")
    (fun malformed ->
      run_case malformed "malformed path payload" (input "alpha"));
  with_world
    ~rg:
      (Script
         "printf '%s\\n' \
          '{\"type\":\"match\",\"data\":{\"path\":{\"text\":\"src/app.ml\"},\"lines\":{},\"line_number\":1}}'")
    (fun malformed ->
      run_case malformed "malformed lines payload" (input "alpha"));
  with_world
    ~rg:
      (Script
         "printf '%s\\n' \
          '{\"type\":\"match\",\"data\":{\"path\":{\"text\":\"src/app.ml\"},\"path\":{\"text\":\"lib/util.ml\"},\"lines\":{\"text\":\"alpha\"},\"line_number\":1}}'")
    (fun malformed ->
      print_case "duplicate path member";
      match Tool.Result.status (run malformed (input "alpha")) with
      | Tool.Result.Failed { kind; message; _ } ->
          Printf.printf "failure=%s duplicate=%b path=%b\n" (failure_name kind)
            (String.includes ~affix:"duplicate"
               (String.lowercase_ascii message))
            (String.includes ~affix:"path" (String.lowercase_ascii message))
      | Tool.Result.Completed -> print_endline "unexpected completion"
      | Tool.Result.Interrupted _ -> print_endline "unexpected interruption");
  with_world ~rg:(Script "exec /usr/bin/yes x") (fun overflowing ->
      run_case overflowing "bounded command output" (input "alpha"));
  with_world
    ~rg:
      (Script
         "exec /usr/bin/yes diagnostic | /usr/bin/head -c 70000 >&2; exit 7")
    (fun verbose ->
      print_case "bounded stderr diagnostic";
      print_bounded_failure (run verbose (input "alpha")));
  with_world ~rg:(Script "printf '\\377bad\\n' >&2; exit 7") (fun invalid ->
      print_case "invalid UTF-8 stderr diagnostic";
      print_bounded_failure (run invalid (input "alpha")));
  let mock_clock = Eio_mock.Clock.Mono.make () in
  Eio_mock.Clock.Mono.set_time mock_clock (Mtime.of_uint64_ns 0L);
  with_world ~rg:(Script "exec /bin/sleep 5") ~clock:mock_clock (fun timed ->
      let rec advance_when_scheduled () =
        Eio.Fiber.yield ();
        if Eio_mock.Clock.Mono.try_advance mock_clock then `Stop_daemon
        else advance_when_scheduled ()
      in
      Eio.Fiber.fork_daemon ~sw:timed.sw advance_when_scheduled;
      run_case timed "timed out command" (input "alpha"));
  [%expect
    {|
    +mock time is now 0
    +mock time is now 60
    -- cancelled before observation --
    status: interrupted cancelled=true
    reason: tool call cancelled
    -- cancelled between roots --
    status: interrupted cancelled=true
    reason: tool call cancelled
    -- cancelled command --
    status: interrupted cancelled=true
    reason: tool call cancelled
    -- malformed rg JSON --
    status: failed failed
    message: rg JSON: Expected u while parsing null but found: o
    File "-", line 1, characters 0-2:
    metadata: false
    -- malformed match event --
    status: failed failed
    message: rg JSON: malformed match event (missing path or lines)
    metadata: false
    -- malformed path payload --
    status: failed failed
    message: rg JSON: malformed match event (invalid path payload)
    metadata: false
    -- malformed lines payload --
    status: failed failed
    message: rg JSON: malformed match event (invalid lines payload)
    metadata: false
    -- duplicate path member --
    failure=failed duplicate=true path=true
    -- bounded command output --
    status: failed failed
    message: ripgrep stdout exceeded internal output limit
    metadata: false
    -- bounded stderr diagnostic --
    failure=failed bytes=65566 bounded=true utf8=true replacement=false marker=true
    -- invalid UTF-8 stderr diagnostic --
    failure=failed bytes=36 bounded=true utf8=true replacement=true marker=false
    -- timed out command --
    status: failed timed_out
    message: ripgrep timed out after 60000ms
    metadata: false |}]

let%expect_test "search is read-only and durable replay preserves presentation"
    =
  with_world @@ fun world ->
  let before = tree_snapshot world.ws_dir "" in
  let scope = Wio.open_claim_scope world.io in
  let result =
    run world
      (input ~paths:[ "src/app.ml" ] ~mode:"matches" ~context_lines:1 "alpha")
  in
  let output = output_exn result in
  let durable = encode result_codec result in
  let replayed = decode result_codec durable in
  let replayed_output = output_exn replayed in
  let frame = Option.get (Tool.Output.json output) in
  let names = json_member_names frame in
  Printf.printf "version: %s\n"
    (json_string (Option.get (json_member "version" frame)));
  Printf.printf "shape: %s\n"
    (json_string (Option.get (json_member "shape" frame)));
  Printf.printf "durable result: %s\n" (json_string durable);
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
    [ "receipt"; "mutation"; "evidence"; "anchor"; "command"; "stdout" ];
  let evidence = Wio.Claim_scope.close scope in
  let after = tree_snapshot world.ws_dir "" in
  Printf.printf "tree unchanged: %b\n" (List.equal String.equal before after);
  Printf.printf "mutation applies: %d\n"
    (List.length evidence.Mentat_edit.Apply_evidence.applies);
  Printf.printf "attributed opaque observations: %d\n"
    (List.length evidence.Mentat_edit.Apply_evidence.observed);
  [%expect
    {|version: 1
shape: "matches"
durable result: {"status":"completed","output":{"text":"pattern=\"alpha\" mode=matches results=2/2 offset=1 limit=100 status=complete\nsrc/app.ml\n  1: let alpha = 1\n  2: let beta = alpha + 1\n  3- let gamma = beta\n","json":{"version":1,"shape":"matches","total":2,"files":1},"truncated":false}}
text replay equal: true
json replay equal: true
truncated replay equal: true
contains receipt: false
contains mutation: false
contains evidence: false
contains anchor: false
contains command: false
contains stdout: false
tree unchanged: true
mutation applies: 0
attributed opaque observations: 0|}]

[%%run_tests "mentat.tools.search_text"]
