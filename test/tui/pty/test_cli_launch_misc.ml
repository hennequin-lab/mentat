(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
open Cli_launch_fixture
module Pty = Pty_session

let%expect_test
    "file completion stays inside cached read authority across a directory race"
    =
  Project.with_temp "cli-enumeration-authority" @@ fun project ->
  let swap = Project.path project "swap" in
  let outside = Project.scratch project "outside" in
  Unix.mkdir swap 0o700;
  (* Completion derives a directory row from the enumerated files beneath it, so
     [swap] is only completable while it holds one. The file is removed again
     before the swap, which is what leaves the directory empty enough to
     replace. *)
  Project.write project "swap/inside-marker" "must stay inside\n";
  Project.write_scratch project "outside/leak-marker" "must stay outside\n";
  Unix.mkdir (Project.path project ".git") 0o700;
  Unix.mkfifo (Project.path project "pipe-marker") 0o600;
  Unix.symlink outside (Project.path project "linked-marker");
  Pty.run project @@ fun terminal ->
  Pty.send terminal "@";
  Pty.wait terminal (Screen.has "swap/");
  Screen.print ~project (Pty.screen terminal);
  [%expect
    {|01 |
02 |
03 |
04 |
05 |                           █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
06 |                           █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
07 |
08 |                          dev · openai/gpt-5.6-sol medium
09 |
10 |      ▎ welcome — and thanks for trying mentat this early.
11 |      ▎ it's experimental: sessions and config may change without migration.
12 |
13 | ❯ +  swap/
14 |   +  dune-project
15 | ────────────────────────────────────────────────────────────────────────────────
16 | ❯ @
17 | ────────────────────────────────────────────────────────────────────────────────
18 |
19 |                          ! /login — no connected account
20 |                               ∅ no recent sessions
21 |
22 |
23 |
24 |   ! not logged in · /login · ~/men… · openai/gpt-5.6-sol … · ! full access ? …|}];
  Pty.send terminal "swap";
  Pty.wait terminal (Screen.has "❯ @swap");
  (* The race: the enumerated directory becomes a symlink out of the workspace
     while a token that named it is still open. One enumeration serves the whole
     token, so this cannot retroactively widen the listing already on screen;
     what it must not do is widen the next one. Retyping the token from scratch
     is what asks for that next enumeration. *)
  Unix.unlink (Project.path project "swap/inside-marker");
  Unix.rmdir swap;
  Unix.symlink outside swap;
  Pty.send terminal (String.concat "" (List.init 5 (fun _ -> Key.backspace)));
  Pty.wait terminal (Screen.has "❯ message mentat");
  Pty.send terminal "@";
  Pty.wait terminal (fun screen ->
      Screen.contains screen "dune-project"
      && not (Screen.contains screen "swap/"));
  Screen.print ~project (Pty.screen terminal);
  [%expect
    {|01 |
02 |
03 |
04 |
05 |                           █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
06 |                           █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
07 |
08 |                          dev · openai/gpt-5.6-sol medium
09 |
10 |      ▎ welcome — and thanks for trying mentat this early.
11 |      ▎ it's experimental: sessions and config may change without migration.
12 |
13 | ❯ +  dune-project
14 | ────────────────────────────────────────────────────────────────────────────────
15 | ❯ @
16 | ────────────────────────────────────────────────────────────────────────────────
17 |
18 |                          ! /login — no connected account
19 |                               ∅ no recent sessions
20 |
21 |
22 |
23 |
24 |   ! not logged in · /login · ~/men… · openai/gpt-5.6-sol … · ! full access ? …|}];
  Pty.send terminal "\003";
  Pty.wait terminal (fun screen -> Screen.contains screen "❯ message mentat");
  Pty.quit terminal

(* The TUI owns the terminal, so its always-on log is diverted to a per-run file
   under the state home instead of the screen. A launch must leave both that
   [<run>.log] and the [latest.json] pointer behind, discoverable from the
   isolated state home without a shared-sink override. *)
let%expect_test "launch diverts the TUI log to the state home" =
  Project.with_temp "cli-run-log" @@ fun project ->
  Pty.run ~unset:[ "MENTAT_LOG"; "MENTAT_LOG_FILE" ] project @@ fun terminal ->
  Pty.settle terminal;
  let logs_dir = Project.state project "logs" in
  let entries =
    try Array.to_list (Sys.readdir logs_dir) with Sys_error _ -> []
  in
  let run_logs =
    List.filter (fun name -> Filename.check_suffix name ".log") entries
  in
  Printf.printf "latest.json present: %b\n" (List.mem "latest.json" entries);
  Printf.printf "per-run log present: %b\n" (run_logs <> []);
  Pty.quit terminal;
  [%expect {|
    latest.json present: true
    per-run log present: true |}]

(* A session opened inside the terminal never crosses the launch boundary, so
   the runtime — not the launch path — owes its identity to the diagnostics
   breadcrumb. Without it every line and every crash report of an ordinary
   session reads [session=-], and [mentat debug session] can correlate nothing.
   The boundary line naming the full id is emitted at info, which is why the
   terminal's default level is info: at warning this file records faults with
   nothing to attribute them to. Asserted at the shipped default, with both
   [MENTAT_LOG] and [MENTAT_LOG_FILE] unset, since an explicit level is exactly
   what masked this. *)
let%expect_test "a session opened in the terminal attributes its own log" =
  let prompt = "explain the attribution path" in
  let answer = "The session named itself in the log." in
  Project.with_temp "cli-log-attribution" @@ fun project ->
  with_provider project ~prompt ~answer @@ fun base_url ->
  Pty.run
    ~env:
      [ ("OPENAI_API_KEY", "test-key"); ("MENTAT_OPENAI_BASE_URL", base_url) ]
    ~unset:[ "MENTAT_LOG"; "MENTAT_LOG_FILE" ]
    ~command:[ "--prompt"; prompt ]
    ~ready:(fun screen ->
      Screen.contains screen answer && Screen.contains screen "❯ message mentat")
    project
  @@ fun terminal ->
  Pty.settle terminal;
  Pty.quit terminal;
  let logs_dir = Project.state project "logs" in
  let entries =
    try Array.to_list (Sys.readdir logs_dir) with Sys_error _ -> []
  in
  let contents =
    entries
    |> List.filter (fun name -> Filename.check_suffix name ".log")
    |> List.map (fun name -> Project.read_path (Filename.concat logs_dir name))
    |> String.concat ""
  in
  let contains substring =
    let width = String.length substring in
    let rec at index =
      index + width <= String.length contents
      && (String.equal (String.sub contents index width) substring
         || at (index + 1))
    in
    at 0
  in
  (* The boundary line carries the full id so a reader can expand the truncated
     per-line tag; the tag proves later lines are attributed too. *)
  Printf.printf "opened boundary line: %b\n" (contains "session s-");
  Printf.printf "per-line session tag: %b\n" (contains "[s=s-");
  [%expect {|
    opened boundary line: true
    per-line session tag: true |}]

(* A committed baseline against a real worktree, driven through the git review
   loader over the executable boundary. Three review units land 0/4 pending on
   open: [config.ml]'s single edit is one hunk, [engine.ml]'s two edits
   twenty-five lines apart split into two hunks at git's context-3, and the
   untracked [notes.ml] is one Added unit. *)
let config_base =
  "let host = \"localhost\"\n\
   let port = 8080\n\
   let debug = false\n\
   let retries = 3\n"

let config_tip =
  "let host = \"localhost\"\n\
   let port = 9090\n\
   let debug = false\n\
   let retries = 3\n"

let engine_lines =
  List.init 34 (fun i -> Printf.sprintf "let v%02d = %d" (i + 1) (i + 1))

let engine_base = String.concat "\n" engine_lines ^ "\n"

let engine_tip =
  engine_lines
  |> List.mapi (fun i line ->
      if i = 4 then "let v05 = 500"
      else if i = 29 then "let v30 = 3000"
      else line)
  |> String.concat "\n"
  |> fun body -> body ^ "\n"

let notes_new = "release checklist\n- run the tests\n- tag the version\n"

(* The git review loader resolves the base spec to a full commit hash — the one
   machine-varying token in the [Review  <base>..WORKTREE] header. Normalize just
   that hash so the header stays a stable golden while progress, the file list,
   and the diff body remain asserted verbatim. *)
let censor_review_base screen =
  let prefix = "Review  " and suffix = "..WORKTREE" in
  match find_substring screen prefix 0 with
  | None -> screen
  | Some start -> (
      let hash_start = start + String.length prefix in
      match find_substring screen suffix hash_start with
      | None -> screen
      | Some hash_end ->
          String.sub screen 0 hash_start
          ^ "$BASE"
          ^ String.sub screen hash_end (String.length screen - hash_end))

(* The review PTY golden: [/review] loads the real git worktree diff through the
   executable, the nav steps file-to-file, and focusing the diff splits the
   twenty-five-line-apart edits into two hunks. The base commit hash is the only
   machine-varying token, censored above; every other row is the real frame. *)
let%expect_test
    "/review renders the real git worktree diff, steps files, and splits hunks"
    =
  Project.with_temp "cli-review-visual" @@ fun project ->
  Project.write project "lib/config.ml" config_base;
  Project.write project "lib/engine.ml" engine_base;
  Project.git_baseline project;
  Project.write project "lib/config.ml" config_tip;
  Project.write project "lib/engine.ml" engine_tip;
  Project.write project "lib/notes.ml" notes_new;
  Pty.run project @@ fun terminal ->
  Pty.send terminal "/review";
  Pty.wait terminal (Screen.has "/review");
  Pty.send terminal "\r";
  (* Content-anchored on the header, the progress line, and the diff-pane body so
     a mid-load frame is never captured on the real-git flow. *)
  Pty.wait terminal (fun screen ->
      Screen.contains screen "0/4 reviewed · pending"
      && Screen.contains screen "lib/config.ml"
      && Screen.contains screen "let host");
  Pty.settle terminal;
  Pty.screen terminal |> censor_review_base |> Screen.print ~project;
  [%expect
    {|01 | ────────────────────────────────────────────────────────────────────────────────
02 | Review  $BASE..WORKTREE0/4 reviewed · pending
03 |  ▾ lib                          │lib/config.ml · unreviewed · +1 −1
04 |    ❯ [ ] config.ml            M │ 1   let host = "localhost"
05 |      [ ] engine.ml            M │ 2 - let port = 8080
06 |      [ ] notes.ml             A │ 2 + let port = 9090
07 |                                 │ 3   let debug = false
08 |                                 │ 4   let retries = 3
09 |                                 │
10 |                                 │
11 |                                 │
12 |                                 │
13 |                                 │
14 |                                 │
15 |                                 │
16 |                                 │
17 |                                 │
18 |                                 │
19 |                                 │
20 |                                 │
21 |                                 │
22 |                                 │
23 |
24 | tab focus diff · space mark · enter open · c comment · a approve · esc close|}];
  (* Down steps the nav to the second file — never into the diff — and the diff
     pane reloads for it. This pins the repaired focus model at the pty level. *)
  Pty.send terminal Key.down;
  Pty.wait terminal (fun screen ->
      Screen.contains screen "lib/engine.ml" && Screen.contains screen "let v05");
  Pty.settle terminal;
  Pty.screen terminal |> censor_review_base |> Screen.print ~project;
  [%expect
    {|01 | ────────────────────────────────────────────────────────────────────────────────
02 | Review  $BASE..WORKTREE0/4 reviewed · pending
03 |  ▾ lib                          │lib/engine.ml · unreviewed · +2 −2
04 |      [ ] config.ml            M │  2   let v02 = 2
05 |    ❯ [ ] engine.ml            M │  3   let v03 = 3
06 |      [ ] notes.ml             A │  4   let v04 = 4
07 |                                 │  5 - let v05 = 5
08 |                                 │  5 + let v05 = 500
09 |                                 │  6   let v06 = 6
10 |                                 │  7   let v07 = 7
11 |                                 │  8   let v08 = 8
12 |                                 │ 27   let v27 = 27
13 |                                 │ 28   let v28 = 28
14 |                                 │ 29   let v29 = 29
15 |                                 │ 30 - let v30 = 30
16 |                                 │ 30 + let v30 = 3000
17 |                                 │ 31   let v31 = 31
18 |                                 │ 32   let v32 = 32
19 |                                 │ 33   let v33 = 33
20 |                                 │
21 |                                 │
22 |                                 │
23 |
24 | tab focus diff · space mark · enter open · c comment · a approve · esc close|}];
  (* Enter focuses the diff; the scope line reports [hunk 1/2], the git context-3
     split of the two separated edits. *)
  Pty.send terminal "\r";
  Pty.wait terminal (Screen.has "hunk 1/2");
  Pty.settle terminal;
  Pty.screen terminal |> censor_review_base |> Screen.print ~project;
  [%expect
    {|01 | ────────────────────────────────────────────────────────────────────────────────
02 | Review  $BASE..WORKTREE0/4 reviewed · pending
03 |  ▾ lib                          │lib/engine.ml · hunk 1/2 · unreviewed · +2 −2
04 |      [ ] config.ml            M │   2   let v02 = 2
05 |    ❯ [ ] engine.ml            M │   3   let v03 = 3
06 |      [ ] notes.ml             A │   4   let v04 = 4
07 |                                 │❯  5 - let v05 = 5
08 |                                 │   5 + let v05 = 500
09 |                                 │   6   let v06 = 6
10 |                                 │   7   let v07 = 7
11 |                                 │   8   let v08 = 8
12 |                                 │  27   let v27 = 27
13 |                                 │  28   let v28 = 28
14 |                                 │  29   let v29 = 29
15 |                                 │  30 - let v30 = 30
16 |                                 │  30 + let v30 = 3000
17 |                                 │  31   let v31 = 31
18 |                                 │  32   let v32 = 32
19 |                                 │  33   let v33 = 33
20 |                                 │
21 |                                 │
22 |                                 │
23 |
24 | tab focus nav · space mark hunk · c comment · ]/[ hunk · ctrl+o context · esc na|}];
  Pty.send terminal Key.escape;
  Pty.wait terminal (Screen.has "esc close");
  Pty.send terminal Key.escape;
  Pty.wait terminal (Screen.has "❯ message mentat");
  Pty.quit terminal

