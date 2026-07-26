(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
open Cli_launch_fixture
module Pty = Pty_session

let%expect_test "bare invocation opens the complete Home UI and restores it" =
  Project.with_temp "cli-bare-visual" @@ fun project ->
  Pty.run project @@ fun terminal ->
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
13 |
14 |           ────────────────────────────────────────────────────────────
15 |           ❯ message mentat
16 |           ────────────────────────────────────────────────────────────
17 |
18 |                          ! /login — no connected account
19 |                               ∅ no recent sessions
20 |
21 |
22 |
23 |
24 |   ! not logged in · /login · /priv… · openai/gpt-5.6-sol … · ! full access ? …|}];
  Pty.quit terminal;
  Screen.print ~project (Pty.screen terminal);
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
04 |
05 |
06 |
07 |
08 |
09 |
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 |
18 |
19 |
20 |
21 |
22 |
23 |
24 ||}]

let print_resume_case name command expected =
  Project.with_temp name @@ fun project ->
  seed_session project ~id:"session-older" ~prompt:"the older prompt"
    ~updated_at:2;
  seed_session project ~id:"session-newer" ~prompt:"the newer prompt"
    ~updated_at:9;
  Pty.run ~command
    ~ready:(fun screen ->
      Screen.contains screen expected
      && Screen.contains screen "❯ message mentat"
      && Screen.contains screen "! not logged in · /login")
    project
  @@ fun terminal ->
  Screen.print ~project (Pty.screen terminal);
  Pty.quit terminal

let%expect_test
    "session launch forms replay their selected target as complete screens" =
  print_resume_case "cli-session-option"
    [ "--session"; "session-older" ]
    "the older prompt";
  print_resume_case "cli-resume-target"
    [ "resume"; "session-older" ]
    "the older prompt";
  print_resume_case "cli-resume-last" [ "resume"; "--last" ] "the newer prompt";
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.6-sol medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ the older prompt
06 |
07 |
08 |
09 |
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 |
18 |
19 |
20 |
21 | ────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · /priv… · openai/gpt-5.6-sol … · ! full access ? …
01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.6-sol medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ the older prompt
06 |
07 |
08 |
09 |
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 |
18 |
19 |
20 |
21 | ────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · /priv… · openai/gpt-5.6-sol … · ! full access ? …
01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.6-sol medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ the newer prompt
06 |
07 |
08 |
09 |
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 |
18 |
19 |
20 |
21 | ────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · /priv… · openai/gpt-5.6-sol … · ! full access ? …|}]

let%expect_test "a launch draft is visible in the complete Home screen" =
  Project.with_temp "cli-launch-draft" @@ fun project ->
  Pty.run
    ~command:[ "--draft"; "fix the parser first" ]
    ~ready:(fun screen ->
      Screen.contains screen "fix the parser first"
      && Screen.contains screen "! /login — no connected account"
      && Screen.contains screen "no recent sessions"
      && Screen.contains screen "! not logged in · /login")
    project
  @@ fun terminal ->
  Screen.print ~project (Pty.screen terminal);
  Pty.quit ~discard_draft:true terminal;
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
13 |
14 |           ────────────────────────────────────────────────────────────
15 |           ❯ fix the parser first
16 |           ────────────────────────────────────────────────────────────
17 |
18 |                          ! /login — no connected account
19 |                               ∅ no recent sessions
20 |
21 |
22 |
23 |
24 |   ! not logged in · /login · /priv… · openai/gpt-5.6-sol … · ! full access ? …|}]

let%expect_test
    "a fresh launch prompt visibly starts and settles its first turn" =
  let prompt = "explain the fresh launch path" in
  let answer = "The fresh prompt crossed the executable boundary." in
  Project.with_temp "cli-launch-prompt" @@ fun project ->
  with_provider project ~prompt ~answer @@ fun base_url ->
  Pty.run
    ~env:
      [ ("OPENAI_API_KEY", "test-key"); ("MENTAT_OPENAI_BASE_URL", base_url) ]
    ~command:[ "--prompt"; prompt ]
    ~ready:(fun screen ->
      Screen.contains screen prompt
      && Screen.contains screen answer
      && Screen.contains screen "❯ message mentat")
    project
  @@ fun terminal ->
  Pty.settle terminal;
  Screen.print ~project (Pty.screen terminal);
  Pty.quit terminal;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.6-sol medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ explain the fresh launch path
06 |
07 | ⏺ The fresh prompt crossed the executable boundary.
08 |
09 |
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 |
18 |
19 |
20 |
21 | ────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   /private/tmp/mentat-tui-… · openai/gpt-5.6-sol me… · ! full access ? for sh…|}]

(* A manual /compact is an engine-side model call, so it takes time. While the
   summary response is held, the composer-issued compaction must show the live
   working affordance rather than a silent screen the user reads as a no-op. The
   held response keeps "Compacting…" on screen; [wait] proves it appears before
   the durable boundary settles. *)
let%expect_test "manual compaction shows a live indicator while it summarizes" =
  let prompt = "summarize the parser investigation" in
  let answer = "The parser investigation is complete." in
  let summary = "Earlier: the parser investigation was completed." in
  Project.with_temp "cli-compact-indicator" @@ fun project ->
  with_compaction_provider project ~prompt ~answer ~summary
    ~summary_delay_ms:2000
  @@ fun base_url ->
  Pty.run
    ~env:
      [ ("OPENAI_API_KEY", "test-key"); ("MENTAT_OPENAI_BASE_URL", base_url) ]
    ~command:[ "--prompt"; prompt ]
    ~ready:(fun screen ->
      Screen.contains screen answer && Screen.contains screen "❯ message mentat")
    project
  @@ fun terminal ->
  Pty.send terminal "/compact";
  Pty.wait terminal (Screen.has "/compact");
  Pty.send terminal "\r";
  (* The live affordance is intrinsically animated (spinner, elapsed), so it is
     asserted by predicate rather than frozen into a golden. Its appearance is
     the fix under test: without it the two-second summary reads as a no-op. *)
  Pty.wait terminal (Screen.has "Compacting");
  (* Then the held summary lands and the durable boundary replaces the live row
     with a settled, deterministic frame. *)
  Pty.wait terminal (Screen.has "compacted");
  Pty.settle terminal;
  Screen.print ~project (Pty.screen terminal);
  Pty.quit terminal;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.6-sol medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ summarize the parser investigation
06 |
07 | ⏺ The parser investigation is complete.
08 |
09 |  ─────────────────────────────────  compacted  ────────────────────────────────
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 |
18 |
19 |
20 |
21 | ────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   /private/tmp/mentat-tui-c… · openai/gpt-5.6-sol me… · ! full acce… ? for sh…|}]

let%expect_test
    "the live main TUI relayouts as a complete screen after a real PTY resize" =
  let id = "session-resize" in
  let prompt = "This resumed transcript must reflow after a real PTY resize." in
  Project.with_temp "cli-main-resize" @@ fun project ->
  seed_session project ~id ~prompt ~updated_at:2;
  Pty.run ~command:[ "resume"; id ] ~ready:(Screen.has prompt) project
  @@ fun terminal ->
  Pty.settle terminal;
  Screen.print ~project (Pty.screen terminal);
  Pty.resize terminal ~rows:18 ~cols:44;
  Pty.wait terminal (Screen.has "  a real PTY resize.");
  Pty.settle terminal;
  Screen.print ~project (Pty.screen terminal);
  Pty.quit terminal;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.6-sol medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ This resumed transcript must reflow after a real PTY resize.
06 |
07 |
08 |
09 |
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 |
18 |
19 |
20 |
21 | ────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · /priv… · openai/gpt-5.6-sol … · ! full access ? …
01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
04 |  dev · openai/gpt-5.6-sol medium
05 |  $TESTCASE_ROOT
06 |
07 | ❯ This resumed transcript must reflow after
08 |   a real PTY resize.
09 |
10 |
11 |
12 |
13 |
14 |
15 | ────────────────────────────────────────────
16 | ❯ message mentat
17 | ────────────────────────────────────────────
18 |   ! not logged in · /log…… · ! full acce…|}]

let%expect_test "an explicit local shell command settles in the complete UI" =
  Project.with_temp "cli-local-shell" @@ fun project ->
  Pty.run project @@ fun terminal ->
  Pty.send terminal "!printf 'local shell from tui\\n'";
  Pty.wait terminal (Screen.has "printf 'local shell from tui\\n'");
  Pty.send terminal "\r";
  Pty.wait terminal (fun screen ->
      Screen.contains screen "local shell from tui"
      && Screen.contains screen "Completed in");
  Pty.screen terminal |> censor_shell_elapsed_ms |> Screen.print ~project;
  Pty.quit terminal;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.6-sol medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ⏺ Shell(printf 'local shell from tui\n')
06 |   ⎿  Completed in $TIME ms
07 |       local shell from tui
08 |
09 |
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 |
18 |
19 |
20 |
21 | ────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · /priv… · openai/gpt-5.6-sol … · ! full access ? …|}]

let%expect_test
    "a resumed launch prompt stays visibly editable instead of submitting" =
  Project.with_temp "cli-resume-prompt" @@ fun project ->
  seed_session project ~id:"session-resumed" ~prompt:"saved conversation"
    ~updated_at:2;
  Pty.run
    ~command:[ "resume"; "session-resumed"; "--prompt"; "follow up safely" ]
    ~ready:(fun screen ->
      Screen.contains screen "saved conversation"
      && Screen.contains screen "follow up safely"
      && Screen.contains screen "! not logged in · /login")
    project
  @@ fun terminal ->
  Screen.print ~project (Pty.screen terminal);
  Pty.quit ~discard_draft:true terminal;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.6-sol medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ saved conversation
06 |
07 |
08 |
09 |
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 |
18 |
19 |
20 |
21 | ────────────────────────────────────────────────────────────────────────────────
22 | ❯ follow up safely
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · /priv… · openai/gpt-5.6-sol … · ! full access ? …|}]

let%expect_test
    "a resumed launch paints the session first, never the home stage" =
  Project.with_temp "cli-resume-first-frame" @@ fun project ->
  seed_session project ~id:"session-first" ~prompt:"resumed first frame"
    ~updated_at:2;
  Pty.run
    ~command:[ "resume"; "session-first" ]
    ~ready:(Screen.has "resumed first frame")
    project
  @@ fun terminal ->
  (* Every byte the child emitted before the transcript settled. The home stage
     is the only surface that paints the recents "loading sessions" line, so its
     absence across the whole stream proves the resume opened straight onto the
     session surface rather than flashing home until the replay landed. *)
  require
    (not (Screen.contains (Pty.raw terminal) "loading sessions"))
    "the home stage flashed before the resumed transcript";
  Screen.print ~project (Pty.screen terminal);
  Pty.quit terminal;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.6-sol medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ resumed first frame
06 |
07 |
08 |
09 |
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 |
18 |
19 |
20 |
21 | ────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · /priva… · openai/gpt-5.6-sol… · ! full access ? …|}]

let%expect_test
    "quitting an active session leaves a complete farewell with its resume hint"
    =
  Project.with_temp "cli-active-goodbye" @@ fun project ->
  seed_session project ~id:"session-goodbye" ~prompt:"keep this conversation"
    ~updated_at:2;
  Pty.run
    ~command:[ "resume"; "session-goodbye" ]
    ~ready:(fun screen ->
      Screen.contains screen "keep this conversation"
      && Screen.contains screen "❯ message mentat"
      && Screen.contains screen "! not logged in · /login")
    project
  @@ fun terminal ->
  Screen.print ~project (Pty.screen terminal);
  Pty.quit terminal;
  Screen.print ~project (Pty.screen terminal);
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.6-sol medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ keep this conversation
06 |
07 |
08 |
09 |
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 |
18 |
19 |
20 |
21 | ────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · /priv… · openai/gpt-5.6-sol … · ! full access ? …
01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
04 |
05 |  continue  mentat resume session-goodbye
06 |
07 |
08 |
09 |
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 |
18 |
19 |
20 |
21 |
22 |
23 |
24 ||}]

(* These two trust-gate journeys wrap a fixture path across terminal rows, so
   the exe-qualified fixture-root digest (Project.with_temp, harness project.ml)
   lands in their screen goldens at a column the censor cannot normalize. They
   ride the primary cli_launch exe — whose basename, and therefore that digest,
   is unchanged by the split — so their goldens stay byte-identical. *)
let%expect_test
    "a narrow trust gate wraps a long workspace root as one complete screen" =
  Project.with_temp
    "cli-trust-narrow-workspace-root-with-a-deliberately-long-name"
  @@ fun project ->
  Pty.run ~trust:false ~rows:36 ~cols:44
    ~ready:(Screen.has "Use ↑/↓ and Enter")
    project
  @@ fun terminal ->
  Pty.settle terminal;
  Screen.print ~project (Pty.screen terminal);
  [%expect
    {|01 |
02 |  Mentat repository activation
03 |
04 |  Repository root: /private/tmp/mentat-tui-
05 |  cli-trust-narrow-workspace-root-with-a-
06 |  deliberately-laf3532a1
07 |  Selection: 1 — continue restricted
08 |
09 |  This repository can control config,
10 |  instructions, skills, Dune rules, local
11 |  tools, evaluator, and Build-mode project
12 |  processes. Activation does not approve
13 |  operations or widen the selected sandbox.
14 |
15 |  ❯ 1. Continue restricted — remember this
16 |       choice
17 |     Native reads, searches, and sandboxed
18 |     edits remain available. Repository-
19 |     controlled code will not run. Files
20 |     edited now may execute if you activate
21 |     the repository later.
22 |    2. Trust and activate this repository —
23 |       remember this choice
24 |     Repository inputs and Build processes
25 |     activate after reload.
26 |    3. Exit
27 |     Save nothing and start no project
28 |     process.
29 |
30 |
31 |
32 |
33 |
34 |  Use ↑/↓ and Enter, or press 1–3. Escape
35 |  or Ctrl+C exits.
36 ||}];
  Pty.send terminal "3";
  Pty.wait_exit terminal

let%expect_test "the live trust gate relayouts after a real terminal resize" =
  Project.with_temp "cli-trust-live-resize" @@ fun project ->
  Pty.run ~trust:false ~ready:(Screen.has "Use ↑/↓ and Enter") project
  @@ fun terminal ->
  Pty.settle terminal;
  Screen.print ~project (Pty.screen terminal);
  [%expect
    {|01 |
02 |  Mentat repository activation
03 |
04 |  Repository root: $TESTCASE_ROOT
05 |  Selection: 1 — continue restricted
06 |
07 |  This repository can control config, instructions, skills, Dune rules, local
08 |  tools, evaluator, and Build-mode project processes. Activation does not
09 |  approve operations or widen the selected sandbox.
10 |
11 |  ❯ 1. Continue restricted — remember this choice
12 |     Native reads, searches, and sandboxed edits remain available. Repository-
13 |     controlled code will not run. Files edited now may execute if you activate
14 |     the repository later.
15 |    2. Trust and activate this repository — remember this choice
16 |     Repository inputs and Build processes activate after reload.
17 |    3. Exit
18 |     Save nothing and start no project process.
19 |
20 |
21 |
22 |
23 |  Use ↑/↓ and Enter, or press 1–3. Escape or Ctrl+C exits.
24 ||}];
  Pty.resize terminal ~rows:36 ~cols:44;
  Pty.wait terminal (Screen.has "Selection: 1");
  Pty.settle terminal;
  Screen.print ~project (Pty.screen terminal);
  [%expect
    {|01 |
02 |  Mentat repository activation
03 |
04 |  Repository root: /private/tmp/mentat-tui-
05 |  cli-trust-liv94277a70
06 |  Selection: 1 — continue restricted
07 |
08 |  This repository can control config,
09 |  instructions, skills, Dune rules, local
10 |  tools, evaluator, and Build-mode project
11 |  processes. Activation does not approve
12 |  operations or widen the selected sandbox.
13 |
14 |  ❯ 1. Continue restricted — remember this
15 |       choice
16 |     Native reads, searches, and sandboxed
17 |     edits remain available. Repository-
18 |     controlled code will not run. Files
19 |     edited now may execute if you activate
20 |     the repository later.
21 |    2. Trust and activate this repository —
22 |       remember this choice
23 |     Repository inputs and Build processes
24 |     activate after reload.
25 |    3. Exit
26 |     Save nothing and start no project
27 |     process.
28 |
29 |
30 |
31 |
32 |
33 |
34 |  Use ↑/↓ and Enter, or press 1–3. Escape
35 |  or Ctrl+C exits.
36 ||}];
  Pty.send terminal "\027[B";
  Pty.wait terminal (Screen.has "Selection: 2");
  Pty.settle terminal;
  Screen.print ~project (Pty.screen terminal);
  [%expect
    {|01 |
02 |  Mentat repository activation
03 |
04 |  Repository root: /private/tmp/mentat-tui-
05 |  cli-trust-liv94277a70
06 |  Selection: 2 — trust and activate this
07 |  repository
08 |
09 |  This repository can control config,
10 |  instructions, skills, Dune rules, local
11 |  tools, evaluator, and Build-mode project
12 |  processes. Activation does not approve
13 |  operations or widen the selected sandbox.
14 |
15 |    1. Continue restricted — remember this
16 |       choice
17 |     Native reads, searches, and sandboxed
18 |     edits remain available. Repository-
19 |     controlled code will not run. Files
20 |     edited now may execute if you activate
21 |     the repository later.
22 |  ❯ 2. Trust and activate this repository —
23 |       remember this choice
24 |     Repository inputs and Build processes
25 |     activate after reload.
26 |    3. Exit
27 |     Save nothing and start no project
28 |     process.
29 |
30 |
31 |
32 |
33 |
34 |  Use ↑/↓ and Enter, or press 1–3. Escape
35 |  or Ctrl+C exits.
36 ||}];
  Pty.send terminal "3";
  Pty.wait_exit terminal

[%%run_tests "mentat.tui.pty.cli_launch"]
