(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
module Protocol = Mentat_protocol
module Session = Mentat_session

let home_is_project project =
  Some (Lpath.Abs.of_string_exn (Project.root project))

let submit t text =
  Tui.paste t text;
  Tui.settle t;
  Tui.enter t;
  Tui.settle t

let open_browser t =
  submit t "/sessions";
  Tui.keys t Key.tab;
  Tui.settle t

let unavailable text =
  Protocol.Error.Unavailable (Mentat_diagnostic.of_text text)

let installed_compaction =
  Tui.Compaction_script.installed
    ~reason:Session.Compaction.Reason.User_requested
    ~request_digest:(Mentat_digest.string "lifecycle summary request")
    ~summary_messages:
      [ Mentat_llm.Message.user_text "The earlier parser investigation." ]
    ~summarized_upto:2
    ~context:
      (Session.Compaction.Context_tokens.make ~before:9_000 ~after:800 ())
    ()

(* Successful fork is not allowed to look like a no-op. The first frame names
   both exact session identities and replays the parent's complete transcript.
   The browser frame then makes the durable lineage and retained parent visible
   without a hidden fixture assertion. *)
let%expect_test "fork acknowledges exact lineage and replays the parent" =
  let turn =
    Tui.Turn_script.complete ~prompt:"trace the parser boundary"
      "The parent transcript remains intact."
  in
  Tui.run ~name:"lifecycle-fork" ~home:home_is_project ~turns:[ turn ]
  @@ fun t ->
  submit t "trace the parser boundary";
  Tui.finish_turn t;
  Tui.settle t;
  submit t "/fork";
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
04 |
05 |  ──────────  forked session-fork-0000001 · from session-visual-00001  ─────────
06 |
07 | ❯ trace the parser boundary
08 |
09 | ⏺ The parent transcript remains intact.
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
24 |   ! not logged in · /login · ~ · openai/gpt-5.5 · ! full access ? for shortcu…|}];
  open_browser t;
  Tui.keys t Key.down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |  sessions ─────────────────────────────────────────────────────────── 2 sessions
02 |
03 |    session                               lifecycle phase  turns updated  recency
04 |    session-visual-00001                  active    idle  1 turn just now today
05 | ❯  session-fork-0000001                  active    idle  1 turn just now today
06 |
07 |
08 |
09 |
10 |
11 |
12 |
13 |
14 |   trace the parser boundary
15 |
16 |   cwd ~
17 |
18 |   fork of session-visual-00001 · 4 copied events
19 |
20 |
21 |
22 |
23 |
24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back|}]

(* Clear is a local detach, not deletion. A preceding owner mutation makes the
   retained document observable beyond its id: the old transcript disappears,
   the next prompt starts a different session, and the browser exposes both
   exact saved documents after the fresh conversation is detached in turn. *)
let%expect_test "clear detaches and a fresh conversation remains independent" =
  let before =
    Tui.Turn_script.complete ~prompt:"inspect the old parser"
      "The old session has durable evidence."
  in
  let after =
    Tui.Turn_script.complete ~prompt:"start with empty context"
      "This answer belongs only to the fresh session."
  in
  Tui.run ~name:"lifecycle-clear" ~home:home_is_project ~turns:[ before; after ]
  @@ fun t ->
  submit t "inspect the old parser";
  Tui.finish_turn t;
  Tui.settle t;
  submit t "/rename Retained parser investigation";
  submit t "/clear";
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
04 |
05 |   conversation cleared · session session-visual-00001 remains saved
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
24 |   ! not logged in · /login · ~ · openai/gpt-5.5 · ! full access ? for shortcu…|}];
  submit t "start with empty context";
  Tui.finish_turn t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
04 |
05 | ❯ start with empty context
06 |
07 | ⏺ This answer belongs only to the fresh session.
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
24 |   ! not logged in · /login · ~ · openai/gpt-5.5 · ! full access ? for shortcu…|}];
  submit t "/clear";
  open_browser t;
  Tui.print t;
  [%expect
    {|01 |  sessions ─────────────────────────────────────────────────────────── 2 sessions
02 |
03 |    session                               lifecycle phase  turns updated  recency
04 | ❯  Retained parser investigation         active    idle  1 turn just now today
05 |    session-visual-00002                  active    idle  1 turn just now today
06 |
07 |
08 |
09 |
10 |
11 |
12 |
13 |
14 |   inspect the old parser
15 |
16 |   cwd ~
17 |
18 |
19 |
20 |
21 |
22 |
23 |
24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back|}]

let settled_turn =
  Tui.Turn_script.complete ~prompt:"summarize the parser work"
    "The durable transcript is ready for compaction."

let reach_idle_chat t =
  submit t "summarize the parser work";
  Tui.finish_turn t;
  Tui.settle t

let%expect_test "installed manual compaction renders its durable boundary" =
  Tui.run ~name:"lifecycle-compact-installed" ~home:home_is_project
    ~compactions:[ installed_compaction ] ~turns:[ settled_turn ]
  @@ fun t ->
  reach_idle_chat t;
  submit t "/compact";
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
04 |
05 | ❯ summarize the parser work
06 |
07 | ⏺ The durable transcript is ready for compaction.
08 |
09 |  ────────────────────────  compacted ~9k → ~800 tokens  ───────────────────────
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
24 |   ! not logged in · /login · ~ · openai/gpt-5.5 · ! full access ? for shortcu…|}]

let%expect_test "skipped and failed manual compactions remain distinct" =
  Tui.run ~name:"lifecycle-compact-outcomes" ~home:home_is_project
    ~compactions:
      [
        Tui.Compaction_script.skipped;
        Tui.Compaction_script.failed
          (unavailable "compaction model is unavailable");
      ]
    ~turns:[ settled_turn ]
  @@ fun t ->
  reach_idle_chat t;
  submit t "/compact";
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
04 |
05 | ❯ summarize the parser work
06 |
07 | ⏺ The durable transcript is ready for compaction.
08 |
09 |   compaction skipped · context already compact
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
24 |   ! not logged in · /login · ~ · openai/gpt-5.5 · ! full access ? for shortcu…|}];
  submit t "/compact";
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
04 |
05 | ❯ summarize the parser work
06 |
07 | ⏺ The durable transcript is ready for compaction.
08 |
09 |   compaction skipped · context already compact
10 |
11 | ✗ compaction model is unavailable
12 |   retry when ready
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
24 |   compaction model is unavailable|}]

let%expect_test "lifecycle commands guard the home stage without a session" =
  Tui.run ~name:"lifecycle-guards" ~home:home_is_project @@ fun t ->
  Tui.settle t;
  submit t "/fork";
  Tui.print t;
  [%expect
    {|01 |
02 |
03 |
04 |
05 |                           █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
06 |                           █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
07 |
08 |                            dev · openai/gpt-5.5 medium
09 |
10 |      ▎ welcome — and thanks for trying mentat this early.
11 |      ▎ it's experimental: sessions and config may change without migration.
12 |
13 |
14 | ────────────────────────────────────────────────────────────────────────────────
15 | ❯ message mentat
16 | ────────────────────────────────────────────────────────────────────────────────
17 |
18 |                          ! /login — no connected account
19 |                               ∅ no recent sessions
20 |
21 |
22 |
23 |
24 |   fork: no active session|}];
  submit t "/compact";
  Tui.print t;
  [%expect
    {|01 |
02 |
03 |
04 |
05 |                           █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
06 |                           █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
07 |
08 |                            dev · openai/gpt-5.5 medium
09 |
10 |      ▎ welcome — and thanks for trying mentat this early.
11 |      ▎ it's experimental: sessions and config may change without migration.
12 |
13 |
14 | ────────────────────────────────────────────────────────────────────────────────
15 | ❯ message mentat
16 | ────────────────────────────────────────────────────────────────────────────────
17 |
18 |                          ! /login — no connected account
19 |                               ∅ no recent sessions
20 |
21 |
22 |
23 |
24 |   no session to compact|}];
  Tui.paste t "/rename";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.keys t "guarded title";
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |
03 |
04 |
05 |                           █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
06 |                           █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
07 |
08 |                            dev · openai/gpt-5.5 medium
09 |
10 |      ▎ welcome — and thanks for trying mentat this early.
11 |      ▎ it's experimental: sessions and config may change without migration.
12 |
13 |
14 | ────────────────────────────────────────────────────────────────────────────────
15 | ❯ message mentat
16 | ────────────────────────────────────────────────────────────────────────────────
17 |
18 |                          ! /login — no connected account
19 |                               ∅ no recent sessions
20 |
21 |
22 |
23 |
24 |   no session to rename|}]

