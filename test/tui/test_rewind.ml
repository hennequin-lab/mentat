(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* In-TUI rewind: [/rewind] opens a picker over prior user turns, arming one
   seeds the composer and previews the discarded tail, and committing rewinds the
   source into a non-destructive branch that continues from the edited message.
   The whole flow crosses the real client, whose fake driver mints and follows
   the rewound child exactly as it does a fork. *)

open Tui_harness
module Protocol = Mentat_protocol

let home_is_project project =
  Some (Lpath.Abs.of_string_exn (Project.root project))

let unavailable text =
  Protocol.Error.Unavailable (Mentat_diagnostic.of_text text)

let submit t text =
  Tui.paste t text;
  Tui.settle t;
  Tui.enter t;
  Tui.settle t

let settle_turn t =
  Tui.finish_turn t;
  Tui.settle t

let first_turn =
  Tui.Turn_script.complete ~prompt:"add a --json flag" "Added the flag."

let second_turn =
  Tui.Turn_script.complete ~prompt:"rename it to models ls" "Renamed it."

let converse t =
  submit t "add a --json flag";
  settle_turn t;
  submit t "rename it to models ls";
  settle_turn t

let%expect_test "the rewind picker lists prior user turns and esc closes it" =
  Tui.run ~name:"rewind-picker" ~home:home_is_project
    ~turns:[ first_turn; second_turn ]
  @@ fun t ->
  converse t;
  submit t "/rewind";
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
04 |
05 | ❯ add a --json flag
06 |
07 | ⏺ Added the flag.
08 |
09 | ❯ rename it to models ls
10 |
11 | ⏺ Renamed it.
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    rewind to message
15 |
16 |   ❯  1 just now rename it to models ls
17 |     2 just now add a --json flag
18 |
19 |
20 |
21 |
22 |
23 |
24 |   ↵ edit & resubmit · type to filter · ↑↓ choose · esc cancel|}];
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
04 |
05 | ❯ add a --json flag
06 |
07 | ⏺ Added the flag.
08 |
09 | ❯ rename it to models ls
10 |
11 | ⏺ Renamed it.
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

let%expect_test "arming seeds the composer and esc cancels the arming" =
  Tui.run ~name:"rewind-arm" ~home:home_is_project
    ~turns:[ first_turn; second_turn ]
  @@ fun t ->
  converse t;
  submit t "/rewind";
  (* The picker lists newest first; step down to the older message and arm it. *)
  Tui.keys t Key.down;
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
04 |
05 |
06 |   rewinding here · 2 messages below will be discarded
07 | ❯ add a --json flag
08 |
09 | ⏺ Added the flag.
10 |
11 | ❯ rename it to models ls
12 |
13 | ⏺ Renamed it.
14 |
15 |
16 |
17 |
18 |   files on disk are not reverted · rewind changes the conversation only
19 |
20 | ────────────────────────────────────────────────────────────────────────────────
21 | ❯ add a --json flag
22 | ────────────────────────────────────────────────────────────────────────────────
23 |   ↵ resubmit (discards 2) · esc cancel
24 |   ! not logged in · /login · ~ · openai/gpt-5.5 · ! full access ? for shortcu…|}];
  (* R1: a single Escape while armed cancels the arming and restores the blank
     draft rather than arming the draft-discard rung. *)
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
04 |
05 | ❯ add a --json flag
06 |
07 | ⏺ Added the flag.
08 |
09 | ❯ rename it to models ls
10 |
11 | ⏺ Renamed it.
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
  (* The guard: idle Up still walks prompt history after rewind was available. *)
  Tui.keys t Key.up;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
04 |
05 | ❯ add a --json flag
06 |
07 | ⏺ Added the flag.
08 |
09 | ❯ rename it to models ls
10 |
11 | ⏺ Renamed it.
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
22 | ❯ rename it to models ls
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · ~ · openai/gpt-5.5 · ! full access ? for shortcu…|}]

let%expect_test "committing a rewind branches into an edited resubmission" =
  let rewound =
    Tui.Turn_script.complete ~prompt:"add a --json flag with tests"
      "Added the flag and its tests."
  in
  Tui.run ~name:"rewind-commit" ~home:home_is_project
    ~turns:[ first_turn; second_turn; rewound ]
  @@ fun t ->
  converse t;
  submit t "/rewind";
  (* Arm the oldest message, edit it, and resubmit. *)
  Tui.keys t Key.down;
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.keys t " with tests";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  settle_turn t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
    04 |
    05 |  ─  rewound session-fork-0000001 · from session-visual-00001 · files not reve…─
    06 |
    07 | ❯ add a --json flag with tests
    08 |
    09 | ⏺ Added the flag and its tests.
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
    24 |   ! not logged in · /login · ~ · openai/gpt-5.5 · ! full access ? for shortcu…
    |}]

let%expect_test "the source session survives a rewind" =
  let rewound =
    Tui.Turn_script.complete ~prompt:"rename it to models ls"
      "Renamed it again."
  in
  Tui.run ~name:"rewind-nondestructive" ~home:home_is_project
    ~turns:[ first_turn; second_turn; rewound ]
  @@ fun t ->
  converse t;
  submit t "/rewind";
  (* Arm the newest message and resubmit it unchanged into the branch. *)
  Tui.enter t;
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  settle_turn t;
  (* Both the rewound child and its intact source appear in the browser. *)
  submit t "/sessions";
  Tui.keys t Key.tab;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |  sessions ─────────────────────────────────────────────────────────── 2 sessions
02 |
03 |    session                              lifecycle phase   turns updated  recency
04 | ❯  session-visual-00001                 active    idle  2 turns just now today
05 |    session-fork-0000001                 active    idle  2 turns just now today
06 |
07 |
08 |
09 |
10 |
11 |
12 |
13 |
14 |   add a --json flag
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

let%expect_test "rewind is unavailable while a turn is in flight" =
  Tui.run ~name:"rewind-midturn" ~home:home_is_project ~turns:[ first_turn ]
  @@ fun t ->
  submit t "add a --json flag";
  (* The turn is held in flight; the idle-only command reports its gate. *)
  submit t "/rewind";
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
04 |
05 | ❯ add a --json flag
06 |
07 | ⠋ Working… (0s · esc to interrupt)
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
22 | ❯ queue a message — sends after this turn
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   /rewind is available after the turn finishes|}]

let%expect_test "a failed rewind restores the edited message to the composer" =
  Tui.run ~name:"rewind-failed" ~home:home_is_project
    ~turns:[ first_turn; second_turn ]
  @@ fun t ->
  converse t;
  submit t "/rewind";
  (* Arm the newest message and edit it. *)
  Tui.enter t;
  Tui.settle t;
  Tui.keys t " now";
  Tui.settle t;
  (* The rewind's follow fails before admission; the feed is untouched and the
     edited message is returned to the composer rather than lost. *)
  Tui.fail_next_resume t (unavailable "rewind follow refused");
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
04 |
05 | ❯ add a --json flag
06 |
07 | ⏺ Added the flag.
08 |
09 | ❯ rename it to models ls
10 |
11 | ⏺ Renamed it.
12 |
13 | ✗ rewind follow refused
14 |   retry when ready
15 |
16 |
17 |
18 |
19 |
20 |
21 | ────────────────────────────────────────────────────────────────────────────────
22 | ❯ rename it to models ls now
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   rewind follow refused|}]

(* The exact "structured prior draft restored" scenario is unreachable through
   the sole entry: invoking [/rewind] clears the composer on the palette-run
   path, so the stashed prior draft is always the blank post-submit draft. This
   instead proves the cancel path handles structured armed edits — a pasted atom
   accumulated while armed — and cleanly restores that blank prior draft. *)
let%expect_test
    "cancelling discards a structured armed edit and restores the draft" =
  Tui.run ~name:"rewind-structured-cancel" ~home:home_is_project
    ~turns:[ first_turn; second_turn ]
  @@ fun t ->
  converse t;
  submit t "/rewind";
  Tui.enter t;
  Tui.settle t;
  (* Paste a multi-line block into the seeded composer; it collapses to one atom. *)
  Tui.paste t "extra\nlines\nof\ncontext";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
04 |
05 | ❯ add a --json flag
06 |
07 | ⏺ Added the flag.
08 |
09 |   rewinding here · 1 message below will be discarded
10 | ❯ rename it to models ls
11 |
12 | ⏺ Renamed it.
13 |
14 |
15 |
16 |
17 |
18 |   files on disk are not reverted · rewind changes the conversation only
19 |
20 | ────────────────────────────────────────────────────────────────────────────────
21 | ❯ rename it to models ls[Pasted text #1 +3 lines]
22 | ────────────────────────────────────────────────────────────────────────────────
23 |   ↵ resubmit (discards 1) · esc cancel
24 |   ! not logged in · /login · ~ · openai/gpt-5.5 · ! full access ? for shortcu…|}];
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
04 |
05 | ❯ add a --json flag
06 |
07 | ⏺ Added the flag.
08 |
09 | ❯ rename it to models ls
10 |
11 | ⏺ Renamed it.
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

let%expect_test "undo arms the boundary seam and reloads the composer" =
  Tui.run ~name:"undo-seam" ~home:home_is_project
    ~turns:[ first_turn; second_turn ]
  @@ fun t ->
  converse t;
  submit t "/undo";
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~
04 |
05 | ❯ add a --json flag
06 |
07 | ⏺ Added the flag.
08 |
09 | ❯ rename it to models ls
10 |
11 | ⏺ Renamed it.
12 |
13 |  ────────────  1 turn undone · no file changes · /redo to restore  ────────────
14 |
15 |
16 |
17 |
18 |
19 |
20 |
21 | ────────────────────────────────────────────────────────────────────────────────
22 | ❯ rename it to models ls
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · ~ · openai/gpt-5.5 · ! full access ? for shortcu…|}]
