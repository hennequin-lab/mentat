(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness

(* The /review screen over the workspace review waist, as full-frame goldens.
   The review is seeded in memory through the harness [~review] fixture (a
   Mentat_review.t built from before/after texts served over a fake
   Driver.Review) — no git is needed. [/review] opens over that review; marks
   and the verdict travel as review commands, CRs as the compose flow. *)

let base = "let alpha = 1\nlet beta = 2\nlet gamma = 3\nlet delta = 4\n"
let tip = "let alpha = 1\nlet beta = 2\nlet gamma = 33\nlet delta = 4\n"
let diff = [ ("lib/code.ml", Some base, Some tip) ]

(* The same edit with a source CR the reviewer will see. *)
let tip_cr =
  "let alpha = 1\n\
   let beta = 2\n\
   (* CR reviewer: widen the type *)\n\
   let gamma = 33\n\
   let delta = 4\n"

let diff_cr = [ ("lib/code.ml", Some base, Some tip_cr) ]

(* Two changed files, each a single hunk, for the nav's file-to-file stepping. *)
let a_base = "let a = 1\nlet b = 2\nlet c = 3\n"
let a_tip = "let a = 1\nlet b = 22\nlet c = 3\n"
let z_base = "let x = 9\nlet y = 8\nlet z = 7\n"
let z_tip = "let x = 9\nlet y = 88\nlet z = 7\n"

let two_files =
  [
    ("lib/a_mod.ml", Some a_base, Some a_tip);
    ("lib/z_mod.ml", Some z_base, Some z_tip);
  ]

(* One file with two edits 25 lines apart. The waist serves it as a single
   wide-context hunk; the pane re-splits it at git's context into two. *)
let far_lines =
  List.init 34 (fun i -> Printf.sprintf "let v%02d = %d" (i + 1) (i + 1))

let far_base = String.concat "\n" far_lines ^ "\n"

let far_tip =
  far_lines
  |> List.mapi (fun i s ->
      if i = 4 then "let v05 = 500" else if i = 29 then "let v30 = 3000" else s)
  |> String.concat "\n"
  |> fun body -> body ^ "\n"

let far_diff = [ ("lib/far.ml", Some far_base, Some far_tip) ]

let open_review t =
  Tui.paste t "/review";
  Tui.enter t;
  Tui.settle t

let%expect_test "the review screen renders the two-pane diff over the base" =
  Tui.run ~name:"review-open" ~review:diff @@ fun t ->
  open_review t;
  Tui.print t;
  [%expect
    {|01 | ────────────────────────────────────────────────────────────────────────────────
02 | Review  HEAD..worktree                                    0/1 reviewed · pending
03 |  ▾ lib                          │lib/code.ml · unreviewed · +1 −1
04 |    ❯ [ ] code.ml              M │ 1   let alpha = 1
05 |                                 │ 2   let beta = 2
06 |                                 │ 3 - let gamma = 3
07 |                                 │ 3 + let gamma = 33
08 |                                 │ 4   let delta = 4
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
24 | tab focus diff · space mark · enter open · c comment · a approve · esc close|}]

let%expect_test
    "marking the unit reviewed advances progress; approve records the verdict" =
  Tui.run ~name:"review-mark" ~review:diff @@ fun t ->
  open_review t;
  Tui.keys t " ";
  Tui.settle t;
  Tui.keys t "a";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 | ────────────────────────────────────────────────────────────────────────────────
02 | Review  HEAD..worktree                                   1/1 reviewed · approved
03 |  ▾ lib                          │lib/code.ml · hunk 1/1 · reviewed · +1 −1
04 |    ❯ [✓] code.ml              M │❯ 1   let alpha = 1
05 |                                 │  2   let beta = 2
06 |                                 │  3 - let gamma = 3
07 |                                 │  3 + let gamma = 33
08 |                                 │  4   let delta = 4
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
24 | tab focus diff · space mark · enter open · c comment · a approve · esc close|}]

let%expect_test "enter focuses the diff pane" =
  Tui.run ~name:"review-focus-diff" ~review:diff @@ fun t ->
  open_review t;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 | ────────────────────────────────────────────────────────────────────────────────
02 | Review  HEAD..worktree                                    0/1 reviewed · pending
03 |  ▾ lib                          │lib/code.ml · hunk 1/1 · unreviewed · +1 −1
04 |    ❯ [ ] code.ml              M │  1   let alpha = 1
05 |                                 │  2   let beta = 2
06 |                                 │❯ 3 - let gamma = 3
07 |                                 │  3 + let gamma = 33
08 |                                 │  4   let delta = 4
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
24 | tab focus nav · space mark hunk · c comment · ]/[ hunk · ctrl+o context · esc na|}]

let%expect_test "a source CR shows in the nav tree" =
  Tui.run ~name:"review-cr" ~review:diff_cr @@ fun t ->
  open_review t;
  Tui.print t;
  [%expect
    {|01 | ────────────────────────────────────────────────────────────────────────────────
02 | Review  HEAD..worktree                                    0/1 reviewed · pending
03 |  ▾ lib                          │lib/code.ml · unreviewed · +2 −1
04 |    ❯ [ ] code.ml              M │ 1   let alpha = 1
05 |        widen the type           │ 2   let beta = 2
06 |                                 │ 3 - let gamma = 3
07 |                                 │ 3 + (* CR reviewer: widen the type *)
08 |                                 │ 4 + let gamma = 33
09 |                                 │ 5   let delta = 4
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
24 | tab focus diff · space mark · enter open · c comment · a approve · esc close|}]

let%expect_test "nav down steps to the next file, never into the diff" =
  Tui.run ~name:"review-nav-down" ~review:two_files @@ fun t ->
  open_review t;
  Tui.keys t Key.down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 | ────────────────────────────────────────────────────────────────────────────────
02 | Review  HEAD..worktree                                    0/2 reviewed · pending
03 |  ▾ lib                          │lib/z_mod.ml · unreviewed · +1 −1
04 |      [ ] a_mod.ml             M │ 1   let x = 9
05 |    ❯ [ ] z_mod.ml             M │ 2 - let y = 8
06 |                                 │ 2 + let y = 88
07 |                                 │ 3   let z = 7
08 |                                 │
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
24 | tab focus diff · space mark · enter open · c comment · a approve · esc close|}]

let%expect_test "in the diff, down steps a single line" =
  Tui.run ~name:"review-line-step" ~review:diff @@ fun t ->
  open_review t;
  Tui.enter t;
  Tui.settle t;
  Tui.keys t Key.down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 | ────────────────────────────────────────────────────────────────────────────────
02 | Review  HEAD..worktree                                    0/1 reviewed · pending
03 |  ▾ lib                          │lib/code.ml · hunk 1/1 · unreviewed · +1 −1
04 |    ❯ [ ] code.ml              M │  1   let alpha = 1
05 |                                 │  2   let beta = 2
06 |                                 │  3 - let gamma = 3
07 |                                 │❯ 3 + let gamma = 33
08 |                                 │  4   let delta = 4
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
24 | tab focus nav · space mark hunk · c comment · ]/[ hunk · ctrl+o context · esc na|}]

let%expect_test "two separated edits split into two hunks; hunk-next jumps" =
  Tui.run ~name:"review-two-hunks" ~review:far_diff @@ fun t ->
  open_review t;
  Tui.enter t;
  Tui.settle t;
  Tui.keys t "]";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 | ────────────────────────────────────────────────────────────────────────────────
02 | Review  HEAD..worktree                                    0/2 reviewed · pending
03 |  ▾ lib                          │lib/far.ml · hunk 2/2 · unreviewed · +2 −2
04 |    ❯ [ ] far.ml               M │   2   let v02 = 2
05 |                                 │   3   let v03 = 3
06 |                                 │   4   let v04 = 4
07 |                                 │   5 - let v05 = 5
08 |                                 │   5 + let v05 = 500
09 |                                 │   6   let v06 = 6
10 |                                 │   7   let v07 = 7
11 |                                 │   8   let v08 = 8
12 |                                 │  27   let v27 = 27
13 |                                 │  28   let v28 = 28
14 |                                 │  29   let v29 = 29
15 |                                 │❯ 30 - let v30 = 30
16 |                                 │  30 + let v30 = 3000
17 |                                 │  31   let v31 = 31
18 |                                 │  32   let v32 = 32
19 |                                 │  33   let v33 = 33
20 |                                 │
21 |                                 │
22 |                                 │
23 |
24 | tab focus nav · space mark hunk · c comment · ]/[ hunk · ctrl+o context · esc na|}]

let%expect_test "the compose input deletes a word with ctrl+w" =
  Tui.run ~name:"review-compose-word" ~review:diff @@ fun t ->
  open_review t;
  Tui.keys t "c";
  Tui.settle t;
  Tui.keys t "reviewer: widen alpha beta";
  Tui.settle t;
  Tui.keys t Key.ctrl_w;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 | ────────────────────────────────────────────────────────────────────────────────
02 | Review  HEAD..worktree                                    0/1 reviewed · pending
03 |  ▾ lib                          │lib/code.ml · unreviewed · +1 −1
04 |    ❯ [ ] code.ml              M │ 1   let alpha = 1
05 |                                 │ 2   let beta = 2
06 |                                 │ 3 - let gamma = 3
07 |                                 │ 3 + let gamma = 33
08 |                                 │ 4   let delta = 4
09 |                                 │
10 |                                 │
11 |
12 |             CR on lib/code.ml:3
13 |             reviewer: widen alpha
14 |
15 |                                 │
16 |                                 │
17 |                                 │
18 |                                 │
19 |                                 │
20 |                                 │
21 |                                 │
22 |                                 │
23 |
24 | enter add CR · esc cancel|}]

(* An SGR left click at a 1-based terminal cell: a press then a release at the
   same spot, which the diff pane reads as a click (not a drag) and reports as
   the source line under the pointer. *)
let click ~column ~row =
  Printf.sprintf "\027[<0;%d;%dM\027[<0;%d;%dm" column row column row

let%expect_test
    "editing a CR closes the dialog on enter; a repeat enter cannot re-submit" =
  Tui.run ~name:"review-edit-once" ~review:diff_cr @@ fun t ->
  open_review t;
  Tui.keys t "n";
  Tui.settle t;
  Tui.keys t "e";
  Tui.settle t;
  Tui.keys t " now";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  (* The dialog closed on the first enter, so this stray enter reaches the pane,
     never the compose — it cannot re-submit the edit against the CR its own
     write just changed, so no stale-ref error appears. *)
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 | ────────────────────────────────────────────────────────────────────────────────
02 | Review  HEAD..worktree                                    0/1 reviewed · pending
03 |  ▾ lib                          │lib/code.ml · hunk 1/1 · unreviewed · +2 −1
04 |    ❯ [ ] code.ml              M │  1   let alpha = 1
05 |        widen the type now       │  2   let beta = 2
06 |                                 │❯ 3 - let gamma = 3
07 |                                 │  3 + (* CR: widen the type now *)
08 |                                 │  4 + let gamma = 33
09 |                                 │  5   let delta = 4
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
24 | CR updated|}]

let%expect_test "clicking a CR comment line in the diff selects the CR" =
  Tui.run ~name:"review-cr-click" ~review:diff_cr @@ fun t ->
  open_review t;
  Tui.keys t (click ~column:50 ~row:7);
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 | ────────────────────────────────────────────────────────────────────────────────
02 | Review  HEAD..worktree                                    0/1 reviewed · pending
03 |  ▾ lib                          │lib/code.ml:3 · CR reviewer · open
04 |      [ ] code.ml              M │  1   let alpha = 1
05 |      ❯ widen the type           │  2   let beta = 2
06 |                                 │  3 - let gamma = 3
07 |                                 │❯ 3 + (* CR reviewer: widen the type *)
08 |                                 │  4 + let gamma = 33
09 |                                 │  5   let delta = 4
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
24 | tab focus nav · space mark hunk · c comment · ]/[ hunk · ctrl+o context · esc na|}]

let%expect_test "the compose dialog opens over the dimmed diff" =
  Tui.run ~name:"review-compose" ~review:diff @@ fun t ->
  open_review t;
  Tui.keys t "c";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 | ────────────────────────────────────────────────────────────────────────────────
02 | Review  HEAD..worktree                                    0/1 reviewed · pending
03 |  ▾ lib                          │lib/code.ml · unreviewed · +1 −1
04 |    ❯ [ ] code.ml              M │ 1   let alpha = 1
05 |                                 │ 2   let beta = 2
06 |                                 │ 3 - let gamma = 3
07 |                                 │ 3 + let gamma = 33
08 |                                 │ 4   let delta = 4
09 |                                 │
10 |                                 │
11 | 
12 |             CR on lib/code.ml:3
13 |             handle: comment
14 |
15 |                                 │
16 |                                 │
17 |                                 │
18 |                                 │
19 |                                 │
20 |                                 │
21 |                                 │
22 |                                 │
23 | 
24 | enter add CR · esc cancel|}]

[%%run_tests "mentat.tui.review"]
