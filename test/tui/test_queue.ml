(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness

let submit t prompt =
  Tui.keys t prompt;
  Tui.enter t;
  Tui.settle t

let finish_turn t =
  Tui.finish_turn t;
  Tui.settle t

let unavailable message =
  Mentat_protocol.Error.Unavailable (Mentat_diagnostic.of_text message)

(* These are complete application frames. Queue state enters through durable
   Journal_queue facts, and queued admission returns through a Turn_started
   whose typed origin names the exact entry consumed by the session fold. *)
let%expect_test
    "an active turn visibly queues a draft and drains it as its own turn" =
  let turns =
    [
      Tui.Turn_script.complete ~prompt:"hold the first turn open"
        "First turn done.";
      Tui.Turn_script.complete ~prompt:"also update the changelog"
        "Changelog updated.";
    ]
  in
  Tui.run ~name:"e" ~turns @@ fun t ->
  submit t "hold the first turn open";
  submit t "also update the changelog";
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-19c0f55e
    04 |
    05 | ❯ hold the first turn open
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
    19 |   ↥ queued · "also update the changelog" (↑ edits)
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ queue a message — sends after this turn
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-… · ! full access ? for s…
    |}];
  finish_turn t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-19c0f55e
    04 |
    05 | ❯ hold the first turn open
    06 |
    07 | ⏺ First turn done.
    08 |
    09 | ❯ also update the changelog
    10 |
    11 | ⠋ Working… (0s · esc to interrupt)
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
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-… · ! full access ? for s…
    |}];
  finish_turn t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-19c0f55e
    04 |
    05 | ❯ hold the first turn open
    06 |
    07 | ⏺ First turn done.
    08 |
    09 | ❯ also update the changelog
    10 |
    11 | ⏺ Changelog updated.
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
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-… · ! full access ? for s…
    |}]

let%expect_test "up recovers the newest draft while preserving the older queue"
    =
  let turns =
    [
      Tui.Turn_script.complete ~prompt:"hold the turn for queue editing"
        "The held turn is complete.";
      Tui.Turn_script.complete ~prompt:"keep the changelog queued"
        "The changelog is updated.";
    ]
  in
  Tui.run ~name:"e" ~turns @@ fun t ->
  submit t "hold the turn for queue editing";
  submit t "keep the changelog queued";
  submit t "recover these release notes";
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-19c0f55e
    04 |
    05 | ❯ hold the turn for queue editing
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
    18 |   ↥ queued · "keep the changelog queued" (↑ edits)
    19 |   ↥ queued · "recover these release notes" (↑ edits)
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ queue a message — sends after this turn
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-… · ! full access ? for s…
    |}];
  Tui.keys t Key.up;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-19c0f55e
    04 |
    05 | ❯ hold the turn for queue editing
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
    19 |   ↥ queued · "keep the changelog queued" (↑ edits)
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ recover these release notes
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-… · ! full access ? for s…
    |}];
  finish_turn t;
  finish_turn t

let%expect_test
    "a failed up edit restores the blank composer without duplicating the queue"
    =
  let turns =
    [
      Tui.Turn_script.complete ~prompt:"hold the turn during recovery"
        "This held response is not released.";
    ]
  in
  Tui.run ~name:"e" ~turns
    ~queue_edit_results:[ Error (unavailable "queued draft edit unavailable") ]
  @@ fun t ->
  submit t "hold the turn during recovery";
  submit t "keep this queued exactly once";
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-19c0f55e
    04 |
    05 | ❯ hold the turn during recovery
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
    19 |   ↥ queued · "keep this queued exactly once" (↑ edits)
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ queue a message — sends after this turn
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-… · ! full access ? for s…
    |}];
  Tui.keys t Key.up;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-19c0f55e
    04 |
    05 | ❯ hold the turn during recovery
    06 |
    07 | ✗ queued draft edit unavailable
    08 |   retry when ready
    09 |
    10 | ⠋ Working… (0s · esc to interrupt)
    11 |
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |   ↥ queued · "keep this queued exactly once" (↑ edits)
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ queue a message — sends after this turn
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   queued draft edit unavailable
    |}]

let%expect_test
    "a pending up edit cannot pop the factual queue twice or overwrite newer \
     input" =
  let turns =
    [
      Tui.Turn_script.complete ~prompt:"hold the turn during a late failure"
        "This held response is not released.";
    ]
  in
  Tui.run ~name:"e" ~turns ~hold_queue_edit_results:true
    ~queue_edit_results:[ Error (unavailable "late queue edit failure") ]
  @@ fun t ->
  submit t "hold the turn during a late failure";
  submit t "preserve this queued correction";
  Tui.keys t Key.up;
  Tui.settle t;
  Tui.keys t Key.ctrl_c;
  Tui.settle t;
  Tui.keys t Key.up;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-19c0f55e
    04 |
    05 | ❯ hold the turn during a late failure
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
    19 |   ↥ queued · "preserve this queued correction" (↑ edits)
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ queue a message — sends after this turn
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   queue edit in progress
    |}];
  Tui.finish_queue_edit_result t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-19c0f55e
    04 |
    05 | ❯ hold the turn during a late failure
    06 |
    07 | ✗ late queue edit failure
    08 |   retry when ready
    09 |
    10 | ⠋ Working… (0s · esc to interrupt)
    11 |
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |   ↥ queued · "preserve this queued correction" (↑ edits)
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ queue a message — sends after this turn
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   late queue edit failure
    |}]

let%expect_test
    "cooperative interrupt visibly drains and runs the queued correction" =
  let turns =
    [
      Tui.Turn_script.complete ~prompt:"go in the wrong direction"
        "This response must never be released.";
      Tui.Turn_script.complete ~prompt:"also update the changelog"
        "Changelog updated after the interrupt.";
    ]
  in
  Tui.run ~name:"e" ~turns @@ fun t ->
  submit t "go in the wrong direction";
  submit t "also update the changelog";
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-19c0f55e
    04 |
    05 | ❯ go in the wrong direction
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
    19 |   ↥ queued · "also update the changelog" (↑ edits)
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ queue a message — sends after this turn
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   press esc again to interrupt
    |}];
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-19c0f55e
    04 |
    05 | ❯ go in the wrong direction
    06 |
    07 | ◌ Interrupted — tell mentat what to do differently.
    08 |
    09 | ❯ also update the changelog
    10 |
    11 | ⠙ Working… (0s · esc to interrupt)
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
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-… · ! full access ? for s…
    |}];
  finish_turn t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-19c0f55e
    04 |
    05 | ❯ go in the wrong direction
    06 |
    07 | ◌ Interrupted — tell mentat what to do differently.
    08 |
    09 | ❯ also update the changelog
    10 |
    11 | ⏺ Changelog updated after the interrupt.
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
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-… · ! full access ? for s…
    |}]
