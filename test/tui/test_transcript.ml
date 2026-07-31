(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
module Json = Jsont.Json
module Llm = Mentat_llm
module Tool = Mentat_tool

let submit t prompt =
  Tui.keys t prompt;
  Tui.enter t;
  Tui.settle t

let settle_turn t =
  Tui.finish_turn t;
  Tui.settle t

let listing_tool ~id text =
  Tui.Turn_script.tool
    ~call:
      (Llm.Tool.Call.make ~id ~name:"list_directory" ~input:(Json.object' []) ())
    ~result:(Tool.Result.completed ~output:(Tool.Output.make ~text ()) ())

let overflow_answer =
  String.concat "\n\n"
    [
      "Retries begin at the base delay.";
      "The delay doubles on each failure.";
      "A jitter term spreads the retries apart.";
      "The ceiling caps the longest wait.";
      "Idempotent requests retry safely.";
      "Non-idempotent ones need an idempotency key.";
      "The budget bounds the total attempts.";
      "Giving up surfaces the last error.";
      "Metrics record every retry attempt.";
      "Backpressure slows the caller down.";
      "Circuit breakers trip on sustained failure.";
      "The final word is to cap the ceiling.";
    ]

let overflow_turn =
  Tui.Turn_script.complete ~prompt:"overflow the viewport" overflow_answer

(* Every expectation is a complete numbered terminal. In particular, the
   composer and footer stay visible while Mosaic moves only the transcript
   viewport, so a reviewer can see clipping or focus regressions in context. *)
let%expect_test "Mosaic pages the complete transcript and returns to its tail" =
  Tui.run ~name:"e" ~turns:[ overflow_turn ] @@ fun t ->
  submit t "overflow the viewport";
  settle_turn t;
  Tui.print t;
  [%expect
    {|
    01 |   A jitter term spreads the retries apart.
    02 |
    03 |   The ceiling caps the longest wait.
    04 |
    05 |   Idempotent requests retry safely.
    06 |
    07 |   Non-idempotent ones need an idempotency key.
    08 |
    09 |   The budget bounds the total attempts.
    10 |
    11 |   Giving up surfaces the last error.
    12 |
    13 |   Metrics record every retry attempt.
    14 |
    15 |   Backpressure slows the caller down.
    16 |
    17 |   Circuit breakers trip on sustained failure.
    18 |
    19 |   The final word is to cap the ceiling.
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-… · ! full access ? for s…
    |}];
  Tui.keys t Key.page_up;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-d56d5b5c
    04 |
    05 | ❯ overflow the viewport
    06 |
    07 | ⏺ Retries begin at the base delay.
    08 |
    09 |   The delay doubles on each failure.
    10 |
    11 |   A jitter term spreads the retries apart.
    12 |
    13 |   The ceiling caps the longest wait.
    14 |
    15 |   Idempotent requests retry safely.
    16 |
    17 |   Non-idempotent ones need an idempotency key.
    18 |
    19 |   The budget bounds the total attempts.
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-… · ! full access ? for s…
    |}];
  Tui.keys t Key.page_down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |   A jitter term spreads the retries apart.
    02 |
    03 |   The ceiling caps the longest wait.
    04 |
    05 |   Idempotent requests retry safely.
    06 |
    07 |   Non-idempotent ones need an idempotency key.
    08 |
    09 |   The budget bounds the total attempts.
    10 |
    11 |   Giving up surfaces the last error.
    12 |
    13 |   Metrics record every retry attempt.
    14 |
    15 |   Backpressure slows the caller down.
    16 |
    17 |   Circuit breakers trip on sustained failure.
    18 |
    19 |   The final word is to cap the ceiling.
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-… · ! full access ? for s…
    |}]

let%expect_test "submitting after paging returns the complete UI to the tail" =
  let turns =
    [
      overflow_turn;
      Tui.Turn_script.complete ~prompt:"follow up from here"
        "The follow-up is complete.";
    ]
  in
  Tui.run ~name:"e" ~turns @@ fun t ->
  submit t "overflow the viewport";
  settle_turn t;
  Tui.keys t Key.page_up;
  Tui.settle t;
  submit t "follow up from here";
  Tui.print t;
  [%expect
    {|
    01 |   Idempotent requests retry safely.
    02 |
    03 |   Non-idempotent ones need an idempotency key.
    04 |
    05 |   The budget bounds the total attempts.
    06 |
    07 |   Giving up surfaces the last error.
    08 |
    09 |   Metrics record every retry attempt.
    10 |
    11 |   Backpressure slows the caller down.
    12 |
    13 |   Circuit breakers trip on sustained failure.
    14 |
    15 |   The final word is to cap the ceiling.
    16 |
    17 | ❯ follow up from here
    18 |
    19 | ⠋ Working… (0s · esc to interrupt)
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ queue a message — sends after this turn
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-… · ! full access ? for s…
    |}];
  settle_turn t

let wheel_up ~column ~row = Printf.sprintf "\027[<64;%d;%dM" column row

let%expect_test "a resize cycle re-engages the complete transcript tail" =
  let turns =
    [
      overflow_turn;
      Tui.Turn_script.complete ~prompt:"the appendix now"
        "The appendix closes the discussion.";
    ]
  in
  Tui.run ~name:"e" ~turns @@ fun t ->
  submit t "overflow the viewport";
  settle_turn t;
  Tui.resize t ~width:40 ~height:12;
  Tui.settle t;
  Tui.keys t (wheel_up ~column:10 ~row:4);
  Tui.settle t;
  Tui.resize t ~width:80 ~height:24;
  Tui.settle t;
  submit t "the appendix now";
  settle_turn t;
  Tui.print t;
  [%expect
    {|
    01 |   Idempotent requests retry safely.
    02 |
    03 |   Non-idempotent ones need an idempotency key.
    04 |
    05 |   The budget bounds the total attempts.
    06 |
    07 |   Giving up surfaces the last error.
    08 |
    09 |   Metrics record every retry attempt.
    10 |
    11 |   Backpressure slows the caller down.
    12 |
    13 |   Circuit breakers trip on sustained failure.
    14 |
    15 |   The final word is to cap the ceiling.
    16 |
    17 | ❯ the appendix now
    18 |
    19 | ⏺ The appendix closes the discussion.
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-… · ! full access ? for s…
    |}]

let markdown_answer =
  "## Retry design\n\n\
   The client backs off exponentially between attempts.\n\n\
   - Keep retries bounded.\n\
   - Add jitter.\n\n\
   ```ocaml\n\
   let retry n = n + 1\n\
   ```\n\n\
   Ship after reviewing the complete terminal."

let%expect_test "Markdown blocks remain spaced inside the complete transcript" =
  let turn =
    Tui.Turn_script.complete ~prompt:"show the retry design" markdown_answer
  in
  Tui.run ~name:"e" ~turns:[ turn ] @@ fun t ->
  submit t "show the retry design";
  settle_turn t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-d56d5b5c
    04 |
    05 | ❯ show the retry design
    06 |
    07 | ⏺ Retry design
    08 |
    09 |   The client backs off exponentially between attempts.
    10 |
    11 |   • Keep retries bounded.
    12 |   • Add jitter.
    13 |
    14 |   let retry n = n + 1
    15 |
    16 |   Ship after reviewing the complete terminal.
    17 |
    18 |
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-… · ! full access ? for s…
    |}]

(* Regression: a final assistant answer that follows a tool call keeps its ⏺
   marker and 2-column hanging gutter even when the answer is wide enough to wrap
   the transcript row. The settled marker previously used a shrinking flex slot,
   so a wide answer collapsed the gutter to zero — the bullet vanished and the
   prose slid to column 0, unlike every fitting answer. *)
let%expect_test "a wide final answer after a tool keeps its marker and gutter" =
  let answer =
    "The workspace listing is complete and its inventory is recorded for the \
     remainder of this settled turn."
  in
  let turn =
    Tui.Turn_script.complete ~prompt:"list the workspace and summarize it"
      ~tools:[ listing_tool ~id:"ls" "src\ntest\nREADME.md" ]
      answer
  in
  Tui.run ~home:(fun _ -> None) ~name:"e" ~size:(52, 20) ~turns:[ turn ]
  @@ fun t ->
  submit t "list the workspace and summarize it";
  settle_turn t;
  Tui.print t;
  [%expect
    {|
    01 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    02 |  dev · openai/gpt-5.5 medium
    03 |  $TESTCASE_ROOT
    04 |
    05 | ❯ list the workspace and summarize it
    06 |
    07 | ⏺ List_directory
    08 |   ⎿  done
    09 |       src
    10 |       test
    11 |       README.md
    12 |
    13 | ⏺ The workspace listing is complete and its
    14 |   inventory is recorded for the remainder of this
    15 |   settled turn.
    16 |
    17 | ────────────────────────────────────────────────────
    18 | ❯ message mentat
    19 | ────────────────────────────────────────────────────
    20 |   ! not logged in · /log… · open… · ! full access
    |}]
