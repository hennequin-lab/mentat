(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
module Task = Mentat_session.Task

let submit t prompt =
  Tui.settle t;
  Tui.keys t prompt;
  Tui.enter t;
  Tui.settle t

let task ~id ~content ~status ~position =
  match
    Task.Item.make ~id:(Task.Id.of_string id) ~owner:Task.Owner.Main ~content
      ~status ~position ()
  with
  | Ok item -> item
  | Error error -> failwith (Task.Error.message error)

let board items =
  match Task.Board.make items with
  | Ok board -> board
  | Error error -> failwith (Task.Error.message error)

let early_board =
  let open Task.Status in
  board
    [
      task ~id:"task-1" ~content:"scaffold the module" ~status:In_progress
        ~position:0;
      task ~id:"task-2" ~content:"write the public interface" ~status:Pending
        ~position:1;
      task ~id:"task-3" ~content:"add complete-screen tests" ~status:Pending
        ~position:2;
    ]

let replacement_board =
  let open Task.Status in
  board
    [
      task ~id:"task-1" ~content:"scaffold the module" ~status:Completed
        ~position:0;
      task ~id:"task-2" ~content:"write the public interface"
        ~status:In_progress ~position:1;
      task ~id:"task-3" ~content:"add complete-screen tests" ~status:Pending
        ~position:2;
    ]

let full_board =
  let open Task.Status in
  board
    [
      task ~id:"task-1" ~content:"scaffold the module" ~status:In_progress
        ~position:0;
      task ~id:"task-2" ~content:"write the public interface" ~status:Pending
        ~position:1;
      task ~id:"task-3" ~content:"wire the application shell" ~status:Pending
        ~position:2;
      task ~id:"task-4" ~content:"add complete-screen tests" ~status:Pending
        ~position:3;
      task ~id:"task-5" ~content:"review the visual diff" ~status:Pending
        ~position:4;
      task ~id:"task-6" ~content:"read the layout RFC" ~status:Completed
        ~position:5;
      task ~id:"task-7" ~content:"survey the legacy UI" ~status:Cancelled
        ~position:6;
    ]

let terminal_board =
  let open Task.Status in
  board
    [
      task ~id:"task-1" ~content:"scaffold the module" ~status:Completed
        ~position:0;
      task ~id:"task-2" ~content:"write the public interface" ~status:Completed
        ~position:1;
      task ~id:"task-3" ~content:"wire the application shell" ~status:Completed
        ~position:2;
      task ~id:"task-4" ~content:"add complete-screen tests" ~status:Completed
        ~position:3;
      task ~id:"task-5" ~content:"review the visual diff" ~status:Completed
        ~position:4;
      task ~id:"task-6" ~content:"read the layout RFC" ~status:Completed
        ~position:5;
      task ~id:"task-7" ~content:"survey the legacy UI" ~status:Cancelled
        ~position:6;
    ]

(* Each expectation is the entire numbered terminal. Board updates enter through
   Session.Event.tasks_replaced and Fact.Journal_task_board in the harness; the
   tests never construct or mutate App state. *)

let%expect_test
    "whole-board replacement stays atomic and open work survives settlement" =
  let prompt = "plan the task-board port" in
  let turn =
    Tui.Turn_script.complete
      ~task_boards:[ early_board; replacement_board ]
      ~prompt "The port is ready for review."
  in
  Tui.run ~name:"todo-replacement" ~turns:[ turn ] @@ fun t ->
  submit t prompt;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-todo-repa6856c9b
    04 |
    05 | ❯ plan the task-board port
    06 |
    07 | ⏺ Todo(3 tasks · 0 done · 1 running)
    08 |
    09 | ⠋ Working… (0s · esc to interrupt)
    10 |
    11 |
    12 |
    13 |
    14 |
    15 | ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈
    16 |   ◻ 3 tasks · 0 done · 1 running
    17 |   ◼ scaffold the module
    18 |   ◻ write the public interface
    19 |   ◻ add complete-screen tests
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ queue a message — sends after this turn
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt-… · ! full acce… ? for …
    |}];
  Tui.next_task_board t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-todo-repa6856c9b
    04 |
    05 | ❯ plan the task-board port
    06 |
    07 | ⏺ Todo(3 tasks · 1 done · 1 running)
    08 |
    09 | ⠋ Working… (0s · esc to interrupt)
    10 |
    11 |
    12 |
    13 |
    14 |
    15 | ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈
    16 |   ◻ 3 tasks · 1 done · 1 running
    17 |   ◼ write the public interface
    18 |   ◻ add complete-screen tests
    19 |   … 1 done
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ queue a message — sends after this turn
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt-… · ! full acce… ? for …
    |}];
  Tui.finish_turn t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-todo-repa6856c9b
    04 |
    05 | ❯ plan the task-board port
    06 |
    07 | ⏺ Todo(3 tasks · 1 done · 1 running)
    08 |
    09 | ⏺ The port is ready for review.
    10 |
    11 |
    12 |
    13 |
    14 |
    15 | ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈
    16 |   ◻ 3 tasks · 1 done · 1 running
    17 |   ◼ write the public interface
    18 |   ◻ add complete-screen tests
    19 |   … 1 done
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt-… · ! full acce… ? for …
    |}]

let%expect_test "an interrupted turn keeps its open task board visible" =
  let prompt = "continue until interrupted" in
  let turn =
    Tui.Turn_script.complete ~task_boards:[ full_board ] ~prompt
      "This response is intentionally interrupted."
  in
  Tui.run ~name:"todo-interrupt" ~turns:[ turn ] @@ fun t ->
  submit t prompt;
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-todo-ida4869d5
    04 |
    05 | ❯ continue until interrupted
    06 |
    07 | ⏺ Todo(7 tasks · 2 done · 1 running)
    08 |
    09 | ◌ Interrupted — tell mentat what to do differently.
    10 |
    11 |
    12 | ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈
    13 |   ◻ 7 tasks · 2 done · 1 running
    14 |   ◼ scaffold the module
    15 |   ◻ write the public interface
    16 |   ◻ wire the application shell
    17 |   ◻ add complete-screen tests
    18 |   ◻ review the visual diff
    19 |   … 2 done
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt… · ! full access ? for …
    |}]

let%expect_test "scarce height keeps the open board complete" =
  let prompt = "finish every task" in
  let turn =
    Tui.Turn_script.complete ~task_boards:[ full_board ] ~prompt
      "Every task is terminal."
  in
  Tui.run ~name:"todo-scarce-height" ~size:(80, 12) ~turns:[ turn ] @@ fun t ->
  submit t prompt;
  Tui.print t;
  [%expect
    {|
    01 | ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈
    02 |   ◻ 7 tasks · 2 done · 1 running
    03 |   ◼ scaffold the module
    04 |   ◻ write the public interface
    05 |   ◻ wire the application shell
    06 |   ◻ add complete-screen tests
    07 |   ◻ review the visual diff
    08 |   … 2 done
    09 | ────────────────────────────────────────────────────────────────────────────────
    10 | ❯ queue a message — sends after this turn
    11 | ────────────────────────────────────────────────────────────────────────────────
    12 |   ! not logged in · /login · ~/mentat-t… · openai/gpt-… · ! full access ? for…
    |}]

let%expect_test "a completed board stays visible with its tasks checked" =
  let prompt = "finish every task" in
  let turn =
    Tui.Turn_script.complete
      ~task_boards:[ full_board; terminal_board ]
      ~prompt "Every task is terminal."
  in
  Tui.run ~name:"todo-completed" ~turns:[ turn ] @@ fun t ->
  submit t prompt;
  Tui.next_task_board t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-todo-c6deeeaba
    04 |
    05 | ❯ finish every task
    06 |
    07 | ⏺ Todo(7 tasks · 7 done · 0 running)
    08 |
    09 | ⠋ Working… (0s · esc to interrupt)
    10 |
    11 | ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈
    12 |   ◻ 7 tasks · 7 done · 0 running
    13 |   ☒ scaffold the module
    14 |   ☒ write the public interface
    15 |   ☒ wire the application shell
    16 |   ☒ add complete-screen tests
    17 |   ☒ review the visual diff
    18 |   ☒ read the layout RFC
    19 |   ☒ survey the legacy UI
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ queue a message — sends after this turn
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt… · ! full access ? for …
    |}];
  Tui.finish_turn t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-todo-c6deeeaba
    04 |
    05 | ❯ finish every task
    06 |
    07 | ⏺ Todo(7 tasks · 7 done · 0 running)
    08 |
    09 | ⏺ Every task is terminal.
    10 |
    11 | ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈
    12 |   ◻ 7 tasks · 7 done · 0 running
    13 |   ☒ scaffold the module
    14 |   ☒ write the public interface
    15 |   ☒ wire the application shell
    16 |   ☒ add complete-screen tests
    17 |   ☒ review the visual diff
    18 |   ☒ read the layout RFC
    19 |   ☒ survey the legacy UI
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt… · ! full access ? for …
    |}]
