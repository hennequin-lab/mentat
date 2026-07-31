(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Custom user commands in the TUI: the slash palette lists discovered commands
   beside the builtins, invoking one expands its template and submits the
   expanded text as a turn, and an unrecognized slash line passes through as a
   literal prompt. The custom-command catalog and the expansion both cross the
   fake client the harness injects. *)

open Tui_harness
module Protocol = Mentat_protocol

let name_exn spelling =
  Result.get_ok (Protocol.User_command.Name.of_string spelling)

(* One project command whose body substitutes its first positional argument. *)
let greet =
  ( {
      Protocol.User_command.name = name_exn "greet";
      description = Some "Greet someone warmly.";
      argument_hint = Some "<name>";
      scope = Protocol.User_command.Project;
    },
    "Say hello to $1 warmly." )

let submit t prompt =
  Tui.keys t prompt;
  Tui.enter t;
  Tui.settle t

let settle_turn t =
  Tui.finish_turn t;
  Tui.settle t

let%expect_test "a custom command appears in the slash palette" =
  Tui.run ~name:"palette" ~user_commands:[ greet ] @@ fun t ->
  Tui.keys t "/greet";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
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
    13 | ❯ /greet   Greet someone warmly.                                <name> (project)
    14 | ────────────────────────────────────────────────────────────────────────────────
    15 | ❯ /greet
    16 | ────────────────────────────────────────────────────────────────────────────────
    17 |
    18 |                          ! /login — no connected account
    19 |                               ∅ no recent sessions
    20 |
    21 |
    22 |
    23 |
    24 |   ! not logged in · /login · ~/men… · openai/gpt-5.5 m… · ! full access ? for…
    |}]

let%expect_test "invoking a custom command expands and submits it" =
  let turn =
    Tui.Turn_script.complete ~prompt:"Say hello to World warmly."
      "Hello, World!"
  in
  Tui.run ~name:"expand" ~user_commands:[ greet ] ~turns:[ turn ] @@ fun t ->
  submit t "/greet World";
  settle_turn t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-a3f879cb
    04 |
    05 | ❯ Say hello to World warmly.
    06 |
    07 | ⏺ Hello, World!
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
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-… · ! full access ? for s…
    |}]

let%expect_test "an unknown slash command is a literal prompt" =
  let turn =
    Tui.Turn_script.complete ~prompt:"/nope keep it literal" "literal reply"
  in
  Tui.run ~name:"literal" ~user_commands:[ greet ] ~turns:[ turn ] @@ fun t ->
  submit t "/nope keep it literal";
  settle_turn t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-2f957122
    04 |
    05 | ❯ /nope keep it literal
    06 |
    07 | ⏺ literal reply
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
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-… · ! full access ? for s…
    |}]

