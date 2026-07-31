(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* [/init] is a prompt macro: submitting it sends a built-in AGENTS.md
   initialization prompt as an ordinary Build-mode turn, so the write flows
   through the normal turn and mutation machinery. It is idle-only and echoes
   nothing — the submitted prompt is the single transcript artifact. *)

open Tui_harness
module Command = Mentat_tui.Command

(* The exact prompt /init carries, read from its public fate so the scripted
   provider expects precisely what the shell submits. *)
let init_prompt =
  match Command.parse "/init" with
  | Some (Command.Exact command) -> (
      match Command.fate command with
      | Command.Init_project prompt -> prompt
      | _ -> failwith "test_init: /init fate is not Init_project")
  | _ -> failwith "test_init: /init did not parse"

let hello_turn =
  Tui.Turn_script.complete ~prompt:"say hello"
    "Hello from the scripted provider."

let init_turn = Tui.Turn_script.complete ~prompt:init_prompt "Wrote AGENTS.md."
let busy_turn = Tui.Turn_script.complete ~prompt:"keep working" "Done."

let reach_transcript t =
  Tui.settle t;
  Tui.keys t "say hello";
  Tui.enter t;
  Tui.settle t;
  Tui.finish_turn t;
  Tui.settle t

let submit t text =
  Tui.paste t text;
  Tui.settle t;
  Tui.enter t;
  Tui.settle t

let%expect_test "the slash palette lists /init" =
  Tui.run ~name:"init-palette" ~turns:[ hello_turn ] @@ fun t ->
  reach_transcript t;
  Tui.keys t "/ini";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-init6a9b57a1
    04 |
    05 | ❯ say hello
    06 |
    07 | ⏺ Hello from the scripted provider.
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
    20 | ❯ /init   Generate or update AGENTS.md with project conventions
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ /ini
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-… · openai/gpt-… · ! full access ? for …
    |}]

let%expect_test "/init is unavailable while a turn is in flight" =
  Tui.run ~name:"init-midturn" ~turns:[ busy_turn ] @@ fun t ->
  Tui.settle t;
  submit t "keep working";
  (* The turn is held in flight; the idle-only command reports its gate. *)
  submit t "/init";
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-initaadf9b7c
    04 |
    05 | ❯ keep working
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
    24 |   /init is available after the turn finishes
    |}]

let%expect_test "/init submits its built-in prompt as an ordinary turn" =
  Tui.run ~name:"init-submit" ~turns:[ hello_turn; init_turn ] @@ fun t ->
  reach_transcript t;
  submit t "/init";
  Tui.finish_turn t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-ini57895def
    04 |
    05 | ❯ say hello
    06 |
    07 | ⏺ Hello from the scripted provider.
    08 |
    09 | ❯ Explore this repository and write or update its AGENTS.md so a new
    10 |   contributor can get productive fast. Cover how to build, test, and run the
    11 |   project, the layout and where the key code lives, and the conventions and
    12 |   architectural rules to follow. Keep it concise, around 20-30 lines. If
    13 |   AGENTS.md already exists, improve it in place rather than replacing it, and
    14 |   fold in any conventions from other agent- or editor-instruction files.
    15 |   Document only what you can verify from the repository; do not invent anything.
    16 |
    17 | ⏺ Wrote AGENTS.md.
    18 |
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-… · openai/gpt-… · ! full access ? for …
    |}]

