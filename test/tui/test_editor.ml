(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* The external-editor escape (Ctrl+X Ctrl+E): the chord seeds the advertised
   editor callback with the fully-expanded draft and installs the returned
   buffer as a plain draft. This drives the reducer fold and the correlated
   round-trip with a fake in-process editor. Under the headless backend the
   runtime's Matrix.suspend ~leave_alt:true / resume bracket is a safe no-op; the
   real terminal handoff and the $EDITOR spawn are covered by the pty suite. *)

open Tui_harness

let hello_turn =
  Tui.Turn_script.complete ~prompt:"say hello"
    "Hello from the scripted provider."

let reach_transcript t =
  Tui.settle t;
  Tui.keys t "say hello";
  Tui.enter t;
  Tui.settle t;
  Tui.finish_turn t;
  Tui.settle t

(* Ctrl+X is byte 0x18; Ctrl+E is byte 0x05. *)
let ctrl_x = "\024"
let ctrl_e = "\005"

let%expect_test "the editor chord replaces the draft with the edited buffer" =
  Tui.run ~name:"editor-roundtrip"
    ~edit_in_editor:(fun seed -> "edited(" ^ seed ^ ")")
    ~turns:[ hello_turn ]
  @@ fun t ->
  reach_transcript t;
  Tui.keys t "draft body";
  Tui.settle t;
  Tui.keys t ctrl_x;
  Tui.settle t;
  Tui.keys t ctrl_e;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-editor-rd09fc73c
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
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ edited(draft body)
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt-… · ! full acce… ? for …
    |}]

(* The armed chord captures only its completion key; any other key aborts the
   chord and types normally, so the editor never opens. *)
let%expect_test
    "a printable key during an armed chord types and does not open the editor" =
  Tui.run ~name:"editor-abort"
    ~edit_in_editor:(fun seed -> "edited(" ^ seed ^ ")")
    ~turns:[ hello_turn ]
  @@ fun t ->
  reach_transcript t;
  Tui.keys t "draft";
  Tui.settle t;
  Tui.keys t ctrl_x;
  Tui.settle t;
  Tui.keys t "z";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-edit65b79e4c
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
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ draftz
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-… · openai/gpt-… · ! full access ? for …
    |}]
