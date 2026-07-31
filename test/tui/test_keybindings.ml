(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* A user keybinding overlay overrides the registry chords the shell resolves.
   This drives one remapped action end-to-end through the real Elm shell: history
   search moved off Ctrl+R onto Ctrl+T — the new chord opens it and the former
   default no longer does. Overlay parsing and diagnostics are unit-tested in
   test/unit/test_command.ml. *)

open Tui_harness
module Command = Mentat_tui.Command

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

let history texts =
  texts
  |> List.mapi (fun index text ->
      Printf.sprintf
        {|{"schema_version":1,"type":"mentat.tui.history_entry","session_id":"session-history","timestamp":"%d","draft":{"text":%S}}|}
        (1_000 - index) text)
  |> String.concat "\n"

(* Ctrl+T is byte 0x14; the terminal parser normalizes it to Char 't' + ctrl. *)
let ctrl_t = "\020"

let remap pairs =
  let overlay, diagnostics =
    Command.Overlay.of_json
      (Jsont.Json.object'
         (List.map
            (fun (name, chord) ->
              Jsont.Json.mem (Jsont.Json.name name) (Jsont.Json.string chord))
            pairs))
  in
  assert (diagnostics = []);
  overlay

let%expect_test "the former default no longer opens history search" =
  let history = history [ "beta two"; "alpha one" ] in
  let overlay = remap [ ("history_search", "ctrl+t") ] in
  Tui.run ~name:"keybindings-old-key" ~overlay ~history ~turns:[ hello_turn ]
  @@ fun t ->
  reach_transcript t;
  Tui.keys t Key.ctrl_r;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-keybindings013eb6b0
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
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt-… · ! full access ? for…
    |}]

let%expect_test "the remapped chord opens history search" =
  let history = history [ "beta two"; "alpha one" ] in
  let overlay = remap [ ("history_search", "ctrl+t") ] in
  Tui.run ~name:"keybindings-new-key" ~overlay ~history ~turns:[ hello_turn ]
  @@ fun t ->
  reach_transcript t;
  Tui.keys t ctrl_t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-keybindings05eb9c65
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
    18 | reverse-i-search:
    19 | ❯ alpha one
    20 |   beta two
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ⌕ search history
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ↵ insert · esc cancel · type to search                             ⌕ history
    |}]

