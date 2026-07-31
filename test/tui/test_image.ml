(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness

(* Complete-screen goldens for the image-attach composer surface (§stage 7): an
   image [@]-mention or a clipboard paste attaches through the runtime, the draft
   carries an atomic [[Image #N]] element, and the submitted media rides ahead of
   the prompt text into the transcript. Every expectation is the full grid. *)

let hello_turn =
  Tui.Turn_script.complete ~prompt:"say hello"
    "Hello from the scripted provider."

let look_turn =
  Tui.Turn_script.complete ~prompt:"look at this" "I see a screenshot."

let reach_transcript t =
  Tui.settle t;
  Tui.keys t "say hello";
  Tui.enter t;
  Tui.settle t;
  Tui.finish_turn t;
  Tui.settle t

let path text = Lpath.Rel.of_string_exn text
let enumerate_files () = Ok [ path "notes.md"; path "shot.png" ]

let%expect_test "an image mention attaches and lands in the transcript" =
  Tui.run ~name:"image-mention" ~enumerate_files ~file_enumeration:true
    ~image_attach:true ~turns:[ hello_turn; look_turn ]
  @@ fun t ->
  reach_transcript t;
  Tui.keys t "@shot";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-image2bdf2df1
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
    22 | ❯ [Image #1]
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-… · openai/gpt-… · ! full access ? for …
    |}];
  Tui.keys t " look at this";
  Tui.enter t;
  Tui.settle t;
  Tui.finish_turn t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-image2bdf2df1
    04 |
    05 | ❯ say hello
    06 |
    07 | ⏺ Hello from the scripted provider.
    08 |
    09 | ❯ [media: image/png] · attachment
    10 |
    11 |   look at this
    12 |
    13 | ⏺ I see a screenshot.
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
    24 |   ! not logged in · /login · ~/mentat-… · openai/gpt-… · ! full access ? for …
    |}]

let%expect_test "an attached image is removable with a single backspace" =
  Tui.run ~name:"image-remove" ~enumerate_files ~file_enumeration:true
    ~image_attach:true ~turns:[ hello_turn ]
  @@ fun t ->
  reach_transcript t;
  Tui.keys t "@shot";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-imagc8ae0cf9
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
    22 | ❯ [Image #1]
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-… · openai/gpt-… · ! full access ? for …
    |}];
  Tui.keys t Key.backspace;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-imagc8ae0cf9
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
    24 |   ! not logged in · /login · ~/mentat-… · openai/gpt-… · ! full access ? for …
    |}]

let%expect_test "the image count cap pre-warns before exceeding it" =
  Tui.run ~name:"image-cap" ~enumerate_files ~file_enumeration:true
    ~image_attach:true ~image_max_count:1 ~turns:[ hello_turn ]
  @@ fun t ->
  reach_transcript t;
  Tui.keys t "@shot";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  (* A separating space starts a fresh [@]-token: the [[Image #1]] atom's own
     space would otherwise absorb an immediately-following [@]. *)
  Tui.keys t " @shot";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-iecb1568f
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
    22 | ❯ [Image #1]
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   image limit reached (1)
    |}]

let%expect_test "a rejected attach surfaces a notice" =
  let rejection =
    Mentat_protocol.Attach.Rejection.Too_large
      { bytes = 9_000_000; cap = 5_242_880 }
  in
  Tui.run ~name:"image-reject" ~enumerate_files ~file_enumeration:true
    ~image_attach:true
    ~attach_responses:
      [ Error (Mentat_protocol.Attach.Error.Rejected rejection) ]
    ~turns:[ hello_turn ]
  @@ fun t ->
  reach_transcript t;
  Tui.keys t "@shot";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-imag25e017f6
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
    24 |   attach failed: image is too large (9000000 bytes, cap 5242880 bytes)
    |}]

let%expect_test "a clipboard image paste attaches" =
  Tui.run ~name:"image-clipboard" ~image_attach:true
    ~clipboard_image:("image/png", "clipboard-png-bytes")
    ~turns:[ hello_turn ]
  @@ fun t ->
  reach_transcript t;
  Tui.paste t "";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-image-c44c08a37
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
    22 | ❯ [Image #1]
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt… · ! full access ? for …
    |}]
