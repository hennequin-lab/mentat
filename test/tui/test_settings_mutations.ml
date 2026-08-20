(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
module Permission = Mentat_permission
module Protocol = Mentat_protocol

let submit t text =
  Tui.paste t text;
  Tui.settle t;
  Tui.enter t;
  Tui.settle t

let open_permission_control t =
  submit t "/config";
  Tui.keys t Key.down;
  Tui.settle t

let choose_bypass t =
  (* Left selects the preceding value in the exact [Enforce; Bypass]
     vocabulary, which from no candidate is Bypass. This only sets a local,
     visibly unconfirmed candidate; it never reaches the engine, so it can
     neither round-trip nor revert under a stale echo. *)
  Tui.keys t Key.left;
  Tui.settle t

let apply t =
  (* Enter applies the chosen candidate and holds the request at the client
     boundary until the test settles it explicitly. *)
  Tui.enter t;
  Tui.settle t

let unavailable text =
  Protocol.Error.Unavailable (Mentat_diagnostic.of_text text)

(* Model selection has the same next-turn ownership contract and is exercised
   through complete frames in [test_model_selection.ml], including success,
   failure, single flight, and inactive-session settlement.
   These journeys cover the other live settings setter: permission review.
   Every expectation is the complete numbered terminal. *)

let%expect_test
    "choosing a review value stays local; Enter applies it and a tab switch \
     clears the confirmation" =
  let establish =
    Tui.Turn_script.complete ~prompt:"establish review owner"
      "The settings owner is ready."
  in
  let next_turn =
    Tui.Turn_script.complete ~prompt:"use accepted review"
      "The accepted review was sealed."
  in
  Tui.run ~name:"settings-review-accepted" ~size:(100, 24)
    ~permission_reviews:[ Permission.Review_behavior.Bypass ]
    ~turns:[ establish; next_turn ]
  @@ fun t ->
  submit t "establish review owner";
  Tui.finish_turn t;
  Tui.settle t;
  open_permission_control t;
  choose_bypass t;
  (* The candidate is shown but nothing is in flight: the footer fact stays
     [unavailable], not [requesting], and the hint is the ordinary action row,
     not [request in flight]. Left made no request, so nothing can revert. *)
  Tui.print t;
  [%expect
    {|
    01 |  settings ────────────────────────────────────────────────────────────────────────────── unavailable
    02 |
    03 | config  status  usage
    04 |
    05 | Session controls below apply to the next turn only.
    06 |
    07 | !  configuration unavailable in the visual harness
    08 |
    09 |      setting                    value                                    source
    10 |      model                      openai/gpt-5.5                           next turn
    11 |  ›   permission review          candidate: bypass                        next turn
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↑↓ move · ←→ change · ↵ apply · tab page · / filter · esc back
    |}];

  apply t;
  Tui.print t;
  [%expect
    {|
    01 |  settings ─────────────────────────────────────────────────────────────────────────────── requesting
    02 |
    03 | config  status  usage
    04 |
    05 | ⚠  Request in flight: permission review bypass for the next turn.
    06 |
    07 | Session controls below apply to the next turn only.
    08 |
    09 | !  configuration unavailable in the visual harness
    10 |
    11 |      setting                    value                                    source
    12 |      model                      openai/gpt-5.5                           next turn
    13 |  ›   permission review          requesting: bypass                       next turn
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   request in flight · tab page · esc back
    |}];

  Tui.finish_permission_review t (Ok ());
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  settings ───────────────────────────────────────────────────────────────────────────────── accepted
    02 |
    03 | config  status  usage
    04 |
    05 | ✓  Client accepted permission review bypass for the next turn.
    06 |
    07 | Session controls below apply to the next turn only.
    08 |
    09 | !  configuration unavailable in the visual harness
    10 |
    11 |      setting                    value                                    source
    12 |      model                      openai/gpt-5.5                           next turn
    13 |  ›   permission review          choose a next-turn request               next turn
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↑↓ move · ←→ change · ↵ apply · tab page · / filter · esc back
    |}];

  (* Switching page dismisses the acceptance acknowledgement; it does not
     linger as stale chrome on the read-only pages. *)
  Tui.keys t Key.tab;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  settings ────────────────────────────────────────────────────────────────────────────── 1 providers
    02 |
    03 | config  status  usage
    04 |
    05 | Runtime
    06 |   version         dev
    07 |   current model   openai/gpt-5.5
    08 |   workspace       ~/mentat-tui-settings-review-8c5171f8
    09 |   context window  128,000 tokens
    10 |   launch sandbox  danger-full-access
    11 |
    12 | Session
    13 |   id            session-visual-00001
    14 |   lifecycle     active
    15 |   phase         idle
    16 |   active model  openai/gpt-5.5
    17 |   waiting       none
    18 |   workflow      build
    19 |   last outcome  completed
    20 |
    21 | Providers
    22 |   openai  missing
    23 |
    24 |   tab page · esc back
    |}];

  Tui.keys t Key.escape;
  Tui.settle t;
  submit t "use accepted review";
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-settings-review-8c5171f8
    04 |
    05 | ❯ establish review owner
    06 |
    07 | ⏺ The settings owner is ready.
    08 |
    09 | ❯ use accepted review
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
    21 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    22 | ❯ queue a message — sends after this turn
    23 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    24 |   ! never ask · /settings · ! not logged in · /login · ~/menta… · openai/gpt… · ! full access ? f…
    |}];
  Tui.finish_turn t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-settings-review-8c5171f8
    04 |
    05 | ❯ establish review owner
    06 |
    07 | ⏺ The settings owner is ready.
    08 |
    09 | ❯ use accepted review
    10 |
    11 | ⏺ The accepted review was sealed.
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    24 |   ! never ask · /settings · ! not logged in · /login · ~/menta… · openai/gpt… · ! full access ? f…
    |}]

let%expect_test
    "rejected review retains its candidate and cannot change the next turn" =
  let establish =
    Tui.Turn_script.complete ~prompt:"establish enforced review"
      "Enforced review is active."
  in
  let next_turn =
    Tui.Turn_script.complete ~prompt:"keep enforced review"
      "The rejected candidate was not applied."
  in
  Tui.run ~name:"settings-review-rejected" ~size:(100, 24)
    ~permission_reviews:[ Permission.Review_behavior.Bypass ]
    ~turns:[ establish; next_turn ]
  @@ fun t ->
  submit t "establish enforced review";
  Tui.finish_turn t;
  Tui.settle t;
  open_permission_control t;
  choose_bypass t;
  apply t;
  Tui.print t;
  [%expect
    {|
    01 |  settings ─────────────────────────────────────────────────────────────────────────────── requesting
    02 |
    03 | config  status  usage
    04 |
    05 | ⚠  Request in flight: permission review bypass for the next turn.
    06 |
    07 | Session controls below apply to the next turn only.
    08 |
    09 | !  configuration unavailable in the visual harness
    10 |
    11 |      setting                    value                                    source
    12 |      model                      openai/gpt-5.5                           next turn
    13 |  ›   permission review          requesting: bypass                       next turn
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   request in flight · tab page · esc back
    |}];

  Tui.finish_permission_review t
    (Error
       (unavailable
          "permission overlay rejected by the owner\n\
           enforced review remains effective"));
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  settings ──────────────────────────────────────────────────────────────────────────────────── error
    02 |
    03 | config  status  usage
    04 |
    05 | !  Client rejected permission review bypass.
    06 |
    07 | permission overlay rejected by the owner
    08 | enforced review remains effective
    09 |
    10 | Session controls below apply to the next turn only.
    11 |
    12 | !  configuration unavailable in the visual harness
    13 |
    14 |      setting                    value                                    source
    15 |      model                      openai/gpt-5.5                           next turn
    16 |  ›   permission review          candidate: bypass                        next turn
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   scroll error · ↑↓ move · ←→ change · ↵ apply · tab page · / filter · esc back
    |}];

  Tui.keys t Key.escape;
  Tui.settle t;
  submit t "keep enforced review";
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-settings-review-db675122
    04 |
    05 | ❯ establish enforced review
    06 |
    07 | ⏺ Enforced review is active.
    08 |
    09 | ❯ keep enforced review
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
    21 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    22 | ❯ queue a message — sends after this turn
    23 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-tui-settings-rev… · openai/gpt-… · ! full access ? for shor…
    |}];
  Tui.finish_turn t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-settings-review-db675122
    04 |
    05 | ❯ establish enforced review
    06 |
    07 | ⏺ Enforced review is active.
    08 |
    09 | ❯ keep enforced review
    10 |
    11 | ⏺ The rejected candidate was not applied.
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-tui-settings-rev… · openai/gpt-… · ! full access ? for shor…
    |}]
