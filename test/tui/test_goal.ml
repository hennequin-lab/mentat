(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
module Protocol = Mentat_protocol
module Session = Mentat_session

let session_id = Session.Id.of_string "session-goal-visual"
let goal_id = Session.Goal.Id.of_string "goal-release-parser"

let append session updates =
  let events =
    List.map (fun update -> Session.Event.goal_updated update) updates
  in
  match Session.append_all events session with
  | Ok session -> session
  | Error error ->
      failwith ("invalid goal visual fixture: " ^ Session.Error.message error)

let document project updates =
  Session.create ~id:session_id
    ~cwd:(Lpath.Abs.of_string_exn (Project.root project))
    ~created_at:(Session.Time.of_unix_ms 1_000_000L)
    ()
  |> fun session -> append session updates

let run ?(size = (80, 24)) ?session_view_results
    ?(hold_session_view_results = false) ~name updates test =
  Tui.run ~name ~size ~session:session_id ?session_view_results
    ~hold_session_view_results
    ~sessions:(fun project -> [ document project updates ])
    test

(* [/goal] now takes an optional objective, so the palette inserts "/goal " on
   the first Enter (offering the argument) and the bare command dispatches on the
   second, exactly like the other argument-taking slash commands. *)
let open_goal t =
  Tui.paste t "/goal";
  Tui.enter t;
  Tui.settle t;
  Tui.enter t;
  Tui.settle t

let active ?(objective = "Ship the parser without regressions") ?token_budget ()
    =
  Session.Goal.Update.declare ~id:goal_id ~objective ?token_budget ()

let unavailable text =
  Protocol.Error.Unavailable (Mentat_diagnostic.of_text text)

let accounted_document project =
  let provider = Mentat_llm.Provider.make "openai" in
  let api = Mentat_llm.Model.Api.make "responses" in
  let model = Mentat_llm.Model.make ~provider ~api ~id:"gpt-5.5" in
  let contract =
    Session.Contract.make ~mode:Session.Contract.Mode.Build ~model
      ~declarations:[] ~policy:Mentat_permission.Policy.default
      ~review:Mentat_permission.Review_behavior.Enforce
      ~sandbox:(Mentat_sandbox.identity Mentat_sandbox.direct)
      ()
  in
  let turn_id = Session.Turn.Id.of_string "turn-goal-continuation" in
  let turn =
    Session.Turn.make ~id:turn_id ~origin:Session.Turn.Origin.Goal_continuation
      ~input:Session.Turn.Input.continue ~max_steps:8 ~contract ()
  in
  let claim =
    Session.Provider_request.Started.make ~turn:turn_id
      ~request_digest:(Mentat_digest.string "goal continuation visual")
  in
  let response =
    Mentat_llm.Response.make ~model
      ~usage:(Mentat_llm.Usage.make ~input:2_500 ~output:750 ())
      (Mentat_llm.Message.Assistant.text "Continuation reached the budget.")
  in
  document project [ active ~token_budget:8_000 () ] |> fun session ->
  match
    Session.append_all
      [
        Session.Event.turn_started turn;
        Session.Event.provider_requested claim;
        Session.Event.provider_settled
          (Session.Provider_request.Settled.responded
             ~id:(Session.Provider_request.Started.id claim)
             response);
        Session.Event.turn_finished ~turn:turn_id Session.Turn.Outcome.completed;
        Session.Event.goal_updated
          (Session.Goal.Update.budget_limited ~id:goal_id);
      ]
      session
  with
  | Ok session -> session
  | Error error ->
      failwith
        ("invalid accounted goal visual fixture: " ^ Session.Error.message error)

(* Each checkpoint is the complete numbered terminal. The mutation is
   deliberately split into request, client acknowledgement, and durable fact
   so a reviewer can see that admission never fabricates owner state. *)
let%expect_test "pause waits for the authoritative goal fact" =
  run ~name:"goal-pause" [ active ~token_budget:24_000 () ] @@ fun t ->
  open_goal t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ─────────────────────────────────────────────────────────────────── active
    02 |
    03 |   status  active
    04 |
    05 |   goal id  goal-release-parser
    06 |
    07 |   objective
    08 |
    09 |     Ship the parser without regressions
    10 |
    11 |   activity  0 continuation turns
    12 |
    13 |   tokens  0 / 24,000 used · 24,000 remaining
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 | ❯ 1. pause goal
    21 |   2. edit objective
    22 |   3. clear goal
    23 |
    24 |   pgup/pgdn details · ↑↓ select · 1-3 choose · ↵ action · esc back
    |}];
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ─────────────────────────────────────────────────────────────────── active
    02 |
    03 |   status  active
    04 |
    05 |   goal id  goal-release-parser
    06 |
    07 |   objective
    08 |
    09 |     Ship the parser without regressions
    10 |
    11 |   activity  0 continuation turns
    12 |
    13 |   tokens  0 / 24,000 used · 24,000 remaining
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 | ⠋ requesting pause goal…
    23 |
    24 |   pgup/pgdn details · request in flight · esc back
    |}];
  Tui.acknowledge_goal_mutation t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ─────────────────────────────────────────────────────────────────── active
    02 |
    03 |   status  active
    04 |
    05 |   goal id  goal-release-parser
    06 |
    07 |   objective
    08 |
    09 |     Ship the parser without regressions
    10 |
    11 |   activity  0 continuation turns
    12 |
    13 |   tokens  0 / 24,000 used · 24,000 remaining
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 | ✓ client accepted pause goal; waiting for the goal fact…
    23 |
    24 |   pgup/pgdn details · request in flight · esc back
    |}];
  Tui.commit_goal_mutation t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ─────────────────────────────────────────────────────────────────── paused
    02 |
    03 |   status  paused
    04 |
    05 |   goal id  goal-release-parser
    06 |
    07 |   objective
    08 |
    09 |     Ship the parser without regressions
    10 |
    11 |   activity  0 continuation turns
    12 |
    13 |   tokens  0 / 24,000 used · 24,000 remaining
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 | ❯ 1. resume goal
    21 |   2. edit objective
    22 |   3. clear goal
    23 |
    24 |   pgup/pgdn details · ↑↓ select · 1-3 choose · ↵ action · esc back
    |}]

let%expect_test "narrow wrapped goal detail is keyboard-pageable" =
  let updates =
    [
      active
        ~objective:
          "Ship the parser across narrow terminals, Unicode paths, and wrapped \
           diagnostics without regressing navigation"
        ~token_budget:8_000 ();
      Session.Goal.Update.block ~id:goal_id
        ~reason:
          "Waiting for the upstream parser release and its signed mirror to \
           become available"
        ();
    ]
  in
  run ~name:"goal-narrow-page" ~size:(80, 12) updates @@ fun t ->
  open_goal t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ────────────────────────────────────────────────────────────────── blocked
    02 |
    03 |   status  blocked                                                              █
    04 |
    05 |   goal id  goal-release-parser
    06 |
    07 |
    08 | ❯ 1. resume goal
    09 |   2. edit objective
    10 |   3. clear goal
    11 |
    12 |   pgup/pgdn details · ↑↓ select · 1-3 choose · ↵ action · esc back
    |}];
  Tui.keys t Key.page_down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ────────────────────────────────────────────────────────────────── blocked
    02 |
    03 |   objective
    04 |                                                                                █
    05 |     Ship the parser across narrow terminals, Unicode paths, and wrapped
    06 |     diagnostics without regressing navigation
    07 |
    08 | ❯ 1. resume goal
    09 |   2. edit objective
    10 |   3. clear goal
    11 |
    12 |   pgup/pgdn details · ↑↓ select · 1-3 choose · ↵ action · esc back
    |}];
  Tui.keys t Key.page_down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ────────────────────────────────────────────────────────────────── blocked
    02 |
    03 |
    04 |   blocked: Waiting for the upstream parser release and its signed mirror to
    05 |   become available                                                             █
    06 |
    07 |
    08 | ❯ 1. resume goal
    09 |   2. edit objective
    10 |   3. clear goal
    11 |
    12 |   pgup/pgdn details · ↑↓ select · 1-3 choose · ↵ action · esc back
    |}];
  Tui.keys t Key.page_down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ────────────────────────────────────────────────────────────────── blocked
    02 |
    03 |
    04 |   activity  0 continuation turns
    05 |
    06 |   tokens  0 / 8,000 used · 8,000 remaining                                     █
    07 |
    08 | ❯ 1. resume goal
    09 |   2. edit objective
    10 |   3. clear goal
    11 |
    12 |   pgup/pgdn details · ↑↓ select · 1-3 choose · ↵ action · esc back
    |}]

(* The exact current objective is visible in the real Mosaic input. An
   unchanged submission remains local, and a structured client rejection
   leaves the owner projection intact before a later accepted edit lands. *)
let%expect_test "edit is prefilled, validated, rejected, and retried" =
  run ~name:"goal-edit" [ active () ] @@ fun t ->
  open_goal t;
  Tui.keys t Key.down;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ─────────────────────────────────────────────────────────────────── active
    02 |
    03 |   status  active
    04 |
    05 |   goal id  goal-release-parser
    06 |
    07 |   objective
    08 |
    09 |     Ship the parser without regressions
    10 |
    11 |   activity  0 continuation turns
    12 |
    13 |   tokens  0 used · no token budget
    14 |
    15 |
    16 |
    17 |
    18 | new objective
    19 |
    20 | ────────────────────────────────────────────────────────────────────────────────
    21 |   ❯ Ship the parser without regressions
    22 | ────────────────────────────────────────────────────────────────────────────────
    23 |
    24 |   type objective · ↵ save · esc cancel · paste works
    |}];
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ─────────────────────────────────────────────────────────────────── active
    02 |
    03 |   status  active
    04 |
    05 |   goal id  goal-release-parser
    06 |
    07 |   objective
    08 |
    09 |     Ship the parser without regressions
    10 |
    11 |   activity  0 continuation turns
    12 |
    13 |   tokens  0 used · no token budget
    14 |
    15 |
    16 | new objective
    17 |
    18 | ────────────────────────────────────────────────────────────────────────────────
    19 |   ❯ Ship the parser without regressions
    20 | ────────────────────────────────────────────────────────────────────────────────
    21 |
    22 | ! the objective is unchanged
    23 |
    24 |   type objective · ↵ save · esc cancel · paste works
    |}];
  Tui.paste t " — including Unicode paths";
  Tui.enter t;
  Tui.settle t;
  Tui.fail_goal_mutation t
    (unavailable "goal changed concurrently; reload the displayed owner fact");
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ─────────────────────────────────────────────────────────────────── active
    02 |
    03 |   status  active
    04 |
    05 |   goal id  goal-release-parser
    06 |
    07 |   objective
    08 |
    09 |     Ship the parser without regressions
    10 |
    11 |   activity  0 continuation turns
    12 |
    13 |   tokens  0 used · no token budget
    14 |
    15 |
    16 | ! could not edit objective
    17 |
    18 | goal changed concurrently; reload the displayed owner fact
    19 |
    20 |   1. pause goal
    21 | ❯ 2. edit objective
    22 |   3. clear goal
    23 |
    24 |   pgup/pgdn details · ↑↓ select · 1-3 choose · ↵ action · esc back
    |}];
  Tui.enter t;
  Tui.settle t;
  Tui.paste t " safely";
  Tui.enter t;
  Tui.settle t;
  Tui.acknowledge_goal_mutation t;
  Tui.settle t;
  Tui.commit_goal_mutation t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ─────────────────────────────────────────────────────────────────── active
    02 |
    03 |   status  active
    04 |
    05 |   goal id  goal-release-parser
    06 |
    07 |   objective
    08 |
    09 |     Ship the parser without regressions safely
    10 |
    11 |   activity  0 continuation turns
    12 |
    13 |   tokens  0 used · no token budget
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 | ❯ 1. pause goal
    21 |   2. edit objective
    22 |   3. clear goal
    23 |
    24 |   pgup/pgdn details · ↑↓ select · 1-3 choose · ↵ action · esc back
    |}]

(* Blocking is model-owned. Resume exposes the optional replacement budget,
   keeps malformed input visible, and transitions only after the real owner
   document accepts the captured update. *)
let%expect_test "blocked goal resumes with a validated replacement budget" =
  let updates =
    [
      active ~token_budget:8_000 ();
      Session.Goal.Update.block ~id:goal_id
        ~reason:"Waiting for the upstream parser release" ();
    ]
  in
  run ~name:"goal-resume" updates @@ fun t ->
  open_goal t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ────────────────────────────────────────────────────────────────── blocked
    02 |
    03 |   status  blocked
    04 |
    05 |   goal id  goal-release-parser
    06 |
    07 |   objective
    08 |
    09 |     Ship the parser without regressions
    10 |
    11 |   blocked: Waiting for the upstream parser release
    12 |
    13 |   activity  0 continuation turns
    14 |
    15 |   tokens  0 / 8,000 used · 8,000 remaining
    16 |
    17 |
    18 |
    19 |
    20 | ❯ 1. resume goal
    21 |   2. edit objective
    22 |   3. clear goal
    23 |
    24 |   pgup/pgdn details · ↑↓ select · 1-3 choose · ↵ action · esc back
    |}];
  Tui.enter t;
  Tui.settle t;
  Tui.paste t "-1";
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ────────────────────────────────────────────────────────────────── blocked
    02 |
    03 |   status  blocked                                                              █
    04 |                                                                                █
    05 |   goal id  goal-release-parser                                                 █
    06 |                                                                                █
    07 |   objective                                                                    █
    08 |                                                                                █
    09 |     Ship the parser without regressions
    10 |
    11 |   blocked: Waiting for the upstream parser release
    12 |
    13 |   activity  0 continuation turns
    14 |
    15 |
    16 | token budget (blank keeps the current budget)
    17 |
    18 | ────────────────────────────────────────────────────────────────────────────────
    19 |   ❯ -1
    20 | ────────────────────────────────────────────────────────────────────────────────
    21 |
    22 | ! enter a nonnegative whole-token budget, or leave it blank
    23 |
    24 |   type budget or leave blank · ↵ resume · esc cancel
    |}];
  Tui.keys t (Key.backspace ^ Key.backspace);
  Tui.paste t "12000";
  Tui.enter t;
  Tui.settle t;
  Tui.acknowledge_goal_mutation t;
  Tui.settle t;
  Tui.commit_goal_mutation t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ─────────────────────────────────────────────────────────────────── active
    02 |
    03 |   status  active
    04 |
    05 |   goal id  goal-release-parser
    06 |
    07 |   objective
    08 |
    09 |     Ship the parser without regressions
    10 |
    11 |   activity  0 continuation turns
    12 |
    13 |   tokens  0 / 12,000 used · 12,000 remaining
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 | ❯ 1. pause goal
    21 |   2. edit objective
    22 |   3. clear goal
    23 |
    24 |   pgup/pgdn details · ↑↓ select · 1-3 choose · ↵ action · esc back
    |}]

(* Clear has an explicit second-Enter confirmation. Terminal projections have
   no mounted action table, while the 80x12 frame remains a complete screen
   whose clipping and hints are directly reviewable. *)
let%expect_test "clear confirms and terminal goals are read-only" =
  run ~name:"goal-clear" [ active () ] @@ fun t ->
  open_goal t;
  Tui.keys t (Key.down ^ Key.down);
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ─────────────────────────────────────────────────────────────────── active
    02 |
    03 |   status  active
    04 |
    05 |   goal id  goal-release-parser
    06 |
    07 |   objective
    08 |
    09 |     Ship the parser without regressions
    10 |
    11 |   activity  0 continuation turns
    12 |
    13 |   tokens  0 used · no token budget
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 | Permanently clear this goal?
    21 |
    22 | Press Enter again to CLEAR. Escape cancels.
    23 |
    24 |   pgup/pgdn details · ↵ CLEAR · esc cancel
    |}];
  Tui.enter t;
  Tui.settle t;
  Tui.acknowledge_goal_mutation t;
  Tui.settle t;
  Tui.commit_goal_mutation t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ────────────────────────────────────────────────────────────────── cleared
    02 |
    03 |   status  cleared
    04 |
    05 |   goal id  goal-release-parser
    06 |
    07 |   objective
    08 |
    09 |     Ship the parser without regressions
    10 |
    11 |   activity  0 continuation turns
    12 |
    13 |   tokens  0 used · no token budget
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
    24 |   pgup/pgdn details · esc back
    |}];
  Tui.resize t ~width:80 ~height:12;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ────────────────────────────────────────────────────────────────── cleared
    02 |
    03 |   status  cleared                                                              █
    04 |                                                                                █
    05 |   goal id  goal-release-parser                                                 █
    06 |                                                                                █
    07 |   objective
    08 |
    09 |     Ship the parser without regressions
    10 |
    11 |
    12 |   pgup/pgdn details · esc back
    |}]

let%expect_test "loaded session without a goal is an honest read-only screen" =
  run ~name:"goal-none" [] @@ fun t ->
  open_goal t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ────────────────────────────────────────────────────────────────── no goal
    02 |
    03 |   No goal is set for this session.
    04 |
    05 |   Declare one with /goal <objective> from the prompt — it rides your next
    06 |   turn. A model may also declare a goal while working.
    07 |
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
    21 |
    22 |
    23 |
    24 |   pgup/pgdn details · esc back
    |}]

let%expect_test "goal query states remain honest and structured" =
  run ~name:"goal-loading" ~hold_session_view_results:true [] @@ fun t ->
  open_goal t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ────────────────────────────────────────────────────────────────── loading
    02 |
    03 |   ⠋ loading goal…
    04 |
    05 |
    06 |
    07 |
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
    21 |
    22 |
    23 |
    24 |   esc back
    |}];
  Tui.finish_session_view_results t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ────────────────────────────────────────────────────────────────── no goal
    02 |
    03 |   No goal is set for this session.
    04 |
    05 |   Declare one with /goal <objective> from the prompt — it rides your next
    06 |   turn. A model may also declare a goal while working.
    07 |
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
    21 |
    22 |
    23 |
    24 |   pgup/pgdn details · esc back
    |}];
  let failure = unavailable "session detail owner is temporarily unavailable" in
  run ~name:"goal-unavailable"
    ~session_view_results:[ Error failure; Error failure ]
    []
  @@ fun t ->
  open_goal t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ────────────────────────────────────────────────────────────── unavailable
    02 |
    03 |   ! goal unavailable
    04 |
    05 |   session detail owner is temporarily unavailable
    06 |
    07 |
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
    21 |
    22 |
    23 |
    24 |   pgup/pgdn details · esc back
    |}];
  let refresh_failure = unavailable "session detail refresh timed out" in
  run ~name:"goal-retained-refresh"
    ~session_view_results:[ Ok (); Ok (); Error refresh_failure ]
    [ active () ]
  @@ fun t ->
  open_goal t;
  Tui.update_goal t
    (Session.Goal.Update.block ~id:goal_id
       ~reason:"Waiting for the release mirror" ());
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ─────────────────────────────────────────────────────────────────── active
    02 |
    03 |   ! could not refresh goal; showing retained state
    04 |
    05 |   session detail refresh timed out
    06 |
    07 |   status  active
    08 |
    09 |   goal id  goal-release-parser
    10 |
    11 |   objective
    12 |
    13 |     Ship the parser without regressions
    14 |
    15 |   activity  0 continuation turns
    16 |
    17 |   tokens  0 used · no token budget
    18 |
    19 |
    20 | ❯ 1. pause goal
    21 |   2. edit objective
    22 |   3. clear goal
    23 |
    24 |   pgup/pgdn details · ↑↓ select · 1-3 choose · ↵ action · esc back
    |}]

let%expect_test "engine terminal and budget states are read-only or actionable"
    =
  let completed =
    [
      active ();
      Session.Goal.Update.complete ~id:goal_id
        ~summary:"Parser release verified across all supported terminals" ();
    ]
  in
  run ~name:"goal-completed" completed @@ fun t ->
  open_goal t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ──────────────────────────────────────────────────────────────── completed
    02 |
    03 |   status  completed
    04 |
    05 |   goal id  goal-release-parser
    06 |
    07 |   objective
    08 |
    09 |     Ship the parser without regressions
    10 |
    11 |   completed: Parser release verified across all supported terminals
    12 |
    13 |   activity  0 continuation turns
    14 |
    15 |   tokens  0 used · no token budget
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   pgup/pgdn details · esc back
    |}];
  Tui.run ~name:"goal-budget-limited" ~size:(80, 24) ~session:session_id
    ~sessions:(fun project -> [ accounted_document project ])
  @@ fun t ->
  open_goal t;
  Tui.print t;
  [%expect
    {|
    01 |  goal ─────────────────────────────────────────────────────────── budget limited
    02 |
    03 |   status  budget limited
    04 |
    05 |   goal id  goal-release-parser
    06 |
    07 |   objective
    08 |
    09 |     Ship the parser without regressions
    10 |
    11 |   activity  1 continuation turn
    12 |
    13 |   tokens  3,250 / 8,000 used · 4,750 remaining
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 | ❯ 1. resume goal
    21 |   2. edit objective
    22 |   3. clear goal
    23 |
    24 |   pgup/pgdn details · ↑↓ select · 1-3 choose · ↵ action · esc back
    |}]

let stage_goal t objective =
  Tui.keys t objective;
  Tui.enter t;
  Tui.settle t

(* [/goal <objective>] never mints a goal locally: it stages the objective, shows
   a standing composer cue, and rides the next turn as [Command.prompt ~goal].
   The cue clears the instant that turn is sent. *)
let%expect_test "an objective staged with /goal rides the next turn and clears"
    =
  let turn =
    Tui.Turn_script.complete ~prompt:"begin" "Working toward the goal."
  in
  Tui.run ~name:"goal-stage" ~turns:[ turn ] @@ fun t ->
  stage_goal t "/goal Ship the parser without regressions";
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
    13 |   ◇ goal staged: Ship the parser without regressions · /goal cancels
    14 |
    15 | ────────────────────────────────────────────────────────────────────────────────
    16 | ❯ message mentat
    17 | ────────────────────────────────────────────────────────────────────────────────
    18 |
    19 |                          ! /login — no connected account
    20 |                               ∅ no recent sessions
    21 |
    22 |
    23 |
    24 |   ! not logged in · /login · ~/men… · openai/gpt-5.5 me… · ! full acce… ? for…
    |}];
  Tui.keys t "begin";
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    goal declared on prompt: Ship the parser without regressions
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-go5917f9de
    04 |
    05 | ❯ begin
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
    24 |   ! not logged in · /login · ~/mentat-… · openai/gpt-… · ! full access ? for …
    |}]

(* A bare [/goal] with an objective staged is the cancellation path advertised in
   the cue; without a staged objective it opens the honest management screen. The
   palette inserts "/goal " on the first Enter, so the bare command dispatches on
   the second. *)
let%expect_test "a bare /goal cancels a staged objective" =
  Tui.run ~name:"goal-stage-cancel" @@ fun t ->
  stage_goal t "/goal Ship the parser without regressions";
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
    13 |   ◇ goal staged: Ship the parser without regressions · /goal cancels
    14 |
    15 | ────────────────────────────────────────────────────────────────────────────────
    16 | ❯ message mentat
    17 | ────────────────────────────────────────────────────────────────────────────────
    18 |
    19 |                          ! /login — no connected account
    20 |                               ∅ no recent sessions
    21 |
    22 |
    23 |
    24 |   ! not logged in · /login · ~/ment… · openai/gpt-5.5 m… · ! full access ? fo…
    |}];
  Tui.keys t "/goal";
  Tui.enter t;
  Tui.settle t;
  Tui.enter t;
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
    13 |
    14 | ────────────────────────────────────────────────────────────────────────────────
    15 | ❯ message mentat
    16 | ────────────────────────────────────────────────────────────────────────────────
    17 |
    18 |                          ! /login — no connected account
    19 |                               ∅ no recent sessions
    20 |
    21 |
    22 |
    23 |
    24 |   goal staging cancelled
    |}]

(* Declaration is engine-owned and a live goal is edited from the management
   screen, so staging a second objective over a live goal is refused, not
   silently replaced. The live goal is declared inside a real turn, exactly as a
   model would. *)
let%expect_test "declaring an objective is refused while a goal is live" =
  let turn = Tui.Turn_script.complete ~prompt:"start" "Declaring a goal now." in
  Tui.run ~name:"goal-live-refuse" ~turns:[ turn ] @@ fun t ->
  Tui.keys t "start";
  Tui.enter t;
  Tui.settle t;
  Tui.update_goal t
    (Session.Goal.Update.declare ~id:goal_id
       ~objective:"Ship the parser without regressions" ());
  Tui.settle t;
  Tui.finish_turn t;
  Tui.settle t;
  stage_goal t "/goal A competing objective";
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-goal-livccbdaeaf
    04 |
    05 | ❯ start
    06 |
    07 | ⏺ Declaring a goal now.
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
    18 | ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈
    19 |   goal · Ship the parser without regressions
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   a goal is already active; edit it from /goal
    |}]
