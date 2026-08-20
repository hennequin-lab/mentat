(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
module Json = Jsont.Json
module Llm = Mentat_llm
module Permission = Mentat_permission
module Session = Mentat_session
module Tool = Mentat_tool

let provider = Llm.Provider.make "openai"
let api = Llm.Model.Api.make "responses"
let model = Llm.Model.make ~provider ~api ~id:"gpt-5.5"

let contract =
  Session.Contract.make ~mode:Session.Contract.Mode.Plan ~model ~declarations:[]
    ~policy:Permission.Policy.default ~review:Permission.Review_behavior.Enforce
    ~sandbox:(Mentat_sandbox.identity Mentat_sandbox.direct)
    ()

let plan ?title body =
  match Session.Plan.make ?title ~body () with
  | Ok plan -> plan
  | Error error -> failwith (Session.Plan.Error.message error)

let pending_session project ~name ~title ~turn_prompt proposal =
  let turn_id = Session.Turn.Id.of_string ("turn-plan-" ^ name) in
  let turn =
    Session.Turn.make ~id:turn_id ~origin:Session.Turn.Origin.User
      ~input:(Session.Turn.Input.user_text turn_prompt)
      ~max_steps:100 ~contract ()
  in
  let call =
    Llm.Tool.Call.make ~id:("call-plan-" ^ name) ~name:"propose_plan"
      ~input:(Json.object' []) ()
  in
  let provider_request =
    Session.Provider_request.Started.make ~turn:turn_id
      ~request_digest:(Mentat_digest.string ("plan visual " ^ name))
  in
  let response =
    Llm.Response.make ~model ~stop:Llm.Response.Stop.tool_call
      (Llm.Message.Assistant.make [ Llm.Message.Assistant.tool_call call ])
  in
  let requested =
    Session.Decision.Requested.make ~turn:turn_id ~call ~stage:Tool.Stage.Direct
      (Session.Decision.Request.Plan proposal)
  in
  let events =
    [
      Session.Event.turn_started turn;
      Session.Event.provider_requested provider_request;
      Session.Event.provider_settled
        (Session.Provider_request.Settled.responded
           ~id:(Session.Provider_request.Started.id provider_request)
           response);
      Session.Event.decision_requested requested;
    ]
  in
  let created_at = Session.Time.of_unix_ms 1_000_000L in
  let metadata =
    Session.Metadata.make ~title
      ~cwd:(Lpath.Abs.of_string_exn (Project.root project))
      ~created_at ~updated_at:created_at ()
  in
  match
    Session.make
      ~id:(Session.Id.of_string ("session-plan-" ^ name))
      ~metadata ~events
  with
  | Ok session -> session
  | Error error -> failwith (Session.Error.message error)

let open_pending t =
  Tui.settle t;
  Tui.enter t;
  Tui.settle t

let no_home _ = None

let history text =
  Printf.sprintf
    {|{"schema_version":1,"type":"mentat.tui.history_entry","session_id":"session-history","timestamp":"1000","draft":{"text":%S}}|}
    text

(* These journeys deliberately retain the transcript, dialog, composer, footer,
   and every blank allocation. Decision values are exact fixtures at the real
   App boundary; all UI evidence remains in complete numbered terminal frames. *)

let%expect_test "current-context approval preserves the restored draft visibly"
    =
  let proposal =
    plan ~title:"Refactor the parser"
      "Split the tokenizer out.\nMake parse return a result."
  in
  let answer =
    Session.Plan.Answer.approve ~body:proposal ~context:`Current ()
  in
  Tui.run ~name:"plan-approve-current" ~home:no_home ~draft:"PLAN-FOLLOW-UP"
    ~session:(Session.Id.of_string "session-plan-approve-current")
    ~sessions:(fun project ->
      [
        pending_session project ~name:"approve-current"
          ~title:"Approve current plan"
          ~turn_prompt:"draft a plan for the parser" proposal;
      ])
    ~decision_answers:[ Session.Decision.Answer.Plan answer ]
  @@ fun t ->
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-plan-approve2f023c27.home/ment…
    04 |
    05 | ❯ draft a plan for the parser
    06 |
    07 | ⋯ Waiting for your answer
    08 |
    09 |
    10 |
    11 |
    12 |
    13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    14 |    plan
    15 |
    16 |   Refactor the parser
    17 |
    18 |   Split the tokenizer out.                                                     ▀
    19 |
    20 | ❯ 1. approve and continue current context
    21 |   2. approve and start fresh
    22 |   3. revise — tell the model what to change
    23 |
    24 |   1-4 select · ↵ choose · ^O more · esc keep
    |}];
  Tui.keys t Key.down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-plan-approve2f023c27.home/ment…
    04 |
    05 | ❯ draft a plan for the parser
    06 |
    07 | ⋯ Waiting for your answer
    08 |
    09 |
    10 |
    11 |
    12 |
    13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    14 |    plan
    15 |
    16 |   Refactor the parser
    17 |
    18 |   Split the tokenizer out.                                                     ▀
    19 |
    20 |   1. approve and continue current context
    21 | ❯ 2. approve and start fresh
    22 |   3. revise — tell the model what to change
    23 |
    24 |   1-4 select · ↵ choose · ^O more · esc keep
    |}];
  Tui.keys t "1";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-plan-approve2f023c27.home/ment…
    04 |
    05 | ❯ draft a plan for the parser
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
    21 |  ⏸ plan ────────────────────────────────────────────────────────────────────────
    22 | ❯ PLAN-FOLLOW-UP
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · /tmp/mentat-tu… · openai/gpt… · ! full acce… ? f…
    |}]

let%expect_test
    "revision is editable, validated, cancellable, and absent from history" =
  let proposal = plan ~title:"Adjust safely" "Start with the parser." in
  let feedback = "preserve callers \206\178" in
  Tui.run ~name:"plan-revise" ~home:no_home
    ~history:(history "plan history prompt")
    ~draft:"PLAN-FOLLOW-UP"
    ~session:(Session.Id.of_string "session-plan-revise")
    ~sessions:(fun project ->
      [
        pending_session project ~name:"revise" ~title:"Revise plan"
          ~turn_prompt:"plan history prompt" proposal;
      ])
    ~decision_answers:
      [ Session.Decision.Answer.Plan (Session.Plan.Answer.revise feedback) ]
  @@ fun t ->
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-plad5f3de42.home/mentat-tui-pl…
    04 |
    05 | ❯ plan history prompt
    06 |
    07 | ⋯ Waiting for your answer
    08 |
    09 |
    10 |
    11 |
    12 |
    13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    14 |    plan
    15 |
    16 |   Adjust safely
    17 |
    18 |   Start with the parser.
    19 |
    20 | ❯ 1. approve and continue current context
    21 |   2. approve and start fresh
    22 |   3. revise — tell the model what to change
    23 |
    24 |   1-4 select · ↵ choose · ^O more · esc keep
    |}];
  Tui.keys t "3";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-plad5f3de42.home/mentat-tui-pl…
    04 |
    05 | ❯ plan history prompt
    06 |
    07 | ⋯ Waiting for your answer
    08 |
    09 |
    10 |
    11 |
    12 |
    13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    14 |    plan
    15 |
    16 |   Adjust safely
    17 |
    18 |   Start with the parser.
    19 |
    20 |   2. approve and start fresh
    21 | ❯ 3. revise — tell the model what to change
    22 |   4. keep planning
    23 |
    24 |   1-4 select · ↵ choose · ^O more · esc keep
    |}];
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-plad5f3de42.home/mentat-tui-pl…
    04 |
    05 | ❯ plan history prompt
    06 |
    07 | ⋯ Waiting for your answer
    08 |
    09 |
    10 |
    11 |
    12 |
    13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    14 |    plan
    15 |
    16 |   Adjust safely
    17 |
    18 |   Start with the parser.
    19 |
    20 | ────────────────────────────────────────────────────────────────────────────────
    21 |   ❯
    22 | ────────────────────────────────────────────────────────────────────────────────
    23 |
    24 |   type feedback · ↵ revise · esc cancel
    |}];
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-plad5f3de42.home/mentat-tui-pl…
    04 |
    05 | ❯ plan history prompt
    06 |
    07 | ⋯ Waiting for your answer
    08 |
    09 |
    10 |
    11 |
    12 |
    13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    14 |    plan
    15 |
    16 |   Adjust safely
    17 |   Start with the parser.
    18 |   describe what should change
    19 | ────────────────────────────────────────────────────────────────────────────────
    20 |   ❯
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 |
    23 |
    24 |   type feedback · ↵ revise · esc cancel
    |}];
  Tui.paste t "discard callers";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-plad5f3de42.home/mentat-tui-pl…
    04 |
    05 | ❯ plan history prompt
    06 |
    07 | ⋯ Waiting for your answer
    08 |
    09 |
    10 |
    11 |
    12 |
    13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    14 |    plan
    15 |
    16 |   Adjust safely
    17 |
    18 |   Start with the parser.
    19 |
    20 | ────────────────────────────────────────────────────────────────────────────────
    21 |   ❯ discard callers
    22 | ────────────────────────────────────────────────────────────────────────────────
    23 |
    24 |   type feedback · ↵ revise · esc cancel
    |}];
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-plad5f3de42.home/mentat-tui-pl…
    04 |
    05 | ❯ plan history prompt
    06 |
    07 | ⋯ Waiting for your answer
    08 |
    09 |
    10 |
    11 |
    12 |
    13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    14 |    plan
    15 |
    16 |   Adjust safely
    17 |
    18 |   Start with the parser.
    19 |
    20 |   2. approve and start fresh
    21 | ❯ 3. revise — tell the model what to change
    22 |   4. keep planning
    23 |
    24 |   1-4 select · ↵ choose · ^O more · esc keep
    |}];
  Tui.enter t;
  Tui.settle t;
  Tui.paste t feedback;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-plad5f3de42.home/mentat-tui-pl…
    04 |
    05 | ❯ plan history prompt
    06 |
    07 | ⋯ Waiting for your answer
    08 |
    09 |
    10 |
    11 |
    12 |
    13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    14 |    plan
    15 |
    16 |   Adjust safely
    17 |
    18 |   Start with the parser.
    19 |
    20 | ────────────────────────────────────────────────────────────────────────────────
    21 |   ❯ preserve callers β
    22 | ────────────────────────────────────────────────────────────────────────────────
    23 |
    24 |   type feedback · ↵ revise · esc cancel
    |}];
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-plad5f3de42.home/mentat-tui-pl…
    04 |
    05 | ❯ plan history prompt
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
    21 |  ⏸ plan ────────────────────────────────────────────────────────────────────────
    22 | ❯ PLAN-FOLLOW-UP
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · /tmp/mentat-t… · openai/gpt… · ! full access ? f…
    |}];
  Tui.keys t Key.up;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-plad5f3de42.home/mentat-tui-pl…
    04 |
    05 | ❯ plan history prompt
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
    21 |  ⏸ plan ────────────────────────────────────────────────────────────────────────
    22 | ❯ plan history prompt
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · /tmp/mentat-t… · openai/gpt… · ! full access ? f…
    |}]

let%expect_test
    "long plan expands, collapses, and Escape visibly keeps planning" =
  let body =
    [
      "1. Inventory every parser entry point and its current callers.";
      "2. Separate token production from grammar decisions.";
      "3. Preserve source spans through the new tokenizer boundary.";
      "4. Return structured parse errors instead of exceptions.";
      "5. Keep malformed UTF-8 diagnostics attached to byte offsets.";
      "6. Move parser fixtures onto the public entry point.";
      "7. Add complete terminal journeys for surfaced diagnostics.";
      "8. Compare allocation behavior before changing buffering.";
      "9. Update the command path to consume the structured result.";
      "10. Remove the obsolete parser compatibility wrapper.";
      "11. Document ownership and replay invariants in the interface.";
      "12. Run the focused suite before the integrated verification.";
      "13. Review every promoted visual frame by hand.";
      "14. Land only after the public behavior remains coherent.";
    ]
    |> String.concat "\n"
  in
  let proposal = plan ~title:"Parser migration evidence" body in
  Tui.run ~name:"plan-disclosure" ~size:(80, 48) ~home:no_home
    ~sessions:(fun project ->
      [
        pending_session project ~name:"disclosure" ~title:"Plan disclosure"
          ~turn_prompt:"show the complete migration plan" proposal;
      ])
    ~decision_answers:
      [ Session.Decision.Answer.Plan Session.Plan.Answer.keep_planning ]
  @@ fun t ->
  open_pending t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-plan-dib97e3a77.home/mentat-tu…
    04 |
    05 | ❯ show the complete migration plan
    06 |
    07 | ⋯ Waiting for your answer
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
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    plan
    27 |
    28 |   Parser migration evidence
    29 |
    30 |   1. Inventory every parser entry point and its current callers.               █
    31 |   2. Separate token production from grammar decisions.                         █
    32 |   3. Preserve source spans through the new tokenizer boundary.                 █
    33 |   4. Return structured parse errors instead of exceptions.                     █
    34 |   5. Keep malformed UTF-8 diagnostics attached to byte offsets.
    35 |   6. Move parser fixtures onto the public entry point.
    36 |   7. Add complete terminal journeys for surfaced diagnostics.
    37 |   8. Compare allocation behavior before changing buffering.
    38 |
    39 | ❯ 1. approve and continue current context
    40 |   2. approve and start fresh
    41 |   3. revise — tell the model what to change
    42 |   4. keep planning
    43 |
    44 |
    45 |
    46 |
    47 |
    48 |   1-4 select · ↵ choose · ^O more · esc keep
    |}];
  Tui.keys t Key.ctrl_o;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-plan-dib97e3a77.home/mentat-tu…
    04 |
    05 | ❯ show the complete migration plan
    06 |
    07 | ⋯ Waiting for your answer
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
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    plan
    27 |
    28 |   Parser migration evidence
    29 |
    30 |   1. Inventory every parser entry point and its current callers.               █
    31 |   2. Separate token production from grammar decisions.                         █
    32 |   3. Preserve source spans through the new tokenizer boundary.                 █
    33 |   4. Return structured parse errors instead of exceptions.                     █
    34 |   5. Keep malformed UTF-8 diagnostics attached to byte offsets.                █
    35 |   6. Move parser fixtures onto the public entry point.                         █
    36 |   7. Add complete terminal journeys for surfaced diagnostics.
    37 |   8. Compare allocation behavior before changing buffering.
    38 |   9. Update the command path to consume the structured result.
    39 |   10. Remove the obsolete parser compatibility wrapper.
    40 |   11. Document ownership and replay invariants in the interface.
    41 |   12. Run the focused suite before the integrated verification.
    42 |
    43 | ❯ 1. approve and continue current context
    44 |   2. approve and start fresh
    45 |   3. revise — tell the model what to change
    46 |   4. keep planning
    47 |
    48 |   1-4 select · ↵ choose · ^O less · esc keep
    |}];
  Tui.keys t Key.ctrl_o;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-plan-dib97e3a77.home/mentat-tu…
    04 |
    05 | ❯ show the complete migration plan
    06 |
    07 | ⋯ Waiting for your answer
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
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    plan
    27 |
    28 |   Parser migration evidence
    29 |
    30 |   1. Inventory every parser entry point and its current callers.               █
    31 |   2. Separate token production from grammar decisions.                         █
    32 |   3. Preserve source spans through the new tokenizer boundary.                 █
    33 |   4. Return structured parse errors instead of exceptions.                     █
    34 |   5. Keep malformed UTF-8 diagnostics attached to byte offsets.
    35 |   6. Move parser fixtures onto the public entry point.
    36 |   7. Add complete terminal journeys for surfaced diagnostics.
    37 |   8. Compare allocation behavior before changing buffering.
    38 |
    39 | ❯ 1. approve and continue current context
    40 |   2. approve and start fresh
    41 |   3. revise — tell the model what to change
    42 |   4. keep planning
    43 |
    44 |
    45 |
    46 |
    47 |
    48 |   1-4 select · ↵ choose · ^O more · esc keep
    |}];
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-plan-dib97e3a77.home/mentat-tu…
    04 |
    05 | ❯ show the complete migration plan
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
    21 |
    22 |
    23 |
    24 |
    25 |
    26 |
    27 |
    28 |
    29 |
    30 |
    31 |
    32 |
    33 |
    34 |
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    41 |
    42 |
    43 |
    44 |
    45 |  ⏸ plan ────────────────────────────────────────────────────────────────────────
    46 | ❯ queue a message — sends after this turn
    47 | ────────────────────────────────────────────────────────────────────────────────
    48 |   ! not logged in · /login · /tmp/mentat-t… · openai/gpt… · ! full access ? f…
    |}]

let%expect_test
    "narrow decisions and revision controls remain legible before approval" =
  let proposal =
    plan
      ~title:
        "Unicode \240\159\145\169\240\159\143\189\226\128\141\240\159\146\187 \
         plan"
      "Keep the exact owner plan while Mosaic wraps this deliberately long \
       body."
  in
  let answer = Session.Plan.Answer.approve ~body:proposal ~context:`Fresh () in
  Tui.run ~name:"plan-narrow-fresh" ~size:(46, 20) ~home:no_home
    ~sessions:(fun project ->
      [
        pending_session project ~name:"narrow-fresh" ~title:"Narrow plan"
          ~turn_prompt:"choose a fresh build context" proposal;
      ])
    ~decision_answers:[ Session.Decision.Answer.Plan answer ]
  @@ fun t ->
  open_pending t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    04 |  dev · openai/gpt-5.5 medium
    05 |  /tmp/mentat-tui-plan-narrb4f70b54.home/ment…
    06 |
    07 | ❯ choose a fresh build context
    08 |
    09 | ⋯ Waiting for your answer
    10 |
    11 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    12 |    plan
    13 |
    14 |   Unicode 👩🏽‍💻 plan
    15 |
    16 |   Keep the exact owner plan while Mosaic     ▀
    17 |
    18 | ❯ 1. approve and continue current context
    19 |
    20 |   1-4 select · ↵ choose · ^O more · esc keep
    |}];
  Tui.keys t Key.up;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    04 |  dev · openai/gpt-5.5 medium
    05 |  /tmp/mentat-tui-plan-narrb4f70b54.home/ment…
    06 |
    07 | ❯ choose a fresh build context
    08 |
    09 | ⋯ Waiting for your answer
    10 |
    11 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    12 |    plan
    13 |
    14 |   Unicode 👩🏽‍💻 plan
    15 |
    16 |   Keep the exact owner plan while Mosaic     ▀
    17 |
    18 | ❯ 4. keep planning
    19 |
    20 |   1-4 select · ↵ choose · ^O more · esc keep
    |}];
  Tui.resize t ~width:46 ~height:24;
  Tui.keys t "3";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    04 |  dev · openai/gpt-5.5 medium
    05 |  /tmp/mentat-tui-plan-narrb4f70b54.home/ment…
    06 |
    07 | ❯ choose a fresh build context
    08 |
    09 | ⋯ Waiting for your answer
    10 |
    11 |
    12 |
    13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    14 |    plan
    15 |
    16 |   Unicode 👩🏽‍💻 plan
    17 |
    18 |   Keep the exact owner plan while Mosaic     ▀
    19 |
    20 | ──────────────────────────────────────────────
    21 |   ❯
    22 | ──────────────────────────────────────────────
    23 |
    24 |   type feedback · ↵ revise · esc cancel
    |}];
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.resize t ~width:46 ~height:18;
  Tui.keys t "2";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    04 |  dev · openai/gpt-5.5 medium
    05 |  /tmp/mentat-tui-plan-narrb4f70b54.home/ment…
    06 |
    07 | ❯ choose a fresh build context
    08 |
    09 | ⋯ Waiting for your answer
    10 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    11 |    plan
    12 |
    13 |   Unicode 👩🏽‍💻 plan
    14 |
    15 |   Keep the exact owner plan while Mosaic
    16 | ❯ 2. approve and start fresh
    17 |
    18 |   1-4 select · ↵ choose · ^O more · esc keep
    |}];
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    04 |  dev · openai/gpt-5.5 medium
    05 |  /tmp/mentat-tui-plan-narrb4f70b54.home/ment…
    06 |
    07 | ❯ choose a fresh build context
    08 |
    09 | ⠋ Working… (0s · esc to interrupt)
    10 |
    11 |
    12 |
    13 |
    14 |
    15 |  ⏸ plan ──────────────────────────────────────
    16 | ❯ queue a message — sends after this turn
    17 | ──────────────────────────────────────────────
    18 |   ! not logged in · /log… ·… · ! full acce…
    |}]
