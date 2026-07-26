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
let no_home = Fun.const None

let contract =
  Session.Contract.make ~mode:Session.Contract.Mode.Build ~model
    ~declarations:[] ~policy:Permission.Policy.default
    ~review:Permission.Review_behavior.Enforce
    ~sandbox:(Mentat_sandbox.identity Mentat_sandbox.direct)
    ()

let question ?choices prompt =
  match Session.Question.make ~prompt ?choices () with
  | Ok question -> question
  | Error error -> failwith (Session.Question.Error.message error)

let pending_session project ~title ~turn_prompt question =
  let turn_id = Session.Turn.Id.of_string "turn-question" in
  let turn =
    Session.Turn.make ~id:turn_id ~origin:Session.Turn.Origin.User
      ~input:(Session.Turn.Input.user_text turn_prompt)
      ~max_steps:100 ~contract ()
  in
  let call =
    Llm.Tool.Call.make ~id:"call-question" ~name:"ask_user"
      ~input:(Json.object' []) ()
  in
  let provider_request =
    Session.Provider_request.Started.make ~turn:turn_id
      ~request_digest:(Mentat_digest.string "question visual")
  in
  let response =
    Llm.Response.make ~model ~stop:Llm.Response.Stop.tool_call
      (Llm.Message.Assistant.make [ Llm.Message.Assistant.tool_call call ])
  in
  let requested =
    Session.Decision.Requested.make ~turn:turn_id ~call ~stage:Tool.Stage.Direct
      (Session.Decision.Request.Question question)
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
    Session.make ~id:(Session.Id.of_string "session-question") ~metadata ~events
  with
  | Ok session -> session
  | Error error -> failwith (Session.Error.message error)

let open_pending t =
  Tui.settle t;
  Tui.enter t;
  Tui.settle t

let press_down t remaining =
  let rec loop remaining =
    if remaining > 0 then begin
      Tui.keys t Key.down;
      loop (remaining - 1)
    end
  in
  loop remaining

let history text =
  Printf.sprintf
    {|{"schema_version":1,"type":"mentat.tui.history_entry","session_id":"session-history","timestamp":"1000","draft":{"text":%S}}|}
    text

(* Each expectation is the complete terminal grid. Dialog tests intentionally
   retain the transcript, modal surface, composer, footer, and narrow viewport
   together so clipping, displacement, and blank allocations stay reviewable. *)

let%expect_test "a numbered choice selects first and resolves only on Enter" =
  let prompt = "Where should the visual journey live?" in
  let choices = [ "next/test/tui"; "legacy test/tui" ] in
  Tui.run ~name:"question-choice" ~home:no_home
    ~sessions:(fun project ->
      [
        pending_session project ~title:"Choose a location"
          ~turn_prompt:"choose the visual test location"
          (question ~choices prompt);
      ])
    ~decision_answers:
      [ Session.Decision.Answer.Question (Session.Question.Answer.choice 1) ]
  @@ fun t ->
  open_pending t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-questiof59856ac.home/mentat-tu…
04 |
05 | ❯ choose the visual test location
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    question
15 |
16 |   Where should the visual journey live?
17 |
18 | ❯ 1. next/test/tui
19 |   2. legacy test/tui
20 |   3. ✎ type your own answer
21 |
22 |
23 |
24 |   1-9 jump · ↑↓ move · enter answer · esc custom|}];
  Tui.keys t "2";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-questiof59856ac.home/mentat-tu…
04 |
05 | ❯ choose the visual test location
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    question
15 |
16 |   Where should the visual journey live?
17 |
18 |   1. next/test/tui
19 | ❯ 2. legacy test/tui
20 |   3. ✎ type your own answer
21 |
22 |
23 |
24 |   1-9 jump · ↑↓ move · enter answer · esc custom|}];
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-questiof59856ac.home/mentat-tu…
04 |
05 | ❯ choose the visual test location
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
24 |   ! not logged in · /login · /tmp/mentat-t… · openai/gpt… · ! full access ? f…|}]

let%expect_test
    "a tool result follows the question whose answer resumed the turn" =
  let turn = Session.Turn.Id.of_string "turn-question" in
  let call =
    Llm.Tool.Call.make ~id:"call-after-question" ~name:"verify_contract"
      ~input:(Json.object' []) ()
  in
  let result =
    Tool.Result.completed
      ~output:
        (Tool.Output.make
           ~text:"The post-answer contract is visible in transcript order." ())
      ()
  in
  let continuation =
    Tui.Decision_continuation.tools ~turn [ Tui.Turn_script.tool ~call ~result ]
  in
  let choices = [ "continue" ] in
  Tui.run ~name:"question-tool-continuation" ~home:no_home
    ~sessions:(fun project ->
      [
        pending_session project ~title:"Continue after question"
          ~turn_prompt:"check the ordered continuation"
          (question ~choices "May the tool verification continue?");
      ])
    ~decision_answers:
      [ Session.Decision.Answer.Question (Session.Question.Answer.choice 0) ]
    ~decision_continuations:[ continuation ]
  @@ fun t ->
  open_pending t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-question-tool-cont50469e35.hom…
04 |
05 | ❯ check the ordered continuation
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    question
15 |
16 |   May the tool verification continue?
17 |
18 | ❯ 1. continue
19 |   2. ✎ type your own answer
20 |
21 |
22 |
23 |
24 |   1-9 jump · ↑↓ move · enter answer · esc custom|}];
  Tui.keys t "1";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-question-tool-cont50469e35.hom…
04 |
05 | ❯ check the ordered continuation
06 |
07 | ⏺ Verify_contract
08 |   ⎿  done
09 |       The post-answer contract is visible in transcript order.
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
21 | ────────────────────────────────────────────────────────────────────────────────
22 | ❯ queue a message — sends after this turn
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · /tmp/mentat-tu… · openai/gpt… · ! full access ? …|}]

let%expect_test
    "many choices remain whole and keep the selected row visible when narrow" =
  let prompt =
    "Which checks should run?\r\n\
     Keep the selected row visible at narrow widths."
  in
  let choices =
    [
      "parser\tunit suite";
      "\u{200D}";
      "CLI integration";
      "Unicode 👩🏽‍💻 flow";
      "Packaging";
      "Documentation";
      "Snapshots";
      "Linux";
      "macOS";
      "Windows";
      "FreeBSD";
      "Release smoke test with a deliberately long label";
    ]
  in
  Tui.run ~name:"question-many" ~size:(56, 20) ~home:no_home
    ~sessions:(fun project ->
      [
        pending_session project ~title:"Choose checks"
          ~turn_prompt:"choose checks" (question ~choices prompt);
      ])
    ~decision_answers:
      [ Session.Decision.Answer.Question (Session.Question.Answer.choice 10) ]
  @@ fun t ->
  open_pending t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 m…
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-quest8…
04 |
05 | ❯ choose checks
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
12 |    question
13 |
14 |   Which checks should run?
15 |   Keep the selected row visible at narrow widths.
16 |
17 | ❯ 1. parser unit suite
18 |   2. [blank choice]
19 |
20 |   1-9 jump · ↑↓ move · enter answer · esc custom|}];
  press_down t 10;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 m…
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-quest8…
04 |
05 | ❯ choose checks
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
12 |    question
13 |
14 |   Which checks should run?
15 |   Keep the selected row visible at narrow widths.
16 |
17 |   10. Windows
18 | ❯ 11. FreeBSD
19 |
20 |   1-9 jump · ↑↓ move · enter answer · esc custom|}];
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 m…
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-quest8…
04 |
05 | ❯ choose checks
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
17 | ────────────────────────────────────────────────────────
18 | ❯ queue a message — sends after this turn
19 | ────────────────────────────────────────────────────────
20 |   ! not logged in · /login · openai/… · ! full access|}]

let%expect_test
    "custom text validates, edits, cancels, pastes, and resolves inline" =
  let choices = [ "stable"; "experimental" ] in
  Tui.run ~name:"question-custom" ~home:no_home
    ~sessions:(fun project ->
      [
        pending_session project ~title:"Custom answer"
          ~turn_prompt:"choose safely"
          (question ~choices "Choose safely");
      ])
    ~decision_answers:
      [
        Session.Decision.Answer.Question
          (Session.Question.Answer.free "criterion β");
      ]
  @@ fun t ->
  open_pending t;
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-questio523b6bc8.home/mentat-tu…
04 |
05 | ❯ choose safely
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    question
15 |
16 |   Choose safely
17 |
18 | ────────────────────────────────────────────────────────────────────────────────
19 |   ❯
20 | ────────────────────────────────────────────────────────────────────────────────
21 |
22 |   type an answer
23 |
24 |   type answer · enter submit · esc cancel · paste works|}];
  Tui.keys t "ac";
  Tui.keys t Key.left;
  Tui.keys t "b";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-questio523b6bc8.home/mentat-tu…
04 |
05 | ❯ choose safely
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    question
15 |
16 |   Choose safely
17 |
18 | ────────────────────────────────────────────────────────────────────────────────
19 |   ❯ abc
20 | ────────────────────────────────────────────────────────────────────────────────
21 |
22 |
23 |
24 |   type answer · enter submit · esc cancel · paste works|}];
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-questio523b6bc8.home/mentat-tu…
04 |
05 | ❯ choose safely
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    question
15 |
16 |   Choose safely
17 |
18 | ❯ 1. stable
19 |   2. experimental
20 |   3. ✎ type your own answer
21 |
22 |
23 |
24 |   1-9 jump · ↑↓ move · enter answer · esc custom|}];
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.paste t "criterion β";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-questio523b6bc8.home/mentat-tu…
04 |
05 | ❯ choose safely
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    question
15 |
16 |   Choose safely
17 |
18 | ────────────────────────────────────────────────────────────────────────────────
19 |   ❯ criterion β
20 | ────────────────────────────────────────────────────────────────────────────────
21 |
22 |
23 |
24 |   type answer · enter submit · esc cancel · paste works|}];
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-questio523b6bc8.home/mentat-tu…
04 |
05 | ❯ choose safely
06 |
07 | ⠙ Working… (0s · esc to interrupt)
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
24 |   ! not logged in · /login · /tmp/mentat-t… · openai/gpt… · ! full access ? f…|}]

let%expect_test
    "activating the free-form choice edits inline under a live terminal caret" =
  let answer = "typed right here" in
  let choices = [ "stable"; "experimental" ] in
  Tui.run ~name:"question-inline-caret" ~home:no_home
    ~sessions:(fun project ->
      [
        pending_session project ~title:"Inline caret"
          ~turn_prompt:"choose safely"
          (question ~choices "Choose safely");
      ])
    ~decision_answers:
      [ Session.Decision.Answer.Question (Session.Question.Answer.free answer) ]
  @@ fun t ->
  open_pending t;
  press_down t 2;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-question-inli805518f9.home/men…
04 |
05 | ❯ choose safely
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    question
15 |
16 |   Choose safely
17 |
18 | ────────────────────────────────────────────────────────────────────────────────
19 |   ❯
20 | ────────────────────────────────────────────────────────────────────────────────
21 |
22 |
23 |
24 |   type answer · enter submit · esc cancel · paste works|}];
  let caret t =
    match Tui.cursor t with
    | None -> print_string "hidden"
    | Some (row, col) -> Printf.printf "row %d col %d" row col
  in
  caret t;
  [%expect {|row 19 col 5|}];
  Tui.keys t answer;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-question-inli805518f9.home/men…
04 |
05 | ❯ choose safely
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    question
15 |
16 |   Choose safely
17 |
18 | ────────────────────────────────────────────────────────────────────────────────
19 |   ❯ typed right here
20 | ────────────────────────────────────────────────────────────────────────────────
21 |
22 |
23 |
24 |   type answer · enter submit · esc cancel · paste works|}];
  caret t;
  [%expect {|row 19 col 21|}];
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-question-inli805518f9.home/men…
04 |
05 | ❯ choose safely
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
24 |   ! not logged in · /login · /tmp/mentat-tu… · openai/gpt… · ! full access ? …|}]

let%expect_test "answering a question preserves the visible prompt draft" =
  let draft = "FOLLOW-UP-DRAFT" in
  let choices = [ "default" ] in
  Tui.run ~name:"question-draft" ~home:no_home ~draft
    ~session:(Session.Id.of_string "session-question")
    ~sessions:(fun project ->
      [
        pending_session project ~title:"Preserve draft"
          ~turn_prompt:"ask with draft"
          (question ~choices "Which value?");
      ])
    ~decision_answers:
      [ Session.Decision.Answer.Question (Session.Question.Answer.choice 0) ]
  @@ fun t ->
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-questi68a2c4e5.home/mentat-tui…
04 |
05 | ❯ ask with draft
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    question
15 |
16 |   Which value?
17 |
18 | ❯ 1. default
19 |   2. ✎ type your own answer
20 |
21 |
22 |
23 |
24 |   1-9 jump · ↑↓ move · enter answer · esc custom|}];
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-questi68a2c4e5.home/mentat-tui…
04 |
05 | ❯ ask with draft
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
22 | ❯ FOLLOW-UP-DRAFT
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · /tmp/mentat-t… · openai/gpt… · ! full access ? f…|}]

let%expect_test "custom answers stay out of visible prompt history" =
  let recalled = "history question" in
  let custom = "history sentinel" in
  let choices = [ "known" ] in
  Tui.run ~name:"question-history" ~history:(history recalled) ~home:no_home
    ~sessions:(fun project ->
      [
        pending_session project ~title:"Question history" ~turn_prompt:recalled
          (question ~choices "Name a value");
      ])
    ~decision_answers:
      [ Session.Decision.Answer.Question (Session.Question.Answer.free custom) ]
  @@ fun t ->
  open_pending t;
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.keys t custom;
  Tui.enter t;
  Tui.settle t;
  Tui.keys t Key.up;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-questionb7ddb6b6.home/mentat-t…
04 |
05 | ❯ history question
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
22 | ❯ history question
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · /tmp/mentat-t… · openai/gpt… · ! full access ? f…|}]

[%%run_tests "mentat.tui.question"]
