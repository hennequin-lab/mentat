(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
module Json = Jsont.Json
module Session = Mentat_session
module Tool = Mentat_tool
module Task = Mentat_session.Task

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

let parent_id = Session.Id.of_string "session-thread-parent"
let provider = Mentat_llm.Provider.make "openai"
let api = Mentat_llm.Model.Api.make "responses"
let model = Mentat_llm.Model.make ~provider ~api ~id:"gpt-5.5"

let contract =
  Session.Contract.make ~mode:Session.Contract.Mode.Build ~model
    ~declarations:[] ~policy:Mentat_permission.Policy.default
    ~review:Mentat_permission.Review_behavior.Enforce
    ~sandbox:(Mentat_sandbox.identity Mentat_sandbox.direct)
    ()

let append session events =
  match Session.append_all events session with
  | Ok session -> session
  | Error error ->
      failwith
        ("invalid child-feed visual fixture: " ^ Session.Error.message error)

let response text =
  Mentat_llm.Response.make ~model (Mentat_llm.Message.Assistant.text text)

let completed_events ~turn ~prompt answer =
  let claim =
    Session.Provider_request.Started.make ~turn
      ~request_digest:(Mentat_digest.string ("thread visual: " ^ prompt))
  in
  [
    Session.Event.provider_requested claim;
    Session.Event.provider_settled
      (Session.Provider_request.Settled.responded
         ~id:(Session.Provider_request.Started.id claim)
         (response answer));
    Session.Event.turn_finished ~turn Session.Turn.Outcome.completed;
  ]

type child_result = Completed of string | Blocked of string

type child_spec = {
  id : Session.Id.t;
  task : string;
  description : string option;
  prompt : string;
  result : child_result;
  board : Session.Task.Board.t option;
}

let child ?(prompt = "Carry out the delegated task.") ?description ?board id
    task answer =
  {
    id = Session.Id.of_string id;
    task;
    description;
    prompt;
    result = Completed answer;
    board;
  }

let waiting_child ?(prompt = "Carry out the delegated task.") ?description id
    task question =
  {
    id = Session.Id.of_string id;
    task;
    description;
    prompt;
    result = Blocked question;
    board = None;
  }

let question prompt =
  match Session.Question.make ~prompt () with
  | Ok question -> question
  | Error error -> failwith (Session.Question.Error.message error)

let blocked_events ~turn ~prompt message =
  let call =
    Mentat_llm.Tool.Call.make ~id:"call-child-question" ~name:"ask"
      ~input:(Json.object' []) ()
  in
  let provider_request =
    Session.Provider_request.Started.make ~turn
      ~request_digest:(Mentat_digest.string "child question")
  in
  let response =
    Mentat_llm.Response.make ~model ~stop:Mentat_llm.Response.Stop.tool_call
      (Mentat_llm.Message.Assistant.make
         [ Mentat_llm.Message.Assistant.tool_call call ])
  in
  let requested =
    Session.Decision.Requested.make ~turn ~call ~stage:Tool.Stage.Direct
      (Session.Decision.Request.Question (question message))
  in
  [
    Session.Event.turn_started
      (Session.Turn.make ~id:turn ~origin:Session.Turn.Origin.User
         ~input:(Session.Turn.Input.user_text prompt)
         ~max_steps:16 ~contract ());
    Session.Event.provider_requested provider_request;
    Session.Event.provider_settled
      (Session.Provider_request.Settled.responded
         ~id:(Session.Provider_request.Started.id provider_request)
         response);
    Session.Event.decision_requested requested;
  ]

let documents project specs =
  let cwd = Lpath.Abs.of_string_exn (Project.root project) in
  let created_at = Session.Time.of_unix_ms 1_000_000L in
  let parent_turn_id = Session.Turn.Id.of_string "turn-thread-parent" in
  let parent_turn =
    Session.Turn.make ~id:parent_turn_id ~origin:Session.Turn.Origin.User
      ~input:(Session.Turn.Input.user_text "Coordinate the delegated work.")
      ~max_steps:16 ~contract ()
  in
  let edges =
    List.mapi
      (fun index spec ->
        Session.Delegation.make ~child:spec.id ~source_turn:parent_turn_id
          ~source_call:(Printf.sprintf "call-child-%d" (index + 1))
          ?description:spec.description
          ~task:[ Mentat_llm.Content.text spec.task ]
          ())
      specs
  in
  let parent =
    Session.create ~id:parent_id ~cwd ~created_at () |> fun session ->
    append session
      (Session.Event.turn_started parent_turn
       :: List.map Session.Event.delegation_recorded edges
      @ completed_events ~turn:parent_turn_id
          ~prompt:"Coordinate the delegated work."
          "Delegated work is in progress.")
  in
  let children =
    List.map2
      (fun edge spec ->
        let delegated_from =
          Session.Metadata.Delegated_from.make ~parent:parent_id
            ~delegation:(Session.Delegation.id edge)
        in
        let turn_id =
          Session.Turn.Id.of_string ("turn-" ^ Session.Id.to_string spec.id)
        in
        let turn =
          Session.Turn.make ~id:turn_id ~origin:Session.Turn.Origin.User
            ~input:(Session.Turn.Input.user_text spec.prompt)
            ~max_steps:16 ~contract ()
        in
        let board_events =
          match spec.board with
          | None -> []
          | Some board -> [ Session.Event.tasks_replaced board ]
        in
        let events =
          match spec.result with
          | Completed answer ->
              (Session.Event.turn_started turn :: board_events)
              @ completed_events ~turn:turn_id ~prompt:spec.prompt answer
          | Blocked question ->
              blocked_events ~turn:turn_id ~prompt:spec.prompt question
        in
        Session.create ~id:spec.id ~delegated_from ~cwd ~created_at ()
        |> fun session -> append session events)
      edges specs
  in
  (parent, children)

let run_tree ?size ?history ~name specs test =
  Tui.run ?size ?history ~name ~session:parent_id
    ~sessions:(fun project ->
      let parent, _ = documents project specs in
      [ parent ])
    ~child_sessions:(fun project ->
      let _, children = documents project specs in
      children)
    test

let history texts =
  texts
  |> List.mapi (fun index text ->
      Printf.sprintf
        {|{"schema_version":1,"type":"mentat.tui.history_entry","session_id":"session-history","timestamp":"%d","draft":{"text":%S}}|}
        (1_000 - index) text)
  |> String.concat "\n"

let lifecycle_child =
  child "session-thread-lifecycle" "inspect the session lifecycle"
    "The lifecycle is consistent."

let%expect_test
    "a resumed child replays through settlement and reports it exactly once" =
  run_tree ~name:"threads-lifecycle" [ lifecycle_child ] @@ fun t ->
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threads-l8c17edc7
04 |
05 | ❯ Coordinate the delegated work.
06 |
07 | ⏺ Delegated work is in progress.
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
18 | ────────────────────────────────────────────────────────────────────────────────
19 | ❯ message mentat
20 | ────────────────────────────────────────────────────────────────────────────────
21 |   ◯ main
22 |   ○ subagent inspect the session lifecycle                             delegated
23 |
24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt-… · ! full access ? for…|}];
  Tui.next_child_fact t lifecycle_child.id;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threads-l8c17edc7
04 |
05 | ❯ Coordinate the delegated work.
06 |
07 | ⏺ Delegated work is in progress.
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
18 | ────────────────────────────────────────────────────────────────────────────────
19 | ❯ message mentat
20 | ────────────────────────────────────────────────────────────────────────────────
21 |   ◯ main
22 |   ◉ subagent inspect the session lifecycle                               working
23 |
24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt-… · ! full access ? for…|}];
  Tui.drain_child t lifecycle_child.id;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threads-l8c17edc7
04 |
05 | ❯ Coordinate the delegated work.
06 |
07 | ⏺ Delegated work is in progress.
08 |
09 |   ● Agent "inspect the session lifecycle" finished
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 |
18 | ────────────────────────────────────────────────────────────────────────────────
19 | ❯ message mentat
20 | ────────────────────────────────────────────────────────────────────────────────
21 |   ◯ main
22 |   ✓ subagent inspect the session lifecycle                             completed
23 |
24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt-… · ! full access ? for…|}];
  (* A drill replays the same child settlement through a separate observation.
     Returning to the parent must retain one derived settlement event, not append
     a duplicate from read-only replay. *)
  Tui.keys t Key.tab;
  Tui.keys t Key.down;
  Tui.enter t;
  Tui.settle t;
  Tui.drain_child ~observation:Tui.Child_observation.Drill t lifecycle_child.id;
  Tui.settle t;
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threads-l8c17edc7
04 |
05 | ❯ Coordinate the delegated work.
06 |
07 | ⏺ Delegated work is in progress.
08 |
09 |   ● Agent "inspect the session lifecycle" finished
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 |
18 | ────────────────────────────────────────────────────────────────────────────────
19 | ❯ message mentat
20 | ────────────────────────────────────────────────────────────────────────────────
21 |   ◯ main
22 |   ✓ subagent inspect the session lifecycle                             completed
23 |
24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt-… · ! full access ? for…|}]

let blocked_child =
  waiting_child ~prompt:"Check whether the parent approves."
    "session-thread-blocked" "ask the parent before continuing"
    "Should I continue with the risky operation?"

let%expect_test "a child decision is visibly blocked rather than working" =
  run_tree ~name:"threads-blocked" [ blocked_child ] @@ fun t ->
  Tui.settle t;
  Tui.drain_child t blocked_child.id;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threadsa691bc0c
04 |
05 | ❯ Coordinate the delegated work.
06 |
07 | ⏺ Delegated work is in progress.
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
18 | ────────────────────────────────────────────────────────────────────────────────
19 | ❯ message mentat
20 | ────────────────────────────────────────────────────────────────────────────────
21 |   ◯ main
22 |   ⋯ subagent ask the parent before continuing                            blocked
23 |
24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt… · ! full access ? for …|}]

let map_child =
  child ~prompt:"Map the configuration flow." "session-thread-map"
    "map the configuration flow" "Mapped the configuration flow."

let verify_child =
  child ~prompt:"Verify the visual contract." "session-thread-verify"
    "verify the complete visual contract" "The visual contract is verified."

let%expect_test
    "reverse child arrival preserves edge order and opens the selected child" =
  run_tree ~name:"threads-order" [ map_child; verify_child ] @@ fun t ->
  Tui.settle t;
  Tui.drain_child t verify_child.id;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threa97af2f0d
04 |
05 | ❯ Coordinate the delegated work.
06 |
07 | ⏺ Delegated work is in progress.
08 |
09 |   ● Agent "verify the complete visual contract" finished
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 | ────────────────────────────────────────────────────────────────────────────────
18 | ❯ message mentat
19 | ────────────────────────────────────────────────────────────────────────────────
20 |   ◯ main
21 |   ○ subagent map the configuration flow                                delegated
22 |   ✓ subagent verify the complete visual contract                       completed
23 |
24 |   ! not logged in · /login · ~/mentat-… · openai/gpt-… · ! full access ? for …|}];
  Tui.keys t Key.tab;
  Tui.keys t Key.down;
  Tui.keys t Key.down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threa97af2f0d
04 |
05 | ❯ Coordinate the delegated work.
06 |
07 | ⏺ Delegated work is in progress.
08 |
09 |   ● Agent "verify the complete visual contract" finished
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 | ────────────────────────────────────────────────────────────────────────────────
18 | ❯ message mentat
19 | ────────────────────────────────────────────────────────────────────────────────
20 |   ◯ main
21 |   ○ subagent map the configuration flow                                delegated
22 | ❯ ✓ subagent verify the complete visual contract                       completed
23 |
24 |   ! not logged in · /login · ~/mentat-… · openai/gpt-… · ! full access ? for …|}];
  Tui.enter t;
  Tui.settle t;
  Tui.drain_child ~observation:Tui.Child_observation.Drill t verify_child.id;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |   ▂▄▆▄▂ @subagent
03 |   started by main
04 | ❯ Verify the visual contract.
05 |
06 | ⏺ The visual contract is verified.
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
17 | ───────────────────────────────────────────────────────────────────── @subagent
18 | ❯ message @subagent
19 | ────────────────────────────────────────────────────────────────────────────────
20 |   ◯ main
21 |   ○ subagent map the configuration flow                                delegated
22 |   ✓ subagent verify the complete visual contract                       completed
23 |
24 |   ! not logged in · /login · ~/mentat-… · openai/gpt-… · ! full access esc ba…|}]

let running_child =
  child ~prompt:"Inspect the live child." "session-thread-running"
    "inspect the live child while it runs" "The live inspection is complete."

let%expect_test "a running child can be drilled and escape restores the parent"
    =
  run_tree ~name:"threads-running-drill" [ running_child ] @@ fun t ->
  Tui.settle t;
  Tui.next_child_fact t running_child.id;
  Tui.settle t;
  Tui.keys t Key.tab;
  Tui.keys t Key.down;
  Tui.enter t;
  Tui.settle t;
  Tui.next_child_fact ~observation:Tui.Child_observation.Drill t
    running_child.id;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |   ▂▄▆▄▂ @subagent
03 |   started by main
04 | ❯ Inspect the live child.
05 |
06 | ⠋ Working… (0s · esc to interrupt)
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
18 | ───────────────────────────────────────────────────────────────────── @subagent
19 | ❯ message @subagent
20 | ────────────────────────────────────────────────────────────────────────────────
21 |   ◯ main
22 |   ◉ subagent inspect the live child while it runs                        working
23 |
24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt-… · ! full access esc b…|}];
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threads-runni74f426d1
04 |
05 | ❯ Coordinate the delegated work.
06 |
07 | ⏺ Delegated work is in progress.
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
18 | ────────────────────────────────────────────────────────────────────────────────
19 | ❯ message mentat
20 | ────────────────────────────────────────────────────────────────────────────────
21 |   ◯ main
22 |   ◉ subagent inspect the live child while it runs                        working
23 |
24 |   ! not logged in · /login · ~/mentat-tu… · openai/gpt… · ! full access ? for…|}];
  Tui.drain_child t running_child.id;
  Tui.settle t

(* In production a delegation's task and the child's opening user message are the
   same spawn prompt. The drill header must therefore identify the child without
   echoing that prompt: the prompt belongs to the transcript, where it wraps. *)
let explored_child =
  let mission =
    "Explore the OCaml library under lib/session in the mentat workspace. Read \
     its dune file, public .mli files, and key implementation modules, then \
     produce a concise brief covering the library's purpose and public API."
  in
  child ~prompt:mission "session-thread-explore" mission
    "The session library brief is ready."

let%expect_test "drilling a child identifies it without repeating the prompt" =
  run_tree ~name:"threads-explore" [ explored_child ] @@ fun t ->
  Tui.settle t;
  Tui.drain_child t explored_child.id;
  Tui.settle t;
  Tui.keys t Key.tab;
  Tui.keys t Key.down;
  Tui.enter t;
  Tui.settle t;
  Tui.drain_child ~observation:Tui.Child_observation.Drill t explored_child.id;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |   ▂▄▆▄▂ @subagent
03 |   started by main
04 | ❯ Explore the OCaml library under lib/session in the mentat workspace. Read its
05 |   dune file, public .mli files, and key implementation modules, then produce a
06 |   concise brief covering the library's purpose and public API.
07 |
08 | ⏺ The session library brief is ready.
09 |
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 |
18 | ───────────────────────────────────────────────────────────────────── @subagent
19 | ❯ message @subagent
20 | ────────────────────────────────────────────────────────────────────────────────
21 |   ◯ main
22 |   ✓ subagent Explore the OCaml library under lib/session in the men... completed
23 |
24 |   ! not logged in · /login · ~/mentat-… · openai/gpt-… · ! full access esc ba…|}]

(* A short description labels the child at every glance: the heap banner suffixes
   it, the strip carries it in place of the wrapping task, and the composer names
   the child it now addresses. Typing proves the drill owns a live composer whose
   draft targets this child. *)
let described_child =
  child ~description:"map the config loader" "session-thread-described"
    "Trace the configuration loader end to end and report the resolution order \
     it applies across user and project sources."
    "The configuration loader is mapped."

let%expect_test "a described child names itself and accepts a composer draft" =
  run_tree ~name:"threads-described" [ described_child ] @@ fun t ->
  Tui.settle t;
  Tui.drain_child t described_child.id;
  Tui.settle t;
  Tui.keys t Key.tab;
  Tui.keys t Key.down;
  Tui.enter t;
  Tui.settle t;
  Tui.drain_child ~observation:Tui.Child_observation.Drill t described_child.id;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |   ▂▄▆▄▂ @subagent — map the config loader
03 |   started by main
04 | ❯ Carry out the delegated task.
05 |
06 | ⏺ The configuration loader is mapped.
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
18 | ───────────────────────────────────────────────────────────────────── @subagent
19 | ❯ message @subagent
20 | ────────────────────────────────────────────────────────────────────────────────
21 |   ◯ main
22 |   ✓ subagent map the config loader                                     completed
23 |
24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt… · ! full access esc ba…|}];
  Tui.keys t "look at lib/config first";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |   ▂▄▆▄▂ @subagent — map the config loader
03 |   started by main
04 | ❯ Carry out the delegated task.
05 |
06 | ⏺ The configuration loader is mapped.
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
18 | ───────────────────────────────────────────────────────────────────── @subagent
19 | ❯ look at lib/config first
20 | ────────────────────────────────────────────────────────────────────────────────
21 |   ◯ main
22 |   ✓ subagent map the config loader                                     completed
23 |
24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt… · ! full access esc ba…|}]

let%expect_test "the drilled child keeps the switcher in its wide pane" =
  run_tree ~name:"threads-drill-pane" ~size:(120, 24)
    [ map_child; verify_child ]
  @@ fun t ->
  Tui.settle t;
  Tui.drain_child t verify_child.id;
  Tui.settle t;
  Tui.keys t Key.tab;
  Tui.keys t Key.down;
  Tui.keys t Key.down;
  Tui.enter t;
  Tui.settle t;
  Tui.drain_child ~observation:Tui.Child_observation.Drill t verify_child.id;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |                                                                                 │ agents
02 |   ▂▄▆▄▂ @subagent                                                               │   ◯ main
03 |   started by main                                                               │   ○ map the configuration... delegated
04 | ❯ Verify the visual contract.                                                   │   ✓ verify the complete v... completed
05 |                                                                                 │
06 | ⏺ The visual contract is verified.                                              │
07 |                                                                                 │
08 |                                                                                 │
09 |                                                                                 │
10 |                                                                                 │
11 |                                                                                 │
12 |                                                                                 │
13 |                                                                                 │
14 |                                                                                 │
15 |                                                                                 │
16 |                                                                                 │
17 |                                                                                 │
18 |                                                                                 │
19 |                                                                                 │
20 |
21 | ───────────────────────────────────────────────────────────────────────────────────────────────────────────── @subagent
22 | ❯ message @subagent
23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · ~/mentat-tui-threads-dr4624afe1 · openai/gpt-5.5 · ! full access         esc back to main|}]

let long_task_child =
  child "session-thread-responsive-long"
    "trace the complete delegation lifecycle through replay settlement and \
     responsive presentation without hiding any state"
    "The responsive child state remains visible."

let responsive_running_child =
  child "session-thread-responsive-running" "verify the wide activity pane"
    "The activity pane remains attached."

let responsive_blocked_child =
  waiting_child "session-thread-responsive-blocked"
    "request approval before changing the layout contract"
    "May I change the layout contract?"

let%expect_test
    "child activity moves beside the transcript and truncates through resize" =
  let children =
    [ long_task_child; responsive_running_child; responsive_blocked_child ]
  in
  run_tree ~name:"threads-responsive" ~size:(120, 24) children @@ fun t ->
  Tui.settle t;
  Tui.next_child_fact t responsive_running_child.id;
  Tui.drain_child t responsive_blocked_child.id;
  Tui.settle t;
  (* In the wide branch the complete child surface belongs beside the
     transcript. Mosaic's table owns the long task's ellipsis. *)
  Tui.print t;
  [%expect
    {|01 |                                                                                 │ agents · 1 running · 1 blocked
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium                     │   ◯ main
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threads-ref42851b3                 │   ○ trace the complete de... delegated
04 |                                                                                 │   ◉ verify the wide activ... working
05 | ❯ Coordinate the delegated work.                                                │   ⋯ request approval befo... blocked
06 |                                                                                 │
07 | ⏺ Delegated work is in progress.                                                │
08 |                                                                                 │
09 |                                                                                 │
10 |                                                                                 │
11 |                                                                                 │
12 |                                                                                 │
13 |                                                                                 │
14 |                                                                                 │
15 |                                                                                 │
16 |                                                                                 │
17 |                                                                                 │
18 |                                                                                 │
19 |                                                                                 │
20 |
21 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · ~/mentat-tui-threads-ref42851b3 · openai/gpt-5.5 · ! full access          ? for shortcuts|}];
  (* Crossing below the breakpoint returns the same semantic rows to the
     narrow flow. No child is sliced or reformatted by the application. *)
  Tui.resize t ~width:109 ~height:24;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threads-ref42851b3
04 |
05 | ❯ Coordinate the delegated work.
06 |
07 | ⏺ Delegated work is in progress.
08 |
09 |
10 |
11 |
12 |
13 |
14 |
15 |
16 | ─────────────────────────────────────────────────────────────────────────────────────────────────────────────
17 | ❯ message mentat
18 | ─────────────────────────────────────────────────────────────────────────────────────────────────────────────
19 |   ◯ main
20 |   ○ subagent trace the complete delegation lifecycle through replay settlement and responsive pr... delegated
21 |   ◉ subagent verify the wide activity pane                                                          working
22 |   ⋯ subagent request approval before changing the layout contract                                   blocked
23 |
24 |   ! not logged in · /login · ~/mentat-tui-threads-ref42851… · openai/gpt-5.5 · ! full access ? for shortcu…|}]

let many_children =
  List.init 12 (fun index ->
      let position = index + 1 in
      child
        (Printf.sprintf "session-thread-many-%02d" position)
        (Printf.sprintf "audit delegated child %02d" position)
        (Printf.sprintf "Child %02d completed its audit." position))

let rec move_thread_focus_down t remaining =
  if remaining > 0 then begin
    Tui.keys t Key.down;
    move_thread_focus_down t (remaining - 1)
  end

let%expect_test
    "many children remain navigable in a scarce-height Mosaic viewport" =
  run_tree ~name:"threads-scarce-height" ~size:(80, 12) many_children
  @@ fun t ->
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 | ⏺ Delegated work is in progress.
02 |
03 | ────────────────────────────────────────────────────────────────────────────────
04 | ❯ message mentat
05 | ────────────────────────────────────────────────────────────────────────────────
06 |   ◯ main
07 |   ○ subagent audit delegated child 01                                  delegated
08 |   ○ subagent audit delegated child 02                                  delegated
09 |   ○ subagent audit delegated child 03                                  delegated
10 |                                                                         ↓ 9 more
11 |
12 |   ! not logged in · /login · ~/mentat-tu… · openai/gpt… · ! full access ? for…|}];
  (* Tab focuses the switcher at main; one move per delegation reaches the final
     child. Mosaic keeps that selected semantic row visible within scarce
     height. *)
  Tui.keys t Key.tab;
  move_thread_focus_down t (List.length many_children);
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 | ⏺ Delegated work is in progress.
02 | ────────────────────────────────────────────────────────────────────────────────
03 | ❯ message mentat
04 | ────────────────────────────────────────────────────────────────────────────────
05 |   ○ subagent audit delegated child 03                                 delegated↑
06 |   ○ subagent audit delegated child 04                                 delegated│
07 |   ○ subagent audit delegated child 05                                 delegated│
08 |   ○ subagent audit delegated child 06                                 delegated│
09 |   ○ subagent audit delegated child 07                                 delegated│
10 |   ○ subagent audit delegated child 08                                 delegated│
11 |   ○ subagent audit delegated child 09                                 delegated│
12 |   ○ subagent audit delegated child 10                                 delegated│|}]

let unavailable message =
  Mentat_protocol.Error.Unavailable (Mentat_diagnostic.of_text message)

let unreachable_child =
  child "session-thread-unavailable" "inspect the unreachable child"
    "never observed"

(* A lost child feed is terminal: the live observation does not re-attach. In
   the side pane the row stays honest about that and, when focused, offers the
   one useful action — a retry — rather than opening an empty drill. Under a
   persistent transport error the retry re-follows and settles back to the same
   error in place, never leaving the switcher. *)
let%expect_test "a feed-unavailable child offers retry and re-follows in place"
    =
  Tui.run ~name:"threads-unavailable" ~size:(120, 24) ~session:parent_id
    ~sessions:(fun project ->
      let parent, _ = documents project [ unreachable_child ] in
      [ parent ])
    ~child_sessions:(fun project ->
      let _, children = documents project [ unreachable_child ] in
      children)
    ~child_observation_error:(unavailable "child transport dropped")
  @@ fun t ->
  Tui.settle t;
  (* The pane's agents row is an honest error, not a working child. *)
  Tui.print t;
  [%expect
    {|01 |                                                                                 │ agents
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium                     │   ◯ main
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threads-unab03fa575                │   ! inspect the un... feed unavailable
04 |                                                                                 │
05 | ❯ Coordinate the delegated work.                                                │
06 |                                                                                 │
07 | ⏺ Delegated work is in progress.                                                │
08 |                                                                                 │
09 | ✗ child transport dropped                                                       │
10 |   retry when ready                                                              │
11 |                                                                                 │
12 |                                                                                 │
13 |                                                                                 │
14 |                                                                                 │
15 |                                                                                 │
16 |                                                                                 │
17 |                                                                                 │
18 |                                                                                 │
19 |                                                                                 │
20 |
21 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
24 |   child transport dropped|}];
  (* Focusing it advertises the retry that Enter performs — not "open". *)
  Tui.keys t Key.tab;
  Tui.settle t;
  Tui.keys t Key.down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |                                                                                 │ agents
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium                     │   ◯ main
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threads-unab03fa575                │ ❯ ! inspect the unre... enter to retry
04 |                                                                                 │
05 | ❯ Coordinate the delegated work.                                                │
06 |                                                                                 │
07 | ⏺ Delegated work is in progress.                                                │
08 |                                                                                 │
09 | ✗ child transport dropped                                                       │
10 |   retry when ready                                                              │
11 |                                                                                 │
12 |                                                                                 │
13 |                                                                                 │
14 |                                                                                 │
15 |                                                                                 │
16 |                                                                                 │
17 |                                                                                 │
18 |                                                                                 │
19 |                                                                                 │
20 |
21 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
24 |   child transport dropped|}];
  (* Enter re-follows in place; the persistent error keeps the row honest and
     never opens a drill. *)
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |                                                                                 │ agents
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium                     │   ◯ main
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threads-unab03fa575                │ ❯ ! inspect the unre... enter to retry
04 |                                                                                 │
05 | ❯ Coordinate the delegated work.                                                │
06 |                                                                                 │
07 | ⏺ Delegated work is in progress.                                                │
08 |                                                                                 │
09 | ✗ child transport dropped × 2                                                   │
10 |   retry when ready                                                              │
11 |                                                                                 │
12 |                                                                                 │
13 |                                                                                 │
14 |                                                                                 │
15 |                                                                                 │
16 |                                                                                 │
17 |                                                                                 │
18 |                                                                                 │
19 |                                                                                 │
20 |
21 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
24 |   child transport dropped|}]

let boarded_child =
  child ~prompt:"Port the loader." "session-thread-boarded"
    "port the configuration loader" "The configuration loader is ported."
    ~board:
      (board
         [
           task ~id:"task-1" ~content:"read the loader"
             ~status:Task.Status.Completed ~position:0;
           task ~id:"task-2" ~content:"port the resolution order"
             ~status:Task.Status.In_progress ~position:1;
           task ~id:"task-3" ~content:"add regression tests"
             ~status:Task.Status.Pending ~position:2;
         ])

(* The drill's side pane carries the drilled child's own durable task board, the
   same tenant the main view shows for its session. The board is folded from the
   drill feed and rendered beside the transcript on a wide terminal. *)
let%expect_test "a drilled child shows its own task board in the wide pane" =
  run_tree ~name:"threads-drill-board" ~size:(120, 24) [ boarded_child ]
  @@ fun t ->
  Tui.settle t;
  Tui.drain_child t boarded_child.id;
  Tui.settle t;
  Tui.keys t Key.tab;
  Tui.keys t Key.down;
  Tui.enter t;
  Tui.settle t;
  Tui.drain_child ~observation:Tui.Child_observation.Drill t boarded_child.id;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |                                                                                 │ agents
02 |   ▂▄▆▄▂ @subagent                                                               │   ◯ main
03 |   started by main                                                               │   ✓ port the configuratio... completed
04 | ❯ Port the loader.                                                              │
05 |                                                                                 │ tasks · 1 done · 1 running
06 | ⏺ Todo(3 tasks · 1 done · 1 running)                                            │   ◼ port the resolution order
07 |                                                                                 │   ◻ add regression tests
08 | ⏺ The configuration loader is ported.                                           │   … 1 done
09 |                                                                                 │
10 |                                                                                 │
11 |                                                                                 │
12 |                                                                                 │
13 |                                                                                 │
14 |                                                                                 │
15 |                                                                                 │
16 |                                                                                 │
17 |                                                                                 │
18 |                                                                                 │
19 |                                                                                 │
20 |
21 | ───────────────────────────────────────────────────────────────────────────────────────────────────────────── @subagent
22 | ❯ message @subagent
23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · ~/mentat-tui-threads-drib05bcafb · openai/gpt-5.5 · ! full access        esc back to main|}]

(* Opening the switcher from a drill lands on the child currently in view, not
   the parent row, so [Enter] holds the current child instead of jumping home.
   The two-child tree drills the second child; the glance must open on it. *)
let%expect_test "the drill switcher opens on the current child, not main" =
  run_tree ~name:"threads-drill-glance" ~size:(100, 24)
    [ map_child; verify_child ]
  @@ fun t ->
  Tui.settle t;
  Tui.drain_child t verify_child.id;
  Tui.settle t;
  Tui.keys t Key.tab;
  Tui.settle t;
  Tui.keys t Key.down;
  Tui.settle t;
  Tui.keys t Key.down;
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.drain_child ~observation:Tui.Child_observation.Drill t verify_child.id;
  Tui.settle t;
  (* Tab opens the glance; it must highlight the drilled second child, not the
     parent [main] row. *)
  Tui.keys t Key.tab;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |   ▂▄▆▄▂ @subagent
03 |   started by main
04 | ❯ Verify the visual contract.
05 |
06 | ⏺ The visual contract is verified.
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
17 | ───────────────────────────────────────────────────────────────────────────────────────── @subagent
18 | ❯ message @subagent
19 | ────────────────────────────────────────────────────────────────────────────────────────────────────
20 |   ◯ main
21 |   ○ subagent map the configuration flow                                                    delegated
22 | ❯ ✓ subagent verify the complete visual contract                                           completed
23 |
24 |   ! not logged in · /login · ~/mentat-tui-threads-dr… · openai/gpt-5… · ! full access esc back to…|}];
  (* Enter on the highlighted current child holds it and drops the switcher focus
     back to the composer, which then accepts typed text. *)
  Tui.enter t;
  Tui.settle t;
  Tui.keys t "still here";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |   ▂▄▆▄▂ @subagent
03 |   started by main
04 | ❯ Verify the visual contract.
05 |
06 | ⏺ The visual contract is verified.
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
17 | ───────────────────────────────────────────────────────────────────────────────────────── @subagent
18 | ❯ still here
19 | ────────────────────────────────────────────────────────────────────────────────────────────────────
20 |   ◯ main
21 |   ○ subagent map the configuration flow                                                    delegated
22 |   ✓ subagent verify the complete visual contract                                           completed
23 |
24 |   ! not logged in · /login · ~/mentat-tui-threads-dr… · openai/gpt-5… · ! full access esc back to…|}]

(* Prompt history is shared across the main view and a drill: the drill composer
   is seeded with the same loaded entries, so Up recalls them exactly as the main
   composer does. *)
let%expect_test "the drill composer recalls shared prompt history" =
  run_tree ~name:"threads-drill-history" ~size:(120, 24)
    ~history:(history [ "recall me in the drill" ])
    [ described_child ]
  @@ fun t ->
  Tui.settle t;
  Tui.drain_child t described_child.id;
  Tui.settle t;
  Tui.keys t Key.tab;
  Tui.settle t;
  Tui.keys t Key.down;
  Tui.enter t;
  Tui.settle t;
  Tui.drain_child ~observation:Tui.Child_observation.Drill t described_child.id;
  Tui.settle t;
  Tui.keys t Key.up;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |                                                                                 │ agents
02 |   ▂▄▆▄▂ @subagent — map the config loader                                       │   ◯ main
03 |   started by main                                                               │   ✓ map the config loader    completed
04 | ❯ Carry out the delegated task.                                                 │
05 |                                                                                 │
06 | ⏺ The configuration loader is mapped.                                           │
07 |                                                                                 │
08 |                                                                                 │
09 |                                                                                 │
10 |                                                                                 │
11 |                                                                                 │
12 |                                                                                 │
13 |                                                                                 │
14 |                                                                                 │
15 |                                                                                 │
16 |                                                                                 │
17 |                                                                                 │
18 |                                                                                 │
19 |                                                                                 │
20 |
21 | ───────────────────────────────────────────────────────────────────────────────────────────────────────────── @subagent
22 | ❯ recall me in the drill
23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · ~/mentat-tui-threads-drillec4eb6df · openai/gpt-5.5 · ! full access      esc back to main|}]

(* A leading slash opens the command palette over the drill composer, the same
   completion surface the main composer offers. *)
let%expect_test "the drill composer opens the slash-command palette" =
  run_tree ~name:"threads-drill-palette" ~size:(120, 24) [ described_child ]
  @@ fun t ->
  Tui.settle t;
  Tui.drain_child t described_child.id;
  Tui.settle t;
  Tui.keys t Key.tab;
  Tui.settle t;
  Tui.keys t Key.down;
  Tui.enter t;
  Tui.settle t;
  Tui.drain_child ~observation:Tui.Child_observation.Drill t described_child.id;
  Tui.settle t;
  Tui.keys t "/";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |                                                                                 │ agents
02 |   ▂▄▆▄▂ @subagent — map the config loader                                       │   ◯ main
03 |   started by main                                                               │   ✓ map the config loader    completed
04 | ❯ Carry out the delegated task.                                                 │
05 |                                                                                 │
06 | ⏺ The configuration loader is mapped.                                           │
07 |                                                                                 │
08 |                                                                                 │
09 |                                                                                 │
10 |                                                                                 │
11 |                                                                                 │
12 |                                                                                 │
13 |                                                                                 │
14 |                                                                                 │
15 |                                                                                 │
16 | ❯ /clear      Start a new session with empty context; previous session stays on disk
17 |   /fork       Fork current session
18 |   /rewind     Jump back to an earlier message and resubmit
19 |   /undo       Undo the last turn: revert its files and reload its message
20 |   /redo       Redo an undone turn, restoring its files and messages
21 | ───────────────────────────────────────────────────────────────────────────────────────────────────────────── @subagent
22 | ❯ /
23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · ~/mentat-tui-threads-drill94c456bf · openai/gpt-5.5 · ! full access      esc back to main|}]

(* A conversation-lifecycle command targets a specific session through main-owned
   request accounting, so in a drill it is gated with an honest note rather than
   silently acting on the hidden main session. *)
let%expect_test "a conversation command in a drill is gated, not silently run" =
  run_tree ~name:"threads-drill-gate" ~size:(120, 24) [ described_child ]
  @@ fun t ->
  Tui.settle t;
  Tui.drain_child t described_child.id;
  Tui.settle t;
  Tui.keys t Key.tab;
  Tui.settle t;
  Tui.keys t Key.down;
  Tui.enter t;
  Tui.settle t;
  Tui.drain_child ~observation:Tui.Child_observation.Drill t described_child.id;
  Tui.settle t;
  Tui.keys t "/compact";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |                                                                                 │ agents
02 |   ▂▄▆▄▂ @subagent — map the config loader                                       │   ◯ main
03 |   started by main                                                               │   ✓ map the config loader    completed
04 | ❯ Carry out the delegated task.                                                 │
05 |                                                                                 │
06 | ⏺ The configuration loader is mapped.                                           │
07 |                                                                                 │
08 |                                                                                 │
09 |                                                                                 │
10 |                                                                                 │
11 |                                                                                 │
12 |                                                                                 │
13 |                                                                                 │
14 |                                                                                 │
15 |                                                                                 │
16 |                                                                                 │
17 |                                                                                 │
18 |                                                                                 │
19 |                                                                                 │
20 |
21 | ───────────────────────────────────────────────────────────────────────────────────────────────────────────── @subagent
22 | ❯ message @subagent
23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
24 |   /compact targets the main session — esc back first|}]

(* A delegated child can itself delegate. The grandchild reaches the switcher
   through its parent's own live feed, so it must join the tree indented one
   level under the child that spawned it — not vanish, and not float up as a
   sibling of the direct children. *)
let nested_child_id = Session.Id.of_string "session-thread-nested-child"

let nested_grandchild_id =
  Session.Id.of_string "session-thread-nested-grandchild"

let nested_documents project =
  let cwd = Lpath.Abs.of_string_exn (Project.root project) in
  let created_at = Session.Time.of_unix_ms 1_000_000L in
  let text s = [ Mentat_llm.Content.text s ] in
  let turn ~id ~prompt =
    Session.Turn.make
      ~id:(Session.Turn.Id.of_string id)
      ~origin:Session.Turn.Origin.User
      ~input:(Session.Turn.Input.user_text prompt)
      ~max_steps:16 ~contract ()
  in
  (* The active session delegates to the child. *)
  let root_turn =
    turn ~id:"turn-nested-root" ~prompt:"Coordinate nested work."
  in
  let child_edge =
    Session.Delegation.make ~child:nested_child_id
      ~source_turn:(Session.Turn.id root_turn)
      ~source_call:"call-nested-child"
      ~task:(text "map the config subsystem")
      ()
  in
  let root =
    Session.create ~id:parent_id ~cwd ~created_at () |> fun session ->
    append session
      (Session.Event.turn_started root_turn
      :: Session.Event.delegation_recorded child_edge
      :: completed_events
           ~turn:(Session.Turn.id root_turn)
           ~prompt:"Coordinate nested work." "Delegated the nested work.")
  in
  (* The child, delegated from the active session, itself delegates to the
     grandchild before settling. *)
  let child_turn =
    turn ~id:"turn-nested-child" ~prompt:"Map the config subsystem."
  in
  let grandchild_edge =
    Session.Delegation.make ~child:nested_grandchild_id
      ~source_turn:(Session.Turn.id child_turn)
      ~source_call:"call-nested-grandchild"
      ~task:(text "audit the loader units")
      ()
  in
  let child =
    Session.create ~id:nested_child_id
      ~delegated_from:
        (Session.Metadata.Delegated_from.make ~parent:parent_id
           ~delegation:(Session.Delegation.id child_edge))
      ~cwd ~created_at ()
    |> fun session ->
    append session
      (Session.Event.turn_started child_turn
      :: Session.Event.delegation_recorded grandchild_edge
      :: completed_events
           ~turn:(Session.Turn.id child_turn)
           ~prompt:"Map the config subsystem." "Mapped the config subsystem.")
  in
  (* The grandchild, delegated from the child. *)
  let grandchild_turn =
    turn ~id:"turn-nested-grandchild" ~prompt:"Audit the loader units."
  in
  let grandchild =
    Session.create ~id:nested_grandchild_id
      ~delegated_from:
        (Session.Metadata.Delegated_from.make ~parent:nested_child_id
           ~delegation:(Session.Delegation.id grandchild_edge))
      ~cwd ~created_at ()
    |> fun session ->
    append session
      (Session.Event.turn_started grandchild_turn
      :: completed_events
           ~turn:(Session.Turn.id grandchild_turn)
           ~prompt:"Audit the loader units." "Audited the loader units.")
  in
  (root, [ child; grandchild ])

let%expect_test "a subagent's subagent nests under it in the agents tree" =
  Tui.run ~name:"threads-nested" ~size:(120, 24) ~session:parent_id
    ~sessions:(fun project ->
      let root, _ = nested_documents project in
      [ root ])
    ~child_sessions:(fun project ->
      let _, children = nested_documents project in
      children)
  @@ fun t ->
  Tui.settle t;
  (* Draining the child delivers its delegation-of-the-grandchild fact, which
     enumerates the grandchild and follows it in turn. *)
  Tui.drain_child t nested_child_id;
  Tui.settle t;
  Tui.drain_child t nested_grandchild_id;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |                                                                                 │ agents
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium                     │   ◯ main
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-thread62b87a8d                     │   ✓ map the config subsystem completed
04 |                                                                                 │   ✓ └ audit the loader units completed
05 | ❯ Coordinate nested work.                                                       │
06 |                                                                                 │
07 | ⏺ Delegated the nested work.                                                    │
08 |                                                                                 │
09 |   ● Agent "map the config subsystem" finished                                   │
10 |                                                                                 │
11 |   ● Agent "audit the loader units" finished                                     │
12 |                                                                                 │
13 |                                                                                 │
14 |                                                                                 │
15 |                                                                                 │
16 |                                                                                 │
17 |                                                                                 │
18 |                                                                                 │
19 |                                                                                 │
20 |
21 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · ~/mentat-tui-thread62b87a8d · openai/gpt-5.5 · ! full access              ? for shortcuts|}];
  (* The grandchild is a real observed session, so the switcher can drill into
     it exactly as it drills a direct child. *)
  Tui.keys t Key.tab;
  Tui.keys t Key.down;
  Tui.keys t Key.down;
  Tui.enter t;
  Tui.settle t;
  Tui.drain_child ~observation:Tui.Child_observation.Drill t
    nested_grandchild_id;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |                                                                                 │ agents
02 |   ▂▄▆▄▂ @subagent                                                               │   ◯ main
03 |   started by main                                                               │   ✓ map the config subsystem completed
04 | ❯ Audit the loader units.                                                       │   ✓ └ audit the loader units completed
05 |                                                                                 │
06 | ⏺ Audited the loader units.                                                     │
07 |                                                                                 │
08 |                                                                                 │
09 |                                                                                 │
10 |                                                                                 │
11 |                                                                                 │
12 |                                                                                 │
13 |                                                                                 │
14 |                                                                                 │
15 |                                                                                 │
16 |                                                                                 │
17 |                                                                                 │
18 |                                                                                 │
19 |                                                                                 │
20 |
21 | ───────────────────────────────────────────────────────────────────────────────────────────────────────────── @subagent
22 | ❯ message @subagent
23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · ~/mentat-tui-thread62b87a8d · openai/gpt-5.5 · ! full access             esc back to main|}]

(* A short-labelled failing child proves the label is shown in full — not
   collapsed to a sliver — right beside its failure fact and, when focused, its
   retry hint. This is the agents-pane label-starvation regression. *)
let short_failing_child =
  child "session-thread-short-fail" "port loader" "never observed"

let%expect_test "a failure row keeps its full label in the agents pane" =
  Tui.run ~name:"threads-short-fail" ~size:(120, 24) ~session:parent_id
    ~sessions:(fun project ->
      let parent, _ = documents project [ short_failing_child ] in
      [ parent ])
    ~child_sessions:(fun project ->
      let _, children = documents project [ short_failing_child ] in
      children)
    ~child_observation_error:(unavailable "child feed dropped")
  @@ fun t ->
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |                                                                                 │ agents
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium                     │   ◯ main
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threads-she9e20a14                 │   ! port loader       feed unavailable
04 |                                                                                 │
05 | ❯ Coordinate the delegated work.                                                │
06 |                                                                                 │
07 | ⏺ Delegated work is in progress.                                                │
08 |                                                                                 │
09 | ✗ child feed dropped                                                            │
10 |   retry when ready                                                              │
11 |                                                                                 │
12 |                                                                                 │
13 |                                                                                 │
14 |                                                                                 │
15 |                                                                                 │
16 |                                                                                 │
17 |                                                                                 │
18 |                                                                                 │
19 |                                                                                 │
20 |
21 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
24 |   child feed dropped|}];
  Tui.keys t Key.tab;
  Tui.keys t Key.down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |                                                                                 │ agents
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium                     │   ◯ main
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threads-she9e20a14                 │ ❯ ! port loader         enter to retry
04 |                                                                                 │
05 | ❯ Coordinate the delegated work.                                                │
06 |                                                                                 │
07 | ⏺ Delegated work is in progress.                                                │
08 |                                                                                 │
09 | ✗ child feed dropped                                                            │
10 |   retry when ready                                                              │
11 |                                                                                 │
12 |                                                                                 │
13 |                                                                                 │
14 |                                                                                 │
15 |                                                                                 │
16 |                                                                                 │
17 |                                                                                 │
18 |                                                                                 │
19 |                                                                                 │
20 |
21 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
24 |   child feed dropped|}]

(* A write-capable subagent parks on a permission review when it asks to run a
   high-impact action. A drilled child is session-like, so the review raises the
   SAME answerable dialog the main view would — not a read-only "blocked" glance
   — and the focused strip row advertises the answer its drill performs, so the
   path to answering is discoverable before drilling in. *)
let review_blocked_id = Session.Id.of_string "session-thread-review"

let review_blocked_documents project =
  let cwd = Lpath.Abs.of_string_exn (Project.root project) in
  let created_at = Session.Time.of_unix_ms 1_000_000L in
  let parent_turn_id = Session.Turn.Id.of_string "turn-review-parent" in
  let parent_turn =
    Session.Turn.make ~id:parent_turn_id ~origin:Session.Turn.Origin.User
      ~input:(Session.Turn.Input.user_text "Coordinate the delegated work.")
      ~max_steps:16 ~contract ()
  in
  let edge =
    Session.Delegation.make ~child:review_blocked_id ~source_turn:parent_turn_id
      ~source_call:"call-review-child"
      ~task:[ Mentat_llm.Content.text "run the migration" ]
      ()
  in
  let parent =
    Session.create ~id:parent_id ~cwd ~created_at () |> fun session ->
    append session
      (Session.Event.turn_started parent_turn
      :: Session.Event.delegation_recorded edge
      :: completed_events ~turn:parent_turn_id
           ~prompt:"Coordinate the delegated work."
           "Delegated work is in progress.")
  in
  let child_turn_id = Session.Turn.Id.of_string "turn-review-child" in
  let access = Mentat_permission.Access.custom "session.child.high-impact" in
  let review =
    let request = Mentat_permission.Request.of_accesses [ access ] in
    match
      Mentat_permission.Policy.Review.restore request
        [ (access, Mentat_permission.Policy.Review.Unmatched) ]
    with
    | Ok review -> review
    | Error _ -> failwith "invalid review fixture"
  in
  let call =
    Mentat_llm.Tool.Call.make ~id:"call-review" ~name:"shell"
      ~input:(Json.object' []) ()
  in
  let provider_request =
    Session.Provider_request.Started.make ~turn:child_turn_id
      ~request_digest:(Mentat_digest.string "child review")
  in
  let response =
    Mentat_llm.Response.make ~model ~stop:Mentat_llm.Response.Stop.tool_call
      (Mentat_llm.Message.Assistant.make
         [ Mentat_llm.Message.Assistant.tool_call call ])
  in
  let requested =
    Session.Decision.Requested.make ~turn:child_turn_id ~call
      ~stage:Tool.Stage.Direct (Session.Decision.Request.Permission review)
  in
  let child =
    Session.create ~id:review_blocked_id
      ~delegated_from:
        (Session.Metadata.Delegated_from.make ~parent:parent_id
           ~delegation:(Session.Delegation.id edge))
      ~cwd ~created_at ()
    |> fun session ->
    append session
      [
        Session.Event.turn_started
          (Session.Turn.make ~id:child_turn_id ~origin:Session.Turn.Origin.User
             ~input:(Session.Turn.Input.user_text "Run the migration.")
             ~max_steps:16 ~contract ());
        Session.Event.provider_requested provider_request;
        Session.Event.provider_settled
          (Session.Provider_request.Settled.responded
             ~id:(Session.Provider_request.Started.id provider_request)
             response);
        Session.Event.decision_requested requested;
      ]
  in
  (parent, [ child ])

let%expect_test
    "a drilled child raises its permission review as an answerable dialog" =
  Tui.run ~name:"threads-review-drill" ~size:(120, 24) ~session:parent_id
    ~sessions:(fun project ->
      let parent, _ = review_blocked_documents project in
      [ parent ])
    ~child_sessions:(fun project ->
      let _, children = review_blocked_documents project in
      children)
  @@ fun t ->
  Tui.settle t;
  Tui.drain_child t review_blocked_id;
  Tui.settle t;
  (* Focusing the blocked child advertises the answer its drill performs, so the
     path off the "blocked" row is discoverable without drilling first. *)
  Tui.keys t Key.tab;
  Tui.keys t Key.down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |                                                                                 │ agents · 1 blocked
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium                     │   ◯ main
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threads-revib7fb3525               │ ❯ ⋯ run the migration  enter to answer
04 |                                                                                 │
05 | ❯ Coordinate the delegated work.                                                │
06 |                                                                                 │
07 | ⏺ Delegated work is in progress.                                                │
08 |                                                                                 │
09 |                                                                                 │
10 |                                                                                 │
11 |                                                                                 │
12 |                                                                                 │
13 |                                                                                 │
14 |                                                                                 │
15 |                                                                                 │
16 |                                                                                 │
17 |                                                                                 │
18 |                                                                                 │
19 |                                                                                 │
20 |
21 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
22 | ❯ message mentat
23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · ~/mentat-tui-threads-revib7fb3525 · openai/gpt-5.5 · ! full access        ? for shortcuts|}];
  (* Drilling in raises the real permission dialog wired to answer the child,
     not a read-only glance of the "blocked" state. *)
  Tui.enter t;
  Tui.settle t;
  Tui.drain_child ~observation:Tui.Child_observation.Drill t review_blocked_id;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |                                                                                 │ agents · 1 blocked
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium                     │   ◯ main
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-threads-revib7fb3525               │   ⋯ run the migration          blocked
04 |                                                                                 │
05 | ❯ Coordinate the delegated work.                                                │
06 |                                                                                 │
07 | ⏺ Delegated work is in progress.                                                │
08 |                                                                                 │
09 |                                                                                 │
10 |                                                                                 │
11 |                                                                                 │
12 |                                                                                 │
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    permission
15 |
16 |   Allow session.child.high-impact?
17 |   access · custom session.child.high-impact
18 | ❯ 1. Yes, allow once
19 |   2. Yes, allow the exact session.child.high-impact access for this conversation
20 |   3. No, deny
21 |   4. No, deny and tell the model what to do differently
22 |
23 |
24 |   1-4 select · ↵ choose · ^O more|}]

[%%run_tests "mentat.tui.threads"]
