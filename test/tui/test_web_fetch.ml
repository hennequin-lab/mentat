(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
module Json = Jsont.Json
module Llm = Mentat_llm
module Session = Mentat_session
module Tool = Mentat_tool
module Fetch = Mentat_tools_output.Web.Fetch

type step = { call : Llm.Tool.Call.t; result : Tool.Output.t Tool.Result.t }

let provider = Llm.Provider.make "openai"
let api = Llm.Model.Api.make "responses"
let model = Llm.Model.make ~provider ~api ~id:"gpt-5.5"

let json_object fields =
  Json.object'
    (List.map (fun (name, value) -> Json.mem (Json.name name) value) fields)

let call id url =
  Llm.Tool.Call.make ~id ~name:"web_fetch"
    ~input:(json_object [ ("url", Json.string url) ])
    ()

let fetch_output ~disposition ~status ~bytes ~text =
  Fetch.make ~disposition ~status ~bytes
  |> Mentat_tools_output.Codec.encode Fetch.jsont ~text

let completed call output = { call; result = Tool.Result.completed ~output () }

let web_steps () =
  let fetched_call = call "fetch-ok" "https://example.com/guide" in
  let redirected_call = call "fetch-redirect" "https://example.com/old" in
  let error_call = call "fetch-error" "https://example.com/unavailable" in
  let fallback_call = call "fetch-fallback" "https://example.com/future" in
  let fetched =
    fetch_output ~disposition:Fetch.Fetched ~status:200 ~bytes:1
      ~text:"Fetched https://example.com/guide"
  in
  let redirected =
    fetch_output ~disposition:Fetch.Redirected ~status:307 ~bytes:0
      ~text:"Redirect 307 to https://docs.example.com/guide"
  in
  let http_error =
    fetch_output ~disposition:Fetch.Http_error ~status:503 ~bytes:19
      ~text:"HTTP response from https://example.com/unavailable"
  in
  let unsupported_projection =
    json_object
      [
        ("version", Json.int 2);
        ("disposition", Json.string "fetched");
        ("status", Json.int 200);
        ("bytes", Json.int 8);
      ]
  in
  let fallback =
    Tool.Output.make
      ~text:"Authoritative fallback for an unsupported durable projection."
      ~json:unsupported_projection ~truncated:true ()
  in
  [
    completed fetched_call fetched;
    completed redirected_call redirected;
    {
      call = error_call;
      result =
        Tool.Result.failed ~output:http_error `Failed
          "web server returned HTTP status 503";
    };
    completed fallback_call fallback;
  ]

let harness_tools steps =
  List.map
    (fun step -> Tui.Turn_script.tool ~call:step.call ~result:step.result)
    steps

let submit t prompt =
  Tui.settle t;
  Tui.keys t prompt;
  Tui.enter t;
  Tui.settle t

let contract =
  Session.Contract.make ~mode:Session.Contract.Mode.Build ~model
    ~declarations:[] ~policy:Mentat_permission.Policy.default
    ~review:Mentat_permission.Review_behavior.Enforce
    ~sandbox:(Mentat_sandbox.identity Mentat_sandbox.direct)
    ()

let assistant_calls steps =
  steps
  |> List.map (fun step -> Llm.Message.Assistant.tool_call step.call)
  |> Llm.Message.Assistant.make

let provider_claim ~turn seed =
  Session.Provider_request.Started.make ~turn
    ~request_digest:(Mentat_digest.string seed)

let returned_event ~turn step =
  let started =
    Session.Tool_claim.Started.make ~turn ~stage:Tool.Stage.Direct
      ~call:step.call
      ~input:(Llm.Tool.Call.input step.call)
      ~requests:[]
  in
  let id = Session.Tool_claim.Started.id started in
  [
    Session.Event.tool_claimed started;
    Session.Event.tool_settled
      (Session.Tool_claim.Settled.returned ~id step.result);
  ]

let durable_roundtrip session =
  match Json.encode Session.jsont session with
  | Error message -> failwith ("session encode failed: " ^ message)
  | Ok json -> (
      match Json.decode Session.jsont json with
      | Ok session -> session
      | Error message -> failwith ("session decode failed: " ^ message))

let replay_session project steps =
  let turn_id = Session.Turn.Id.of_string "turn-web-fetch" in
  let turn =
    Session.Turn.make ~id:turn_id ~origin:Session.Turn.Origin.User
      ~input:(Session.Turn.Input.user_text "inspect the web outcomes")
      ~max_steps:100 ~contract ()
  in
  let request = provider_claim ~turn:turn_id "web-fetch-request" in
  let requested =
    Llm.Response.make ~model ~stop:Llm.Response.Stop.tool_call
      (assistant_calls steps)
  in
  let completion = provider_claim ~turn:turn_id "web-fetch-completion" in
  let completed =
    Llm.Response.make ~model
      (Llm.Message.Assistant.text "All four web outcomes are recorded.")
  in
  let events =
    [
      Session.Event.turn_started turn;
      Session.Event.provider_requested request;
      Session.Event.provider_settled
        (Session.Provider_request.Settled.responded
           ~id:(Session.Provider_request.Started.id request)
           requested);
    ]
    @ List.concat_map (returned_event ~turn:turn_id) steps
    @ [
        Session.Event.provider_requested completion;
        Session.Event.provider_settled
          (Session.Provider_request.Settled.responded
             ~id:(Session.Provider_request.Started.id completion)
             completed);
        Session.Event.turn_finished ~turn:turn_id Session.Turn.Outcome.completed;
      ]
  in
  let created_at = Session.Time.of_unix_ms 1_000_000L in
  let metadata =
    Session.Metadata.make ~title:"Web fetch outcomes"
      ~cwd:(Lpath.Abs.of_string_exn (Project.root project))
      ~created_at ~updated_at:created_at ()
  in
  match
    Session.make
      ~id:(Session.Id.of_string "session-web-fetch")
      ~metadata ~events
  with
  | Ok session -> durable_roundtrip session
  | Error error -> failwith (Session.Error.message error)

(* These are complete terminal frames. The live and replay cases intentionally
   contain the same four durable outputs: both must use the summary facts when
   valid and the same short unavailable-details state when they are not. *)

let%expect_test
    "live web fetch results render dispositions and unavailable details" =
  let prompt = "inspect the web outcomes" in
  let tools = harness_tools (web_steps ()) in
  let turn =
    Tui.Turn_script.complete ~prompt ~tools
      "All four web outcomes are recorded."
  in
  Tui.run ~home:(fun _ -> None) ~name:"w" ~size:(92, 30) ~turns:[ turn ]
  @@ fun t ->
  submit t prompt;
  Tui.finish_turn t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ inspect the web outcomes
06 |
07 | ⏺ Fetch(https://example.com/guide)
08 |   ⎿  Fetched 1 byte (HTTP 200)
09 |
10 | ⏺ Fetch(https://example.com/old)
11 |   ⎿  Redirected (HTTP 307)
12 |
13 | ⏺ Fetch(https://example.com/unavailable)
14 |   ⎿  web server returned HTTP status 503
15 |
16 |      HTTP error 503 (19 bytes)
17 |
18 | ⏺ Fetch(https://example.com/future)
19 |   ⎿  details unavailable · output truncated
20 |
21 | ⏺ All four web outcomes are recorded.
22 |
23 |
24 |
25 |
26 |
27 | ────────────────────────────────────────────────────────────────────────────────────────────
28 | ❯ message mentat
29 | ────────────────────────────────────────────────────────────────────────────────────────────
30 |   ! not logged in · /login · /private/tmp/menta… · openai/gpt-5… · ! full acce… ? for sho…|}]

let%expect_test
    "replayed web fetch results preserve summaries and unavailable details" =
  let steps = web_steps () in
  Tui.run
    ~home:(fun _ -> None)
    ~name:"w" ~size:(92, 30)
    ~sessions:(fun project -> [ replay_session project steps ])
  @@ fun t ->
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ inspect the web outcomes
06 |
07 | ⏺ Fetch(https://example.com/guide)
08 |   ⎿  Fetched 1 byte (HTTP 200)
09 |
10 | ⏺ Fetch(https://example.com/old)
11 |   ⎿  Redirected (HTTP 307)
12 |
13 | ⏺ Fetch(https://example.com/unavailable)
14 |   ⎿  web server returned HTTP status 503
15 |
16 |      HTTP error 503 (19 bytes)
17 |
18 | ⏺ Fetch(https://example.com/future)
19 |   ⎿  details unavailable · output truncated
20 |
21 | ⏺ All four web outcomes are recorded.
22 |
23 |
24 |
25 |
26 |
27 | ────────────────────────────────────────────────────────────────────────────────────────────
28 | ❯ message mentat
29 | ────────────────────────────────────────────────────────────────────────────────────────────
30 |   ! not logged in · /login · /private/tmp/menta… · openai/gpt-5… · ! full acce… ? for sho…|}]

[%%run_tests "mentat.tui.web_fetch"]
