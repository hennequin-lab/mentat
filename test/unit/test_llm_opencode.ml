(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap
open Test_support
module Llm = Mentat_llm
module Opencode = Mentat_llm_opencode
module Json = Jsont.Json

let expect_stream_ok msg = function
  | Ok value -> value
  | Error (events, error) ->
      ignore events;
      failf "%s: %a" msg Llm.Error.pp error

let expect_stream_error msg = function
  | Ok value ->
      ignore value;
      failf "%s: expected stream error" msg
  | Error value -> value

let json_string json =
  match Jsont_bytesrw.encode_string Jsont.json json with
  | Ok text -> text
  | Error message -> failf "JSON encode failed: %s" message

let json_of_string text =
  match Jsont_bytesrw.decode_string Jsont.json text with
  | Ok json -> json
  | Error message -> failf "JSON decode failed: %s" message

let object_field name = function
  | Jsont.Object (fields, _) -> Option.map snd (Json.find_mem name fields)
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Array _ ->
      None

let field msg name json =
  match object_field name json with
  | Some value -> value
  | None -> failf "%s: missing JSON field %s" msg name

let string_field msg name json =
  match field msg name json with
  | Jsont.String (value, _) -> value
  | value ->
      failf "%s: expected string field %s, got %s" msg name (json_string value)

let request_line request = request.Llm_test_server.request_line
let request_body request = json_of_string request.Llm_test_server.body

let only_request = function
  | [ request ] -> request
  | requests -> failf "expected one request, got %d" (List.length requests)

let equal_error_kind msg kind error =
  equal string ~msg (Llm.Error.label kind)
    (Llm.Error.label (Llm.Error.kind error))

let http_response ?(headers = []) ?(content_type = "application/json") status
    body =
  Llm_test_server.response_head ~headers ~content_type status
    ~content_length:(String.length body)
  ^ body

let sse_response body = http_response ~content_type:"text/event-stream" 200 body

let sse_events chunks =
  String.concat ""
    (List.map (fun chunk -> "data: " ^ json_string chunk ^ "\n\n") chunks)
  ^ "data: [DONE]\n\n"

let delta_chunk ?id ?model ?finish_reason delta =
  let choice =
    [ ("delta", json_object delta) ]
    @ Option.fold ~none:[]
        ~some:(fun reason -> [ ("finish_reason", Json.string reason) ])
        finish_reason
  in
  json_object
    (Option.fold ~none:[] ~some:(fun id -> [ ("id", Json.string id) ]) id
    @ Option.fold ~none:[]
        ~some:(fun model -> [ ("model", Json.string model) ])
        model
    @ [ ("choices", Json.list [ json_object choice ]) ])

let text_stream text =
  sse_response
    (sse_events
       [ delta_chunk ~finish_reason:"stop" [ ("content", Json.string text) ] ])

let user_transcript text =
  Llm.Transcript.of_list_exn [ Llm.Message.user_text text ]

let request ?model () =
  let model = Option.value model ~default:(Opencode.chat_model "kimi-k3") in
  Llm.Request.make_exn ~model (user_transcript "hello")

let run_stream ?(credential = Opencode.Credential.api_key "opencode-key")
    ?config port request =
  let config =
    Option.value config
      ~default:
        (Opencode.Config.make
           ~base_url:("http://127.0.0.1:" ^ string_of_int port ^ "/")
           ~max_retries:0 ())
  in
  Eio_main.run @@ fun env ->
  let client = Opencode.client ~env ~config ~credential () in
  let events = ref [] in
  let observe event = events := event :: !events in
  match Llm.Client.response ~on_event:observe client request with
  | Ok response -> Ok (List.rev !events, response)
  | Error error -> Error (List.rev !events, error)

let text_deltas events =
  List.filter_map
    (function Llm.Event.Text_delta text -> Some text | _ -> None)
    events

let retry_events events =
  List.filter_map
    (function Llm.Event.Retry retry -> Some retry | _ -> None)
    events

let model_config_and_credentials () =
  let model = Opencode.chat_model "kimi-k3" in
  equal string ~msg:"model provider" "opencode-go"
    (Llm.Provider.id (Llm.Model.provider model));
  equal string ~msg:"model api" "chat-completions"
    (Llm.Model.Api.id (Llm.Model.api model));
  equal string ~msg:"model id" "kimi-k3" (Llm.Model.id model);
  expect_invalid_arg "model id cannot be empty" (fun () ->
      ignore (Opencode.chat_model ""));
  equal string ~msg:"default base_url" "https://opencode.ai/zen/go"
    (Opencode.Config.base_url Opencode.Config.default);
  equal string ~msg:"trailing slash normalized"
    (Opencode.Config.base_url
       (Opencode.Config.make ~base_url:"https://gateway.example.test" ()))
    (Opencode.Config.base_url
       (Opencode.Config.make ~base_url:"https://gateway.example.test/" ()));
  equal string ~msg:"base_url trimmed" "https://gateway.example.test"
    (Opencode.Config.base_url
       (Opencode.Config.make ~base_url:"https://gateway.example.test///" ()));
  check "default timeout"
    (Float.equal (Opencode.Config.timeout_s Opencode.Config.default) 1800.);
  expect_invalid_arg "base_url cannot be empty" (fun () ->
      ignore (Opencode.Config.make ~base_url:"" ()));
  expect_invalid_arg "base_url cannot be only slashes" (fun () ->
      ignore (Opencode.Config.make ~base_url:"///" ()));
  expect_invalid_arg "base_url cannot contain newline" (fun () ->
      ignore (Opencode.Config.make ~base_url:"https://x\nbad" ()));
  expect_invalid_arg "timeout must be positive" (fun () ->
      ignore (Opencode.Config.make ~timeout_s:0. ()));
  expect_invalid_arg "retry budget cannot be negative" (fun () ->
      ignore (Opencode.Config.make ~max_retries:(-1) ()));
  ignore (Opencode.Credential.api_key "opencode-key" : Opencode.Credential.t);
  ignore (Opencode.Credential.bearer "session-token" : Opencode.Credential.t);
  expect_invalid_arg "api key cannot be empty" (fun () ->
      ignore (Opencode.Credential.api_key ""));
  expect_invalid_arg "api key cannot contain newline" (fun () ->
      ignore (Opencode.Credential.api_key "key\nbad"));
  expect_invalid_arg "bearer cannot be empty" (fun () ->
      ignore (Opencode.Credential.bearer ""));
  expect_invalid_arg "bearer cannot contain newline" (fun () ->
      ignore (Opencode.Credential.bearer "token\rbad"))

let completed_stream_decodes_events_and_response () =
  let result, requests =
    Llm_test_server.with_server ~name:"opencode-completed-stream"
      (fun _index _request ->
        Llm_test_server.Reply
          (sse_response
             (sse_events
                [
                  delta_chunk ~id:"chatcmpl-1" ~model:"kimi-k3"
                    [ ("content", Json.string "Hel") ];
                  delta_chunk ~finish_reason:"stop"
                    [ ("content", Json.string "lo") ];
                ])))
      (fun port -> run_stream port (request ()))
  in
  let events, response = expect_stream_ok "completed stream" result in
  let recorded = only_request requests in
  equal string ~msg:"method and path" "POST /v1/chat/completions HTTP/1.1"
    (request_line recorded);
  equal (option string) ~msg:"authorization" (Some "Bearer opencode-key")
    (Llm_test_server.header recorded "authorization");
  equal string ~msg:"model sent verbatim" "kimi-k3"
    (string_field "body" "model" (request_body recorded));
  equal (list string) ~msg:"text deltas" [ "Hel"; "lo" ] (text_deltas events);
  equal string ~msg:"response text" "Hello" (Llm.Response.text response);
  equal (option string) ~msg:"response id" (Some "chatcmpl-1")
    (Llm.Response.response_id response);
  equal (option string) ~msg:"response model" (Some "kimi-k3")
    (Llm.Response.response_model response)

let refused_without_transport name model =
  let result, requests =
    Llm_test_server.with_server ~name
      (fun _index _request -> failf "%s: transport should not be used" name)
      (fun port -> run_stream port (request ~model ()))
  in
  let events, error = expect_stream_error name result in
  equal int ~msg:(name ^ " events") 0 (List.length events);
  equal_error_kind name Llm.Error.Invalid_request error;
  equal int ~msg:(name ^ " requests") 0 (List.length requests)

let foreign_api_models_are_refused () =
  refused_without_transport "opencode-foreign-api"
    (Llm.Model.make ~provider:Opencode.provider
       ~api:(Llm.Model.Api.make "messages")
       ~id:"minimax-m3")

let foreign_provider_models_are_refused () =
  refused_without_transport "opencode-foreign-provider"
    (Llm.Model.make
       ~provider:(Llm.Provider.make "ollama")
       ~api:(Llm.Model.api (Opencode.chat_model "kimi-k3"))
       ~id:"qwen3-coder:30b")

let transient_http_failures_are_retried () =
  let result, requests =
    Llm_test_server.with_server ~name:"opencode-transient-retry"
      (fun index _request ->
        if index = 0 then
          Llm_test_server.Reply
            (http_response
               ~headers:[ ("retry-after", "0") ]
               429 {|{"error":{"message":"transient"}}|})
        else Llm_test_server.Reply (text_stream "ok"))
      (fun port ->
        let config =
          Opencode.Config.make
            ~base_url:("http://127.0.0.1:" ^ string_of_int port)
            ~max_retries:1 ()
        in
        run_stream ~config port (request ()))
  in
  let events, response = expect_stream_ok "retry wiring" result in
  equal string ~msg:"recovered text" "ok" (Llm.Response.text response);
  equal int ~msg:"retried once then recovered" 2 (List.length requests);
  match retry_events events with
  | [ retry ] ->
      equal int ~msg:"retry attempt" 1 (Llm.Event.Retry.attempt retry);
      equal int ~msg:"capacity limit" 10 (Llm.Event.Retry.limit retry);
      equal string ~msg:"retry reason" "rate limited"
        (Llm.Event.Retry.reason retry)
  | announced ->
      failf "expected one retry announcement, got %d" (List.length announced)

let stream_faults_are_retried () =
  let result, requests =
    Llm_test_server.with_server ~name:"opencode-stream-retry"
      (fun index _request ->
        if index = 0 then Llm_test_server.Reply (sse_response "")
        else Llm_test_server.Reply (text_stream "recovered"))
      (fun port ->
        let config =
          Opencode.Config.make
            ~base_url:("http://127.0.0.1:" ^ string_of_int port)
            ()
        in
        run_stream ~config port (request ()))
  in
  let events, response = expect_stream_ok "stream retry" result in
  equal int ~msg:"stream fault retried once" 2 (List.length requests);
  equal string ~msg:"recovered text" "recovered" (Llm.Response.text response);
  match retry_events events with
  | [ retry ] ->
      equal int ~msg:"retry attempt" 1 (Llm.Event.Retry.attempt retry);
      equal int ~msg:"default stream retry limit" 5
        (Llm.Event.Retry.limit retry);
      equal string ~msg:"retry reason" "stream interrupted"
        (Llm.Event.Retry.reason retry)
  | announced ->
      failf "expected one stream retry announcement, got %d"
        (List.length announced)

let disabled_retries_surface_the_first_failure () =
  let result, requests =
    Llm_test_server.with_server ~name:"opencode-disabled-retries"
      (fun _index _request -> Llm_test_server.Reply (sse_response ""))
      (fun port ->
        let config =
          Opencode.Config.make
            ~base_url:("http://127.0.0.1:" ^ string_of_int port)
            ~max_retries:0 ()
        in
        run_stream ~config port (request ()))
  in
  let events, error = expect_stream_error "disabled retries" result in
  equal int ~msg:"no retry announcements" 0 (List.length (retry_events events));
  equal int ~msg:"single request" 1 (List.length requests);
  equal_error_kind "disabled retries kind" Llm.Error.Malformed_stream error

let () =
  run "mentat.llm.opencode"
    [
      test "model, config, and credentials" model_config_and_credentials;
      test "completed stream decodes events and response"
        completed_stream_decodes_events_and_response;
      test "foreign-api models are refused" foreign_api_models_are_refused;
      test "foreign-provider models are refused"
        foreign_provider_models_are_refused;
      test "transient HTTP failures are retried"
        transient_http_failures_are_retried;
      test "stream faults are retried" stream_faults_are_retried;
      test "disabled retries surface the first failure"
        disabled_retries_surface_the_first_failure;
    ]
