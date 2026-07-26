(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap
open Test_support
module Llm = Mentat_llm
module Anthropic = Mentat_llm_anthropic
module Json = Jsont.Json

type recorded_request = {
  request_line : string;
  headers : (string * string) list;
  body : string;
}

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

let equal_json msg expected actual = check msg (Json.equal expected actual)

let object_field name = function
  | Jsont.Object (fields, _) -> Option.map snd (Json.find_mem name fields)
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Array _ ->
      None

let field msg name json =
  match object_field name json with
  | Some value -> value
  | None -> failf "%s: missing JSON field %s" msg name

let has_field name json = Option.is_some (object_field name json)

(* Whether [name] appears as a member anywhere in [json], however deeply
   nested. Used to hold a retired wire field to zero occurrences rather than to
   the one place it used to sit. *)
let rec mentions_field name = function
  | Jsont.Object (members, _) ->
      List.exists
        (fun (member, value) ->
          String.equal (fst member) name || mentions_field name value)
        members
  | Jsont.Array (items, _) -> List.exists (mentions_field name) items
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _ -> false

let string_field msg name json =
  match field msg name json with
  | Jsont.String (value, _) -> value
  | value ->
      failf "%s: expected string field %s, got %s" msg name (json_string value)

let bool_field msg name json =
  match field msg name json with
  | Jsont.Bool (value, _) -> value
  | value ->
      failf "%s: expected bool field %s, got %s" msg name (json_string value)

let int_field msg name json =
  match field msg name json with
  | Jsont.Number (value, _) when Float.is_integer value -> int_of_float value
  | value ->
      failf "%s: expected int field %s, got %s" msg name (json_string value)

let number_field msg name json =
  match field msg name json with
  | Jsont.Number (value, _) -> value
  | value ->
      failf "%s: expected number field %s, got %s" msg name (json_string value)

let list_field msg name json =
  match field msg name json with
  | Jsont.Array (items, _) -> items
  | value ->
      failf "%s: expected array field %s, got %s" msg name (json_string value)

let header request name =
  let name = String.lowercase_ascii name in
  List.find_map
    (fun (key, value) ->
      if String.equal (String.lowercase_ascii key) name then Some value
      else None)
    request.headers

let request_body request = json_of_string request.body

let only_request = function
  | [ request ] -> request
  | requests -> failf "expected one request, got %d" (List.length requests)

let equal_error_kind msg kind error =
  equal string ~msg (Llm.Error.label kind)
    (Llm.Error.label (Llm.Error.kind error))

let equal_error_phase msg expected error =
  let label = function
    | Llm.Error.Startup -> "startup"
    | Llm.Error.Stream -> "stream"
  in
  equal string ~msg (label expected) (label (Llm.Error.phase error))

let equal_stop msg expected response =
  match Llm.Response.stop response with
  | None -> failf "%s: expected stop" msg
  | Some stop ->
      equal string ~msg
        (Llm.Response.Stop.label expected)
        (Llm.Response.Stop.label stop)

let rec waitpid_nointr flags pid =
  match Unix.waitpid flags pid with
  | result -> result
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_nointr flags pid

let strip_cr line =
  let len = String.length line in
  if len > 0 && Char.equal line.[len - 1] '\r' then
    String.take_first (len - 1) line
  else line

let split_header line =
  match String.split_first ~sep:":" line with
  | None -> (String.lowercase_ascii line, "")
  | Some (name, value) ->
      let name = String.lowercase_ascii name in
      let value = String.trim value in
      (name, value)

let read_http_request fd =
  let ic = Unix.in_channel_of_descr fd in
  let request_line = input_line ic |> strip_cr in
  let rec read_headers acc =
    let line = input_line ic |> strip_cr in
    if String.is_empty line then List.rev acc
    else read_headers (split_header line :: acc)
  in
  let headers = read_headers [] in
  let content_length =
    match header { request_line; headers; body = "" } "content-length" with
    | None -> 0
    | Some value -> Option.value (int_of_string_opt value) ~default:0
  in
  let body = really_input_string ic content_length in
  { request_line; headers; body }

let http_response ?(headers = []) ?(content_type = "application/json") status
    body =
  let reason = if status = 200 then "OK" else "Status" in
  let headers =
    ("Content-Type", content_type)
    :: ("Content-Length", string_of_int (String.length body))
    :: ("Connection", "close") :: headers
  in
  let header_text =
    headers
    |> List.map (fun (name, value) -> name ^ ": " ^ value ^ "\r\n")
    |> String.concat ""
  in
  Printf.sprintf "HTTP/1.1 %d %s\r\n%s\r\n%s" status reason header_text body

let read_recorded_requests path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let rec loop acc =
        match Marshal.from_channel ic with
        | request -> loop (request :: acc)
        | exception End_of_file -> List.rev acc
      in
      loop [])

let with_anthropic_server respond f =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 8;
  let port =
    match Unix.getsockname socket with
    | Unix.ADDR_INET (address, port) ->
        ignore address;
        port
    | Unix.ADDR_UNIX path ->
        failf "expected inet socket, got unix socket %s" path
  in
  let requests_path = Filename.temp_file "mentat-anthropic-requests" ".bin" in
  match Unix.fork () with
  | 0 -> (
      match
        let oc = open_out_bin requests_path in
        Fun.protect
          ~finally:(fun () -> close_out_noerr oc)
          (fun () ->
            let rec serve index =
              let client, address = Unix.accept socket in
              ignore address;
              Fun.protect
                ~finally:(fun () -> Unix.close client)
                (fun () ->
                  let request = read_http_request client in
                  Marshal.to_channel oc request [];
                  flush oc;
                  let response = respond index request in
                  let bytes = Bytes.of_string response in
                  ignore (Unix.write client bytes 0 (Bytes.length bytes)));
              serve (index + 1)
            in
            serve 0)
      with
      | () -> exit 0
      | exception exn ->
          prerr_endline (Printexc.to_string exn);
          exit 2)
  | pid ->
      let cleanup () =
        Unix.close socket;
        match waitpid_nointr [ Unix.WNOHANG ] pid with
        | 0, _ ->
            (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
            ignore (waitpid_nointr [] pid)
        | _ -> ()
      in
      Fun.protect
        ~finally:(fun () -> Sys.remove requests_path)
        (fun () ->
          let value = Fun.protect ~finally:cleanup (fun () -> f port) in
          (value, read_recorded_requests requests_path))

let sse_event name data =
  "event: " ^ name ^ "\n" ^ "data: " ^ json_string data ^ "\n\n"

let sse_response events =
  http_response ~content_type:"text/event-stream" 200 (String.concat "" events)

let usage ?(input = 0) ?(output = 0) ?(cache_read = 0) ?(cache_write = 0) () =
  json_object
    [
      ("input_tokens", Json.int input);
      ("output_tokens", Json.int output);
      ("cache_read_input_tokens", Json.int cache_read);
      ("cache_creation_input_tokens", Json.int cache_write);
    ]

let message_start ?usage () =
  let message_fields =
    [
      ("id", Json.string "msg_1");
      ("type", Json.string "message");
      ("role", Json.string "assistant");
      ("model", Json.string "claude-test");
      ("content", Json.list []);
    ]
  in
  let message_fields =
    match usage with
    | None -> message_fields
    | Some usage -> ("usage", usage) :: message_fields
  in
  sse_event "message_start"
    (json_object
       [
         ("type", Json.string "message_start");
         ("message", json_object message_fields);
       ])

let content_block_start index block =
  sse_event "content_block_start"
    (json_object
       [
         ("type", Json.string "content_block_start");
         ("index", Json.int index);
         ("content_block", block);
       ])

let content_block_delta index delta =
  sse_event "content_block_delta"
    (json_object
       [
         ("type", Json.string "content_block_delta");
         ("index", Json.int index);
         ("delta", delta);
       ])

let content_block_stop index =
  sse_event "content_block_stop"
    (json_object
       [ ("type", Json.string "content_block_stop"); ("index", Json.int index) ])

let text_delta text =
  content_block_delta 0
    (json_object
       [ ("type", Json.string "text_delta"); ("text", Json.string text) ])

let thinking_delta text =
  content_block_delta 2
    (json_object
       [
         ("type", Json.string "thinking_delta"); ("thinking", Json.string text);
       ])

let signature_delta index signature =
  content_block_delta index
    (json_object
       [
         ("type", Json.string "signature_delta");
         ("signature", Json.string signature);
       ])

let input_json_delta index text =
  content_block_delta index
    (json_object
       [
         ("type", Json.string "input_json_delta");
         ("partial_json", Json.string text);
       ])

let message_delta ?usage stop_reason =
  let fields =
    [
      ("type", Json.string "message_delta");
      ("delta", json_object [ ("stop_reason", Json.string stop_reason) ]);
    ]
  in
  let fields =
    match usage with None -> fields | Some usage -> ("usage", usage) :: fields
  in
  sse_event "message_delta" (json_object fields)

let message_stop =
  sse_event "message_stop"
    (json_object [ ("type", Json.string "message_stop") ])

let text_stream text =
  sse_response
    [
      message_start ();
      content_block_start 0
        (json_object [ ("type", Json.string "text"); ("text", Json.string "") ]);
      text_delta text;
      content_block_stop 0;
      message_delta "end_turn";
      message_stop;
    ]

let schema =
  json_object
    [
      ("type", Json.string "object");
      ( "properties",
        json_object [ ("path", json_object [ ("type", Json.string "string") ]) ]
      );
      ("required", Json.list [ Json.string "path" ]);
      ("additionalProperties", Json.bool false);
    ]

let user_transcript text =
  Llm.Transcript.of_list_exn [ Llm.Message.user_text text ]

let request ?prelude ?tools ?options ?transcript () =
  let transcript = Option.value transcript ~default:(user_transcript "hello") in
  Llm.Request.make_exn
    ~model:(Anthropic.model "claude-test")
    ?prelude ?tools ?options transcript

let run_stream ?(cancelled = fun () -> false) ?config
    ?(credential = Anthropic.Credential.api_key "sk-ant-test") ?on_event port
    request =
  let base_url = "http://127.0.0.1:" ^ string_of_int port in
  Eio_main.run @@ fun env ->
  let config =
    match config with
    | Some config -> config
    | None -> Anthropic.Config.make ~base_url ~max_retries:0 ()
  in
  let client = Anthropic.client ~env ~config ~credential () in
  let events = ref [] in
  let observe event =
    Option.iter (fun f -> f event) on_event;
    events := event :: !events
  in
  match Llm.Client.response ~cancelled ~on_event:observe client request with
  | Ok response -> Ok (List.rev !events, response)
  | Error error -> Error (List.rev !events, error)

let model_config_and_credentials () =
  let model = Anthropic.model "claude-test" in
  check "model provider"
    (Llm.Provider.equal (Llm.Model.provider model) Anthropic.provider);
  check "model api" (Llm.Model.Api.equal (Llm.Model.api model) Anthropic.api);
  equal string ~msg:"model id" "claude-test" (Llm.Model.id model);
  expect_invalid_arg "model id cannot be empty" (fun () ->
      ignore (Anthropic.model ""));
  let config =
    Anthropic.Config.make ~base_url:"https://anthropic.example.test///"
      ~timeout_s:5. ~max_retries:2 ~max_stream_retries:7 ()
  in
  equal (option string) ~msg:"base_url trimmed"
    (Some "https://anthropic.example.test")
    (Anthropic.Config.base_url config);
  check "timeout" (Float.equal (Anthropic.Config.timeout_s config) 5.);
  check "default timeout"
    (Float.equal (Anthropic.Config.timeout_s Anthropic.Config.default) 600.);
  equal (option int) ~msg:"max retries" (Some 2)
    (Anthropic.Config.max_retries config);
  equal (option int) ~msg:"max stream retries" (Some 7)
    (Anthropic.Config.max_stream_retries config);
  equal (option int) ~msg:"max stream retries unset by default" None
    (Anthropic.Config.max_stream_retries Anthropic.Config.default);
  expect_invalid_arg "base_url cannot normalize empty" (fun () ->
      ignore (Anthropic.Config.make ~base_url:"///" ()));
  expect_invalid_arg "base_url cannot contain newline" (fun () ->
      ignore (Anthropic.Config.make ~base_url:"https://x\nbad" ()));
  expect_invalid_arg "timeout must be positive" (fun () ->
      ignore (Anthropic.Config.make ~timeout_s:0. ()));
  expect_invalid_arg "max_retries cannot be negative" (fun () ->
      ignore (Anthropic.Config.make ~max_retries:(-1) ()));
  expect_invalid_arg "max_stream_retries cannot be negative" (fun () ->
      ignore (Anthropic.Config.make ~max_stream_retries:(-1) ()));
  ignore (Anthropic.Credential.api_key "sk-ant-test" : Anthropic.Credential.t);
  ignore (Anthropic.Credential.bearer "token" : Anthropic.Credential.t);
  expect_invalid_arg "api key cannot be empty" (fun () ->
      ignore (Anthropic.Credential.api_key ""));
  expect_invalid_arg "api key cannot contain newline" (fun () ->
      ignore (Anthropic.Credential.api_key "sk\nbad"));
  expect_invalid_arg "bearer cannot be empty" (fun () ->
      ignore (Anthropic.Credential.bearer ""));
  expect_invalid_arg "bearer cannot contain newline" (fun () ->
      ignore (Anthropic.Credential.bearer "token\rbad"))

let maximal_request_encoding () =
  let call =
    Llm.Tool.Call.make ~id:"toolu_1" ~name:"read_file"
      ~input:(json_object [ ("path", Json.string "a.ml") ])
      ()
  in
  let transcript =
    Llm.Transcript.of_list_exn
      [
        Llm.Message.system "system";
        Llm.Message.developer "developer";
        Llm.Message.user
          [
            Llm.Content.text "inspect this";
            Llm.Content.media ~media_type:"image/png" (`Base64 "abcd");
          ];
        Llm.Message.assistant
          (Llm.Message.Assistant.make
             [
               Llm.Message.Assistant.text_part "checking";
               Llm.Message.Assistant.tool_call call;
             ]);
        Llm.Message.tool_result (Llm.Tool.Result.text call "file contents");
      ]
  in
  let tool =
    Llm.Tool.make ~name:"read_file" ~description:"Read a file."
      ~input_schema:schema ()
  in
  let options =
    Llm.Request.Options.make ~tool_choice:(Llm.Request.Options.Tool "read_file")
      ~max_output_tokens:8192 ~temperature:0.25 ()
  in
  let prelude =
    match
      Llm.Request.Prelude.make
        [
          Llm.Message.system "host system";
          Llm.Message.developer "host developer";
        ]
    with
    | Ok prelude -> prelude
    | Error error -> failf "prelude failed: %a" Llm.Request.Error.pp error
  in
  let result, requests =
    with_anthropic_server
      (fun index request ->
        ignore index;
        ignore request;
        text_stream "ok")
      (fun port ->
        run_stream port
          (request ~prelude ~tools:[ tool ] ~options ~transcript ()))
  in
  ignore (expect_stream_ok "stream" result);
  let request = only_request requests in
  equal string ~msg:"method and path" "POST /messages HTTP/1.1"
    request.request_line;
  equal (option string) ~msg:"api key" (Some "sk-ant-test")
    (header request "x-api-key");
  equal (option string) ~msg:"version" (Some "2023-06-01")
    (header request "anthropic-version");
  let body = request_body request in
  equal string ~msg:"model" "claude-test" (string_field "body" "model" body);
  equal bool ~msg:"stream" true (bool_field "body" "stream" body);
  equal int ~msg:"max tokens" 8192 (int_field "body" "max_tokens" body);
  (* No requested effort leaves thinking to the model, which current Anthropic
     models resolve as adaptive, so sampling parameters stay behind. *)
  check "temperature omitted" (not (has_field "temperature" body));
  let system = list_field "body" "system" body in
  equal int ~msg:"system block count" 4 (List.length system);
  equal string ~msg:"host system text" "host system"
    (string_field "system" "text" (List.nth system 0));
  equal string ~msg:"host developer text" "host developer"
    (string_field "developer" "text" (List.nth system 1));
  equal string ~msg:"system text" "system"
    (string_field "system" "text" (List.nth system 2));
  equal string ~msg:"developer text" "developer"
    (string_field "developer" "text" (List.nth system 3));
  let messages = list_field "body" "messages" body in
  equal int ~msg:"message count" 3 (List.length messages);
  let user_content = list_field "user" "content" (List.nth messages 0) in
  equal string ~msg:"user role" "user"
    (string_field "user" "role" (List.nth messages 0));
  equal string ~msg:"user text" "inspect this"
    (string_field "user text" "text" (List.nth user_content 0));
  let image = field "image" "source" (List.nth user_content 1) in
  equal string ~msg:"image media type" "image/png"
    (string_field "image" "media_type" image);
  equal string ~msg:"image data" "abcd" (string_field "image" "data" image);
  let assistant_content =
    list_field "assistant" "content" (List.nth messages 1)
  in
  equal string ~msg:"assistant role" "assistant"
    (string_field "assistant" "role" (List.nth messages 1));
  equal string ~msg:"assistant text" "checking"
    (string_field "assistant text" "text" (List.nth assistant_content 0));
  equal string ~msg:"tool_use id" "toolu_1"
    (string_field "tool use" "id" (List.nth assistant_content 1));
  equal_json "tool_use input"
    (json_object [ ("path", Json.string "a.ml") ])
    (field "tool use" "input" (List.nth assistant_content 1));
  let tool_result =
    List.hd (list_field "tool result" "content" (List.nth messages 2))
  in
  equal string ~msg:"tool result id" "toolu_1"
    (string_field "tool result" "tool_use_id" tool_result);
  equal string ~msg:"tool result content" "file contents"
    (string_field "tool result" "content" tool_result);
  let tools = list_field "body" "tools" body in
  equal int ~msg:"tool count" 1 (List.length tools);
  equal string ~msg:"tool name" "read_file"
    (string_field "tool" "name" (List.hd tools));
  equal string ~msg:"tool choice type" "tool"
    (string_field "tool_choice" "type" (field "body" "tool_choice" body));
  equal string ~msg:"tool choice name" "read_file"
    (string_field "tool_choice" "name" (field "body" "tool_choice" body));
  check "thinking omitted" (not (has_field "thinking" body));
  check "output_config omitted" (not (has_field "output_config" body))

let headers_and_default_request_encoding () =
  let result, requests =
    with_anthropic_server
      (fun index request ->
        ignore index;
        ignore request;
        text_stream "ok")
      (fun port ->
        run_stream
          ~credential:(Anthropic.Credential.bearer "session-token")
          port (request ()))
  in
  ignore (expect_stream_ok "stream" result);
  let request = only_request requests in
  equal (option string) ~msg:"bearer authorization"
    (Some "Bearer session-token")
    (header request "authorization");
  let body = request_body request in
  equal int ~msg:"default max tokens" 4096 (int_field "body" "max_tokens" body);
  check "system omitted" (not (has_field "system" body));
  check "tools omitted" (not (has_field "tools" body));
  check "tool choice omitted" (not (has_field "tool_choice" body));
  check "thinking omitted" (not (has_field "thinking" body))

(* Every requested effort travels as adaptive thinking with its ceiling in the
   sibling [output_config.effort]; a token budget inside [thinking] is refused
   by current Anthropic models, so it must not appear anywhere in the body.
   [max_output_tokens = 1024] is deliberate: the old budget needed headroom
   above it, and adaptive thinking does not. *)
let reasoning_effort_encoding () =
  let adaptive_case (effort, expected) =
    let options =
      Llm.Request.Options.make ~max_output_tokens:1024 ~reasoning_effort:effort
        ~temperature:0.25 ()
    in
    let check_body body =
      let thinking = field "body" "thinking" body in
      equal string ~msg:(expected ^ " thinking type") "adaptive"
        (string_field "thinking" "type" thinking);
      equal string ~msg:(expected ^ " thinking display") "summarized"
        (string_field "thinking" "display" thinking);
      equal string ~msg:(expected ^ " effort") expected
        (string_field "output_config" "effort"
           (field "body" "output_config" body));
      check
        (expected ^ " omits temperature")
        (not (has_field "temperature" body))
    in
    ("effort " ^ expected, options, check_body)
  in
  let cases =
    let open Llm.Request.Options in
    ( "disabled",
      make ~reasoning_effort:Reasoning_effort.Disabled ~temperature:0.25 (),
      fun body ->
        equal string ~msg:"disabled thinking" "disabled"
          (string_field "thinking" "type" (field "body" "thinking" body));
        check "disabled omits output_config"
          (not (has_field "output_config" body));
        check "disabled keeps temperature"
          (number_field "body" "temperature" body = 0.25) )
    :: List.map adaptive_case
         Reasoning_effort.
           [
             (Minimal, "low");
             (Low, "low");
             (Medium, "medium");
             (High, "high");
             (Extra_high, "xhigh");
             (Max, "max");
           ]
  in
  let results, requests =
    with_anthropic_server
      (fun index request ->
        ignore index;
        ignore request;
        text_stream "ok")
      (fun port ->
        List.map
          (fun (_name, options, _check) ->
            run_stream port (request ~options ()))
          cases)
  in
  List.iter2
    (fun (name, _options, _check) result ->
      ignore (expect_stream_ok name result))
    cases results;
  List.iter2
    (fun (name, _options, check_body) request ->
      let body = request_body request in
      check (name ^ " sends no thinking budget")
        (not (mentions_field "budget_tokens" body));
      check_body body)
    cases requests

let unsupported_requests_do_not_touch_transport () =
  let unsupported_api =
    let model =
      Llm.Model.make ~provider:Anthropic.provider
        ~api:(Llm.Model.Api.make "unknown")
        ~id:"claude-test"
    in
    Llm.Request.make_exn ~model (user_transcript "hello")
  in
  let pdf_media =
    let transcript =
      Llm.Transcript.of_list_exn
        [
          Llm.Message.user
            [ Llm.Content.media ~media_type:"application/pdf" (`Base64 "abcd") ];
        ]
    in
    request ~transcript ()
  in
  let unresolved_reference =
    let reference = Mentat_digest.Content_ref.of_contents "image bytes" in
    let transcript =
      Llm.Transcript.of_list_exn
        [
          Llm.Message.user
            [ Llm.Content.media ~media_type:"image/png" (`Ref reference) ];
        ]
    in
    request ~transcript ()
  in
  let json_schema =
    let options =
      Llm.Request.Options.make
        ~response_format:
          (Llm.Request.Options.Json_schema
             { name = "answer"; schema; strict = true })
        ()
    in
    request ~options ()
  in
  let forced_thinking =
    let tool =
      Llm.Tool.make ~name:"read_file" ~description:"Read a file."
        ~input_schema:schema ()
    in
    let options =
      Llm.Request.Options.make ~tool_choice:Llm.Request.Options.Required
        ~max_output_tokens:2048
        ~reasoning_effort:Llm.Request.Options.Reasoning_effort.High ()
    in
    request ~tools:[ tool ] ~options ()
  in
  List.iter
    (fun (name, kind, request) ->
      let result, requests =
        with_anthropic_server
          (fun index request ->
            ignore index;
            ignore request;
            http_response 500 "{}")
          (fun port -> run_stream port request)
      in
      let events, error = expect_stream_error name result in
      ignore events;
      equal_error_kind name kind error;
      equal int ~msg:(name ^ " request count") 0 (List.length requests))
    [
      ("unsupported api", Llm.Error.Invalid_request, unsupported_api);
      ("pdf media", Llm.Error.Unsupported, pdf_media);
      ( "unresolved content reference",
        Llm.Error.Invalid_request,
        unresolved_reference );
      ("json schema", Llm.Error.Unsupported, json_schema);
      ("forced thinking", Llm.Error.Invalid_request, forced_thinking);
    ]

let http_errors_are_classified () =
  let cases =
    [
      (400, Llm.Error.Invalid_request);
      (401, Llm.Error.Auth);
      (408, Llm.Error.Timeout);
      (413, Llm.Error.Context_overflow);
      (429, Llm.Error.Rate_limited);
      (500, Llm.Error.Provider);
    ]
  in
  List.iter
    (fun (status, kind) ->
      let body =
        {|{"error":{"type":"bad_request_error","message":"provider said no"}}|}
      in
      let result, requests =
        with_anthropic_server
          (fun index request ->
            ignore index;
            ignore request;
            http_response ~headers:[ ("request-id", "req_123") ] status body)
          (fun port -> run_stream port (request ()))
      in
      let events, error =
        expect_stream_error ("status " ^ string_of_int status) result
      in
      ignore events;
      equal_error_kind ("status " ^ string_of_int status) kind error;
      equal (option int) ~msg:"status retained" (Some status)
        (Llm.Error.status error);
      equal (option string) ~msg:"request id retained" (Some "req_123")
        (Llm.Error.request_id error);
      equal (option string) ~msg:"body redacted"
        (Some
           ("<redacted Anthropic error body: "
           ^ string_of_int (String.length body)
           ^ " bytes>"))
        (Llm.Error.redacted_body error);
      equal int ~msg:"no retries in mapping test" 1 (List.length requests))
    cases

let non_json_http_error_body_is_not_a_log_payload () =
  let body = "secret token sk-ant-test" in
  let result, requests =
    with_anthropic_server
      (fun index request ->
        ignore index;
        ignore request;
        http_response ~content_type:"text/plain" 500 body)
      (fun port -> run_stream port (request ()))
  in
  equal int ~msg:"request count" 1 (List.length requests);
  let events, error = expect_stream_error "non-json body" result in
  ignore events;
  equal_error_kind "non-json body" Llm.Error.Provider error;
  check "message does not contain raw body"
    (not (String.includes ~affix:"sk-ant-test" (Llm.Error.message error)));
  check "redacted body does not contain raw body"
    (match Llm.Error.redacted_body error with
    | None -> false
    | Some body -> not (String.includes ~affix:"sk-ant-test" body))

let retry_policy () =
  let result, requests =
    with_anthropic_server
      (fun index request ->
        ignore request;
        if index = 0 then
          http_response
            ~headers:[ ("retry-after", "0") ]
            500 {|{"error":{"message":"retry"}}|}
        else text_stream "ok")
      (fun port ->
        let base_url = "http://127.0.0.1:" ^ string_of_int port in
        let config = Anthropic.Config.make ~base_url ~max_retries:1 () in
        run_stream ~config port (request ()))
  in
  let events, _response = expect_stream_ok "retry" result in
  equal int ~msg:"500 retried" 2 (List.length requests);
  (match
     List.filter_map
       (function Llm.Event.Retry retry -> Some retry | _ -> None)
       events
   with
  | [ retry ] ->
      equal int ~msg:"retry attempt" 1 (Llm.Event.Retry.attempt retry);
      equal string ~msg:"retry reason" "provider error"
        (Llm.Event.Retry.reason retry)
  | announced ->
      failf "expected one retry announcement, got %d" (List.length announced));
  let result, requests =
    with_anthropic_server
      (fun index request ->
        ignore index;
        ignore request;
        http_response 400 {|{"error":{"message":"bad request"}}|})
      (fun port ->
        let base_url = "http://127.0.0.1:" ^ string_of_int port in
        let config = Anthropic.Config.make ~base_url ~max_retries:1 () in
        run_stream ~config port (request ()))
  in
  let events, error = expect_stream_error "400 is not retried" result in
  ignore events;
  equal_error_kind "400 kind" Llm.Error.Invalid_request error;
  equal int ~msg:"400 not retried" 1 (List.length requests)

let deadline_config port ?(max_retries = 0) () =
  Anthropic.Config.make
    ~base_url:("http://127.0.0.1:" ^ string_of_int port)
    ~timeout_s:0.3 ~max_retries ()

let request_deadline_bounds_response_headers () =
  let result, requests =
    Llm_test_server.with_server ~name:"anthropic-stalled-headers"
      (fun _index _request -> Llm_test_server.Hold)
      (fun port ->
        run_stream ~config:(deadline_config port ()) port (request ()))
  in
  let events, error = expect_stream_error "stalled response headers" result in
  equal int ~msg:"no events before response headers" 0 (List.length events);
  equal_error_kind "stalled headers kind" Llm.Error.Timeout error;
  equal_error_phase "stalled headers phase" Llm.Error.Startup error;
  equal int ~msg:"stalled headers request count" 1 (List.length requests)

let request_deadline_bounds_partial_stream () =
  let delta =
    message_start ()
    ^ content_block_start 0
        (json_object [ ("type", Json.string "text"); ("text", Json.string "") ])
    ^ text_delta "first"
  in
  let prefix =
    Llm_test_server.response_head ~content_type:"text/event-stream" 200
      ~content_length:(String.length delta + 128)
    ^ delta
  in
  let result, requests =
    Llm_test_server.with_server ~name:"anthropic-partial-stream"
      (fun _index _request -> Llm_test_server.Hold_after prefix)
      (fun port ->
        run_stream ~config:(deadline_config port ()) port (request ()))
  in
  let events, error = expect_stream_error "partial stream" result in
  equal int ~msg:"one event before partial stream timeout" 1
    (List.length events);
  equal_error_kind "partial stream kind" Llm.Error.Timeout error;
  equal_error_phase "partial stream phase" Llm.Error.Stream error;
  equal int ~msg:"partial stream request count" 1 (List.length requests)

let request_deadline_includes_retry_backoff () =
  let response =
    http_response
      ~headers:[ ("retry-after", "60") ]
      500 {|{"error":{"message":"retry"}}|}
  in
  let result, requests =
    Llm_test_server.with_server ~name:"anthropic-retry-backoff"
      (fun _index _request -> Llm_test_server.Reply response)
      (fun port ->
        run_stream
          ~config:(deadline_config port ~max_retries:1 ())
          port (request ()))
  in
  let events, error = expect_stream_error "retry backoff" result in
  (match events with
  | [ Llm.Event.Retry retry ] ->
      equal string ~msg:"backoff retry reason" "provider error"
        (Llm.Event.Retry.reason retry)
  | events ->
      failf "expected exactly the retry announcement, got %d events"
        (List.length events));
  equal_error_kind "retry backoff kind" Llm.Error.Timeout error;
  equal_error_phase "retry backoff phase" Llm.Error.Startup error;
  equal int ~msg:"deadline prevents second attempt" 1 (List.length requests)

let request_deadline_bounds_partial_error_body () =
  let prefix = Llm_test_server.response_head 500 ~content_length:128 ^ "{" in
  let result, requests =
    Llm_test_server.with_server ~name:"anthropic-partial-error"
      (fun _index _request -> Llm_test_server.Hold_after prefix)
      (fun port ->
        run_stream ~config:(deadline_config port ()) port (request ()))
  in
  let events, error = expect_stream_error "partial error body" result in
  equal int ~msg:"no partial error events" 0 (List.length events);
  equal_error_kind "partial error kind" Llm.Error.Timeout error;
  equal_error_phase "partial error phase" Llm.Error.Startup error;
  equal int ~msg:"partial error request count" 1 (List.length requests)

let completed_stream_decodes_events_and_response () =
  let result, requests =
    with_anthropic_server
      (fun index request ->
        ignore index;
        ignore request;
        sse_response
          [
            message_start ~usage:(usage ~input:7 ~cache_read:2 ()) ();
            content_block_start 0
              (json_object
                 [ ("type", Json.string "text"); ("text", Json.string "") ]);
            text_delta "hel";
            text_delta "lo";
            content_block_stop 0;
            content_block_start 1
              (json_object
                 [
                   ("type", Json.string "tool_use");
                   ("id", Json.string "toolu_1");
                   ("name", Json.string "read_file");
                   ("input", json_object []);
                 ]);
            input_json_delta 1 {|{"path"|};
            input_json_delta 1 {|:"a.ml"}|};
            content_block_stop 1;
            content_block_start 2
              (json_object
                 [
                   ("type", Json.string "thinking"); ("thinking", Json.string "");
                 ]);
            thinking_delta "thought";
            signature_delta 2 "sig_1";
            content_block_stop 2;
            message_delta ~usage:(usage ~output:5 ~cache_write:1 ()) "tool_use";
            message_stop;
          ])
      (fun port -> run_stream port (request ()))
  in
  let events, response = expect_stream_ok "completed stream" result in
  equal int ~msg:"request count" 1 (List.length requests);
  let text_deltas, input_deltas, calls, usage_events, reasoning =
    List.fold_left
      (fun (texts, inputs, calls, usages, reasoning) -> function
        | Llm.Event.Text_delta text ->
            (text :: texts, inputs, calls, usages, reasoning)
        | Llm.Event.Tool_input_delta input ->
            (texts, input :: inputs, calls, usages, reasoning)
        | Llm.Event.Tool_call call ->
            (texts, inputs, call :: calls, usages, reasoning)
        | Llm.Event.Usage usage ->
            (texts, inputs, calls, usage :: usages, reasoning)
        | Llm.Event.Reasoning_summary_delta text ->
            (texts, inputs, calls, usages, text :: reasoning)
        | Llm.Event.Retry _ -> (texts, inputs, calls, usages, reasoning))
      ([], [], [], [], []) events
  in
  equal (list string) ~msg:"text deltas" [ "lo"; "hel" ] text_deltas;
  equal int ~msg:"input delta count" 2 (List.length input_deltas);
  equal int ~msg:"live call count" 1 (List.length calls);
  equal int ~msg:"usage event count" 2 (List.length usage_events);
  equal (list string) ~msg:"reasoning delta" [ "thought" ] reasoning;
  equal string ~msg:"response text" "hello" (Llm.Response.text response);
  let call = List.hd (Llm.Response.tool_calls response) in
  equal string ~msg:"tool call id" "toolu_1" (Llm.Tool.Call.id call);
  equal string ~msg:"tool call name" "read_file" (Llm.Tool.Call.name call);
  equal_json "tool call input"
    (json_object [ ("path", Json.string "a.ml") ])
    (Llm.Tool.Call.input call);
  equal_stop "tool stop" Llm.Response.Stop.tool_call response;
  equal (option string) ~msg:"provider stop" (Some "tool_use")
    (Llm.Response.provider_stop response);
  equal (option string) ~msg:"response id" (Some "msg_1")
    (Llm.Response.response_id response);
  equal (option string) ~msg:"response model" (Some "claude-test")
    (Llm.Response.response_model response);
  equal (list string) ~msg:"reasoning retained" [ "thought" ]
    (Llm.Response.reasoning_summary response);
  let reasonings =
    Llm.Message.Assistant.reasonings (Llm.Response.assistant response)
  in
  equal int ~msg:"durable reasoning count" 1 (List.length reasonings);
  let reasoning = List.hd reasonings in
  equal (option string) ~msg:"durable reasoning text" (Some "thought")
    (Llm.Message.Assistant.Reasoning.text reasoning);
  equal (option string) ~msg:"durable reasoning signature" (Some "sig_1")
    (Llm.Message.Assistant.Reasoning.signature reasoning);
  begin match Llm.Response.usage response with
  | None -> failf "expected usage"
  | Some usage ->
      equal int ~msg:"input" 7 usage.Llm.Usage.input;
      equal int ~msg:"output" 5 usage.Llm.Usage.output;
      equal int ~msg:"cache read" 2 usage.Llm.Usage.cache_read;
      equal int ~msg:"cache write" 1 usage.Llm.Usage.cache_write
  end

let durable_parts_preserve_content_block_order () =
  let result, _requests =
    with_anthropic_server
      (fun index request ->
        ignore index;
        ignore request;
        sse_response
          [
            message_start ();
            content_block_start 0
              (json_object
                 [
                   ("type", Json.string "thinking"); ("thinking", Json.string "");
                 ]);
            content_block_delta 0
              (json_object
                 [
                   ("type", Json.string "thinking_delta");
                   ("thinking", Json.string "thought");
                 ]);
            signature_delta 0 "sig_1";
            content_block_stop 0;
            content_block_start 1
              (json_object
                 [ ("type", Json.string "text"); ("text", Json.string "") ]);
            content_block_delta 1
              (json_object
                 [
                   ("type", Json.string "text_delta");
                   ("text", Json.string "answer");
                 ]);
            content_block_stop 1;
            message_delta "end_turn";
            message_stop;
          ])
      (fun port -> run_stream port (request ()))
  in
  let _events, response = expect_stream_ok "durable part order" result in
  let parts = Llm.Message.Assistant.parts (Llm.Response.assistant response) in
  begin match parts with
  | [
   Llm.Message.Assistant.Reasoning reasoning; Llm.Message.Assistant.Text text;
  ] ->
      equal (option string) ~msg:"reasoning text" (Some "thought")
        (Llm.Message.Assistant.Reasoning.text reasoning);
      equal (option string) ~msg:"reasoning signature" (Some "sig_1")
        (Llm.Message.Assistant.Reasoning.signature reasoning);
      equal string ~msg:"text" "answer" text
  | parts ->
      failf "expected reasoning before text, got %d parts" (List.length parts)
  end

let stream_failures_are_classified () =
  let cases =
    [
      ( "eof",
        sse_response
          [
            message_start ();
            content_block_start 0
              (json_object
                 [ ("type", Json.string "text"); ("text", Json.string "") ]);
            text_delta "x";
          ],
        Llm.Error.Malformed_stream );
      ( "malformed json",
        http_response ~content_type:"text/event-stream" 200
          "event: content_block_delta\ndata: {broken\n\n",
        Llm.Error.Decode );
      ( "provider error",
        sse_response
          [
            sse_event "error"
              (json_object
                 [
                   ("type", Json.string "error");
                   ( "error",
                     json_object
                       [
                         ("type", Json.string "overloaded_error");
                         ("message", Json.string "overloaded");
                       ] );
                 ]);
          ],
        Llm.Error.Rate_limited );
      ( "invalid tool input",
        sse_response
          [
            message_start ();
            content_block_start 1
              (json_object
                 [
                   ("type", Json.string "tool_use");
                   ("id", Json.string "toolu_1");
                   ("name", Json.string "read_file");
                   ("input", json_object []);
                 ]);
            input_json_delta 1 "{broken";
            content_block_stop 1;
          ],
        Llm.Error.Decode );
      ( "tool input before block start",
        sse_response
          [ message_start (); input_json_delta 1 {|{"path":"a.ml"}|} ],
        Llm.Error.Decode );
      ( "text delta before block start",
        sse_response [ message_start (); text_delta "early" ],
        Llm.Error.Decode );
      ( "duplicate block start",
        sse_response
          [
            message_start ();
            content_block_start 0
              (json_object
                 [ ("type", Json.string "text"); ("text", Json.string "") ]);
            content_block_start 0
              (json_object
                 [ ("type", Json.string "text"); ("text", Json.string "") ]);
          ],
        Llm.Error.Decode );
      ( "block stop before start",
        sse_response [ message_start (); content_block_stop 0 ],
        Llm.Error.Decode );
      ( "delta after block stop",
        sse_response
          [
            message_start ();
            content_block_start 0
              (json_object
                 [ ("type", Json.string "text"); ("text", Json.string "") ]);
            content_block_stop 0;
            text_delta "late";
          ],
        Llm.Error.Decode );
    ]
  in
  List.iter
    (fun (name, response, kind) ->
      let result, requests =
        with_anthropic_server
          (fun index request ->
            ignore index;
            ignore request;
            response)
          (fun port -> run_stream port (request ()))
      in
      equal int ~msg:(name ^ " request count") 1 (List.length requests);
      let events, error = expect_stream_error name result in
      ignore events;
      equal_error_kind name kind error;
      equal string ~msg:(name ^ " phase") "stream"
        (match Llm.Error.phase error with
        | Llm.Error.Startup -> "startup"
        | Llm.Error.Stream -> "stream"))
    cases

let retry_events events =
  List.filter_map
    (function Llm.Event.Retry retry -> Some retry | _ -> None)
    events

let stream_error_event ?(type_ = "overloaded_error") message =
  sse_event "error"
    (json_object
       [
         ("type", Json.string "error");
         ( "error",
           json_object
             [ ("type", Json.string type_); ("message", Json.string message) ]
         );
       ])

let output_then_error_stream error =
  sse_response
    [
      message_start ();
      content_block_start 0
        (json_object [ ("type", Json.string "text"); ("text", Json.string "") ]);
      text_delta "partial";
      error;
    ]

(* A mid-stream overload carries a 200 response, so the pre-stream HTTP retry
   never sees it; the provider re-runs the whole request and rides through to
   the recovered stream. The default stream budget is the deeper codex-style 5. *)
let stream_overload_is_retried () =
  let result, requests =
    with_anthropic_server
      (fun index request ->
        ignore request;
        if index = 0 then sse_response [ stream_error_event "overloaded" ]
        else text_stream "recovered")
      (fun port ->
        let base_url = "http://127.0.0.1:" ^ string_of_int port in
        let config = Anthropic.Config.make ~base_url () in
        run_stream ~config port (request ()))
  in
  let events, response = expect_stream_ok "stream retry" result in
  equal int ~msg:"stream error retried once" 2 (List.length requests);
  equal string ~msg:"recovered text" "recovered" (Llm.Response.text response);
  match retry_events events with
  | [ retry ] ->
      equal int ~msg:"retry attempt" 1 (Llm.Event.Retry.attempt retry);
      equal int ~msg:"default stream retry limit" 5
        (Llm.Event.Retry.limit retry);
      equal string ~msg:"retry reason" "rate limited"
        (Llm.Event.Retry.reason retry)
  | announced ->
      failf "expected one stream retry announcement, got %d"
        (List.length announced)

(* max_retries=0 is the caller's blanket "no retries" and silences the deeper
   stream budget too, so a retryable stream fault surfaces on the first request. *)
let stream_retry_is_disabled_by_max_retries_zero () =
  let result, requests =
    with_anthropic_server
      (fun index request ->
        ignore index;
        ignore request;
        sse_response [ stream_error_event "overloaded" ])
      (fun port ->
        let base_url = "http://127.0.0.1:" ^ string_of_int port in
        let config = Anthropic.Config.make ~base_url ~max_retries:0 () in
        run_stream ~config port (request ()))
  in
  let events, error = expect_stream_error "stream retry disabled" result in
  equal int ~msg:"no stream re-run when disabled" 1 (List.length requests);
  equal int ~msg:"no retry announced" 0 (List.length (retry_events events));
  equal_error_kind "disabled stream error kind" Llm.Error.Rate_limited error

(* A retryable stream error that arrives after a delta reached the caller is
   surfaced, not re-run: replaying a fresh attempt would double the shown text. *)
let stream_error_after_output_is_not_retried () =
  let result, requests =
    with_anthropic_server
      (fun index request ->
        ignore index;
        ignore request;
        output_then_error_stream (stream_error_event "overloaded"))
      (fun port ->
        let base_url = "http://127.0.0.1:" ^ string_of_int port in
        let config = Anthropic.Config.make ~base_url () in
        run_stream ~config port (request ()))
  in
  let events, error = expect_stream_error "error after output" result in
  equal int ~msg:"not retried after output" 1 (List.length requests);
  equal int ~msg:"no retry announced" 0 (List.length (retry_events events));
  equal_error_kind "error after output kind" Llm.Error.Rate_limited error

(* Terminal, request-shaped stream kinds never re-run even with budget to spare:
   a fresh request would fail identically. *)
let terminal_stream_error_is_not_retried () =
  let result, requests =
    with_anthropic_server
      (fun index request ->
        ignore index;
        ignore request;
        sse_response
          [ stream_error_event ~type_:"invalid_request_error" "bad input" ])
      (fun port ->
        let base_url = "http://127.0.0.1:" ^ string_of_int port in
        let config = Anthropic.Config.make ~base_url () in
        run_stream ~config port (request ()))
  in
  let events, error = expect_stream_error "terminal stream error" result in
  equal int ~msg:"terminal error not retried" 1 (List.length requests);
  equal int ~msg:"no retry announced" 0 (List.length (retry_events events));
  equal_error_kind "terminal error kind" Llm.Error.Invalid_request error

let response_stop_reasons_are_normalized () =
  let cases =
    [
      ("end turn", "end_turn", Llm.Response.Stop.end_turn);
      ("stop sequence", "stop_sequence", Llm.Response.Stop.end_turn);
      ("pause turn", "pause_turn", Llm.Response.Stop.other "pause_turn");
      ("length", "max_tokens", Llm.Response.Stop.length);
      ("tool use", "tool_use", Llm.Response.Stop.tool_call);
      ("refusal", "refusal", Llm.Response.Stop.refusal);
      ("other", "safety_stop", Llm.Response.Stop.other "safety_stop");
      ( "non-normalized provider label",
        "future-stop",
        Llm.Response.Stop.other "provider_stop" );
    ]
  in
  List.iter
    (fun (name, reason, stop) ->
      let result, requests =
        with_anthropic_server
          (fun index request ->
            ignore index;
            ignore request;
            sse_response
              [
                message_start ();
                content_block_start 0
                  (json_object
                     [ ("type", Json.string "text"); ("text", Json.string "") ]);
                text_delta "partial";
                content_block_stop 0;
                message_delta reason;
                message_stop;
              ])
          (fun port -> run_stream port (request ()))
      in
      equal int ~msg:(name ^ " request count") 1 (List.length requests);
      let events, response = expect_stream_ok name result in
      ignore events;
      equal_stop name stop response)
    cases

let cancellation_is_reported_without_leaking_requests () =
  let cancelled = ref true in
  let result, requests =
    with_anthropic_server
      (fun index request ->
        ignore index;
        ignore request;
        http_response 500 "{}")
      (fun port ->
        run_stream ~cancelled:(fun () -> !cancelled) port (request ()))
  in
  let events, error = expect_stream_error "startup cancellation" result in
  ignore events;
  equal_error_kind "startup cancellation" Llm.Error.Cancelled error;
  equal int ~msg:"no request when already cancelled" 0 (List.length requests);
  cancelled := false;
  let result, requests =
    with_anthropic_server
      (fun index request ->
        ignore index;
        ignore request;
        sse_response
          [
            message_start ();
            content_block_start 0
              (json_object
                 [ ("type", Json.string "text"); ("text", Json.string "") ]);
            text_delta "first";
            text_delta "late";
            message_delta "end_turn";
            message_stop;
          ])
      (fun port ->
        run_stream
          ~cancelled:(fun () -> !cancelled)
          ~on_event:(function
            | Llm.Event.Text_delta "first" -> cancelled := true
            | Llm.Event.Text_delta _ | Llm.Event.Reasoning_summary_delta _
            | Llm.Event.Tool_input_delta _ | Llm.Event.Tool_call _
            | Llm.Event.Usage _ | Llm.Event.Retry _ ->
                ())
          port (request ()))
  in
  equal int ~msg:"request before stream cancellation" 1 (List.length requests);
  let events, error = expect_stream_error "stream cancellation" result in
  equal int ~msg:"one event before cancellation" 1 (List.length events);
  equal_error_kind "stream cancellation" Llm.Error.Cancelled error;
  equal string ~msg:"stream cancellation phase" "stream"
    (match Llm.Error.phase error with
    | Llm.Error.Startup -> "startup"
    | Llm.Error.Stream -> "stream")

let cache_breakpoints_mark_the_prefix () =
  let transcript =
    Llm.Transcript.of_list_exn
      [
        Llm.Message.system "system";
        Llm.Message.user [ Llm.Content.text "first prompt" ];
        Llm.Message.assistant (Llm.Message.Assistant.text "first answer");
        Llm.Message.user
          [ Llm.Content.text "second prompt"; Llm.Content.text "and detail" ];
      ]
  in
  let tools =
    [
      Llm.Tool.make ~name:"read_file" ~description:"Read a file."
        ~input_schema:schema ();
      Llm.Tool.make ~name:"shell" ~description:"Run a command."
        ~input_schema:schema ();
    ]
  in
  let result, requests =
    with_anthropic_server
      (fun index request ->
        ignore index;
        ignore request;
        text_stream "ok")
      (fun port -> run_stream port (request ~tools ~transcript ()))
  in
  ignore (expect_stream_ok "stream" result);
  let body = request_body (only_request requests) in
  let marked json = has_field "cache_control" json in
  let ephemeral msg json =
    match object_field "cache_control" json with
    | Some control ->
        equal string ~msg "ephemeral" (string_field msg "type" control)
    | None -> failf "%s: missing cache_control" msg
  in
  let tools = list_field "body" "tools" body in
  equal int ~msg:"tool count" 2 (List.length tools);
  check "first tool is unmarked" (not (marked (List.nth tools 0)));
  ephemeral "last tool" (List.nth tools 1);
  let system = list_field "body" "system" body in
  equal int ~msg:"system block count" 1 (List.length system);
  ephemeral "last system block" (List.nth system 0);
  let messages = list_field "body" "messages" body in
  equal int ~msg:"message count" 3 (List.length messages);
  let content index =
    list_field "message content" "content" (List.nth messages index)
  in
  check "first message is unmarked"
    (List.for_all (fun block -> not (marked block)) (content 0));
  check "middle message is unmarked"
    (List.for_all (fun block -> not (marked block)) (content 1));
  let last = content 2 in
  equal int ~msg:"last message block count" 2 (List.length last);
  check "non-final block of last message is unmarked"
    (not (marked (List.nth last 0)));
  ephemeral "final block of last message" (List.nth last 1)

let omitted_display_thinking_decodes_to_signature_only_reasoning () =
  (* Models that default to [display: omitted] — Fable 5, Opus 5, Opus 4.8,
     Opus 4.7, and Sonnet 5 — sign the thinking block over empty thinking text:
     a signature_delta arrives with no thinking_delta, so the durable reasoning
     part carries a signature but no text. The encoder asks for [summarized]
     display, but a stream that omits the text anyway must still decode. *)
  let result, _requests =
    with_anthropic_server
      (fun _index _request ->
        sse_response
          [
            message_start ();
            content_block_start 0
              (json_object
                 [
                   ("type", Json.string "thinking"); ("thinking", Json.string "");
                 ]);
            signature_delta 0 "sig_omit";
            content_block_stop 0;
            content_block_start 1
              (json_object
                 [ ("type", Json.string "text"); ("text", Json.string "") ]);
            content_block_delta 1
              (json_object
                 [
                   ("type", Json.string "text_delta");
                   ("text", Json.string "answer");
                 ]);
            content_block_stop 1;
            message_delta "end_turn";
            message_stop;
          ])
      (fun port -> run_stream port (request ()))
  in
  let _events, response = expect_stream_ok "omitted thinking" result in
  match Llm.Message.Assistant.parts (Llm.Response.assistant response) with
  | [
   Llm.Message.Assistant.Reasoning reasoning;
   Llm.Message.Assistant.Text "answer";
  ] ->
      equal (option string) ~msg:"reasoning text is empty" None
        (Llm.Message.Assistant.Reasoning.text reasoning);
      equal (option string) ~msg:"reasoning keeps its signature"
        (Some "sig_omit")
        (Llm.Message.Assistant.Reasoning.signature reasoning)
  | parts ->
      failf "expected signature-only reasoning then text, got %d parts"
        (List.length parts)

let signature_only_reasoning_replays_as_signed_thinking () =
  (* The reported "reasoning parts require text or encrypted data" error: a
     signed but textless reasoning part must replay as a signed thinking block,
     not fault the turn. *)
  let reasoning =
    Llm.Message.Assistant.Reasoning.make ~signature:"sig_omit" ()
  in
  let transcript =
    Llm.Transcript.of_list_exn
      [
        Llm.Message.user_text "hi";
        Llm.Message.assistant
          (Llm.Message.Assistant.make
             [
               Llm.Message.Assistant.reasoning_part reasoning;
               Llm.Message.Assistant.text_part "answer";
             ]);
        Llm.Message.user_text "again";
      ]
  in
  let result, requests =
    with_anthropic_server
      (fun _index _request -> text_stream "ok")
      (fun port -> run_stream port (request ~transcript ()))
  in
  ignore (expect_stream_ok "signed thinking replay" result);
  let body = request_body (only_request requests) in
  let messages = list_field "body" "messages" body in
  let assistant_content =
    list_field "assistant" "content" (List.nth messages 1)
  in
  equal int ~msg:"assistant block count" 2 (List.length assistant_content);
  let thinking = List.nth assistant_content 0 in
  equal string ~msg:"reasoning replays as thinking" "thinking"
    (string_field "thinking" "type" thinking);
  equal string ~msg:"thinking text is empty" ""
    (string_field "thinking" "thinking" thinking);
  equal string ~msg:"signature is preserved" "sig_omit"
    (string_field "thinking" "signature" thinking);
  equal string ~msg:"visible text follows" "answer"
    (string_field "assistant text" "text" (List.nth assistant_content 1))

let unreplayable_reasoning_parts_are_dropped () =
  (* A reasoning part lacking Anthropic continuity state — e.g. an OpenAI item
     with only an id — cannot be replayed and is dropped, not faulted. An
     encrypted part round-trips as redacted_thinking. *)
  let id_only = Llm.Message.Assistant.Reasoning.make ~id:"rs_openai" () in
  let redacted =
    Llm.Message.Assistant.Reasoning.make ~encrypted:"enc_blob" ()
  in
  let transcript =
    Llm.Transcript.of_list_exn
      [
        Llm.Message.user_text "hi";
        Llm.Message.assistant
          (Llm.Message.Assistant.make
             [
               Llm.Message.Assistant.reasoning_part id_only;
               Llm.Message.Assistant.reasoning_part redacted;
               Llm.Message.Assistant.text_part "answer";
             ]);
        Llm.Message.user_text "again";
      ]
  in
  let result, requests =
    with_anthropic_server
      (fun _index _request -> text_stream "ok")
      (fun port -> run_stream port (request ~transcript ()))
  in
  ignore (expect_stream_ok "drop unreplayable reasoning" result);
  let body = request_body (only_request requests) in
  let messages = list_field "body" "messages" body in
  let assistant_content =
    list_field "assistant" "content" (List.nth messages 1)
  in
  equal int ~msg:"id-only reasoning is dropped" 2
    (List.length assistant_content);
  equal string ~msg:"encrypted becomes redacted_thinking" "redacted_thinking"
    (string_field "redacted" "type" (List.nth assistant_content 0));
  equal string ~msg:"redacted data is preserved" "enc_blob"
    (string_field "redacted" "data" (List.nth assistant_content 0));
  equal string ~msg:"visible text is kept" "answer"
    (string_field "assistant text" "text" (List.nth assistant_content 1))

let () =
  run "mentat.llm.anthropic"
    [
      test "model, config, and credentials" model_config_and_credentials;
      test "maximal request encoding" maximal_request_encoding;
      test "cache breakpoints mark the prefix" cache_breakpoints_mark_the_prefix;
      test "headers and default request encoding"
        headers_and_default_request_encoding;
      test "reasoning effort encoding" reasoning_effort_encoding;
      test "unsupported requests do not touch transport"
        unsupported_requests_do_not_touch_transport;
      test "HTTP errors are classified" http_errors_are_classified;
      test "non-json HTTP error body is not a log payload"
        non_json_http_error_body_is_not_a_log_payload;
      test "retry policy" retry_policy;
      test ~timeout:2.0 "request deadline bounds response headers"
        request_deadline_bounds_response_headers;
      test ~timeout:2.0 "request deadline bounds a partial stream"
        request_deadline_bounds_partial_stream;
      test ~timeout:2.0 "request deadline includes retry backoff"
        request_deadline_includes_retry_backoff;
      test ~timeout:2.0 "request deadline bounds a partial error body"
        request_deadline_bounds_partial_error_body;
      test "completed stream decodes events and response"
        completed_stream_decodes_events_and_response;
      test "omitted-display thinking decodes to signature-only reasoning"
        omitted_display_thinking_decodes_to_signature_only_reasoning;
      test "signature-only reasoning replays as signed thinking"
        signature_only_reasoning_replays_as_signed_thinking;
      test "unreplayable reasoning parts are dropped"
        unreplayable_reasoning_parts_are_dropped;
      test "durable parts preserve content block order"
        durable_parts_preserve_content_block_order;
      test "stream failures are classified" stream_failures_are_classified;
      test ~timeout:5.0 "stream overload is retried" stream_overload_is_retried;
      test "stream retry is disabled by max_retries zero"
        stream_retry_is_disabled_by_max_retries_zero;
      test "stream error after output is not retried"
        stream_error_after_output_is_not_retried;
      test "terminal stream error is not retried"
        terminal_stream_error_is_not_retried;
      test "response stop reasons are normalized"
        response_stop_reasons_are_normalized;
      test "cancellation is reported without leaking requests"
        cancellation_is_reported_without_leaking_requests;
    ]
