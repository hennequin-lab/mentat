(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for the [web_search] tool over its remote
   MCP search backends. Every provider value crosses [Tool.Call.decode], the
   permission is read from the resulting call, and every request crosses the
   injected structured transport. The fake transport performs no I/O; socket
   admission, DNS pinning, and TLS belong to the shared transport's own tests.
   These pin the strict input decode, per-backend endpoint permission authority,
   the JSON-RPC tools/call request shape and where each backend's key rides,
   plain-JSON and SSE response parsing, and every failure lowering. *)

open Windtrap
open Test_tools_support
module Access = Mentat_permission.Access
module Json = Jsont.Json
module Permission = Mentat_permission
module Tool = Mentat_tool
module Web = Mentat_tools.Web
module Search_service = Web.Search_service
module Web_search = Web.Web_search

let input ?num_results query =
  let fields = [ ("query", Json.string query) ] in
  let fields =
    match num_results with
    | None -> fields
    | Some n -> fields @ [ ("num_results", Json.int n) ]
  in
  json_object fields

let decode_result tool provider_input =
  Tool.Call.decode tool (model_call tool provider_input)

let decode tool provider_input =
  match decode_result tool provider_input with
  | Ok call -> call
  | Error error ->
      failf "call decode failed: %s" (Tool.Call.Decode_error.message error)

let run ?(cancelled = fun () -> false) tool provider_input =
  decode tool provider_input |> Tool.Call.run ~cancelled |> finished

let output_text_exn result =
  match Tool.Result.output result with
  | Some output -> Tool.Output.text output
  | None -> fail "tool result carried no output"

let print_status result =
  match Tool.Result.status result with
  | Tool.Result.Completed -> print_endline "status: completed"
  | Tool.Result.Failed { kind; message; _ } ->
      Printf.printf "status: failed %s\nmessage: %S\n" (failure_name kind)
        message
  | Tool.Result.Interrupted { reason; cancelled } ->
      Printf.printf "status: interrupted cancelled=%b\nreason: %S\n" cancelled
        reason

let policy ?(allow_private_network = false) ?(max_fetch_bytes = 65536)
    ?(max_output_chars = 4000) ?(default_timeout_ms = 3_000)
    ?(max_timeout_ms = 9_000) () =
  match
    Web.Policy.make ~allow_private_network ~max_fetch_bytes ~max_output_chars
      ~default_timeout_ms ~max_timeout_ms ()
  with
  | Ok policy -> policy
  | Error _ -> fail "test constructed an invalid web policy"

type transport = {
  mutable requests : Web.Transport.Request.t list;
  respond :
    cancelled:(unit -> bool) ->
    Web.Transport.Request.t ->
    (Web.Transport.response, Web.Transport.error) result;
}

let make_transport respond = { requests = []; respond }

let fetch transport ~sw:_ ~cancelled request =
  transport.requests <- transport.requests @ [ request ];
  transport.respond ~cancelled request

let make_tool ?(backend = Search_service.Exa) ?api_key transport =
  Web_search.make ~policy:(policy ()) ~backend ?api_key ~fetch:(fetch transport)
    ()

(* An MCP tools/call result body: {"result":{"content":[{"type","text"}]}}. *)
let mcp_result text =
  json_string
    (json_object
       [
         ( "result",
           json_object
             [
               ( "content",
                 Json.list
                   [
                     json_object
                       [
                         ("type", Json.string "text"); ("text", Json.string text);
                       ];
                   ] );
             ] );
       ])

let body ?(status = 200) ?(content_type = "application/json") contents =
  Web.Transport.Body
    {
      head =
        Web.Transport.make_response_head
          ~effective_url:(Uri.of_string "https://mcp.exa.ai/mcp")
          ~status ~content_type ~duration_ms:11 ();
      body = contents;
    }

let protocol_name = function
  | `Http -> "http"
  | `Https -> "https"
  | `Ssh -> "ssh"
  | `Tcp -> "tcp"
  | `Udp -> "udp"
  | `Other protocol -> protocol

let print_permissions call =
  match Tool.Call.permissions call with
  | [ request ] ->
      Printf.printf "source=%s accesses=%d\n"
        (Option.value (Permission.Request.source request) ~default:"-")
        (List.length (Permission.Request.accesses request));
      Permission.Request.accesses request
      |> List.iter (function
        | Access.Network { protocol; host; port } ->
            Printf.printf "network=%s://%s:%s\n" (protocol_name protocol) host
              (Option.fold ~none:"-" ~some:string_of_int port)
        | Access.Path _ | Access.Command _ | Access.Custom _ ->
            print_endline "non-network access")
  | requests -> Printf.printf "requests=%d\n" (List.length requests)

let meth_name = function
  | Web.Transport.Request.Get -> "GET"
  | Web.Transport.Request.Post -> "POST"

(* Report a request without leaking a key: the Exa key rides the endpoint query
   and the Parallel key an Authorization header, so both are redacted. *)
let print_request request =
  let url = Web.Transport.Request.url request in
  let key_in_query =
    match Uri.get_query_param url "exaApiKey" with
    | Some _ -> " exaApiKey=<redacted>"
    | None -> ""
  in
  Printf.printf "method=%s host=%s path=%s%s\n"
    (meth_name (Web.Transport.Request.meth request))
    (Option.value (Uri.host url) ~default:"-")
    (Uri.path url) key_in_query;
  Printf.printf "body=%s\n"
    (Option.value (Web.Transport.Request.body request) ~default:"<none>");
  List.iter
    (fun (name, value) ->
      let shown =
        if String.equal (String.lowercase_ascii name) "authorization" then
          "<redacted>"
        else value
      in
      Printf.printf "header %s=%s\n" name shown)
    (Web.Transport.Request.headers request)

let%expect_test "declaration is a strict-input search tool" =
  let tool = make_tool (make_transport (fun ~cancelled:_ _ -> fail "unused")) in
  Printf.printf "name=%s\n" (Mentat_llm.Tool.name (Tool.declaration tool));
  let decodes label provider_input =
    Printf.printf "%s: %s\n" label
      (match decode_result tool provider_input with
      | Ok _ -> "accepted"
      | Error _ -> "rejected")
  in
  decodes "query only" (input "ocaml 5.5 release notes");
  decodes "query + num_results" (input ~num_results:3 "weather");
  decodes "missing query" (json_object []);
  decodes "empty query" (input "");
  decodes "non-positive num_results" (input ~num_results:0 "x");
  decodes "unknown member"
    (json_object [ ("query", Json.string "x"); ("extra", Json.bool true) ]);
  [%expect
    {|
    name=web_search
    query only: accepted
    query + num_results: accepted
    missing query: rejected
    empty query: rejected
    non-positive num_results: rejected
    unknown member: rejected
    |}]

let%expect_test "permission is network authority for the selected backend" =
  let exa = make_tool (make_transport (fun ~cancelled:_ _ -> fail "unused")) in
  print_permissions (decode exa (input "hello"));
  let parallel =
    make_tool ~backend:Search_service.Parallel
      (make_transport (fun ~cancelled:_ _ -> fail "unused"))
  in
  print_permissions (decode parallel (input "hello"));
  [%expect
    {|
    source=web_search accesses=1
    network=https://mcp.exa.ai:443
    source=web_search accesses=1
    network=https://search.parallel.ai:443
    |}]

let%expect_test "exa issues a keyless JSON-RPC tools/call POST" =
  Eio_main.run @@ fun _ ->
  let transport =
    make_transport (fun ~cancelled:_ _ -> Ok (body (mcp_result "ok")))
  in
  ignore (run (make_tool transport) (input ~num_results:2 "ocaml"));
  (match transport.requests with
  | [ request ] -> print_request request
  | requests -> Printf.printf "requests=%d\n" (List.length requests));
  [%expect
    {|
    method=POST host=mcp.exa.ai path=/mcp
    body={"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"web_search_exa","arguments":{"query":"ocaml","type":"auto","numResults":2,"livecrawl":"fallback"}}}
    header user-agent=mentat/0 (+https://github.com/invariant-hq/mentat)
    header accept=application/json, text/event-stream
    header content-type=application/json
    |}]

let%expect_test "an exa key rides the endpoint query, not a header" =
  Eio_main.run @@ fun _ ->
  let transport =
    make_transport (fun ~cancelled:_ _ -> Ok (body (mcp_result "ok")))
  in
  ignore (run (make_tool ~api_key:"sk-exa-secret" transport) (input "ocaml"));
  (match transport.requests with
  | [ request ] -> print_request request
  | requests -> Printf.printf "requests=%d\n" (List.length requests));
  [%expect
    {|
    method=POST host=mcp.exa.ai path=/mcp exaApiKey=<redacted>
    body={"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"web_search_exa","arguments":{"query":"ocaml","type":"auto","numResults":5,"livecrawl":"fallback"}}}
    header user-agent=mentat/0 (+https://github.com/invariant-hq/mentat)
    header accept=application/json, text/event-stream
    header content-type=application/json
    |}]

let%expect_test "parallel targets its endpoint and bears the key as a header" =
  Eio_main.run @@ fun _ ->
  let transport =
    make_transport (fun ~cancelled:_ _ -> Ok (body (mcp_result "ok")))
  in
  ignore
    (run
       (make_tool ~backend:Search_service.Parallel ~api_key:"pk-secret"
          transport)
       (input "ocaml"));
  (match transport.requests with
  | [ request ] -> print_request request
  | requests -> Printf.printf "requests=%d\n" (List.length requests));
  [%expect
    {|
    method=POST host=search.parallel.ai path=/mcp
    body={"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"web_search","arguments":{"objective":"ocaml","search_queries":["ocaml"]}}}
    header user-agent=mentat/0 (+https://github.com/invariant-hq/mentat)
    header accept=application/json, text/event-stream
    header content-type=application/json
    header authorization=<redacted>
    |}]

let%expect_test "a JSON result body is parsed to its text" =
  Eio_main.run @@ fun _ ->
  let transport =
    make_transport (fun ~cancelled:_ _ ->
        Ok (body (mcp_result "1. OCaml 5.5 — https://ocaml.org/releases/5.5.0")))
  in
  let result = run (make_tool transport) (input "ocaml 5.5") in
  print_status result;
  print_endline (output_text_exn result);
  [%expect
    {|
    status: completed
    1. OCaml 5.5 — https://ocaml.org/releases/5.5.0
    |}]

let%expect_test "an SSE result body is parsed to its text" =
  Eio_main.run @@ fun _ ->
  let sse = "event: message\ndata: " ^ mcp_result "streamed answer" ^ "\n\n" in
  let transport =
    make_transport (fun ~cancelled:_ _ ->
        Ok (body ~content_type:"text/event-stream" sse))
  in
  let result = run (make_tool transport) (input "x") in
  print_status result;
  print_endline (output_text_exn result);
  [%expect {|
    status: completed
    streamed answer
    |}]

let%expect_test "an empty result text completes with a note" =
  Eio_main.run @@ fun _ ->
  let transport =
    make_transport (fun ~cancelled:_ _ -> Ok (body (mcp_result "")))
  in
  let result = run (make_tool transport) (input "no such thing") in
  print_status result;
  print_endline (output_text_exn result);
  [%expect {|
    status: completed
    No results.
    |}]

let%expect_test "a non-2xx status and unparseable bodies both fail cleanly" =
  Eio_main.run @@ fun _ ->
  let http_error =
    make_transport (fun ~cancelled:_ _ ->
        Ok (body ~status:401 "{\"error\":\"unauthorized\"}"))
  in
  print_status (run (make_tool http_error) (input "x"));
  let malformed =
    make_transport (fun ~cancelled:_ _ -> Ok (body "not json at all"))
  in
  print_status (run (make_tool malformed) (input "x"));
  let no_result =
    make_transport (fun ~cancelled:_ _ -> Ok (body "{\"other\":1}"))
  in
  print_status (run (make_tool no_result) (input "x"));
  [%expect
    {|
    status: failed failed
    message: "search endpoint returned HTTP status 401: {\"error\":\"unauthorized\"}"
    status: failed failed
    message: "search response could not be read: response carried no search result text"
    status: failed failed
    message: "search response could not be read: response carried no search result text"
    |}]

let%expect_test "transport failures lower to stable tool statuses" =
  Eio_main.run @@ fun _ ->
  let fail_with error =
    print_status
      (run
         (make_tool (make_transport (fun ~cancelled:_ _ -> Error error)))
         (input "x"))
  in
  fail_with Web.Transport.Timed_out;
  fail_with
    (Web.Transport.Private_address
       { url = Uri.of_string "https://mcp.exa.ai/mcp"; address = "10.0.0.1" });
  fail_with (Web.Transport.Transport "connection refused");
  [%expect
    {|
    status: failed timed_out
    message: "web search timed out"
    status: failed permission_denied
    message: "web search resolved to a disallowed address: 10.0.0.1"
    status: failed unavailable
    message: "web transport failure: connection refused"
    |}]

let%expect_test "cancellation before the transport yields an interruption" =
  let transport =
    make_transport (fun ~cancelled:_ _ -> fail "transport must not run")
  in
  print_status
    (run ~cancelled:(fun () -> true) (make_tool transport) (input "x"));
  [%expect
    {|
    status: interrupted cancelled=true
    reason: "tool call cancelled"
    |}]

[%%run_tests "mentat.tools.web_search"]
