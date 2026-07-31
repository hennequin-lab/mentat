(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for portable [web_fetch].
   Every provider value crosses [Tool.Call.decode], every permission is read
   from the resulting call, and every request crosses the injected structured
   transport. The fake transport performs no I/O and exposes no workspace
   capability, which keeps these tests focused on the portable tool contract;
   socket admission, DNS pinning, and TLS belong to the composition adapter's
   own integration tests.

   Compared with the legacy suite, this pins strict and safe-integer provider
   decoding, post-upgrade permission authority, product-owned request fields,
   every transport outcome, cancellation, textual MIME and UTF-8 admission,
   redirect evidence, Unicode-safe output bounds, diagnostic sanitization, and
   durable compact semantics. *)

open Windtrap
open Test_tools_support
module Access = Mentat_permission.Access
module Json = Jsont.Json
module Permission = Mentat_permission
module Tool = Mentat_tool
module Web = Mentat_tools.Web
module Web_fetch = Web.Web_fetch
module Fetch = Mentat_tools_output.Web.Fetch
module Output_codec = Mentat_tools_output.Codec

let input ?format ?timeout_ms url =
  let fields = [ ("url", Json.string url) ] in
  let fields =
    match format with
    | None -> fields
    | Some format -> fields @ [ ("format", Json.string format) ]
  in
  let fields =
    match timeout_ms with
    | None -> fields
    | Some timeout_ms -> fields @ [ ("timeout_ms", Json.int timeout_ms) ]
  in
  json_object fields

let result_roundtrip result =
  let encoded =
    match Json.encode result_codec result with
    | Ok encoded -> encoded
    | Error diagnostic -> failf "result encoding failed: %s" diagnostic
  in
  let decoded =
    match Json.decode result_codec encoded with
    | Ok decoded -> decoded
    | Error diagnostic -> failf "result decoding failed: %s" diagnostic
  in
  (encoded, Tool.Result.equal Tool.Output.equal result decoded)

let decode_result tool provider_input =
  Tool.Call.decode tool (model_call tool provider_input)

let decode tool provider_input =
  match decode_result tool provider_input with
  | Ok call -> call
  | Error error ->
      failf "call decode failed: %s" (Tool.Call.Decode_error.message error)

let run ?(cancelled = fun () -> false) tool provider_input =
  decode tool provider_input |> Tool.Call.run ~cancelled |> finished

let fetch_output_exn result =
  match Output_codec.decode Fetch.jsont (output_exn result) with
  | Some output -> output
  | None -> fail "tool output carried no web-fetch semantics"

let disposition_name = function
  | Fetch.Fetched -> "fetched"
  | Fetch.Redirected -> "redirected"
  | Fetch.Http_error -> "http_error"

let print_optional_output result =
  match Tool.Result.output result with
  | None -> print_endline "output: none"
  | Some _ ->
      let output = output_exn result in
      let fetch = fetch_output_exn result in
      Printf.printf "output: %s status=%d bytes=%d truncated=%b\n"
        (disposition_name (Fetch.disposition fetch))
        (Fetch.status fetch) (Fetch.bytes fetch)
        (Tool.Output.truncated output)

let contains ~needle text =
  let needle_length = String.length needle in
  let text_length = String.length text in
  let rec loop index =
    index + needle_length <= text_length
    && (String.equal (String.sub text index needle_length) needle
       || loop (index + 1))
  in
  String.is_empty needle || loop 0

let after_headers text =
  let separator = "\n\n" in
  let separator_length = String.length separator in
  let rec loop index =
    if index + separator_length > String.length text then text
    else if String.equal (String.sub text index separator_length) separator then
      String.sub text (index + separator_length)
        (String.length text - index - separator_length)
    else loop (index + 1)
  in
  loop 0

let print_status result =
  match Tool.Result.status result with
  | Tool.Result.Completed -> print_endline "status: completed"
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf "status: failed %s\nmessage: %S\nmetadata: %b\n"
        (failure_name kind) message (Option.is_some metadata)
  | Tool.Result.Interrupted { reason; cancelled } ->
      Printf.printf "status: interrupted cancelled=%b\nreason: %S\n" cancelled
        reason

let failure_message_exn result =
  match Tool.Result.status result with
  | Tool.Result.Failed { message; _ } -> message
  | Tool.Result.Completed | Tool.Result.Interrupted _ ->
      fail "expected a failed tool result"

let policy ?(allow_private_network = false) ?(max_fetch_bytes = 1024)
    ?(max_output_chars = 256) ?(default_timeout_ms = 3_000)
    ?(max_timeout_ms = 9_000) () =
  match
    Web.Policy.make ~allow_private_network ~max_fetch_bytes ~max_output_chars
      ~default_timeout_ms ~max_timeout_ms ()
  with
  | Ok policy -> policy
  | Error _ -> fail "test constructed an invalid web policy"

type transport = {
  mutable calls : int;
  mutable requests : Web.Transport.Request.t list;
  respond :
    cancelled:(unit -> bool) ->
    Web.Transport.Request.t ->
    (Web.Transport.response, Web.Transport.error) result;
}

let make_transport respond = { calls = 0; requests = []; respond }

let fetch transport ~sw:_ ~cancelled request =
  transport.calls <- transport.calls + 1;
  transport.requests <- transport.requests @ [ request ];
  transport.respond ~cancelled request

let make_tool transport policy = Web_fetch.make ~policy ~fetch:(fetch transport)
let no_response ~cancelled:_ _ = fail "transport must not have been called"

let decode_verdict tool label provider_input =
  match decode_result tool provider_input with
  | Ok call ->
      Printf.printf "%s: accepted canonical=%s\n" label
        (json_string (Tool.Call.input call))
  | Error error ->
      Printf.printf "%s: rejected diagnostic=%b\n" label
        (not (String.is_empty (Tool.Call.Decode_error.message error)))

let decode_only_verdict tool label provider_input =
  Printf.printf "%s: %s\n" label
    (match decode_result tool provider_input with
    | Ok _ -> "accepted"
    | Error _ -> "rejected")

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
      Printf.printf "source=%s display=%s accesses=%d\n"
        (Option.value (Permission.Request.source request) ~default:"-")
        (Option.value (Permission.Request.display request) ~default:"-")
        (List.length (Permission.Request.accesses request));
      Permission.Request.accesses request
      |> List.iter (function
        | Access.Network { protocol; host; port } ->
            Printf.printf "network=%s://%s:%s\n" (protocol_name protocol) host
              (Option.fold ~none:"-" ~some:string_of_int port)
        | Access.Path _ | Access.Command _ | Access.Custom _ ->
            print_endline "non-network access")
  | requests -> Printf.printf "requests=%d\n" (List.length requests)

let head ?content_type ?(duration_ms = 17) ?(status = 200) effective_url =
  Web.Transport.make_response_head
    ~effective_url:(Uri.of_string effective_url)
    ~status ?content_type ~duration_ms ()

let body ?content_type ?duration_ms ?status ?(contents = "ok") effective_url =
  Web.Transport.Body
    {
      head = head ?content_type ?duration_ms ?status effective_url;
      body = contents;
    }

let print_request request =
  let module Request = Web.Transport.Request in
  Printf.printf "url=%s\n" (Uri.to_string (Request.url request));
  Printf.printf "headers=%s\n"
    (Request.headers request
    |> List.map (fun (name, value) -> name ^ ": " ^ value)
    |> String.concat " | ");
  Printf.printf "timeout_ms=%d max_body_bytes=%d private=%b max_redirects=%d\n"
    (Request.timeout_ms request)
    (Request.max_body_bytes request)
    (Request.allow_private_network request)
    (Request.max_redirects request)

let invalid_argument fn =
  match fn () with _ -> false | exception Invalid_argument _ -> true

let request_verdict label ?(url = "https://example.com/")
    ?(headers = [ ("accept", "text/plain") ]) ?(timeout_ms = 1)
    ?(max_body_bytes = 1) ?(max_redirects = 0) () =
  let rejected =
    invalid_argument (fun () ->
        Web.Transport.Request.make ~url:(Uri.of_string url) ~headers ~timeout_ms
          ~max_body_bytes ~allow_private_network:false ~max_redirects ())
  in
  Printf.printf "%s: %s\n" label (if rejected then "rejected" else "accepted")

let policy_field_name = function
  | Web.Policy.Error.Max_fetch_bytes -> "max_fetch_bytes"
  | Web.Policy.Error.Max_output_chars -> "max_output_chars"
  | Web.Policy.Error.Default_timeout_ms -> "default_timeout_ms"
  | Web.Policy.Error.Max_timeout_ms -> "max_timeout_ms"

let policy_error_name = function
  | Web.Policy.Error.Non_positive { field; value } ->
      Printf.sprintf "non_positive(%s=%d)" (policy_field_name field) value
  | Web.Policy.Error.Default_timeout_exceeds_max
      { default_timeout_ms; max_timeout_ms } ->
      Printf.sprintf "default_exceeds_max(%d>%d)" default_timeout_ms
        max_timeout_ms

let timeout_error_name = function
  | Web.Policy.Timeout_error.Non_positive value ->
      Printf.sprintf "non_positive(%d)" value
  | Web.Policy.Timeout_error.Exceeds_max { requested; maximum } ->
      Printf.sprintf "exceeds_max(%d>%d)" requested maximum

let head_verdict label ?(effective_url = "https://example.com/") ?(status = 200)
    ?content_type ?(duration_ms = 0) () =
  let rejected =
    invalid_argument (fun () ->
        Web.Transport.make_response_head
          ~effective_url:(Uri.of_string effective_url)
          ~status ?content_type ~duration_ms ())
  in
  Printf.printf "%s: %s\n" label (if rejected then "rejected" else "accepted")

let%expect_test "web_fetch declaration and provider codec are strict" =
  let transport = make_transport no_response in
  let tool = make_tool transport (policy ()) in
  let declaration = Tool.declaration tool in
  Printf.printf "public name: %s\n" Web_fetch.name;
  Printf.printf "name: %s\n" (Mentat_llm.Tool.name declaration);
  Printf.printf "description nonempty: %b\n"
    (Option.fold ~none:false
       ~some:(fun description -> not (String.is_empty description))
       (Mentat_llm.Tool.description declaration));
  Printf.printf "schema: %s\n"
    (json_string (Mentat_llm.Tool.input_schema declaration));
  decode_verdict tool "minimal" (input "https://example.com/a");
  decode_verdict tool "all fields"
    (input ~format:"text" ~timeout_ms:7_000 "https://example.com/a");
  decode_verdict tool "missing url" (json_object []);
  decode_verdict tool "unknown member"
    (json_object
       [ ("url", Json.string "https://example.com"); ("headers", Json.list []) ]);
  decode_verdict tool "duplicate url"
    (json_object
       [
         ("url", Json.string "https://example.com/a");
         ("url", Json.string "https://example.com/b");
       ]);
  decode_verdict tool "unknown format"
    (input ~format:"json" "https://example.com");
  decode_verdict tool "zero timeout" (input ~timeout_ms:0 "https://example.com");
  decode_verdict tool "fractional timeout"
    (json_object
       [
         ("url", Json.string "https://example.com");
         ("timeout_ms", Json.number 1.5);
       ]);
  decode_verdict tool "unsafe timeout"
    (json_object
       [
         ("url", Json.string "https://example.com");
         ("timeout_ms", Json.number 9_007_199_254_740_992.);
       ]);
  decode_verdict tool "infinite timeout"
    (json_object
       [
         ("url", Json.string "https://example.com");
         ("timeout_ms", Json.number Float.infinity);
       ]);
  decode_only_verdict tool "empty URL" (input "");
  decode_only_verdict tool "NUL URL" (input "https://example.com/\x00tail");
  decode_only_verdict tool "invalid UTF-8 URL"
    (input "https://example.com/\xff");
  decode_only_verdict tool "URL at 2000 bytes"
    (input ("https://example.com/" ^ String.make 1_980 'x'));
  decode_only_verdict tool "URL above 2000 bytes"
    (input ("https://example.com/" ^ String.make 1_981 'x'));
  decode_only_verdict tool "Unicode URL above 2000 encoded bytes"
    (input
       ("https://example.com/" ^ String.concat "" (List.init 991 (fun _ -> "é"))));
  Printf.printf "transport calls: %d\n" transport.calls;
  [%expect
    {|
    public name: web_fetch
    name: web_fetch
    description nonempty: true
    schema: {"type":"object","properties":{"url":{"type":"string","description":"Public HTTP or HTTPS URL to fetch. HTTP is upgraded to HTTPS. Its UTF-8 encoding must not exceed 2000 bytes.","minLength":1},"format":{"type":"string","enum":["markdown","text","html"],"description":"Response rendering format. Defaults to markdown."},"timeout_ms":{"type":"integer","description":"Whole-request timeout in milliseconds, bounded by host policy.","minimum":1,"maximum":9007199254740991}},"required":["url"],"additionalProperties":false}
    minimal: accepted canonical={"format":"markdown","url":"https://example.com/a"}
    all fields: accepted canonical={"format":"text","timeout_ms":7000,"url":"https://example.com/a"}
    missing url: rejected diagnostic=true
    unknown member: rejected diagnostic=true
    duplicate url: rejected diagnostic=true
    unknown format: rejected diagnostic=true
    zero timeout: rejected diagnostic=true
    fractional timeout: rejected diagnostic=true
    unsafe timeout: rejected diagnostic=true
    infinite timeout: rejected diagnostic=true
    empty URL: rejected
    NUL URL: rejected
    invalid UTF-8 URL: rejected
    URL at 2000 bytes: accepted
    URL above 2000 bytes: rejected
    Unicode URL above 2000 encoded bytes: rejected
    transport calls: 0 |}]

let%expect_test "URL upgrade and exact permission authority precede transport" =
  let transport = make_transport no_response in
  let tool = make_tool transport (policy ()) in
  let cases =
    [
      ("https default", "https://Example.COM/docs?q=1");
      ("https explicit", "https://Example.COM:443/docs?q=1");
      ("http upgrade", "http://Example.COM/docs?q=1");
      ("non-default", "https://Example.COM:8443/docs");
    ]
  in
  List.iter
    (fun (label, url) ->
      Printf.printf "-- %s --\n" label;
      input url |> decode tool |> print_permissions)
    cases;
  Printf.printf "transport calls: %d\n" transport.calls;
  [%expect
    {|
    -- https default --
    source=web_fetch display=- accesses=1
    network=https://example.com:443
    -- https explicit --
    source=web_fetch display=- accesses=1
    network=https://example.com:443
    -- http upgrade --
    source=web_fetch display=- accesses=1
    network=https://example.com:443
    -- non-default --
    source=web_fetch display=- accesses=1
    network=https://example.com:8443
    transport calls: 0 |}]

let%expect_test
    "URL normalization failures neither request permission nor fetch" =
  Eio_main.run @@ fun _ ->
  let transport =
    make_transport (fun ~cancelled:_ request ->
        let effective_url =
          Web.Transport.Request.url request |> Uri.to_string
        in
        Ok (body ~content_type:"text/plain" effective_url))
  in
  let tool = make_tool transport (policy ()) in
  let run_case label url =
    Printf.printf "-- %s --\n" label;
    let call = decode tool (input url) in
    Printf.printf "permissions=%d\n" (List.length (Tool.Call.permissions call));
    run tool (input url) |> print_status
  in
  run_case "unsupported scheme" "file:///tmp/secret";
  run_case "missing scheme" "example.com/private";
  run_case "missing host" "https:///private";
  run_case "userinfo" "https://user:secret@example.com/private";
  run_case "fragment" "https://example.com/private#token";
  run_case "space in path" "https://example.com/not allowed";
  run_case "invalid host" "https://bad_host.example/private";
  run_case "private literal" "https://127.0.0.1/private";
  run_case "IANA protocol assignment exception .9" "https://192.0.0.9/";
  run_case "IANA protocol assignment exception .10" "https://192.0.0.10/";
  run_case "nearby reserved literal .8" "https://192.0.0.8/";
  run_case "local name" "https://localhost/private";
  run_case "trim and normalize" " \thttps://Example.COM/docs?q=1\r\n";
  Printf.printf "transport calls=%d\n" transport.calls;
  let private_transport =
    make_transport (fun ~cancelled:_ request ->
        let effective_url =
          Web.Transport.Request.url request |> Uri.to_string
        in
        print_request request;
        Ok (body ~content_type:"text/plain" effective_url))
  in
  let private_tool =
    make_tool private_transport (policy ~allow_private_network:true ())
  in
  print_endline "-- private admitted by host policy --";
  let private_call = decode private_tool (input "http://LOCALHOST/private") in
  print_permissions private_call;
  run private_tool (input "http://LOCALHOST/private") |> print_status;
  [%expect
    {|
    -- unsupported scheme --
    permissions=0
    status: failed invalid_input
    message: "url must use HTTP or HTTPS"
    metadata: false
    -- missing scheme --
    permissions=0
    status: failed invalid_input
    message: "url must use HTTP or HTTPS"
    metadata: false
    -- missing host --
    permissions=0
    status: failed invalid_input
    message: "url must include a host"
    metadata: false
    -- userinfo --
    permissions=0
    status: failed invalid_input
    message: "url must not include username or password"
    metadata: false
    -- fragment --
    permissions=0
    status: failed invalid_input
    message: "url must not include a fragment"
    metadata: false
    -- space in path --
    permissions=0
    status: failed invalid_input
    message: "url is not a valid absolute HTTP or HTTPS URL"
    metadata: false
    -- invalid host --
    permissions=0
    status: failed invalid_input
    message: "url host must be an ASCII DNS name or IP literal"
    metadata: false
    -- private literal --
    permissions=0
    status: failed invalid_input
    message: "private or local URL hosts are not allowed"
    metadata: false
    -- IANA protocol assignment exception .9 --
    permissions=1
    status: completed
    -- IANA protocol assignment exception .10 --
    permissions=1
    status: completed
    -- nearby reserved literal .8 --
    permissions=0
    status: failed invalid_input
    message: "private or local URL hosts are not allowed"
    metadata: false
    -- local name --
    permissions=0
    status: failed invalid_input
    message: "private or local URL hosts are not allowed"
    metadata: false
    -- trim and normalize --
    permissions=1
    status: completed
    transport calls=3
    -- private admitted by host policy --
    source=web_fetch display=- accesses=1
    network=https://localhost:443
    url=https://localhost/private
    headers=user-agent: mentat/0 (+https://github.com/invariant-hq/mentat) | accept: text/markdown;q=1.0, text/x-markdown;q=0.9, text/plain;q=0.8, text/html;q=0.7, */*;q=0.1 | accept-language: en-US,en;q=0.9
    timeout_ms=3000 max_body_bytes=1024 private=true max_redirects=10
    status: completed |}]

let%expect_test
    "normalized URLs and policy bounds reach only the injected transport" =
  Eio_main.run @@ fun _ ->
  let transport =
    make_transport (fun ~cancelled request ->
        Printf.printf "cancelled initially=%b\n" (cancelled ());
        print_request request;
        Ok
          (body ~content_type:"text/plain; charset=utf-8" ~contents:"response"
             "https://example.com/docs?q=1"))
  in
  let policy =
    policy ~allow_private_network:true ~max_fetch_bytes:12_345
      ~max_output_chars:120 ~default_timeout_ms:4_321 ~max_timeout_ms:8_000 ()
  in
  let tool = make_tool transport policy in
  let call = decode tool (input " http://Example.COM/docs?q=1 ") in
  Printf.printf "canonical input=%s\n" (json_string (Tool.Call.input call));
  let result = Tool.Call.run call ~cancelled:(fun () -> false) |> finished in
  print_status result;
  Printf.printf "output=%S truncated=%b\n"
    (Tool.Output.text (output_exn result))
    (Tool.Output.truncated (output_exn result));
  Printf.printf "transport calls=%d\n" transport.calls;
  [%expect
    {|
    canonical input={"format":"markdown","url":" http://Example.COM/docs?q=1 "}
    cancelled initially=false
    url=https://example.com/docs?q=1
    headers=user-agent: mentat/0 (+https://github.com/invariant-hq/mentat) | accept: text/markdown;q=1.0, text/x-markdown;q=0.9, text/plain;q=0.8, text/html;q=0.7, */*;q=0.1 | accept-language: en-US,en;q=0.9
    timeout_ms=4321 max_body_bytes=12345 private=true max_redirects=10
    status: completed
    output="Fetched https://example.com/docs?q=1\nStatus: 200\nContent-Type: text/plain; charset=utf-8\nBytes: 8\nDuration: 17ms\n\nresponse" truncated=false
    transport calls=1 |}]

let%expect_test "each format generates the exact credential-free request" =
  Eio_main.run @@ fun _ ->
  let transport =
    make_transport (fun ~cancelled:_ request ->
        print_request request;
        Ok
          (body ~content_type:"text/plain" ~contents:"ok"
             (Web.Transport.Request.url request |> Uri.to_string)))
  in
  let tool =
    make_tool transport (policy ~default_timeout_ms:111 ~max_timeout_ms:999 ())
  in
  let run_format label ?format ?timeout_ms () =
    Printf.printf "-- %s --\n" label;
    run tool (input ?format ?timeout_ms "https://example.com/resource")
    |> print_status
  in
  run_format "markdown default" ();
  run_format "text explicit timeout" ~format:"text" ~timeout_ms:999 ();
  run_format "html" ~format:"html" ();
  Printf.printf "transport calls=%d\n" transport.calls;
  [%expect
    {|
    -- markdown default --
    url=https://example.com/resource
    headers=user-agent: mentat/0 (+https://github.com/invariant-hq/mentat) | accept: text/markdown;q=1.0, text/x-markdown;q=0.9, text/plain;q=0.8, text/html;q=0.7, */*;q=0.1 | accept-language: en-US,en;q=0.9
    timeout_ms=111 max_body_bytes=1024 private=false max_redirects=10
    status: completed
    -- text explicit timeout --
    url=https://example.com/resource
    headers=user-agent: mentat/0 (+https://github.com/invariant-hq/mentat) | accept: text/plain;q=1.0, text/markdown;q=0.9, text/html;q=0.8, */*;q=0.1 | accept-language: en-US,en;q=0.9
    timeout_ms=999 max_body_bytes=1024 private=false max_redirects=10
    status: completed
    -- html --
    url=https://example.com/resource
    headers=user-agent: mentat/0 (+https://github.com/invariant-hq/mentat) | accept: text/html;q=1.0, application/xhtml+xml;q=0.9, text/plain;q=0.8, text/markdown;q=0.7, */*;q=0.1 | accept-language: en-US,en;q=0.9
    timeout_ms=111 max_body_bytes=1024 private=false max_redirects=10
    status: completed
    transport calls=3 |}]

let%expect_test "policy rejects inconsistent bounds and resolves call timeouts"
    =
  let make ~max_fetch_bytes ~max_output_chars ~default_timeout_ms
      ~max_timeout_ms =
    Web.Policy.make ~allow_private_network:false ~max_fetch_bytes
      ~max_output_chars ~default_timeout_ms ~max_timeout_ms ()
  in
  let verdict label result =
    Printf.printf "%s: %s\n" label
      (match result with
      | Ok _ -> "accepted"
      | Error error -> policy_error_name error)
  in
  verdict "valid"
    (make ~max_fetch_bytes:1 ~max_output_chars:1 ~default_timeout_ms:1
       ~max_timeout_ms:1);
  verdict "zero fetch bytes"
    (make ~max_fetch_bytes:0 ~max_output_chars:1 ~default_timeout_ms:1
       ~max_timeout_ms:1);
  verdict "negative fetch bytes"
    (make ~max_fetch_bytes:(-1) ~max_output_chars:1 ~default_timeout_ms:1
       ~max_timeout_ms:1);
  verdict "zero output chars"
    (make ~max_fetch_bytes:1 ~max_output_chars:0 ~default_timeout_ms:1
       ~max_timeout_ms:1);
  verdict "negative output chars"
    (make ~max_fetch_bytes:1 ~max_output_chars:(-1) ~default_timeout_ms:1
       ~max_timeout_ms:1);
  verdict "zero default timeout"
    (make ~max_fetch_bytes:1 ~max_output_chars:1 ~default_timeout_ms:0
       ~max_timeout_ms:1);
  verdict "zero maximum timeout"
    (make ~max_fetch_bytes:1 ~max_output_chars:1 ~default_timeout_ms:1
       ~max_timeout_ms:0);
  verdict "default exceeds maximum"
    (make ~max_fetch_bytes:1 ~max_output_chars:1 ~default_timeout_ms:2
       ~max_timeout_ms:1);
  let policy = policy ~default_timeout_ms:30 ~max_timeout_ms:120 () in
  let resolved label requested =
    Printf.printf "%s: %s\n" label
      (match Web.Policy.resolve_timeout_ms policy requested with
      | Ok timeout -> string_of_int timeout
      | Error error -> timeout_error_name error)
  in
  resolved "default" None;
  resolved "explicit zero" (Some 0);
  resolved "explicit negative" (Some (-1));
  resolved "explicit lower" (Some 1);
  resolved "explicit maximum" (Some 120);
  resolved "explicit above maximum" (Some 121);
  [%expect
    {|
    valid: accepted
    zero fetch bytes: non_positive(max_fetch_bytes=0)
    negative fetch bytes: non_positive(max_fetch_bytes=-1)
    zero output chars: non_positive(max_output_chars=0)
    negative output chars: non_positive(max_output_chars=-1)
    zero default timeout: non_positive(default_timeout_ms=0)
    zero maximum timeout: non_positive(max_timeout_ms=0)
    default exceeds maximum: default_exceeds_max(2>1)
    default: 30
    explicit zero: non_positive(0)
    explicit negative: non_positive(-1)
    explicit lower: 1
    explicit maximum: 120
    explicit above maximum: exceeds_max(121>120) |}]

let%expect_test "transport values reject authority, credential, and bound drift"
    =
  let at_limit = "https://example.com/" ^ String.make 1_980 'x' in
  let above_limit = at_limit ^ "x" in
  request_verdict "valid" ();
  request_verdict "URL at 2000 bytes" ~url:at_limit ();
  request_verdict "URL above 2000 bytes" ~url:above_limit ();
  request_verdict "relative URL" ~url:"/private" ();
  request_verdict "unsupported scheme" ~url:"file:///private" ();
  request_verdict "missing host" ~url:"https:///private" ();
  request_verdict "userinfo" ~url:"https://user:secret@example.com/" ();
  request_verdict "fragment" ~url:"https://example.com/#private" ();
  request_verdict "invalid port" ~url:"https://example.com:65536/" ();
  request_verdict "duplicate header"
    ~headers:[ ("Accept", "text/plain"); ("accept", "text/html") ]
    ();
  request_verdict "invalid header token" ~headers:[ ("bad name", "value") ] ();
  request_verdict "header line break" ~headers:[ ("accept", "a\r\nb") ] ();
  request_verdict "authorization"
    ~headers:[ ("Authorization", "Bearer secret") ]
    ();
  request_verdict "cookie" ~headers:[ ("Cookie", "secret=1") ] ();
  request_verdict "proxy authorization"
    ~headers:[ ("Proxy-Authorization", "secret") ]
    ();
  request_verdict "API key" ~headers:[ ("X-Api-Key", "secret") ] ();
  request_verdict "zero timeout" ~timeout_ms:0 ();
  request_verdict "zero body bound" ~max_body_bytes:0 ();
  request_verdict "negative redirects" ~max_redirects:(-1) ();
  request_verdict "redirect maximum" ~max_redirects:10 ();
  request_verdict "above redirect maximum" ~max_redirects:11 ();
  head_verdict "valid response head" ();
  head_verdict "informational lower bound" ~status:100 ();
  head_verdict "status below range" ~status:99 ();
  head_verdict "status above range" ~status:600 ();
  head_verdict "negative duration" ~duration_ms:(-1) ();
  head_verdict "content type line break" ~content_type:"text/plain\nsecret" ();
  head_verdict "effective userinfo" ~effective_url:"https://user@example.com/"
    ();
  head_verdict "effective fragment" ~effective_url:"https://example.com/#secret"
    ();
  [%expect
    {|
    valid: accepted
    URL at 2000 bytes: accepted
    URL above 2000 bytes: rejected
    relative URL: rejected
    unsupported scheme: rejected
    missing host: rejected
    userinfo: rejected
    fragment: rejected
    invalid port: rejected
    duplicate header: rejected
    invalid header token: rejected
    header line break: rejected
    authorization: rejected
    cookie: rejected
    proxy authorization: rejected
    API key: rejected
    zero timeout: rejected
    zero body bound: rejected
    negative redirects: rejected
    redirect maximum: accepted
    above redirect maximum: rejected
    valid response head: accepted
    informational lower bound: accepted
    status below range: rejected
    status above range: rejected
    negative duration: rejected
    content type line break: rejected
    effective userinfo: rejected
    effective fragment: rejected |}]

let%expect_test "every transport failure lowers to its stable tool status" =
  Eio_main.run @@ fun _ ->
  let run_error label error =
    Printf.printf "-- %s --\n" label;
    let transport = make_transport (fun ~cancelled:_ _ -> Error error) in
    let result =
      run (make_tool transport (policy ())) (input "https://example.com/")
    in
    print_status result;
    print_optional_output result
  in
  let ok_head = head ~status:200 "https://example.com/" in
  let missing_head = head ~status:404 "https://example.com/missing" in
  let redirect_head = head ~status:302 "https://example.com/loop" in
  run_error "cancelled" Web.Transport.Cancelled;
  run_error "timed out" Web.Transport.Timed_out;
  run_error "private address"
    (Web.Transport.Private_address
       { url = Uri.of_string "https://example.com/"; address = "127.0.0.1" });
  run_error "Content-Length preflight"
    (Web.Transport.Response_too_large
       { head = ok_head; limit = 1_024; received = 0 });
  run_error "streaming HTTP-error overflow"
    (Web.Transport.Response_too_large
       { head = missing_head; limit = 1_024; received = 1_025 });
  run_error "too many redirects"
    (Web.Transport.Too_many_redirects { head = redirect_head; limit = 10 });
  run_error "protocol without head"
    (Web.Transport.Protocol { head = None; diagnostic = "malformed chunk" });
  run_error "protocol with head"
    (Web.Transport.Protocol
       { head = Some missing_head; diagnostic = "early EOF" });
  run_error "backend unavailable" (Web.Transport.Transport "TLS handshake");
  print_endline "-- callback exception --";
  let raising = make_transport (fun ~cancelled:_ _ -> failwith "boom") in
  let raised_result =
    run (make_tool raising (policy ())) (input "https://example.com/")
  in
  print_status raised_result;
  print_optional_output raised_result;
  print_endline "-- malformed negative byte observation --";
  let malformed =
    make_transport (fun ~cancelled:_ _ ->
        Error
          (Web.Transport.Response_too_large
             { head = missing_head; limit = 1_024; received = -1 }))
  in
  let malformed_result =
    run (make_tool malformed (policy ())) (input "https://example.com/")
  in
  (match Tool.Result.status malformed_result with
  | Tool.Result.Failed { kind; _ } ->
      Printf.printf "status: failed %s\n" (failure_name kind)
  | Tool.Result.Completed -> print_endline "status: completed"
  | Tool.Result.Interrupted _ -> print_endline "status: interrupted");
  print_optional_output malformed_result;
  [%expect
    {|
    -- cancelled --
    status: interrupted cancelled=true
    reason: "tool call cancelled"
    output: none
    -- timed out --
    status: failed timed_out
    message: "web fetch timed out"
    metadata: false
    output: none
    -- private address --
    status: failed permission_denied
    message: "web fetch resolved to a disallowed address: 127.0.0.1"
    metadata: false
    output: none
    -- Content-Length preflight --
    status: failed failed
    message: "Response exceeded the 1024-byte limit after 0 bytes."
    metadata: false
    output: none
    -- streaming HTTP-error overflow --
    status: failed failed
    message: "Response exceeded the 1024-byte limit after 1025 bytes."
    metadata: false
    output: http_error status=404 bytes=1025 truncated=true
    -- too many redirects --
    status: failed failed
    message: "web fetch exceeded the 10-redirect limit"
    metadata: false
    output: none
    -- protocol without head --
    status: failed failed
    message: "web protocol failure: malformed chunk"
    metadata: false
    output: none
    -- protocol with head --
    status: failed failed
    message: "web protocol failure: early EOF"
    metadata: false
    output: none
    -- backend unavailable --
    status: failed unavailable
    message: "web transport failure: TLS handshake"
    metadata: false
    output: none
    -- callback exception --
    status: failed unavailable
    message: "web transport raised: Failure(\"boom\")"
    metadata: false
    output: none
    -- malformed negative byte observation --
    status: failed failed
    output: none |}]

let%expect_test "cancellation before and after transport never publishes a body"
    =
  Eio_main.run @@ fun _ ->
  let before = make_transport no_response in
  let before_result =
    run
      ~cancelled:(fun () -> true)
      (make_tool before (policy ()))
      (input "https://example.com/")
  in
  print_endline "-- before transport --";
  print_status before_result;
  print_optional_output before_result;
  Printf.printf "transport calls=%d\n" before.calls;
  let cancelled = ref false in
  let after =
    make_transport (fun ~cancelled:poll request ->
        Printf.printf "callback initially cancelled=%b\n" (poll ());
        cancelled := true;
        Ok
          (body ~content_type:"text/plain" ~contents:"must not escape"
             (Web.Transport.Request.url request |> Uri.to_string)))
  in
  let after_result =
    run
      ~cancelled:(fun () -> !cancelled)
      (make_tool after (policy ()))
      (input "https://example.com/")
  in
  print_endline "-- after transport --";
  print_status after_result;
  print_optional_output after_result;
  Printf.printf "transport calls=%d\n" after.calls;
  [%expect
    {|
    -- before transport --
    status: interrupted cancelled=true
    reason: "tool call cancelled"
    output: none
    transport calls=0
    callback initially cancelled=false
    -- after transport --
    status: interrupted cancelled=true
    reason: "tool call cancelled"
    output: none
    transport calls=1 |}]

let%expect_test
    "external diagnostics are repaired, scrubbed, and scalar-bounded" =
  Eio_main.run @@ fun _ ->
  let result_of_error error =
    let transport = make_transport (fun ~cancelled:_ _ -> Error error) in
    run (make_tool transport (policy ())) (input "https://example.com/")
  in
  let dirty =
    result_of_error
      (Web.Transport.Transport
         "\027[31mred\027[0m|\027]0;window title\007tail\rline\001\xff")
  in
  let dirty_message = failure_message_exn dirty in
  Printf.printf "dirty=%S\n" dirty_message;
  Printf.printf
    "dirty valid=%b CSI/OSC gone=%b CR gone=%b NUL gone=%b repaired=%b\n"
    (String.is_valid_utf_8 dirty_message)
    (not (String.contains dirty_message '\027'))
    (not (String.contains dirty_message '\r'))
    (not (String.contains dirty_message '\x00'))
    (contains ~needle:"\xef\xbf\xbd" dirty_message);
  let empty =
    result_of_error (Web.Transport.Transport "\027[31m\027[0m\r\001")
  in
  Printf.printf "empty=%S\n" (failure_message_exn empty);
  let unicode =
    List.init 3_000 (fun _ -> "é") |> String.concat "" |> fun diagnostic ->
    result_of_error (Web.Transport.Transport diagnostic)
  in
  let unicode_message = failure_message_exn unicode in
  Printf.printf "bounded bytes=%d within_limit=%b valid=%b\n"
    (String.length unicode_message)
    (String.length unicode_message <= 4_096)
    (String.is_valid_utf_8 unicode_message);
  let raising =
    make_transport (fun ~cancelled:_ _ ->
        failwith "\027[32mboom\027[0m\r\001\xff")
  in
  let raised =
    run (make_tool raising (policy ())) (input "https://example.com/")
  in
  let raised_message = failure_message_exn raised in
  Printf.printf
    "exception valid=%b ANSI gone=%b CR gone=%b bounded=%b nonempty=%b\n"
    (String.is_valid_utf_8 raised_message)
    (not (String.contains raised_message '\027'))
    (not (String.contains raised_message '\r'))
    (String.length raised_message <= 4_096)
    (not (String.is_empty raised_message));
  [%expect
    {|
    dirty="web transport failure: red|tail\nline \239\191\189"
    dirty valid=true CSI/OSC gone=true CR gone=true NUL gone=true repaired=true
    empty="web transport failure: web fetch failed"
    bounded bytes=4095 within_limit=true valid=true
    exception valid=true ANSI gone=true CR gone=true bounded=true nonempty=true |}]

let%expect_test "effective URLs and redirects cannot widen exact authority" =
  Eio_main.run @@ fun _ ->
  let run_response ?(show_text = false) label response =
    Printf.printf "-- %s --\n" label;
    let transport = make_transport (fun ~cancelled:_ _ -> Ok response) in
    let result =
      run (make_tool transport (policy ())) (input "http://example.com/start")
    in
    print_status result;
    print_optional_output result;
    if show_text then
      match Tool.Result.output result with
      | None -> ()
      | Some output -> Printf.printf "text=%S\n" (Tool.Output.text output)
  in
  run_response "same authority final body"
    (body ~content_type:"text/plain" ~contents:"followed"
       "https://example.com:443/final");
  run_response "cross-authority effective body"
    (body ~content_type:"text/plain" "https://other.example/final");
  run_response "www effective body is distinct"
    (body ~content_type:"text/plain" "https://www.example.com/final");
  run_response "different-scheme effective body"
    (body ~content_type:"text/plain" "http://example.com/final");
  run_response "different-port effective body"
    (body ~content_type:"text/plain" "https://example.com:8443/final");
  let redirect_head = head ~status:302 "https://example.com/current" in
  run_response ~show_text:true "cross-authority redirect"
    (Web.Transport.Redirect
       {
         head = redirect_head;
         target = Uri.of_string "https://other.example/a";
       });
  run_response "www redirect is cross-authority"
    (Web.Transport.Redirect
       {
         head = redirect_head;
         target = Uri.of_string "https://www.example.com/a";
       });
  run_response "private redirect is reported for a new call"
    (Web.Transport.Redirect
       { head = redirect_head; target = Uri.of_string "https://127.0.0.1/a" });
  run_response "same-authority redirect violates transport contract"
    (Web.Transport.Redirect
       {
         head = redirect_head;
         target = Uri.of_string "https://example.com/next";
       });
  run_response "explicit default port remains same authority"
    (Web.Transport.Redirect
       {
         head = redirect_head;
         target = Uri.of_string "https://example.com:443/next";
       });
  run_response "redirect with non-3xx head"
    (Web.Transport.Redirect
       {
         head = head ~status:200 "https://example.com/current";
         target = Uri.of_string "https://other.example/a";
       });
  run_response "redirect target with fragment"
    (Web.Transport.Redirect
       {
         head = redirect_head;
         target = Uri.of_string "https://other.example/a#secret";
       });
  run_response "redirect target with userinfo"
    (Web.Transport.Redirect
       {
         head = redirect_head;
         target = Uri.of_string "https://user:secret@other.example/a";
       });
  run_response "3xx body is an observed HTTP error"
    (body ~status:302 ~content_type:"text/plain" ~contents:"no location"
       "https://example.com/current");
  run_response "empty 204 body is a successful observation"
    (body ~status:204 ~content_type:"text/plain" ~contents:""
       "https://example.com/final");
  [%expect
    {|
    -- same authority final body --
    status: completed
    output: fetched status=200 bytes=8 truncated=false
    -- cross-authority effective body --
    status: failed failed
    message: "transport crossed the permitted network authority"
    metadata: false
    output: none
    -- www effective body is distinct --
    status: failed failed
    message: "transport crossed the permitted network authority"
    metadata: false
    output: none
    -- different-scheme effective body --
    status: failed failed
    message: "transport crossed the permitted network authority"
    metadata: false
    output: none
    -- different-port effective body --
    status: failed failed
    message: "transport crossed the permitted network authority"
    metadata: false
    output: none
    -- cross-authority redirect --
    status: completed
    output: redirected status=302 bytes=0 truncated=false
    text="Redirect 302 from https://example.com/current to https://other.example/a\nFetch the target URL in a new web_fetch call so its network authority can be reviewed."
    -- www redirect is cross-authority --
    status: completed
    output: redirected status=302 bytes=0 truncated=false
    -- private redirect is reported for a new call --
    status: completed
    output: redirected status=302 bytes=0 truncated=false
    -- same-authority redirect violates transport contract --
    status: failed failed
    message: "transport returned a same-authority redirect instead of following it"
    metadata: false
    output: none
    -- explicit default port remains same authority --
    status: failed failed
    message: "transport returned a same-authority redirect instead of following it"
    metadata: false
    output: none
    -- redirect with non-3xx head --
    status: failed failed
    message: "transport returned a redirect with a non-3xx status"
    metadata: false
    output: none
    -- redirect target with fragment --
    status: failed failed
    message: "transport returned an invalid redirect URL"
    metadata: false
    output: none
    -- redirect target with userinfo --
    status: failed failed
    message: "transport returned an invalid redirect URL"
    metadata: false
    output: none
    -- 3xx body is an observed HTTP error --
    status: failed failed
    message: "web server returned HTTP status 302"
    metadata: false
    output: http_error status=302 bytes=11 truncated=false
    -- empty 204 body is a successful observation --
    status: completed
    output: fetched status=204 bytes=0 truncated=false |}]

let%expect_test "MIME, charset, UTF-8, and format projection are explicit" =
  Eio_main.run @@ fun _ ->
  let run_body ?(max_fetch_bytes = 1_024) label ?format ?content_type contents =
    Printf.printf "-- %s --\n" label;
    let transport =
      make_transport (fun ~cancelled:_ _ ->
          Ok (body ?content_type ~contents "https://example.com/resource"))
    in
    let result =
      run
        (make_tool transport (policy ~max_fetch_bytes ()))
        (input ?format "https://example.com/resource")
    in
    print_status result;
    match Tool.Result.output result with
    | None -> print_endline "output: none"
    | Some output ->
        let fetch = fetch_output_exn result in
        Printf.printf "output: %s status=%d bytes=%d truncated=%b\n"
          (disposition_name (Fetch.disposition fetch))
          (Fetch.status fetch) (Fetch.bytes fetch)
          (Tool.Output.truncated output);
        Printf.printf "projection=%S\n"
          (after_headers (Tool.Output.text output))
  in
  run_body "unspecified MIME" "plain";
  run_body "vendor JSON" ~content_type:"application/problem+json; charset=UTF-8"
    "{\"ok\":true}";
  run_body "XML with quoted ASCII"
    ~content_type:"application/xml; charset=\"US-ASCII\"" "<ok/>";
  run_body "JavaScript" ~content_type:"application/javascript" "let x = 1;";
  run_body "unsupported binary MIME" ~content_type:"application/octet-stream"
    "bytes";
  run_body "unsupported charset" ~content_type:"text/plain; charset=iso-8859-1"
    "plain";
  run_body "invalid UTF-8" ~content_type:"text/plain" "good\xffbad";
  run_body "transport violated byte bound" ~max_fetch_bytes:4
    ~content_type:"text/plain" "12345";
  run_body "ANSI stripped from plain body" ~content_type:"text/plain"
    "safe \027[31mred\027[0m text";
  let html =
    "<html><head><meta \
     name=secret><style>hidden{}</style></head><body><h1>Title</h1><p>Hello \
     &amp; \
     goodbye.</p><script>steal()</script><ul><li>One</li></ul></body></html>"
  in
  run_body "HTML as Markdown" ~format:"markdown"
    ~content_type:"text/html; charset=utf-8" html;
  run_body "HTML as visible text" ~format:"text"
    ~content_type:"text/html; charset=utf8" html;
  run_body "HTML sanitized" ~format:"html" ~content_type:"application/xhtml+xml"
    html;
  run_body "stray container closers retain visible content" ~format:"html"
    ~content_type:"text/html"
    "</script><p>visible</p></style><p>still visible</p>";
  [%expect
    {|
    -- unspecified MIME --
    status: completed
    output: fetched status=200 bytes=5 truncated=false
    projection="plain"
    -- vendor JSON --
    status: completed
    output: fetched status=200 bytes=11 truncated=false
    projection="{\"ok\":true}"
    -- XML with quoted ASCII --
    status: completed
    output: fetched status=200 bytes=5 truncated=false
    projection="<ok/>"
    -- JavaScript --
    status: completed
    output: fetched status=200 bytes=10 truncated=false
    projection="let x = 1;"
    -- unsupported binary MIME --
    status: failed failed
    message: "web response has unsupported non-text MIME type: application/octet-stream"
    metadata: false
    output: none
    -- unsupported charset --
    status: failed failed
    message: "web response has unsupported charset: iso-8859-1"
    metadata: false
    output: none
    -- invalid UTF-8 --
    status: failed failed
    message: "web response body is not valid UTF-8"
    metadata: false
    output: none
    -- transport violated byte bound --
    status: failed failed
    message: "web transport returned 5 bytes, exceeding the 4-byte response limit"
    metadata: false
    output: none
    -- ANSI stripped from plain body --
    status: completed
    output: fetched status=200 bytes=22 truncated=false
    projection="safe red text"
    -- HTML as Markdown --
    status: completed
    output: fetched status=200 bytes=166 truncated=false
    projection="# Title\n\nHello & goodbye.\n\n- One"
    -- HTML as visible text --
    status: completed
    output: fetched status=200 bytes=166 truncated=false
    projection="Title\n\nHello & goodbye.\n\nOne"
    -- HTML sanitized --
    status: completed
    output: fetched status=200 bytes=166 truncated=false
    projection="<html><head></head><body><h1>Title</h1><p>Hello &amp; goodbye.</p><ul><li>One</li></ul></body></html>"
    -- stray container closers retain visible content --
    status: completed
    output: fetched status=200 bytes=51 truncated=false
    projection="<p>visible</p><p>still visible</p>" |}]

let%expect_test "Unicode truncation is scalar-safe and durable" =
  Eio_main.run @@ fun _ ->
  let contents = "αβγδεζηθικλμνξοπρστυφχψω😀ABCDE" in
  let run_bound max_output_chars =
    let transport =
      make_transport (fun ~cancelled:_ _ ->
          Ok
            (body ~content_type:"text/plain; charset=utf-8" ~contents
               "https://example.com/unicode"))
    in
    run
      (make_tool transport (policy ~max_output_chars ()))
      (input "https://example.com/unicode")
  in
  let print_bound max_output_chars =
    Printf.printf "-- max chars %d --\n" max_output_chars;
    let result = run_bound max_output_chars in
    print_status result;
    let output = output_exn result in
    let semantic = fetch_output_exn result in
    let projection = after_headers (Tool.Output.text output) in
    Printf.printf "bytes=%d truncated=%b valid_utf8=%b\nprojection=%S\n"
      (Fetch.bytes semantic)
      (Tool.Output.truncated output)
      (String.is_valid_utf_8 projection)
      projection;
    result
  in
  ignore (print_bound 30);
  let middle = print_bound 25 in
  ignore (print_bound 3);
  let output = output_exn middle in
  Printf.printf "semantic json=%s\n"
    (match Tool.Output.json output with
    | Some json -> json_string json
    | None -> fail "web fetch output omitted compact JSON");
  let encoded, equal = result_roundtrip middle in
  Printf.printf "result roundtrip=%b durable_nonempty=%b\n" equal
    (not (String.is_empty (json_string encoded)));
  [%expect
    {|
    -- max chars 30 --
    status: completed
    bytes=57 truncated=false valid_utf8=true
    projection="\206\177\206\178\206\179\206\180\206\181\206\182\206\183\206\184\206\185\206\186\206\187\206\188\206\189\206\190\206\191\207\128\207\129\207\131\207\132\207\133\207\134\207\135\207\136\207\137\240\159\152\128ABCDE"
    -- max chars 25 --
    status: completed
    bytes=57 truncated=true valid_utf8=true
    projection="\206\177\206\178\206\179\n[... omitted ...]\nCDE\n\n[Output omitted 24 Unicode characters.]"
    -- max chars 3 --
    status: completed
    bytes=57 truncated=true valid_utf8=true
    projection="\206\177\206\178\206\179\n\n[Output omitted 27 Unicode characters.]"
    semantic json={"version":1,"disposition":"fetched","status":200,"bytes":57}
    result roundtrip=true durable_nonempty=true |}]

