(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let name = "web_search"
let user_agent = "mentat/0 (+https://github.com/invariant-hq/mentat)"
let maximum_diagnostic_bytes = 4_096
let default_num_results = 5
let max_num_results = 10
let json_int value = Jsont.Json.int value

module Input = struct
  type t = { query : string; num_results : int option }

  let make query num_results =
    Mentat_tool.Codec.decode_invalid_arg @@ fun () ->
    if String.is_empty query then invalid_arg "query must not be empty";
    if not (String.is_valid_utf_8 query) then
      invalid_arg "query must be valid UTF-8";
    if String.contains query '\x00' then
      invalid_arg "query must not contain NUL";
    (match num_results with
    | Some n when n <= 0 -> invalid_arg "num_results must be positive"
    | Some _ | None -> ());
    { query; num_results }

  let max_input_integer =
    Float.min 9_007_199_254_740_991. (float_of_int Int.max_int)

  let exact_integer =
    let decode = function
      | Jsont.Number (value, _)
        when Float.is_integer value && Float.abs value <= max_input_integer ->
          int_of_float value
      | Jsont.Number _ | Jsont.Null _ | Jsont.Bool _ | Jsont.String _
      | Jsont.Array _ | Jsont.Object _ ->
          Codec.decode_error "expected an integer in JSON's safe integer range"
    in
    Jsont.map ~kind:"integer" ~dec:decode ~enc:json_int Jsont.json

  let object_codec =
    Jsont.Object.map ~kind:"web_search input" make
    |> Jsont.Object.mem "query" Jsont.string ~enc:(fun input -> input.query)
    |> Jsont.Object.opt_mem "num_results" exact_integer ~enc:(fun input ->
        input.num_results)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec = Codec.strict_object ~kind:"strict web_search input" object_codec

  let schema =
    Codec.obj
      [
        ("type", Jsont.Json.string "object");
        ( "properties",
          Codec.obj
            [
              ( "query",
                Codec.obj
                  [
                    ("type", Jsont.Json.string "string");
                    ( "description",
                      Jsont.Json.string
                        "The search query. Include the current year for recent \
                         topics." );
                    ("minLength", json_int 1);
                  ] );
              ( "num_results",
                Codec.obj
                  [
                    ("type", Jsont.Json.string "integer");
                    ( "description",
                      Jsont.Json.string
                        "Maximum number of results to return, bounded by host \
                         policy." );
                    ("minimum", json_int 1);
                  ] );
            ] );
        ("required", Jsont.Json.list [ Jsont.Json.string "query" ]);
        ("additionalProperties", Jsont.Json.bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
end

(* Diagnostics carry untrusted backend text, so repair to UTF-8, strip ANSI,
   scrub control characters, trim, and bound before presenting. *)
let diagnostic text =
  let text =
    text |> Text_helpers.utf8_lossy |> Text_helpers.strip_ansi
    |> Text_helpers.scrub_controls |> String.trim
  in
  let text = if String.is_empty text then "web search failed" else text in
  Text_helpers.valid_utf8_prefix text maximum_diagnostic_bytes

let interrupted () = Mentat_tool.Result.cancelled ()
let failed kind message = Mentat_tool.Result.failed kind (diagnostic message)

(* UTF-8 character counting and truncation: bound the projected output by the
   policy's character budget without splitting a multi-byte character. *)
let utf8_length text =
  let rec loop index count =
    if index >= String.length text then count
    else
      let decoded = String.get_utf_8_uchar text index in
      loop (index + Uchar.utf_decode_length decoded) (count + 1)
  in
  loop 0 0

let utf8_prefix text max_chars =
  let rec loop index remaining =
    if remaining = 0 || index >= String.length text then index
    else
      let decoded = String.get_utf_8_uchar text index in
      loop (index + Uchar.utf_decode_length decoded) (remaining - 1)
  in
  String.sub text 0 (loop 0 max_chars)

let truncate_chars text max_chars =
  if utf8_length text <= max_chars then (text, false)
  else (utf8_prefix text max_chars, true)

(* Endpoint permission. *)

let endpoint_protocol endpoint =
  match Option.map String.lowercase_ascii (Uri.scheme endpoint) with
  | Some "http" -> `Http
  | Some _ | None -> `Https

let endpoint_port endpoint =
  match Uri.port endpoint with
  | Some port -> port
  | None -> (
      match endpoint_protocol endpoint with `Http -> 80 | `Https -> 443)

let permission endpoint =
  let host = Option.value (Uri.host endpoint) ~default:"" in
  [
    Mentat_permission.Request.of_accesses ~source:name
      [
        Mentat_permission.Access.network
          ~protocol:(endpoint_protocol endpoint)
          ~port:(endpoint_port endpoint) ~host ();
      ];
  ]

(* Output. *)

type output = { text : string; truncated : bool }

let encode_output { text; truncated } =
  Mentat_tool.Output.make ~text ~truncated
    ~json:(Codec.obj [ ("kind", Jsont.Json.string name) ])
    ()

let transport_error = function
  | Transport.Cancelled -> interrupted ()
  | Transport.Timed_out -> failed `Timed_out "web search timed out"
  | Transport.Private_address { address; _ } ->
      failed `Permission_denied
        ("web search resolved to a disallowed address: " ^ address)
  | Transport.Response_too_large { limit; received; _ } ->
      failed `Failed
        (Printf.sprintf
           "search response exceeded the %d-byte limit after %d bytes" limit
           received)
  | Transport.Too_many_redirects _ ->
      failed `Failed "search endpoint returned an unexpected redirect"
  | Transport.Protocol { diagnostic = message; _ } ->
      failed `Failed ("web protocol failure: " ^ message)
  | Transport.Transport message ->
      failed `Unavailable ("web transport failure: " ^ message)

(* The remote MCP server formats the result text; the tool sanitizes and bounds
   it. An empty answer is a valid empty search, not a failure. *)
let response_result policy head body =
  let status = head.Transport.status in
  if status < 200 || status > 299 then
    let preview, _ =
      truncate_chars (diagnostic body) (Policy.max_output_chars policy)
    in
    failed `Failed
      (Printf.sprintf "search endpoint returned HTTP status %d: %s" status
         preview)
  else if not (String.is_valid_utf_8 body) then
    failed `Failed "search response body is not valid UTF-8"
  else
    match Search_service.parse_response body with
    | Error message ->
        failed `Failed
          ("search response could not be read: " ^ diagnostic message)
    | Ok answer ->
        let answer =
          answer |> Text_helpers.utf8_lossy |> Text_helpers.strip_ansi
          |> String.trim
        in
        if String.is_empty answer then
          Mentat_tool.Result.completed
            ~output:{ text = "No results."; truncated = false }
            ()
        else
          let text, truncated =
            truncate_chars answer (Policy.max_output_chars policy)
          in
          Mentat_tool.Result.completed ~output:{ text; truncated } ()

let run policy ~backend ~api_key ~endpoint fetch ~cancelled input =
  if cancelled () then interrupted ()
  else
    let num_results =
      match input.Input.num_results with
      | Some n -> min n max_num_results
      | None -> default_num_results
    in
    let request =
      Transport.Request.make ~meth:Transport.Request.Post
        ~body:
          (Search_service.request_body backend ~query:input.Input.query
             ~num_results)
        ~extra_allowed_headers:[ "content-type"; "authorization" ]
        ~url:endpoint
        ~headers:(Search_service.headers backend ?api_key ~user_agent ())
        ~timeout_ms:(Policy.default_timeout_ms policy)
        ~max_body_bytes:(Policy.max_fetch_bytes policy)
        ~allow_private_network:(Policy.allow_private_network policy)
        ~max_redirects:0 ()
    in
    let result =
      match Eio.Switch.run (fun sw -> fetch ~sw ~cancelled request) with
      | result -> Ok result
      | exception (Eio.Cancel.Cancelled _ as cancelled) -> raise cancelled
      | exception exn -> Error (Printexc.to_string exn)
    in
    match result with
    | Error _ when cancelled () -> interrupted ()
    | Error message -> failed `Unavailable ("web transport raised: " ^ message)
    | Ok _ when cancelled () -> interrupted ()
    | Ok (Error error) -> transport_error error
    | Ok (Ok (Transport.Redirect { head; _ })) ->
        failed `Failed
          (Printf.sprintf
             "search endpoint returned an unexpected redirect (HTTP %d)"
             head.Transport.status)
    | Ok (Ok (Transport.Body { head; body })) ->
        response_result policy head body

let make ~policy ~backend ?api_key ~fetch () =
  let endpoint = Search_service.endpoint backend ?api_key () in
  Mentat_tool.make ~name ~description:Mentat_prompts.Tools.web_search
    ~input:Input.contract ~output:encode_output
    ~permissions:(fun _input -> permission endpoint)
    ~run:(fun ~cancelled input ->
      run policy ~backend ~api_key ~endpoint fetch ~cancelled input)
    ()
