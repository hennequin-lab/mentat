(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Llm = Mentat_llm
module Chat_completions = Mentat_llm_http.Chat_completions
module Messages = Mentat_llm_http.Messages

let provider = Llm.Provider.make "opencode-go"
let chat_model id = Llm.Model.make ~provider ~api:Chat_completions.api ~id
let messages_model id = Llm.Model.make ~provider ~api:Messages.api ~id

let invalid fn message =
  invalid_arg ("Mentat_llm_opencode." ^ fn ^ ": " ^ message)

let contains_newline value =
  String.exists (function '\n' | '\r' -> true | _ -> false) value

module Config = struct
  type t = {
    base_url : string;
    timeout_s : float;
    max_retries : int option;
    max_stream_retries : int option;
  }

  let default_base_url = "https://opencode.ai/zen/go"
  let default_timeout_s = 1800.

  let check_max_retries field = function
    | Some retries when retries < 0 ->
        invalid "Config.make" (field ^ " must not be negative")
    | Some _ | None -> ()

  let make ?(base_url = default_base_url) ?(timeout_s = default_timeout_s)
      ?max_retries ?max_stream_retries () =
    if String.is_empty base_url then
      invalid "Config.make" "base_url must not be empty";
    if contains_newline base_url then
      invalid "Config.make" "base_url must not contain newline";
    let base_url = String.drop_last_while (Char.equal '/') base_url in
    if String.is_empty base_url then
      invalid "Config.make" "base_url must not be only slashes";
    if (not (Float.is_finite timeout_s)) || timeout_s <= 0. then
      invalid "Config.make" "timeout_s must be positive and finite";
    check_max_retries "max_retries" max_retries;
    check_max_retries "max_stream_retries" max_stream_retries;
    { base_url; timeout_s; max_retries; max_stream_retries }

  let default = make ()
  let base_url t = t.base_url
  let timeout_s t = t.timeout_s
  let max_retries t = t.max_retries
  let max_stream_retries t = t.max_stream_retries
end

module Credential = struct
  type t = Api_key of string | Bearer of string

  let check fn value =
    if String.is_empty value then invalid fn "value must not be empty";
    if contains_newline value then invalid fn "value must not contain newline"

  let api_key key =
    check "Credential.api_key" key;
    Api_key key

  let bearer token =
    check "Credential.bearer" token;
    Bearer token

  (* Both kinds carry one secret; the header spelling is the route's. The
     chat-completions endpoint reads [Authorization: Bearer]; the messages
     endpoint reads the dialect's [x-api-key]. *)
  let header = function
    | Api_key value | Bearer value -> ("authorization", "Bearer " ^ value)

  let messages_header = function
    | Api_key value | Bearer value -> ("x-api-key", value)
end

(* The gateway requires [x-opencode-session] — one stable id per conversation —
   on every request and keys its routing optimizations on it. A conversation
   request carries that identity already: the agent stamps each with its
   session id as [Llm.Request.cache_key], so that id rides the header. A
   request with no cache key — an account check, a one-off completion — is not
   a conversation, so it rides the process-stable fallback id, minted on first
   use and shared by every such request. *)
let uuid_v4 () =
  Mirage_crypto_rng_unix.use_default ();
  let bytes = Bytes.of_string (Mirage_crypto_rng.generate 16) in
  (* Version 4, variant 1. *)
  Bytes.set bytes 6
    (Char.chr ((Char.code (Bytes.get bytes 6) land 0x0f) lor 0x40));
  Bytes.set bytes 8
    (Char.chr ((Char.code (Bytes.get bytes 8) land 0x3f) lor 0x80));
  let hex = "0123456789abcdef" in
  let buffer = Buffer.create 36 in
  let add_byte index =
    let value = Char.code (Bytes.get bytes index) in
    Buffer.add_char buffer hex.[value lsr 4];
    Buffer.add_char buffer hex.[value land 0xf]
  in
  let group first last =
    for index = first to last do
      add_byte index
    done
  in
  group 0 3;
  Buffer.add_char buffer '-';
  group 4 5;
  Buffer.add_char buffer '-';
  group 6 7;
  Buffer.add_char buffer '-';
  group 8 9;
  Buffer.add_char buffer '-';
  group 10 15;
  Buffer.contents buffer

let fallback_session_id = lazy (uuid_v4 ())

let session_id request =
  Option.value (Llm.Request.cache_key request)
    ~default:(Lazy.force fallback_session_id)

let session_header request = ("x-opencode-session", session_id request)

let fallback_session_header () =
  ("x-opencode-session", Lazy.force fallback_session_id)

(* The gateway disambiguates failures only in [error.type]: usage limits
   arrive as 429 and account problems — a lapsed subscription, an unknown
   model — as 401, so the status alone misclassifies both. The classifier
   reads the one token that tells them apart, and the terminal predicate stops
   the retry ladder on a limit no retry can outwait. *)

let error_type body =
  match Jsont_bytesrw.decode_string Jsont.json body with
  | Error _ -> None
  | Ok (Jsont.Object (fields, _)) -> (
      match Jsont.Json.find_mem "error" fields with
      | Some (_, Jsont.Object (error_fields, _)) -> (
          match Jsont.Json.find_mem "type" error_fields with
          | Some (_, Jsont.String (value, _)) -> Some value
          | Some _ | None -> None)
      | Some _ | None -> None)
  | Ok _ -> None

let quota_type = function
  | "GoUsageLimitError" | "FreeUsageLimitError" | "BlackUsageLimitError"
  | "CreditsError" | "MonthlyLimitError" | "UserLimitError" ->
      true
  | _ -> false

let quota_exhausted ~body =
  match error_type body with
  | Some type_ -> quota_type type_
  | None -> false

let classify_error ~status:_ ~body =
  match error_type body with
  | Some type_ when quota_type type_ -> Some Llm.Error.Quota
  | Some "ModelError" -> Some Llm.Error.Invalid_request
  | Some _ | None -> None

let terminal (response : Mentat_llm_http.response) =
  quota_exhausted ~body:response.Mentat_llm_http.body

let run_chat config credential ~env ~cancelled ~on_event request =
  Eio.Switch.run ~name:"opencode.request" @@ fun sw ->
  let endpoint =
    Chat_completions.make ~provider
      ~headers:
        [
          Credential.header credential;
          ("user-agent", "mentat-llm-opencode/0");
          session_header request;
        ]
      ~timeout_s:(Config.timeout_s config)
      ?max_retries:(Config.max_retries config)
      ?max_stream_retries:(Config.max_stream_retries config)
      ~terminal ~classify:classify_error
      ~base_url:(Config.base_url config) ~sw ~env ()
  in
  Chat_completions.run endpoint ~cancelled ~on_event request

(* The gateway's messages route honors cache breakpoints and accepts sampling
   parameters unconditionally, unlike the dialect's first party. *)
let run_messages config credential ~env ~cancelled ~on_event request =
  Eio.Switch.run ~name:"opencode.request" @@ fun sw ->
  let endpoint =
    Messages.make ~provider
      ~headers:[ Credential.messages_header credential; session_header request ]
      ~timeout_s:(Config.timeout_s config)
      ?max_retries:(Config.max_retries config)
      ?max_stream_retries:(Config.max_stream_retries config)
      ~terminal ~classify:classify_error ~cache:true ~sampling:true
      ~endpoint:(Config.base_url config ^ "/v1/messages")
      ~sw ~env ()
  in
  Messages.run endpoint ~cancelled ~on_event request

let arms config credential =
  [
    (Chat_completions.api, run_chat config credential);
    (Messages.api, run_messages config credential);
  ]

let client ~env ?(config = Config.default) ~credential () =
  let arms = arms config credential in
  let accepts model =
    Llm.Provider.equal provider (Llm.Model.provider model)
    && List.mem_assoc (Llm.Model.api model) arms
  in
  let run ~cancelled ~on_event request =
    match List.assoc_opt (Llm.Model.api (Llm.Request.model request)) arms with
    | Some arm -> arm ~env ~cancelled ~on_event request
    | None -> assert false (* accepts derives from the same list *)
  in
  Llm.Client.make ~provider ~accepts ~run ()
