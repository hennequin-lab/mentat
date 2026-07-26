(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let maximum_url_bytes = 2_000
let maximum_redirects = 10
let maximum_content_type_bytes = 4_096

let invalid scope message =
  invalid_arg ("Mentat_tools.Web.Transport." ^ scope ^ ": " ^ message)

let contains_line_control text =
  String.exists (function '\x00' | '\r' | '\n' -> true | _ -> false) text

let authority_has_userinfo uri =
  let text = Uri.to_string uri in
  match String.index_opt text ':' with
  | None -> false
  | Some colon ->
      let authority_start = colon + 1 in
      let length = String.length text in
      if
        authority_start + 1 >= length
        || (not (Char.equal text.[authority_start] '/'))
        || not (Char.equal text.[authority_start + 1] '/')
      then false
      else
        let rec loop index =
          if index >= length then false
          else
            match text.[index] with
            | '@' -> true
            | '/' | '?' | '#' -> false
            | _ -> loop (index + 1)
        in
        loop (authority_start + 2)

let validate_url scope uri =
  let text = Uri.to_string uri in
  if String.length text > maximum_url_bytes then
    invalid scope "URL must be at most 2000 bytes";
  if
    (not (String.is_valid_utf_8 text))
    || String.exists
         (fun byte ->
           let code = Char.code byte in
           code <= 0x20 || code = 0x7f)
         text
  then invalid scope "URL must contain valid encoded UTF-8 URI text";
  (match Uri.scheme uri with
  | Some ("http" | "https") -> ()
  | Some _ | None -> invalid scope "URL must use HTTP or HTTPS");
  (match Uri.host uri with
  | Some host
    when (not (String.is_empty host))
         && String.equal host (String.lowercase_ascii host)
         && String.for_all (fun byte -> Char.code byte < 128) host
         && not (String.ends_with ~suffix:"." host) ->
      ()
  | Some _ | None -> invalid scope "URL must have a host");
  if authority_has_userinfo uri then
    invalid scope "URL must not contain userinfo";
  if Option.is_some (Uri.fragment uri) then
    invalid scope "URL must not contain a fragment";
  match Uri.port uri with
  | Some port when port < 1 || port > 65_535 ->
      invalid scope "URL port must be between 1 and 65535"
  | Some _ | None -> ()

let token_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_' | '`'
  | '|' | '~' ->
      true
  | _ -> false

let generated_header = function
  | "user-agent" | "accept" | "accept-language" -> true
  | _ -> false

module Request = struct
  type meth = Get | Post

  type t = {
    meth : meth;
    url : Uri.t;
    headers : (string * string) list;
    body : string option;
    timeout_ms : int;
    max_body_bytes : int;
    allow_private_network : bool;
    max_redirects : int;
  }

  (* [web_fetch] passes no [extra_allowed_headers] and keeps the strict
     credential-free allowlist (User-Agent, Accept, Accept-Language). A caller
     that must send request-specific headers — [web_search]'s content type and
     search-service API key — widens the allowlist for that one request only, so
     one request's credential header never becomes another's. *)
  let validate_headers ~extra_allowed headers =
    let allowed normalized =
      generated_header normalized || List.mem normalized extra_allowed
    in
    let rec loop seen = function
      | [] -> ()
      | (name, value) :: rest ->
          if String.is_empty name || not (String.for_all token_char name) then
            invalid "Request.make" "header names must be HTTP tokens";
          if contains_line_control value then
            invalid "Request.make"
              "header values must not contain line breaks or NUL";
          let normalized = String.lowercase_ascii name in
          if not (allowed normalized) then
            invalid "Request.make"
              "header name is not in this request's allowed set";
          if List.mem normalized seen then
            invalid "Request.make" "header names must be unique";
          loop (normalized :: seen) rest
    in
    loop [] headers

  let make ?(meth = Get) ?body ?(extra_allowed_headers = []) ~url ~headers
      ~timeout_ms ~max_body_bytes ~allow_private_network ~max_redirects () =
    validate_url "Request.make" url;
    validate_headers
      ~extra_allowed:(List.map String.lowercase_ascii extra_allowed_headers)
      headers;
    (match (meth, body) with
    | Get, Some _ ->
        invalid "Request.make" "a GET request must not carry a body"
    | (Get | Post), _ -> ());
    if timeout_ms <= 0 then invalid "Request.make" "timeout_ms must be positive";
    if max_body_bytes <= 0 then
      invalid "Request.make" "max_body_bytes must be positive";
    if max_redirects < 0 || max_redirects > maximum_redirects then
      invalid "Request.make" "max_redirects must be between 0 and 10";
    {
      meth;
      url;
      headers;
      body;
      timeout_ms;
      max_body_bytes;
      allow_private_network;
      max_redirects;
    }

  let meth request = request.meth
  let url request = request.url
  let headers request = request.headers
  let body request = request.body
  let timeout_ms request = request.timeout_ms
  let max_body_bytes request = request.max_body_bytes
  let allow_private_network request = request.allow_private_network
  let max_redirects request = request.max_redirects
end

type response_head = {
  effective_url : Uri.t;
  status : int;
  content_type : string option;
  duration_ms : int;
}

let make_response_head ~effective_url ~status ?content_type ~duration_ms () =
  validate_url "make_response_head" effective_url;
  if status < 100 || status > 599 then
    invalid "make_response_head" "status must be between 100 and 599";
  if duration_ms < 0 then
    invalid "make_response_head" "duration_ms must be non-negative";
  (match content_type with
  | Some content_type when contains_line_control content_type ->
      invalid "make_response_head"
        "content_type must not contain line breaks or NUL"
  | Some content_type
    when String.length content_type > maximum_content_type_bytes ->
      invalid "make_response_head" "content_type must be at most 4096 bytes"
  | Some _ | None -> ());
  { effective_url; status; content_type; duration_ms }

type response =
  | Body of { head : response_head; body : string }
  | Redirect of { head : response_head; target : Uri.t }

type error =
  | Cancelled
  | Timed_out
  | Private_address of { url : Uri.t; address : string }
  | Response_too_large of { head : response_head; limit : int; received : int }
  | Too_many_redirects of { head : response_head; limit : int }
  | Protocol of { head : response_head option; diagnostic : string }
  | Transport of string

type t =
  sw:Eio.Switch.t ->
  cancelled:(unit -> bool) ->
  Request.t ->
  (response, error) result

let error_message = function
  | Cancelled -> "web fetch was cancelled"
  | Timed_out -> "web fetch timed out"
  | Private_address { address; _ } ->
      "web fetch resolved to a disallowed address: " ^ address
  | Response_too_large { limit; received; _ } ->
      Printf.sprintf "web response exceeded the %d-byte limit after %d bytes"
        limit received
  | Too_many_redirects { limit; _ } ->
      Printf.sprintf "web fetch exceeded the %d-redirect limit" limit
  | Protocol { diagnostic; _ } -> "web protocol failure: " ^ diagnostic
  | Transport diagnostic -> "web transport failure: " ^ diagnostic

let pp_error ppf error = Format.pp_print_string ppf (error_message error)
