(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Effectful HTTP boundary used by [web_fetch].

    This module describes a complete, credential-free fetch request and the
    observations a transport may return. It intentionally contains no HTTP
    client, DNS resolver, TLS configuration, clock, or network handle. Those
    capabilities and their lifecycle remain owned by the callback.

    The transport must resolve and vet every address immediately before
    connecting, connect only to the vetted address, enforce the body and
    redirect limits while streaming, and use the supplied switch for all
    resources. *)

module Request : sig
  type meth =
    | Get
    | Post
        (** The request method. [Get] is the credential-free public fetch;
            [Post] carries a body and is used by the authenticated search call.
        *)

  type t
  (** The type for a validated transport request.

      Requests use an absolute HTTP(S) URL of at most 2,000 bytes with a host,
      no user information, and no fragment. Headers default to the strict
      credential-free set — one each of [User-Agent], [Accept], and
      [Accept-Language] — with no line breaks; a caller may widen the allowlist
      per request via [extra_allowed_headers]. Timeouts and body limits are
      positive; redirect limits are in [[0];[10]]. *)

  val make :
    ?meth:meth ->
    ?body:string ->
    ?extra_allowed_headers:string list ->
    url:Uri.t ->
    headers:(string * string) list ->
    timeout_ms:int ->
    max_body_bytes:int ->
    allow_private_network:bool ->
    max_redirects:int ->
    unit ->
    t
  (** [make ~url ~headers ~timeout_ms ~max_body_bytes ~allow_private_network
       ~max_redirects ()] is a complete transport request.

      [meth] defaults to {!Get}. [body], allowed only with {!Post}, is the
      request entity. [extra_allowed_headers] names additional header keys this
      one request may carry beyond the default credential-free set — for example
      a content type and a search-service API key — so widening one request's
      allowlist never widens another's. Header keys are matched
      case-insensitively.

      Raises [Invalid_argument] when an invariant of {!t} is violated (including
      a body on a {!Get}). This constructor is for trusted tool code after input
      and policy validation; invalid construction is a programming error, not a
      transport failure. *)

  val meth : t -> meth
  (** [meth request] is the request method. *)

  val url : t -> Uri.t
  (** [url request] is the normalized URL to fetch. *)

  val headers : t -> (string * string) list
  (** [headers request] are the generated request headers. *)

  val body : t -> string option
  (** [body request] is the request entity, present only for a {!Post}. *)

  val timeout_ms : t -> int
  (** [timeout_ms request] is the total request deadline in milliseconds. *)

  val max_body_bytes : t -> int
  (** [max_body_bytes request] is the response-body byte limit. *)

  val allow_private_network : t -> bool
  (** [allow_private_network request] is the address-admission policy the host
      must apply to every DNS result and direct IP literal. *)

  val max_redirects : t -> int
  (** [max_redirects request] is the maximum number of same-authority redirects
      the host may follow. *)
end

type response_head = private {
  effective_url : Uri.t;
  status : int;
  content_type : string option;
  duration_ms : int;
}
(** Validated facts observed before or while consuming a response.

    The effective URL is credential-free HTTP(S), has a host and no fragment,
    and is at most 2,000 bytes. Status is in [[100];[599]], duration is
    non-negative, and content type is at most 4,096 bytes and contains no
    control line breaks. *)

val make_response_head :
  effective_url:Uri.t ->
  status:int ->
  ?content_type:string ->
  duration_ms:int ->
  unit ->
  response_head
(** [make_response_head ~effective_url ~status ?content_type ~duration_ms ()] is
    an observed response head.

    Raises [Invalid_argument] when an invariant of {!response_head} is violated.
    A host adapter should turn malformed backend state into {!Protocol} instead
    of constructing such a value. *)

type response =
  | Body of { head : response_head; body : string }
      (** A final response and its complete bounded body. *)
  | Redirect of { head : response_head; target : Uri.t }
      (** A cross-authority redirect that was not followed. The target is
          validated again by the tool before it is shown or requested later. *)

type error =
  | Cancelled
  | Timed_out
  | Private_address of { url : Uri.t; address : string }
      (** Address policy rejected [address] while resolving [url]. *)
  | Response_too_large of { head : response_head; limit : int; received : int }
      (** The response body exceeded [limit]. [received] is the number of body
          bytes consumed before refusal: it is [0] for a [Content-Length]
          preflight refusal and may exceed [limit] while streaming. *)
  | Too_many_redirects of { head : response_head; limit : int }
  | Protocol of { head : response_head option; diagnostic : string }
      (** The peer or backend violated an HTTP or transport invariant. *)
  | Transport of string
      (** Connection, TLS, DNS, or other backend failure before a usable
          protocol observation. *)

type t =
  sw:Eio.Switch.t ->
  cancelled:(unit -> bool) ->
  Request.t ->
  (response, error) result
(** The type for a fetch callback.

    The callback owns DNS, private-address enforcement, connection pinning, TLS
    verification, HTTP parsing, same-authority redirect following, body
    streaming, timeout enforcement, and cancellation. It must allocate all
    per-call resources beneath [sw]. It must never forward credentials or
    ambient cookies. *)

val error_message : error -> string
(** [error_message error] is a human-readable backend diagnostic. It may contain
    untrusted transport text; callers must sanitize and bound it before
    presenting it to the model or a terminal. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf error] formats [error] with {!error_message}. *)
