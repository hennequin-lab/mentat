(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** OpenCode Go provider adapter.

    This library interprets {!Mentat_llm.Request.t} values against the OpenCode
    Go gateway. The gateway assigns each model one of several wire protocols:
    most ride its OpenAI-compatible chat-completions endpoint, some its
    Anthropic-compatible messages endpoint, and a model's protocol family is
    part of its identity — construct it with the matching constructor. The
    gateway owns the model set: ids are whatever it serves (e.g. ["kimi-k3"]),
    spelled exactly as OpenCode publishes them. Mentat's provider-neutral
    request, event, and response types remain the public boundary.

    The client connects to {!Config.make}'s [base_url]
    ([https://opencode.ai/zen/go] by default) — the bare gateway root, under
    whose [/v1] path both endpoints live. Every request authenticates with a
    {!Credential.t}; the chat-completions route reads it as a bearer
    authorization header and the messages route as the dialect's [x-api-key].
    A request for a model the gateway does not serve fails at request time
    with the gateway's own error. *)

val provider : Mentat_llm.Provider.t
(** [provider] is the [opencode-go] provider namespace. *)

val chat_model : string -> Mentat_llm.Model.t
(** [chat_model id] is chat-completions model [id] under {!provider}. *)

val messages_model : string -> Mentat_llm.Model.t
(** [messages_model id] is messages-protocol model [id] under {!provider}. *)

val quota_exhausted : body:string -> bool
(** [quota_exhausted ~body] is [true] iff [body] is a gateway error whose
    [error.type] names a usage-limit or balance condition no retry can
    outwait. The gateway answers those with retryable statuses, so this one
    predicate is both the client's retry-terminal rule and the account
    check's quota lowering — one definition, so the two readings cannot
    drift. *)

module Config : sig
  type t
  (** Connection configuration.

      [base_url] is the gateway's root URL and defaults to
      [https://opencode.ai/zen/go]; the chat-completions endpoint lives under
      its [/v1] path. *)

  val make :
    ?base_url:string ->
    ?timeout_s:float ->
    ?max_retries:int ->
    ?max_stream_retries:int ->
    unit ->
    t
  (** [make ()] is a checked connection configuration.

      [timeout_s] is the whole logical-request deadline, covering connection
      setup and streamed response consumption. It defaults to 1800 seconds.

      [max_retries] bounds the pre-first-token retry and [max_stream_retries]
      the stream re-run; both default to the protocol's own budget. The gateway
      rate-limits and sheds load like any remote endpoint, so the default
      budgets apply unchanged.

      Raises [Invalid_argument] if [base_url] is empty or contains a newline,
      [timeout_s] is not positive and finite, or a retry budget is negative. *)

  val default : t
  (** [default] is [make ()]. *)

  val base_url : t -> string
  (** [base_url t] is [t]'s normalized gateway root URL. *)

  val timeout_s : t -> float
  (** [timeout_s t] is the whole logical-request deadline in seconds. *)

  val max_retries : t -> int option
  (** [max_retries t] is [t]'s pre-first-token retry budget, if set. *)

  val max_stream_retries : t -> int option
  (** [max_stream_retries t] is [t]'s stream re-run budget, if set. *)
end

module Credential : sig
  type t
  (** Authentication material for the gateway.

      Credential values are inert; the client sends them on every request,
      spelled per route — a bearer authorization header on chat-completions,
      [x-api-key] on messages. *)

  val api_key : string -> t
  (** [api_key key] is API-key material.

      Raises [Invalid_argument] if [key] is empty or contains a newline. *)

  val bearer : string -> t
  (** [bearer token] is bearer-token material.

      Raises [Invalid_argument] if [token] is empty or contains a newline. *)
end

val client :
  env:Eio_unix.Stdenv.base ->
  ?config:Config.t ->
  credential:Credential.t ->
  unit ->
  Mentat_llm.Client.t
(** [client ~env ~credential ()] is an OpenCode Go client. Each request
    streams over the endpoint of its model's protocol family. The client
    accepts models built with {!chat_model} or {!messages_model}; a model from
    another provider or an unserved protocol family is refused before any
    transport is opened. Each response closes its gateway connection before
    returning. *)
