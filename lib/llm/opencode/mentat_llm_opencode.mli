(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** OpenCode Go provider adapter.

    This library interprets {!Mentat_llm.Request.t} values against the OpenCode
    Go gateway over its OpenAI-compatible chat-completions endpoint. The gateway
    owns the model set: ids are whatever it serves (e.g. ["kimi-k3"]), spelled
    exactly as OpenCode publishes them, and the adapter constrains only the
    protocol family. Mentat's provider-neutral request, event, and response
    types remain the public boundary.

    The client connects to {!Config.make}'s [base_url]
    ([https://opencode.ai/zen/go] by default) — the bare gateway root, under
    whose [/v1] path the chat-completions endpoint lives. Every request
    authenticates with a {!Credential.t} sent as a bearer authorization header.
    A request for a model the gateway does not serve fails at request time with
    the gateway's own error. *)

val provider : Mentat_llm.Provider.t
(** [provider] is the [opencode-go] provider namespace. *)

val chat_model : string -> Mentat_llm.Model.t
(** [chat_model id] is chat-completions model [id] under {!provider}. *)

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

      Credential values are inert; the client sends them as a bearer
      authorization header on every request. *)

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
(** [client ~env ~credential ()] is an OpenCode Go client streaming over the
    gateway's chat-completions endpoint. Requests carry [credential] as a bearer
    authorization header. The client accepts models built with {!chat_model}; a
    model from another provider or protocol family is refused before any
    transport is opened. Each response closes its gateway connection before
    returning. *)
