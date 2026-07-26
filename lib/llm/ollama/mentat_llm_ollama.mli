(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Ollama provider adapter.

    This library interprets {!Mentat_llm.Request.t} values against a running
    Ollama daemon over its OpenAI-compatible chat-completions endpoint. The
    daemon owns the model set: Mentat declares no static Ollama models, ids are
    whatever the daemon serves (e.g. ["qwen3-coder:30b"]), and pulling models is
    the user's `ollama pull`, not Mentat's concern. Mentat's provider-neutral
    request, event, and response types remain the public boundary.

    The client connects to {!Config.make}'s [base_url] ([http://127.0.0.1:11434]
    by default; override it for a daemon on another machine via the provider
    base-URL config). Authentication is optional: a bare daemon needs none, a
    key-protected one takes a {!Credential.t} sent as a bearer authorization
    header. A request for a model the daemon does not have fails at request time
    with the daemon's own error. *)

val provider : Mentat_llm.Provider.t
(** [provider] is the [ollama] provider namespace. *)

val api : Mentat_llm.Model.Api.t
(** [api] is the OpenAI-compatible chat-completions protocol family. *)

val model : string -> Mentat_llm.Model.t
(** [model id] is Ollama model [id] under {!provider}. *)

module Config : sig
  type t
  (** Connection configuration.

      [base_url] is the daemon's root URL and defaults to
      [http://127.0.0.1:11434]; the OpenAI-compatible endpoint lives under its
      [/v1] path and the native discovery API under [/api]. *)

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
      the stream re-run; both default to the protocol's own budget. A daemon on
      loopback rarely needs either, but the same endpoint shape reaches remote
      OpenAI-compatible gateways that rate-limit and shed load.

      Raises [Invalid_argument] if [base_url] is empty or contains a newline,
      [timeout_s] is not positive and finite, or a retry budget is negative. *)

  val default : t
  (** [default] is [make ()]. *)

  val base_url : t -> string
  (** [base_url t] is [t]'s normalized daemon root URL. *)

  val timeout_s : t -> float
  (** [timeout_s t] is the whole logical-request deadline in seconds. *)

  val max_retries : t -> int option
  (** [max_retries t] is [t]'s pre-first-token retry budget, if set. *)

  val max_stream_retries : t -> int option
  (** [max_stream_retries t] is [t]'s stream re-run budget, if set. *)
end

module Credential : sig
  type t
  (** Authentication material for a key-protected daemon.

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
  ?credential:Credential.t ->
  unit ->
  Mentat_llm.Client.t
(** [client ~env ()] is an Ollama client streaming over the daemon's
    chat-completions endpoint. Without [credential] requests carry no
    authorization header — the bare local-daemon default. Each response closes
    its daemon connection before returning. *)
