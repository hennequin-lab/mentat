(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** OpenAI-compatible Chat Completions protocol endpoints.

    This module owns the common request projection, HTTP/SSE wire protocol,
    streamed event decoding, and terminal response construction used by
    providers backed by [/v1/chat/completions]. Provider-specific endpoint
    discovery, credentials, model lifecycle, and client construction remain in
    the provider libraries. *)

val api : Mentat_llm.Model.Api.t
(** [api] is the [chat-completions] model API family. *)

type t
(** A configured Chat Completions endpoint. *)

val make :
  provider:Mentat_llm.Provider.t ->
  ?headers:(string * string) list ->
  ?timeout_s:float ->
  ?max_retries:int ->
  ?max_stream_retries:int ->
  ?terminal:(Transport.response -> bool) ->
  ?classify:(status:int -> body:string -> Mentat_llm.Error.kind option) ->
  base_url:string ->
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  unit ->
  t
(** [make ~provider ~base_url ~sw ~env ()] is a protocol endpoint.

    [headers] are included in every request. [timeout_s], when present, bounds
    request encoding, response headers, and streamed response consumption.

    [max_retries] (default [2]) bounds the pre-first-token retry and
    [max_stream_retries] (default [5]) the stream re-run, both under the shared
    {!Retry} policy. [max_retries = 0] silences the stream re-run as well, so a
    caller that wants exactly one attempt needs only the one argument.

    [terminal] recognizes responses whose retryable status hides an
    unrecoverable condition — a dollar-budget exhaustion answering with a
    rate-limit status — and short-circuits the retry ladder; the server's own
    retry header still outranks it. [classify], consulted before the status
    table, lets a provider whose gateway disambiguates failures only in the
    error body assign the error kind from it. Both default to absent: the
    shared status policy alone. *)

val check_request :
  provider:Mentat_llm.Provider.t ->
  Mentat_llm.Request.t ->
  (unit, Mentat_llm.Error.t) result
(** [check_request ~provider request] checks the protocol projection without
    opening transport.

    Providers with expensive endpoint discovery use this before resolving or
    loading a model, so unsupported content and unresolved references fail at
    the semantic boundary rather than after startup work. *)

val run : t -> Mentat_llm.Client.run
(** [run t ~cancelled ~on_event request] projects [request] to the Chat
    Completions protocol, delivers live events in wire order, and returns the
    terminal response.

    Raw transport streams never escape this call. The transport is released if
    [on_event] raises, and the exception propagates. *)
