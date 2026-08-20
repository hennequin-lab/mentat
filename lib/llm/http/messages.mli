(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Anthropic-compatible Messages protocol endpoints.

    This module owns the common request projection, HTTP/SSE wire protocol,
    streamed event decoding, and terminal response construction used by
    providers backed by a Messages endpoint. Provider-specific endpoint
    discovery, credentials, model lifecycle, and client construction remain in
    the provider libraries.

    The projection: system and developer messages become [system] text blocks;
    user text becomes [text] content; user media and tool-result media must be
    images and become [image] blocks with [base64] or [url] sources. Assistant
    text becomes [text], tool calls become [tool_use], and provider-owned
    reasoning replays as signed [thinking] or [redacted_thinking] blocks —
    reasoning with neither signature nor encrypted data is dropped rather than
    sent as a block the protocol rejects. Tool results are user-role
    [tool_result] blocks. A requested reasoning effort selects adaptive
    thinking with its ceiling in [output_config.effort]; [Disabled] sends
    [thinking: disabled]. Response-format JSON schemas are not supported.
    [max_tokens] defaults to [4096].

    Two dialect policies are the consumer's ruling, not the codec's:

    - [cache] plants ephemeral [cache_control] breakpoints on the last tool,
      the last system block, and the last content block of the last message,
      so each request's longer prefix reads the previous one back.
    - [sampling] forwards the request's [temperature] unconditionally. Without
      it, sampling parameters travel only alongside an explicitly disabled
      thinking — the rule for models that run adaptive thinking by default and
      reject sampling parameters otherwise. *)

val api : Mentat_llm.Model.Api.t
(** [api] is the [messages] model API family. *)

type t
(** A configured Messages endpoint. *)

val make :
  provider:Mentat_llm.Provider.t ->
  ?headers:(string * string) list ->
  ?timeout_s:float ->
  ?max_retries:int ->
  ?max_stream_retries:int ->
  ?terminal:(Transport.response -> bool) ->
  ?classify:(status:int -> body:string -> Mentat_llm.Error.kind option) ->
  cache:bool ->
  sampling:bool ->
  endpoint:string ->
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  unit ->
  t
(** [make ~provider ~cache ~sampling ~endpoint ~sw ~env ()] is a protocol
    endpoint.

    [endpoint] is the full Messages URL — the codec appends nothing, so each
    consumer keeps its own public base-URL convention. [headers] are included
    in every request; authentication header spelling ([x-api-key] or a bearer
    authorization) is the caller's. The codec supplies the protocol headers:
    content type, event-stream accept, the [anthropic-version] the dialect
    requires, a neutral user agent, and the per-attempt retry count.

    [cache] and [sampling] are required: the codec defaults neither consumer's
    policy. [timeout_s], when present, bounds request encoding, response
    headers, and streamed response consumption. [max_retries] (default [2])
    bounds the pre-first-token retry and [max_stream_retries] (default [5])
    the stream re-run, both under the shared {!Retry} policy.
    [max_retries = 0] silences the stream re-run as well.

    [terminal] recognizes responses whose retryable status hides an
    unrecoverable condition and short-circuits the retry ladder; the server's
    own retry header still outranks it. [classify], consulted before the
    status table, lets a provider whose gateway disambiguates failures only in
    the error body assign the error kind from it. Both default to absent. *)

val check_request :
  provider:Mentat_llm.Provider.t ->
  Mentat_llm.Request.t ->
  (unit, Mentat_llm.Error.t) result
(** [check_request ~provider request] checks the protocol projection without
    opening transport, so unsupported content and unresolved references fail
    at the semantic boundary rather than after startup work. *)

val run : t -> Mentat_llm.Client.run
(** [run t ~cancelled ~on_event request] projects [request] to the Messages
    protocol, delivers live events in wire order, and returns the terminal
    response.

    Live events expose text deltas, reasoning summary deltas, partial
    tool-input deltas, completed tool calls, and usage snapshots; the terminal
    response is produced on [message_stop]. Raw transport streams never escape
    this call: the transport is released if [on_event] raises, and the
    exception propagates. Startup and stream failures are distinguished by
    {!Mentat_llm.Error.phase}; HTTP statuses classify into provider-neutral
    error kinds and raw error bodies are redacted before being attached. *)
