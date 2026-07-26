(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Anthropic Messages provider for [mentat.llm].

    This library translates checked {!Mentat_llm.Request.t} values to the
    Anthropic Messages protocol and translates Anthropic responses back to
    semantic live events and terminal responses.

    The adapter is intentionally a thin provider projection: construct a model
    with {!model}, make a client with {!client}, then use
    {!Mentat_llm.Client.response}. It accepts only the Anthropic Messages API
    family and rejects unsupported request features before opening transport.

    Credentials, endpoint overrides, and HTTP policy are supplied explicitly.
    The library does not read process environment variables, choose CLI
    precedence, persist sessions, discover accounts, price models, execute
    tools, or own UI model selection. *)

(** {1:configuration Configuration} *)

module Config = Config
(** Anthropic client configuration. *)

module Credential : sig
  type t
  (** The type for Anthropic credentials.

      Credential values are inert and are only turned into authorization headers
      when {!client} starts a request. *)

  val api_key : string -> t
  (** [api_key key] is an Anthropic API-key credential sent as [x-api-key].

      Raises [Invalid_argument] if [key] is empty or contains a newline. *)

  val bearer : string -> t
  (** [bearer token] is a bearer-token credential.

      Raises [Invalid_argument] if [token] is empty or contains a newline. *)
end

(** {1:identity Provider identity} *)

val provider : Mentat_llm.Provider.t
(** [provider] is the Anthropic provider namespace. *)

val api : Mentat_llm.Model.Api.t
(** [api] is the Anthropic Messages API family. *)

val model : string -> Mentat_llm.Model.t
(** [model id] is Anthropic Messages model [id].

    Raises [Invalid_argument] if [id] is empty. *)

(** {1:projection Provider projection}

    Prelude and transcript system/developer messages become Anthropic [system]
    text blocks, preserving prelude order before transcript order. User text
    becomes [text] content; user media and tool-result media must be images and
    become Anthropic [image] blocks with [base64] or [url] sources.

    Assistant text becomes [text], assistant tool calls become [tool_use], and
    provider-owned reasoning becomes [thinking] or [redacted_thinking].
    Reasoning without text, summary, or encrypted data is rejected. Tool results
    are sent as user-role [tool_result] blocks; empty results encode as an empty
    string, single text results as a string, and richer image/text results as a
    content array.

    Tool declarations carry their name, optional description, and input schema.
    [Auto] sends Anthropic [auto] only when tools are present; [No_tools] omits
    tools and [tool_choice]; [Required] maps to Anthropic [any]; [Tool name]
    maps to Anthropic [tool]. Response-format JSON schemas and provider replay
    are not supported by this adapter.

    [max_output_tokens] defaults to [4096]. A requested reasoning effort selects
    Anthropic adaptive thinking, whose ceiling travels as [output_config.effort]
    beside the [thinking] object rather than as a token budget inside it:
    [Minimal] and [Low] map to [low], [Medium] to [medium], [High] to [high],
    [Extra_high] to [xhigh], and [Max] to [max]. Thinking asks for summarized
    display so reasoning arrives as readable text. A [Disabled] effort sends
    Anthropic [disabled] and no [output_config]; no requested effort sends
    neither, leaving the model its own default.

    [temperature] is sent only alongside an explicitly disabled thinking, since
    current Anthropic models reject sampling parameters and treat an absent
    [thinking] as adaptive. Forced tool choice with thinking is rejected because
    Anthropic does not support that combination. *)

(** {1:clients Clients} *)

val client :
  env:Eio_unix.Stdenv.base ->
  ?config:Config.t ->
  credential:Credential.t ->
  unit ->
  Mentat_llm.Client.t
(** [client ~env ~credential ()] is an Anthropic Messages client.

    The returned client accepts requests whose model names {!provider} and
    {!api}. Other models are rejected by {!Mentat_llm.Client.response} before
    the adapter opens transport.

    The client borrows [env] for HTTP calls, TLS, retries, timeouts, and stream
    reads. Each response owns a request-local transport scope and releases it
    before returning. Startup and stream failures are distinguished by
    {!Mentat_llm.Error.phase}.

    Startup errors include unsupported request features, cancellation observed
    before transport, non-2xx HTTP responses after retries, transport failures,
    timeouts, and local JSON encoding failures. HTTP status codes are classified
    into provider-neutral {!Mentat_llm.Error.kind} values and raw response
    bodies are redacted before being attached to errors.

    Live events expose Anthropic text deltas, reasoning summary deltas, partial
    tool-input deltas, completed tool calls, and usage snapshots. The terminal
    response is produced on [message_stop] and contains the durable assistant
    message, provider response id/model, normalized stop reason, provider stop
    label, merged usage, and retained reasoning parts. Malformed SSE JSON,
    invalid tool-call input, duplicate or late content-block events, EOF before
    [message_stop], cancellation during streaming, provider [error] events, and
    streams with no assistant parts are returned as stream-phase failures. *)
