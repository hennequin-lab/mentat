(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Internal Anthropic Messages protocol binding.

    This module is the provider-specific boundary below {!Mentat_llm_anthropic}.
    It owns Anthropic HTTP request construction, retry configuration, SSE
    parsing, and the raw Messages request shape; the shared retry loops
    themselves live in {!Mentat_llm_http.Retry}. It deliberately does not know
    about {!Mentat_llm.Message.t}, {!Mentat_llm.Transcript.t}, or
    {!Mentat_llm.Response.t}; the public provider module performs that
    translation.

    Maintainers should keep this module close to the Anthropic wire protocol so
    semantic request validation and stream interpretation remain isolated in the
    public adapter. *)

val api : string
(** [api] is the Anthropic API family name used by {!Mentat_llm.Model.Api.t}. *)

val default_stream_max_retries : int
(** [default_stream_max_retries] is the stream-phase re-run budget applied when
    a {!Config.t} leaves [max_stream_retries] unset. Following codex's deeper
    stream budget, it outlasts more of a transient-fault window than the
    pre-stream HTTP retry (bounded internally by [max_retries]), which surfaces
    once its shallower budget is spent. *)

module Error : sig
  (** Raw provider binding errors. *)

  type response = {
    status : int;  (** HTTP status code. *)
    headers : (string * string) list;
        (** HTTP response headers as reported by Cohttp. Header names preserve
            transport casing. *)
    body : string;
        (** Raw response body, capped by the binding. The caller owns redaction
            before logging. *)
  }
  (** The type for non-2xx HTTP responses. *)

  (** The type for failures at the raw Anthropic binding boundary. *)
  type t =
    | Response of response  (** Anthropic returned a non-2xx HTTP response. *)
    | Transport of string
        (** The HTTP transport failed before a valid Anthropic response was
            available. *)
    | Decode of string  (** Local JSON or SSE decoding failed. *)
end

module Client : sig
  (** Anthropic HTTP clients. *)

  (** The type for Anthropic authorization material.

      Values are assumed to be non-empty header values validated by the public
      credential API. *)
  type auth =
    | Api_key of string  (** [Api_key key] is sent as [x-api-key]. *)
    | Bearer of string
        (** [Bearer token] is sent as an [Authorization] bearer token. *)

  type t
  (** The type for raw Anthropic clients.

      Clients retain the Eio switch, environment, configuration, and
      authorization material used for subsequent requests. *)

  val make :
    Config.t ->
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    auth:auth ->
    unit ->
    t
  (** [make config ~sw ~env ~auth ()] is a raw Anthropic client.

      The client borrows [sw] and [env]. Requests made with the client use [sw]
      for Cohttp calls and [env] for networking, TLS, sleep between retries, and
      timeouts. *)
end

module Messages : sig
  (** Raw Anthropic Messages API calls. *)

  type request = {
    model : string;  (** Provider-native model id. *)
    system : Jsont.json list;
        (** Raw [system] blocks. Empty means no [system] member. *)
    messages : Jsont.json list;  (** Raw Anthropic [messages] array. *)
    tools : Jsont.json list;
        (** Raw Anthropic tool declarations. Empty means no [tools] member. *)
    tool_choice : Jsont.json option;
        (** Raw Anthropic [tool_choice] value, if any. *)
    thinking : Jsont.json option;
        (** Raw Anthropic [thinking] object, if any. *)
    max_tokens : int;  (** Raw [max_tokens] value. *)
    temperature : float option;  (** Raw [temperature] value, if any. *)
    stream : bool;  (** Raw [stream] value. The public adapter sets [true]. *)
  }
  (** The type for raw Anthropic Messages request bodies.

      Values are expected to be provider-valid. This module only applies the
      Anthropic JSON shape: empty [system] and [tools] lists are omitted, absent
      optional fields are omitted, and the remaining fields are sent as raw JSON
      values. Semantic validation belongs in the Mentat adapter. *)

  type event = { name : string; data : Jsont.json }
  (** The type for decoded server-sent events.

      [name] is the SSE event name when present, otherwise the JSON [type]
      member. [data] is the raw decoded event payload. *)

  type stream
  (** The type for raw Anthropic event streams.

      Streams are pull-based and must be closed by callers that stop reading
      before terminal completion. *)

  val next : stream -> (event, Error.t) result option
  (** [next stream] is the next raw SSE event from [stream].

      It is [Some (Ok event)] for a decoded event, [Some (Error e)] for a
      decoding or transport failure, and [None] after EOF or after {!close}.
      Cancellation propagates to the caller. A premature [None] is not
      classified here; the Mentat adapter decides whether EOF is a malformed
      model stream for its higher-level contract. *)

  val close : stream -> unit
  (** [close stream] closes [stream] locally.

      [close] is idempotent. After [close stream], {!next}[ stream] is [None].
      The underlying HTTP flow is owned by the Eio/Cohttp call. Closing is a
      local stop signal for this pull stream; callers should still close streams
      they abandon before terminal provider completion. *)

  val create_stream :
    ?on_retry:(attempt:int -> limit:int -> delay:float -> reason:string -> unit) ->
    Client.t ->
    request ->
    (stream, Error.t) result
  (** [create_stream client request] sends [request] to Anthropic [/messages]
      and returns a raw event stream.

      The call uses the client's base URL and retry policy. Retryable statuses
      are [408], [409], [429], and [5xx]; transport failures are also retryable.
      [retry-after-ms] and [retry-after] response headers are honored when
      present. [on_retry], which defaults to a no-op, observes each upcoming
      retry with its 1-based ordinal, the budget for the failure class, the wait
      in seconds, and a short lowercase display reason.

      Returns [Error (Response r)] for the final non-2xx HTTP response,
      [Error (Transport message)] for exhausted transport failures, and
      [Error (Decode message)] if the request body cannot be JSON-encoded. *)
end
