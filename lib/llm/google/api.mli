(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Internal Google Gemini protocol binding.

    This module is the provider-specific boundary below {!Mentat_llm_google}. It
    owns Google Generative Language HTTP request construction, retry
    configuration and its terminal-quota and body-delay refinements, SSE
    parsing, and the raw Gemini [streamGenerateContent] request shape; the
    shared retry loops themselves live in {!Mentat_llm_http.Retry}. It
    deliberately does not know about {!Mentat_llm.Message.t},
    {!Mentat_llm.Transcript.t}, or {!Mentat_llm.Response.t}; the public provider
    module performs that translation. *)

val api : string
(** [api] is the Gemini API family name used by {!Mentat_llm.Model.Api.t}. *)

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
        (** Raw response body, truncated by the binding's error-body limit. The
            caller owns redaction before logging. *)
  }
  (** The type for non-2xx HTTP responses. *)

  (** The type for failures at the raw Google Gemini binding boundary. *)
  type t =
    | Response of response  (** Google returned a non-2xx HTTP response. *)
    | Transport of string
        (** The HTTP transport failed before a valid Google response was
            available. *)
    | Decode of string  (** Local JSON or SSE decoding failed. *)
end

module Client : sig
  (** Google Gemini HTTP clients. *)

  (** The type for Google authorization material.

      Values are assumed to be validated by the public credential API. *)
  type auth =
    | Api_key of string  (** [Api_key key] is sent as [x-goog-api-key]. *)

  type t
  (** The type for raw Google Gemini clients.

      Clients retain the Eio switch, environment, configuration, and
      authorization material used for subsequent requests. *)

  val make :
    Config.t ->
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    auth:auth ->
    unit ->
    t
  (** [make config ~sw ~env ~auth ()] is a raw Google Gemini client.

      The client borrows [sw] and [env]. Requests made with the client use [sw]
      for Cohttp calls and [env] for networking and timeouts. *)
end

module Generate_content : sig
  (** Raw Google Gemini [streamGenerateContent] calls. *)

  type request = {
    model : string;  (** Provider-native model id used in the URL path. *)
    contents : Jsont.json list;  (** Raw Gemini [contents] array items. *)
    system_instruction : Jsont.json option;
        (** Raw Gemini [systemInstruction] object, if any. *)
    tools : Jsont.json list;
        (** Raw Gemini [tools] entries. Empty means no [tools] member. *)
    tool_config : Jsont.json option;
        (** Raw Gemini [toolConfig] object, if any. *)
    generation_config : Jsont.json option;
        (** Raw Gemini [generationConfig] object, if any. *)
  }
  (** The type for raw Gemini [streamGenerateContent] requests.

      [model] is percent-encoded into
      [/models/{model}:streamGenerateContent?alt=sse] and is not encoded in the
      JSON body. Values are expected to be provider-valid. This module encodes
      the record to JSON and sends it; semantic validation belongs in the Mentat
      adapter. *)

  type event = { data : Jsont.json }
  (** The type for decoded server-sent events.

      Gemini events are data-only for this binding. Multiple [data] lines are
      joined with newline characters. Non-[data] SSE fields are ignored. *)

  type stream
  (** The type for raw Gemini event streams.

      Streams are pull-based and must be closed by callers that stop reading
      before terminal completion. *)

  val next : stream -> (event, Error.t) result option
  (** [next stream] is the next raw SSE event from [stream].

      It is [Some (Ok event)] for a decoded event, [Some (Error e)] for a
      decoding or transport failure, and [None] after EOF or after {!close}.
      Cancellation propagates to the caller. EOF is not classified here; the
      Mentat adapter converts EOF into the terminal semantic response for its
      higher-level contract. *)

  val close : stream -> unit
  (** [close stream] closes [stream] locally.

      [close] is idempotent. After [close stream], [next stream] is [None]. The
      underlying HTTP flow is owned by the Eio/Cohttp call. *)

  val create_stream :
    ?on_retry:(attempt:int -> limit:int -> delay:float -> reason:string -> unit) ->
    Client.t ->
    request ->
    (stream, Error.t) result
  (** [create_stream client request] sends [request] to Google Gemini and
      returns a raw event stream.

      Returns [Error (Response r)] for non-2xx HTTP responses,
      [Error (Transport message)] for transport failures, and
      [Error (Decode message)] if the request body cannot be JSON-encoded.
      Retryable transport failures and HTTP statuses [408], [409], [429], and
      [5xx] are retried according to {!Config.max_retries}; [retry-after-ms] and
      [retry-after] response headers override the local backoff delay when
      present. [on_retry], which defaults to a no-op, observes each upcoming
      retry with its 1-based ordinal, the budget for the failure class, the wait
      in seconds, and a short lowercase display reason. *)
end
