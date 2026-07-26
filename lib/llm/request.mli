(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Model requests.

    A request combines a checked {!Transcript.t} with the model, the
    model-visible tools, provider-neutral options, an optional host
    {!Prelude.t}, and an optional cache-routing key. Provider clients interpret
    requests; they never repair transcript grammar, execute tools, or mutate
    session state.

    Construct requests with {!make}; recoverable failures are {!Error.t}. The
    exception-raising constructors are for programmer-local static setup only.
*)

module Options : sig
  (** Provider-neutral request options: tool-choice policy, reasoning effort,
      output shape, and sampling caps.

      Deliberately {e closed}: it carries only knobs every provider can honor.
      Provider-specific knobs are derived by the adapter from these fields, or
      configured when the {!Client.t} is built by a provider adapter — never
      smuggled through a JSON passthrough here. Keeping this closed preserves
      neutral request semantics, a stable {!Request.digest}, and a fail-closed
      security posture.

      Construct with {!make}; {!default} is the neutral option set. *)

  type tool_choice =
    | Auto
    | No_tools
    | Required
    | Tool of string
        (** The type for tool-choice policy.

            [Auto] lets the provider choose. [No_tools] disables tool calls even
            when declarations are supplied. [Required] requires at least one
            call. [Tool name] requires the named declared tool. Request
            construction validates [Required] and [Tool name] against the
            request's declarations. *)

  module Reasoning_effort : sig
    (** Provider-neutral reasoning effort levels and their stable spellings. *)

    type t =
      | Disabled
      | Minimal
      | Low
      | Medium
      | High
      | Extra_high
      | Max
          (** The type for provider-neutral reasoning effort.

              Absence of a reasoning effort in the request options means
              provider default. [Disabled] explicitly asks to disable reasoning
              where supported. Adapters must reject unsupported levels rather
              than silently lowering them. *)

    val to_string : t -> string
    (** [to_string e] is [e]'s stable spelling: ["none"], ["minimal"], ["low"],
        ["medium"], ["high"], ["xhigh"], or ["max"].

        This is the single reasoning-effort vocabulary shared by request
        options, provider declarations, CLI flags, and diagnostics. *)

    val of_string : string -> t option
    (** [of_string s] decodes a stable spelling; see {!to_string}. Unknown
        spellings are [None]. *)

    val all : t list
    (** [all] are the efforts in declaration order. *)

    val jsont : t Jsont.t
    (** [jsont] maps efforts to and from their stable spellings. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf t] formats {!to_string}[ t]. *)
  end

  type response_format =
    | Text
    | Json_schema of { name : string; schema : Jsont.json; strict : bool }
        (** The type for requested assistant output shape.

            [Json_schema] requests JSON output matching [schema]. [schema] must
            be a JSON object, but JSON Schema semantics are not validated.
            [strict] is passed through for adapters that support it. *)

  type t
  (** The type for provider-neutral request options. *)

  val default : t
  (** [default] is [make ()]. *)

  val make :
    ?tool_choice:tool_choice ->
    ?max_output_tokens:int ->
    ?temperature:float ->
    ?reasoning_effort:Reasoning_effort.t ->
    ?response_format:response_format ->
    unit ->
    t
  (** [make ()] is a checked option set.

      Raises [Invalid_argument] on an invalid [Tool] name, non-positive
      [max_output_tokens], negative or non-finite [temperature], or a
      [Json_schema] with an empty name or non-object schema. *)

  val tool_choice : t -> tool_choice
  (** [tool_choice t] is [t]'s tool-choice policy. *)

  val max_output_tokens : t -> int option
  (** [max_output_tokens t] is [t]'s output-token cap, if any. *)

  val temperature : t -> float option
  (** [temperature t] is [t]'s sampling temperature, if any. *)

  val reasoning_effort : t -> Reasoning_effort.t option
  (** [reasoning_effort t] is [t]'s requested reasoning effort, if any. *)

  val response_format : t -> response_format
  (** [response_format t] is [t]'s requested assistant output shape. *)

  val jsont : t Jsont.t
  (** [jsont] maps request options to and from JSON objects.

      Decoding errors if the object violates {!make}. *)
end

module Error : sig
  (** Request construction errors: handle by matching, not by parsing
      {!message}. *)

  type t =
    | Empty_transcript
    | Invalid_prelude_message of Message.t
    | Pending_tool_results of Tool.Call.t list
    | Duplicate_tool of string
    | Tool_choice_without_tools
    | Unknown_tool_choice of string
        (** The type for request construction errors. These are user- or
            state-boundary errors, handled as data rather than by parsing
            {!message}. *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] formats [e] for diagnostics. *)
end

module Prelude : sig
  (** Host-generated model-visible request context sent before the transcript
      but not part of the checked conversation transcript (project instructions,
      mode instructions, environment context).

      Accepts only {!Message.System}, {!Message.Developer}, and {!Message.User};
      prelude messages are request-scoped and never appended back to the
      transcript. Adapters that need the full model-visible message list use
      {!messages} on the containing request. *)

  type t
  (** The type for checked request prelude messages. *)

  val empty : t
  (** [empty] is the empty request prelude. *)

  val make : Message.t list -> (t, Error.t) result
  (** [make messages] is a checked request prelude, preserving message order.

      Errors {!Error.Invalid_prelude_message} on an assistant or tool-result
      message. *)

  val append : t -> Message.t list -> (t, Error.t) result
  (** [append t messages] is [t] with [messages] appended, checked as in {!make}
      and order-preserving.

      Errors {!Error.Invalid_prelude_message} on an assistant or tool-result
      message. *)

  val messages : t -> Message.t list
  (** [messages t] is [t]'s checked message list in provider order. *)
end

type t
(** The type for checked model requests. *)

val make :
  model:Model.t ->
  ?prelude:Prelude.t ->
  ?tools:Tool.t list ->
  ?options:Options.t ->
  ?cache_key:string ->
  Transcript.t ->
  (t, Error.t) result
(** [make ~model ?prelude ?tools ?options ?cache_key transcript] is a checked
    request.

    The model-visible message order is the prelude messages then the transcript
    messages (see {!messages}). [prelude] defaults to {!Prelude.empty}, [tools]
    to [[]], [options] to {!Options.default}. [cache_key], when present, is a
    non-empty stable routing hint providers may use for prompt caching; it never
    changes request semantics and is excluded from {!digest}.

    Errors with structured {!Error.t} on an empty or awaiting transcript,
    duplicate tool names, [Required] with no tools, or [Tool] naming an
    undeclared tool. Raises [Invalid_argument] if [cache_key] is present and
    empty. *)

val make_exn :
  model:Model.t ->
  ?prelude:Prelude.t ->
  ?tools:Tool.t list ->
  ?options:Options.t ->
  ?cache_key:string ->
  Transcript.t ->
  t
(** [make_exn] is {!make} raising [Invalid_argument] on construction failure.
    Prefer {!make} at user-input, persistence, provider, or session boundaries.
*)

val append_prelude : t -> Message.t list -> (t, Error.t) result
(** [append_prelude t messages] is [t] with [messages] appended to its prelude.

    The model, tools, options, cache key, and transcript are preserved; the
    appended messages are checked with {!Prelude.append} and the result is
    re-checked with the {!make} invariants. On success, {!messages} of the
    result is the original prelude messages, then [messages], then the original
    transcript messages. *)

val model : t -> Model.t
(** [model t] is [t]'s requested model. *)

val tools : t -> Tool.t list
(** [tools t] are [t]'s model-visible tool declarations. *)

val options : t -> Options.t
(** [options t] are [t]'s request options. *)

val prelude : t -> Prelude.t
(** [prelude t] is [t]'s host-generated request context. *)

val transcript : t -> Transcript.t
(** [transcript t] is [t]'s checked model-visible history. *)

val messages : t -> Message.t list
(** [messages t] is the full provider-order message list:
    [Prelude.messages (prelude t) @ Transcript.messages (transcript t)]. *)

val cache_key : t -> string option
(** [cache_key t] is [t]'s stable prompt-cache routing key, if any. *)

val jsont : t Jsont.t
(** [jsont] maps a request to and from a JSON object; decoding re-checks every
    invariant of {!make}. It is the persistence and audit codec for the exact
    request and the basis {!digest} canonicalizes. *)

val digest : t -> Mentat_digest.t
(** [digest t] is a canonical, stable content digest of the semantically
    billable request: the model identity, the full model-visible message list
    ({!messages}), the tool declarations, and the options. It is independent of
    OCaml value layout and JSON key order (a version-tagged canonical encoding),
    so equal requests digest equally on any run and across a {!jsont}
    round-trip. Changing the model, any prelude or transcript message, any tool
    declaration, or any option changes the digest.

    The message list covers the {!prelude}: prelude messages are billable input,
    so they are part of the identity. Cross-run identity therefore requires a
    {e deterministic} prelude — environment context or timestamps folded into it
    change the digest and defeat deduplication. Only identical resends are meant
    to dedup, so a caller relying on stable identity keeps the prelude
    deterministic.

    [cache_key] is excluded (a routing hint that never changes semantics). The
    digest is taken over the pre-resolution [`Ref] form of any attachment, so it
    is computable without fetching bytes; resolution of [`Ref] to
    [`Base64]/[`Uri] is a strictly later, non-identity-bearing transform above
    [mentat.llm]. The digest can identify a durable execution-intent claim, but
    the caller owns the claim's storage, atomicity, and resend policy. It is not
    a wire format or a cache key. *)
