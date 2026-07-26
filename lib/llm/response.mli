(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Completed provider responses.

    A response records provider-reported metadata around the durable assistant
    message produced by a request. Add {!message} to a {!Transcript.t}; keep
    stop reason, usage, provider ids, and reasoning summary as metadata.

    {b Terminal authority.} This response is the authoritative transcript fact
    for its turn: the assistant message here, appended through
    {!Transcript.add_response}, is what a consumer replays and what it
    reconciles live {!Event.t} progress against. Events carry no replay
    authority. *)

module Stop : sig
  (** Why a provider completed successfully.

      Failures (cancellation, transport, provider errors) are {!Error.t} values,
      not stop reasons. Provider-native stop labels are retained separately on
      {!Response.t}. *)

  type t = private
    | End_turn
    | Tool_call
    | Length
    | Content_filter
    | Refusal
    | Other of string
        (** The type for provider-neutral stop reasons.

            [Other label] is a provider-neutral reason with no dedicated
            constructor; [label] is a normalized lowercase label distinct from
            every dedicated constructor. It starts with a lowercase ASCII letter
            and then contains lowercase letters, digits, or ['_'].

            The variant is private so callers can pattern-match without
            constructing non-canonical values. *)

  val end_turn : t
  (** [end_turn] is the ordinary successful-completion reason. *)

  val tool_call : t
  (** [tool_call] is completion that requests one or more tools. *)

  val length : t
  (** [length] is completion at the model's output limit. *)

  val content_filter : t
  (** [content_filter] is completion caused by provider content filtering. *)

  val refusal : t
  (** [refusal] is completion caused by a provider refusal. *)

  val other : string -> t
  (** [other label] is a forward-compatible normalized reason.

      Raises [Invalid_argument] if [label] is invalid or names a dedicated
      constructor. *)

  val of_label : string -> t option
  (** [of_label label] is the normalized reason named by [label], or [None] if
      [label] does not satisfy the stop-label grammar. Dedicated labels map to
      their dedicated constructors. *)

  val label : t -> string
  (** [label t] is [t]'s stable lowercase label. Adapters serialize the
      provider-recorded label through this. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps reasons to and from lowercase labels. Dedicated labels decode
      to their constructors, other valid labels to [Other], and invalid labels
      produce a decoding error. Every constructible value round-trips. *)
end

type t
(** The type for completed provider responses. *)

val make :
  model:Model.t ->
  ?response_model:string ->
  ?response_id:string ->
  ?provider_stop:string ->
  ?stop:Stop.t ->
  ?usage:Usage.t ->
  ?reasoning_summary:string list ->
  Message.Assistant.t ->
  t
(** [make ~model ?response_model ?response_id ?provider_stop ?stop ?usage
     ?reasoning_summary assistant] is a completed response.

    [model] is the requested model, not necessarily the provider-selected one
    that produced the response. [assistant] may contain text, tool calls,
    provider-owned reasoning parts, or be {!Message.Assistant.empty}.
    [response_model], [response_id], and [provider_stop] record provider
    metadata. [reasoning_summary] defaults to [[]] and is metadata, not
    transcript content.

    Raises [Invalid_argument] if a present string field, or any reasoning
    summary line, is empty. *)

val assistant : t -> Message.Assistant.t
(** [assistant t] is [t]'s durable assistant message. *)

val message : t -> Message.t
(** [message t] is [assistant t] as a transcript message. *)

val texts : t -> string list
(** [texts t] is the visible assistant text blocks in order. *)

val text : ?sep:string -> t -> string
(** [text ?sep t] concatenates {!texts}[ t] with [sep] (default [""]). No-text
    responses give [""]. *)

val tool_calls : t -> Tool.Call.t list
(** [tool_calls t] is the complete assistant tool calls in order. *)

val has_tool_calls : t -> bool
(** [has_tool_calls t] is [true] iff {!tool_calls}[ t] is non-empty. *)

val model : t -> Model.t
(** [model t] is the requested model. *)

val response_model : t -> string option
(** [response_model t] is the provider-selected model id, if known. *)

val response_id : t -> string option
(** [response_id t] is the provider response id, if known. *)

val provider_stop : t -> string option
(** [provider_stop t] is the provider-native stop label, if known. *)

val stop : t -> Stop.t option
(** [stop t] is the normalized stop reason, if known. *)

val usage : t -> Usage.t option
(** [usage t] is the provider-reported usage, if any. *)

val reasoning_summary : t -> string list
(** [reasoning_summary t] is provider-approved reasoning summary text in order.
*)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] contain the same durable response
    data.

    Equality is structural, and semantic over any embedded tool-call JSON input
    (see {!Tool.Call.equal}): it ignores JSON parse metadata, so decoded and
    constructed responses with the same payload compare equal. *)

val jsont : t Jsont.t
(** [jsont] maps responses to and from JSON objects.

    Decoding errors if the object violates {!make}. *)
