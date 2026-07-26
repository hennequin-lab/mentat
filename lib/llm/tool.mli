(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Model-visible tools.

    A {!type:t} declares a tool the model may call in a request. {!Call.t}
    records a complete model-requested invocation. {!Result.t} records the
    model-visible answer to a call. This module does not execute tools, grant
    permissions, or validate tool input against JSON Schema; it defines only
    model-visible declaration, call, and result facts. *)

type t
(** The type for model-visible tool declarations.

    Tool names are ASCII identifiers of at most 64 characters: the first
    character is an ASCII letter or ['_']; later characters may also include
    digits or ['-']. *)

val no_input_schema : Jsont.json
(** [no_input_schema] is the strict object schema for tools that accept no input
    fields.

    The value is immutable JSON data and may be reused across declarations. *)

val make :
  name:string -> ?description:string -> input_schema:Jsont.json -> unit -> t
(** [make ~name ?description ~input_schema ()] is a tool declaration.

    [input_schema] is the JSON Schema object advertised to the model, retained
    as supplied; JSON Schema semantics are not validated. Request construction
    checks that tool names are unique per request.

    Raises [Invalid_argument] if [name] is not a tool name, [description] is
    empty when present, or [input_schema] is not a JSON object. *)

val name : t -> string
(** [name t] is [t]'s model-visible name. *)

val description : t -> string option
(** [description t] is [t]'s model-visible description, if any. *)

val input_schema : t -> Jsont.json
(** [input_schema t] is [t]'s JSON Schema object. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] declare the same tool: equal name and
    description, and semantically equal {!input_schema}.

    Schema equality is over the JSON {e value}: member order and parse metadata
    (source text locations and whitespace) are irrelevant, so a decoded
    declaration and an otherwise-identical constructed one compare equal. *)

val jsont : t Jsont.t
(** [jsont] maps tool declarations to and from JSON objects.

    Decoding errors if the object violates {!make}. *)

module Call : sig
  (** Complete model-requested tool invocations.

      A call pairs a provider call id with a tool name and decoded JSON input,
      optionally carrying a provider signature to echo when the call is
      replayed. *)

  type t
  (** The type for complete model-requested tool calls. *)

  val make :
    id:string ->
    name:string ->
    input:Jsont.json ->
    ?signature:string ->
    unit ->
    t
  (** [make ~id ~name ~input ?signature ()] is a tool call.

      [id] is the provider call identifier that a later {!Result.t} echoes.
      [input] is complete decoded JSON input. [signature], when present, is a
      non-empty opaque provider continuity token echoed verbatim on replay (for
      example Gemini thought signatures); llm never interprets it.

      Raises [Invalid_argument] if [id] is empty, [name] is not a tool name, or
      [signature] is empty when present. *)

  val id : t -> string
  (** [id t] is the call identifier. *)

  val name : t -> string
  (** [name t] is the requested tool name. *)

  val input : t -> Jsont.json
  (** [input t] is the complete decoded JSON input.

      The value has not been checked against the declaration's input schema by
      this module. *)

  val signature : t -> string option
  (** [signature t] is the provider signature to echo on replay, if any. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same call: equal id, name,
      and signature, and semantically equal {!input}.

      Input equality is over the JSON {e value}: it ignores parse metadata
      (source text locations and whitespace), so a decoded call and an
      otherwise-identical constructed one compare equal. *)

  val jsont : t Jsont.t
  (** [jsont] maps tool calls to and from JSON objects.

      Decoding errors if the object violates {!make}. *)
end

module Result : sig
  (** Model-visible answers to tool calls.

      A result echoes the call id and name it answers and carries the
      authoritative model-visible output. {!Transcript} checks that a result
      answers a pending call. *)

  type t
  (** The type for model-visible tool results. *)

  val make : ?error:bool -> Call.t -> Content.t list -> t
  (** [make ?error call content] is a result answering [call].

      [error] defaults to [false]. [content] is the authoritative model-visible
      output; empty content is a valid empty result. {!Transcript} checks that
      the result answers a pending call with the same id and name. *)

  val empty : ?error:bool -> Call.t -> t
  (** [empty ?error call] is an empty result answering [call]. *)

  val text : ?error:bool -> Call.t -> string -> t
  (** [text ?error call s] is a text result answering [call].

      Raises [Invalid_argument] if [s] is empty. *)

  val call_id : t -> string
  (** [call_id t] is the call identifier [t] answers. *)

  val name : t -> string
  (** [name t] is the tool name [t] answers. *)

  val content : t -> Content.t list
  (** [content t] is [t]'s authoritative model-visible content. *)

  val with_content : t -> Content.t list -> t
  (** [with_content t content] is [t] with its content replaced by [content],
      preserving the call id, name, and error flag. Used above [mentat.llm] to
      resolve a tool result's [`Ref] media to [`Base64] at the provider boundary
      and to strip it when the channel cannot carry it. *)

  val texts : t -> string list
  (** [texts t] is [t]'s text content in order. *)

  val is_error : t -> bool
  (** [is_error t] is [true] iff [t] reports tool failure. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same result payload. *)

  val jsont : t Jsont.t
  (** [jsont] maps tool results to and from JSON objects.

      Decoding errors if [call_id] is empty or [name] is not a tool name. *)
end
