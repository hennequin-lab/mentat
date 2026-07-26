(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Typed JSON input contracts for tools.

    An input contract pairs two related but distinct descriptions of a tool's
    input:

    - a {!Jsont.t} runtime codec, used by {!Mentat_tool.Call.decode} to turn
      provider JSON into the typed value a callback consumes, and to re-encode
      that value canonically with {!encode};
    - a JSON Schema object, carried on the tool's declaration and read via
      {!Mentat_llm.Tool.input_schema} of {!Mentat_tool.declaration}, for model
      and provider declarations.

    [Input] stores the schema as data. It checks only that the schema root is a
    JSON object; it neither validates JSON Schema semantics, infers a schema
    from the codec, nor checks that schema and codec accept the same values. *)

type 'a t
(** The type for tool input contracts decoded to values of type ['a].

    The JSON Schema projection and the runtime codec are fixed at construction.
    Decoding and re-encoding use only the codec. *)

val make : 'a Jsont.t -> schema:Jsont.json -> 'a t
(** [make codec ~schema] is an input contract decoded with [codec].

    [schema] is the JSON Schema object advertised externally for this input; it
    is retained unchanged and later returned by {!schema}.

    Raises [Invalid_argument] if [schema] is not a JSON object. *)

val empty : unit t
(** [empty] is the no-argument object input contract.

    [empty] decodes exactly an object with no members to [()] and rejects
    non-objects and unknown members. Its schema is
    {!Mentat_llm.Tool.no_input_schema}. *)

val schema : _ t -> Jsont.json
(** [schema t] is the model-visible JSON Schema object retained by [t]. *)

val decode : 'a t -> Jsont.json -> ('a, string) result
(** [decode t json] decodes [json] with [t]'s codec.

    The result is [Ok input] if [json] satisfies the codec and
    [Error diagnostic] otherwise. [diagnostic] is a human-readable [Jsont]
    decoder message and is not a stable programmatic format; it carries no ANSI
    styling. *)

val encode : 'a t -> 'a -> Jsont.json
(** [encode t value] re-encodes [value] to JSON with [t]'s codec.

    [value] is a value [t] previously decoded (or one the callback returned in
    the same domain). {!Mentat_tool.Call.input} canonicalizes this encoding to
    source the durable claim input from a single owner.

    Raises [Invalid_argument] if the codec cannot encode [value]: a tool whose
    codec fails to round-trip a value it accepts is a construction defect. *)
