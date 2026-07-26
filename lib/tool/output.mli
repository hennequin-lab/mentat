(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Model-visible tool outputs.

    Tool callbacks return typed OCaml values inside {!Mentat_tool.Result.t}. An
    {!type:encoder} erases those typed values to {!type:t}, the model-visible
    output shape a call yields at its boundary.

    Text is the authoritative model-history projection because every model and
    log sink can consume it. A settled result stores that producer-bounded text
    durably and reuses it for live delivery and replay. JSON is optional compact
    durable summary data for hosts that need machine-readable facts; it does not
    duplicate bodies or own presentation wording. Callers must not reconstruct
    required behavior by parsing {!text}. *)

type t
(** The type for erased tool output. {!text} is non-empty durable model history;
    {!json} is optional compact durable summary data and never body content or
    mutation evidence. *)

val make :
  text:string ->
  ?json:Jsont.json ->
  ?media:Mentat_llm.Content.t list ->
  ?truncated:bool ->
  unit ->
  t
(** [make ~text ?json ?media ?truncated ()] is an erased tool output.

    [json] is already-encoded compact summary data, preserved as provided.
    [media] is optional model-visible media blocks, e.g. an image a read
    returned; it defaults to [[]], and [text] remains the authoritative
    log/replay projection. [truncated] is [true] iff [text] omits information
    because of a size or policy limit; it defaults to [false] and describes the
    text projection, not the optional JSON.

    Raises [Invalid_argument] if [text] is empty. *)

type 'a encoder = 'a -> t
(** The type for typed output encoders from ['a] to erased {!type:t}.

    Encoders are passed to {!Mentat_tool.make}; a call applies the encoder to
    the typed output carried by a callback's terminal {!Mentat_tool.Result.t}.
    Build one directly from {!make}. *)

val text : t -> string
(** [text t] is [t]'s authoritative durable model-visible text. The string is
    non-empty and remains in the stored result for replay. Its content and bound
    are owned by the producing tool. *)

val json : t -> Jsont.json option
(** [json t] is [Some json] if [t] carries compact durable summary facts and
    [None] otherwise. The model-visible content is still {!text}; [json] does
    not duplicate bodies or determine presentation wording. *)

val media : t -> Mentat_llm.Content.t list
(** [media t] is [t]'s model-visible media blocks in order (default [[]]).
    [text] remains the authoritative log/replay text; [media] is additional
    model-visible content, e.g. an image a read returned. *)

val truncated : t -> bool
(** [truncated t] is [true] iff [text t] was truncated. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have the same text, JSON, media, and
    truncation projections. *)

val jsont : t Jsont.t
(** [jsont] maps erased outputs to durable JSON, preserving {!text}, {!json},
    {!media}, and {!truncated}. The [media] member is absent when empty, so a
    text-only output stays byte-identical to a pre-media output; its presence
    raises the enclosing session document to version 2. *)
