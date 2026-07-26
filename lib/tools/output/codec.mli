(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Shared summary-fact vocabulary bridging tool-output producers and
    presenters.

    Tool-specific modules own compact values and their {!Jsont.t} codecs.
    Producers combine one such value with bounded durable model-history text
    through {!encode}; presentation code recovers the same facts through
    {!decode} for both live and replayed outputs. Bodies remain solely in
    {!Mentat_tool.Output.text}. *)

val encode :
  'a Jsont.t ->
  text:string ->
  ?media:Mentat_llm.Content.t list ->
  ?truncated:bool ->
  'a ->
  Mentat_tool.Output.t
(** [encode jsont ~text ?media ?truncated value] is a tool output whose
    authoritative durable model-history text is [text] and whose compact summary
    JSON is [value] encoded by [jsont]. [media] is optional model-visible media
    blocks (e.g. an image a read returned), defaulting to [[]]; [truncated]
    defaults to [false].

    Raises [Invalid_argument] if [text] is empty or [jsont] cannot encode
    [value]. This indicates a defect in a tool's output construction. *)

val decode : 'a Jsont.t -> Mentat_tool.Output.t -> 'a option
(** [decode jsont output] is the compact summary value encoded in [output], or
    [None] if [output] has no JSON or its JSON does not satisfy [jsont]. A
    built-in presenter treats [None] as unavailable structured details; it must
    not promote {!Mentat_tool.Output.text} into a semantic summary. An extension
    presenter may expose that text only as separately bounded disclosure. *)
