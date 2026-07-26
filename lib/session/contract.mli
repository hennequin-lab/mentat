(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The sealed turn execution contract.

    A contract is the exact set of accepted upstream values the engine freezes
    at turn start and stores inside a {!Turn.t}: the product {!Mode.t}, the
    model and request options, the provider-facing tool declarations, the
    effective permission {!Mentat_permission.Policy.t}, the
    {!Mentat_permission.Review_behavior.t}, and the sealed sandbox
    {!Mentat_sandbox.Identity.t}. Resume identity checks against these stored
    values; the engine seals them once and never re-resolves them for a running
    turn.

    The contract holds only accepted values — it re-invents no rule, computes no
    write flag (write authority is a policy decision over
    {!Mentat_permission.Request.t}), and owns no resume-drift workflow (that is
    the turn planner's, comparing these accepted values to live resolved ones).
    Conversation grants and conversation-lifetime rules layered over the
    accepted policy remain {!State} projections. *)

(** {1:mode Mode} *)

module Mode : sig
  (** The type for the durable product mode of a turn. *)
  type t =
    | Build  (** An ordinary building turn. *)
    | Plan  (** A read-only planning turn. *)
    | Review  (** A review turn. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same mode. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a mode for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps a mode to its stable tag, rejecting unknown tags. *)
end

(** {1:contracts Contracts} *)

type t
(** The type for a sealed turn execution contract. *)

val make :
  mode:Mode.t ->
  model:Mentat_llm.Model.t ->
  ?options:Mentat_llm.Request.Options.t ->
  declarations:Mentat_llm.Tool.t list ->
  ?output_tool:Mentat_llm.Tool.t ->
  policy:Mentat_permission.Policy.t ->
  review:Mentat_permission.Review_behavior.t ->
  sandbox:Mentat_sandbox.Identity.t ->
  unit ->
  t
(** [make ~mode ~model ?options ~declarations ?output_tool ~policy ~review
     ~sandbox ()] is a sealed contract. [options] defaults to
    {!Mentat_llm.Request.Options.default}. [declarations] is the exact
    provider-facing tool snapshot; [output_tool] defaults to [None]; [policy] is
    the exact effective permission policy; [sandbox] is the sealed confinement
    identity. *)

val mode : t -> Mode.t
(** [mode t] is [t]'s frozen product mode. *)

val model : t -> Mentat_llm.Model.t
(** [model t] is [t]'s frozen model. *)

val options : t -> Mentat_llm.Request.Options.t
(** [options t] is [t]'s provider-neutral request options. *)

val declarations : t -> Mentat_llm.Tool.t list
(** [declarations t] is [t]'s exact provider-facing tool snapshot. Compare on
    resume with {!Mentat_llm.Tool.equal}. *)

val output_tool : t -> Mentat_llm.Tool.t option
(** [output_tool t] is the synthetic [structured_output] declaration for a
    schema-constrained turn, or [None]. Its [input_schema] is the caller's
    schema; the step reads it both to add the tool to the request and to
    validate the model's terminating call. It is deliberately {e not} part of
    {!declarations} — it is dispatched by the step's own terminal branch, never
    through the catalog, so it stays out of the resume-drift declaration
    comparison. Sealed and replayed like {!declarations}; compared by {!equal}
    over its JSON value. *)

val policy : t -> Mentat_permission.Policy.t
(** [policy t] is [t]'s frozen effective permission policy. Runtime grants are
    layered over it by {!State}, not stored here. *)

val review : t -> Mentat_permission.Review_behavior.t
(** [review t] is [t]'s frozen review behaviour. *)

val sandbox : t -> Mentat_sandbox.Identity.t
(** [sandbox t] is [t]'s sealed sandbox identity. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same contract. Declarations
    and options compare over their JSON values, so a decoded contract equals an
    otherwise-identical constructed one. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a contract for diagnostics. The output is not stable storage
    syntax. *)

val jsont : t Jsont.t
(** [jsont] maps a contract to a JSON object composing the upstream codecs;
    unknown members are decode errors. *)
