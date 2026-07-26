(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Main-model selection.

    Selection resolves the main model for a run. It is a pure, deterministic
    function of a catalog, a caller-owned provider-preference predicate, an
    optional preferred model (the configured or explicit choice, already
    resolved through the catalog), and optional run requirements. It reads no
    clock, environment, or configuration and performs no I/O; catalog and model
    declaration order break every tie.

    Selection does not record {e where} a preferred model came from — a config
    layer, a [--model] flag. That provenance is configuration vocabulary; the
    caller that resolved the preference already holds it and frames failures
    with it. *)

(** {1:requirements Requirements} *)

module Requirement : sig
  (** A run's model requirements, checked against a candidate model.

      Selection always gates a candidate on lifecycle eligibility
      ({!Model.selectable}); a requirement adds capability and reasoning
      constraints on top. {!none} adds none of those, leaving only the lifecycle
      gate. Pure resolution with no gate at all is {!Catalog.find}, not
      selection. *)

  type t
  (** The type for a run's model requirements. *)

  module Mismatch : sig
    (** The reason a model fails a requirement. *)
    type t =
      | Lifecycle of Model.status
          (** The model is not {!Model.selectable}; its status is the reason. *)
      | Missing_capability of Model.Capability.t
          (** The model lacks a required capability. *)
      | Unsupported_reasoning of {
          requested : Mentat_llm.Request.Options.Reasoning_effort.t;
          supported : Mentat_llm.Request.Options.Reasoning_effort.t list;
        }
          (** The model does not support the required reasoning effort: it lacks
              {!Model.Capability.reasoning} or does not list [requested] in
              {!Model.supported_reasoning}. [supported] is the model's declared
              list. *)

    val message : t -> string
    (** [message m] is a human-readable diagnostic for [m]. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf m] formats {!message}. *)
  end

  val none : t
  (** [none] requires no capabilities or reasoning effort. A model passes it iff
      it is {!Model.selectable}. *)

  val make :
    ?capabilities:Model.Capability.t list ->
    ?reasoning_effort:Mentat_llm.Request.Options.Reasoning_effort.t ->
    unit ->
    t
  (** [make ()] is a requirement. Every argument defaults to empty or absent. *)

  val check : t -> Model.t -> (unit, Mismatch.t) result
  (** [check t model] is [Ok ()] iff [model] is {!Model.selectable} and
      satisfies every constraint in [t], and [Error mismatch] for the first
      constraint it fails. Lifecycle is checked first, then capabilities and
      reasoning effort. *)
end

(** {1:errors Errors} *)

module Error : sig
  type t =
    | No_model  (** No eligible main model resolved. *)
    | Requirement_mismatch of {
        selector : Selector.t;
        mismatch : Requirement.Mismatch.t;
        alternative : Selector.t option;
      }
        (** The preferred [selector] fails a requirement. [alternative], when
            present, is a same-provider model that satisfies the whole
            requirement set. *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] formats {!message}. *)
end

(** {1:selection Selection} *)

val main :
  catalog:Catalog.t ->
  provider_preferred:(Mentat_llm.Provider.t -> bool) ->
  ?preferred:Model.t ->
  ?requirements:Requirement.t ->
  unit ->
  (Model.t, Error.t) result
(** [main ~catalog ~provider_preferred ()] resolves the main model role.

    [requirements] defaults to {!Requirement.none}. Precedence, highest first:
    + [preferred], if present; if it fails [requirements] the result is
      [Requirement_mismatch], never a silent fallback;
    + the first eligible default whose provider satisfies [provider_preferred];
    + the first eligible provider default overall;
    + the first eligible model in catalog order;
    + otherwise [No_model].

    [provider_preferred] is a caller-owned preference. It biases only the
    derived-default rung; it is not consulted for [preferred], does not gate
    lifecycle eligibility or provider calls, and does not replace catalog
    checks.

    [preferred] is an already-resolved catalog model: the caller resolves the
    preferred selector with {!Catalog.find} when it reads its configuration —
    that is where the precise [Unknown_provider]/[Unknown_model] diagnostics
    surface, before selection runs — and keeps its own record of where the
    preference came from. *)

val small :
  main:Model.t ->
  ?preferred:Model.t ->
  ?requirements:Requirement.t ->
  unit ->
  Model.t
(** [small ~main ()] resolves the auxiliary small-model role.

    The small role is for cheap side calls (session auto-titling and similar);
    it is never allowed to block a run, so unlike {!main} it returns a total
    {!Model.t} rather than a result. Precedence, highest first:
    + [preferred], if present and it satisfies [requirements] (checked exactly
      as {!Requirement.check}, lifecycle included);
    + otherwise [main], the already-resolved main model, unconditionally.

    [requirements] defaults to {!Requirement.none}. [preferred] is an
    already-resolved catalog model: the caller resolves the configured
    [small_model] selector with {!Catalog.find} and passes it here; a selector
    that failed to resolve, or a resolved model that fails [requirements],
    silently falls back to [main] instead of surfacing an error. *)
