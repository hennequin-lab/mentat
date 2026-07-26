(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The per-turn execution selection.

    An execution is the bundle of values the executable selects together for one
    turn and the engine seals into its contract: the dispatch {!Catalog}, the
    workspace capability {!Ports.workspace}, the effective permission
    {!Mentat_permission.Policy.t}, and the model-visible context
    {!Mentat_llm.Request.Prelude.t} (the system prompt, workspace instructions,
    and skills catalog). The executable's [execution_for_mode] and
    [delegated_execution] callbacks return one of these per mode, so the engine
    never reassembles context from parts — it drains workspace notices and
    appends them to this prelude, nothing more. *)

type t = private {
  catalog : Mentat_agent_step.Catalog.t;
      (** The validated dispatch surface for the turn. *)
  workspace : Ports.workspace;  (** The workspace capability for the turn. *)
  policy : Mentat_permission.Policy.t;
      (** The effective policy for the turn. *)
  prelude : Mentat_llm.Request.Prelude.t;
      (** The model-visible context sent before the transcript. *)
}
(** The type for a per-turn execution selection. The record is [private]: build
    one with {!make}, read its fields directly. *)

val make :
  catalog:Mentat_agent_step.Catalog.t ->
  workspace:Ports.workspace ->
  policy:Mentat_permission.Policy.t ->
  prelude:Mentat_llm.Request.Prelude.t ->
  t
(** [make ~catalog ~workspace ~policy ~prelude] is the execution selection with
    those four sealed values. *)

type selector =
  configured:Config.t ->
  model:Mentat_llm.Model.t ->
  sealed_declarations:Mentat_llm.Tool.t list option ->
  Mentat_session.Contract.Mode.t ->
  t
(** The type for a per-turn execution selector: applied at each new turn with
    the freshly resolved [configured] value, the [model] the turn contract
    seals, and [sealed_declarations] (the recovered tool set on active-turn
    recovery, [None] otherwise), it selects the {!t} the engine consumes for the
    turn's [mode]. *)

type factory =
  background:Eio.Switch.t ->
  selector * (unit -> Mentat_protocol.Process.View.t list)
(** The type for a per-session execution factory. The driver applies it once to
    its own nested per-session switch ([background]), yielding a per-turn
    {!selector} and a live background-process view over the session's registry
    (backing {!Mentat_client.running_processes}). *)

type delegated_factory =
  role:Mentat_session.Delegation.Role.t option ->
  background:Eio.Switch.t ->
  selector * (unit -> Mentat_protocol.Process.View.t list)
(** The type for a delegated child's execution factory: a {!factory} carrying
    the child's durable [role] (a specialized read-only subagent when present, a
    generic write-capable delegate when [None]). *)
