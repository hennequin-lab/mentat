(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The tree-capacity permit and the child bookkeeping.

    One per process. Stateless about status: it holds the edge-keyed permit set,
    the in-memory settled-results buffer, and the child-to-parent index — all
    rebuilt from journals on recovery. A permit is held by a delegation edge
    from its grant until that edge's first settlement, so releases pair with
    reservations structurally. Every durable fact (the edge, the child's turns,
    the wait answer) lives in a journal; child status derives from the child
    session. The split is Erlang-shaped: this module is the ETS-like table — the
    in-memory permit, results, and child-index bookkeeping, rebuilt from
    journals and deciding nothing about lifecycle — while the runtime is the
    supervisor that drives fibers and enacts the delegation choreography. This
    module never creates a driver. *)

type child_result = Mentat_llm.Content.t list * Mentat_llm.Usage.t option
(** The type for a settled child's final result: content plus provider usage,
    both derived from the child journal. *)

type t
(** The type for the process scheduler. *)

val create : capacity:int -> t
(** [create ~capacity] is a scheduler granting at most [capacity] concurrent
    child reservations tree-wide.

    Raises [Invalid_argument] if [capacity] is not positive. *)

val try_reserve :
  t ->
  delegation:Mentat_session.Delegation.Id.t ->
  [ `Granted | `Refused of Mentat_agent_step.Step.Reservation.Refusal.t ]
(** [try_reserve t ~delegation] atomically takes [delegation]'s capacity permit.
    Two racing drivers get at most one new hold per free slot; re-reserving an
    edge that already holds is idempotently [`Granted]. *)

val force_reserve : t -> delegation:Mentat_session.Delegation.Id.t -> unit
(** [force_reserve t ~delegation] takes [delegation]'s permit unconditionally —
    the recovery rebuild: a journal-proven running child holds its slot even
    when the configured capacity shrank, driving the held set over capacity and
    blocking new spawns until it drains. *)

val release : t -> delegation:Mentat_session.Delegation.Id.t -> unit
(** [release t ~delegation] returns [delegation]'s permit — on a failed edge
    commit and when the child settles. Releasing an edge that holds no permit (a
    follow-up turn's re-settle) is a no-op, so no settlement can free capacity
    its edge never reserved. *)

val bind :
  t ->
  child:Mentat_session.Id.t ->
  parent:Mentat_session.Id.t ->
  delegation:Mentat_session.Delegation.Id.t ->
  unit
(** [bind t ~child ~parent ~delegation] indexes a driven child. Idempotent. *)

val parent_of :
  t ->
  Mentat_session.Id.t ->
  (Mentat_session.Id.t * Mentat_session.Delegation.Id.t) option
(** [parent_of t child] is the bound parent and delegation edge of [child]. *)

val note_settled : t -> Mentat_session.Delegation.Id.t -> child_result -> unit
(** [note_settled t delegation result] buffers a settled child's final result. A
    later settlement for the same edge replaces the earlier buffer entry
    (follow-up turns re-settle). *)

val settled_for :
  t ->
  Mentat_session.Delegation.Id.t list ->
  (Mentat_session.Delegation.Id.t * child_result) list
(** [settled_for t children] is the known-settled subset of [children], in
    [children] order — the [deliver_child] payload. *)
