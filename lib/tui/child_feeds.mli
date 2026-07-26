(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Runtime-owned observation of delegated child sessions.

    [Child_feeds] owns the Eio fibers and client feed handles used for the
    thread strip's live observations and for one read-only drill-in. It forwards
    the client's exact live and replay outcomes: it does not derive or fold a
    second child-status model, or expose feed handles to the application
    reducer.

    A manager is bound to the switch supplied to {!create}. Live observations
    and drill-ins start at [`Beginning], replay the complete journal in order,
    and then remain attached to the tail. Live observation stops after the
    delegated turn settles; drill-in continues tailing later turns. *)

(** {1:types Types} *)

module Generation : sig
  type t
  (** The type for one observation generation.

      A generation distinguishes a newly opened observation from callbacks
      already admitted by an observation it replaced. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] identify the same observation. *)
end

(** The type for why a child session is being observed. *)
type observation =
  | Live
      (** A beginning replay that reconstructs committed child status, tails
          live updates, and stops when the delegated turn settles. *)
  | Drill
      (** A read-only full-history observation that replays and then tails. *)

type t
(** The type for the child-feed owner.

    A manager has at most one live observation per exact child session id and at
    most one drill-in. Its state transitions are serialized across the Eio
    fibers using it. *)

(** {1:ownership Construction and ownership} *)

val create :
  sw:Eio.Switch.t ->
  client:Mentat_client.t ->
  emit:
    (observation:observation ->
    generation:Generation.t ->
    child:Mentat_session.Id.t ->
    (Mentat_client.Feed.outcome, Mentat_protocol.Error.t) result ->
    unit) ->
  t
(** [create ~sw ~client ~emit] is a child-feed owner bound to [sw].

    Whenever the manager reports an outcome, [emit] receives the exact child id
    and the exact result returned by the client feed. In particular, committed
    values retain their {!Mentat_protocol.Position.t}, facts and progress are
    not projected into a second status vocabulary, and closure remains
    {!Mentat_client.Feed.Closed}. Outcomes from one observation are emitted in
    feed order; outcomes from different children may interleave.

    Callbacks are never made inline by {!follow_live} or {!drill}. Before each
    callback the manager admits the outcome only if its generation is still
    current. A callback admitted immediately before a concurrent close or
    replacement may finish afterwards and retains its old [generation]; the
    reducer must discard generations it no longer owns. All other late callbacks
    are suppressed. If [emit] raises, that observation is closed and the
    exception is contained so sibling feeds remain live.

    The exceptions to reporting are deliberate closure and one bounded recovery.
    Manager-initiated closure is suppressed because its [Closed] carries no
    child-session information. An unexpected [Closed] is re-followed at most
    once when an exact last position permits [`After position]. A drill-in that
    has emitted no committed item may instead retry [`Beginning], which cannot
    duplicate a durable row. A live observation with no position and a second
    [Closed] are emitted and stopped, preventing a busy re-follow loop. An
    [Invalid_position] returned for the manager's own previously delivered
    position is surfaced unchanged and stops: because callers cannot inject a
    position here, restarting at [`Beginning] would duplicate already emitted
    history rather than recover honestly. [Unavailable] is likewise surfaced
    unchanged and stops only that observation.

    Releasing [sw] closes all feeds. Calling other operations after [sw] starts
    releasing is invalid. *)

(** {1:live Live child observations} *)

val follow_live : t -> Mentat_session.Id.t -> Generation.t
(** [follow_live t child] follows [child] from [`Beginning] and is the
    observation generation receiving its callbacks. If [child] is already
    followed, it is left untouched and its existing generation is returned.

    The pull fiber replays and drains every update, including facts that the
    current UI does not render, so a resumed parent reconstructs the child's
    exact committed state rather than leaving a historical delegation as a stale
    edge-only row. A {!Mentat_protocol.Fact.Turn_settled} outcome is emitted and
    then closes and removes the live observation. *)

(** {1:drill Read-only drill-in} *)

val drill : t -> Mentat_session.Id.t -> Generation.t
(** [drill t child] replaces the current drill-in with a new observation of
    [child] from [`Beginning] and is its generation. The old feed is closed
    before the new pulling fiber can emit.

    The complete committed history is emitted in order and the observation then
    tails. A turn settlement does not close a drill-in: a child may acquire a
    later turn, and [`Beginning] has no replay-boundary sentinel. *)

val close_drill :
  t -> child:Mentat_session.Id.t -> generation:Generation.t -> unit
(** [close_drill t ~child ~generation] closes the drill-in only if both [child]
    and [generation] still identify the current one. It is otherwise a no-op, so
    a delayed close cannot tear down a replacement observation. *)

val close_pane : t -> unit
(** [close_pane t] removes and closes every live observation and the current
    drill-in. It is the threads-pane teardown operation and is idempotent. *)
