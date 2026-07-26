(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The feed-serving seam.

    One hub per driven session: it holds the committed head — the current
    immutable session value plus its replayed mutation state — and its
    materialized projection, the sequence of every emitted fact, grown once per
    commit and shared by every feed. It also holds an advance condition and the
    subscribed feeds' bounded progress rings. The driver's whole fan-out line is
    non-blocking: advance the projection by the commit's events, broadcast,
    offer to the rings; no user code ever runs on a driver or provider fiber.

    The projection is materialized incrementally through the one projector
    ({!Mentat_protocol.Projection.advance}): each commit's events are folded
    onto the carried state, so the live projection is byte-identical to a
    from-scratch replay, split across commits. A feed then serves a suffix of
    that shared array by index — O(1) per fact, no re-fold — and catch-up from a
    position is an O(log E) membership lookup that yields a structured error for
    a foreign or forged position, never a silent skip. The seam's never-skip
    obligation: positions are minted only by that projector, and for any
    position this seam delivered, every committed fact strictly after it is
    served in order — a resumed feed can skip nothing. There is no
    per-subscriber history buffering: a slow reader costs only itself, indexing
    the same array. Progress is a queue, not control flow: overflow drops
    oldest, and no completion is reported only through progress. *)

module Hub : sig
  type t
  (** The type for a session's feed hub. *)

  val create : session:Mentat_session.t -> mutation:Mentat_mutation.State.t -> t
  (** [create ~session ~mutation] is a hub whose head is [session] joined with
      [mutation], its projection materialized by folding [session]'s whole
      journal once. On a fresh session this is empty; on re-follow after
      eviction it rebuilds a projection byte-identical to the evicted one. *)

  val publish :
    t ->
    delta:Mentat_session.Event.t list ->
    Mentat_session.t ->
    Mentat_mutation.State.t ->
    unit
  (** [publish t ~delta session mutation] advances the head to the newly
      committed [(session, mutation)] and grows the materialized projection by
      the facts [delta] emits — [delta] being exactly the events this commit
      appended. It folds only those events onto the carried state, so the cost
      is O([delta]), not the journal length, and broadcasts the advance. The
      driver passes the events it just committed; a projection built this way is
      byte-identical to a from-scratch one. Non-blocking. *)

  val sync : t -> Mentat_session.t -> Mentat_mutation.State.t -> unit
  (** [sync t session mutation] advances the head and projection to [session], a
      freshly loaded head that another process may have extended since this hub
      last published. It folds only [session]'s events beyond what the
      projection already covers, so a hub current with the store does no
      projection work; the cost is O(J) only when the journal genuinely grew.
      This is the follow / attach refresh path — a driver advances its own hub
      through {!publish} with the delta it commits. Non-blocking; broadcasts. *)

  val pulse : t -> Mentat_protocol.Progress.t -> unit
  (** [pulse t progress] offers [progress] to every subscribed feed's ring,
      dropping each ring's oldest entry on overflow, and broadcasts.
      Non-blocking; safe from a worker fiber. *)

  val head : t -> Mentat_session.t
  (** [head t] is the current committed session value. *)

  val feedless : t -> bool
  (** [feedless t] is [true] when no feed is subscribed to [t] — the feed side
      of the runtime's hub-retention rule. A feedless hub with no attached
      driver holds nothing but a pure re-projection of the journal, so its owner
      may release it: a later {!subscribe} rebuilds a byte-identical head by
      folding the same journal. The progress ring is never journal state, and a
      feedless hub has no ring to lose. *)

  val head_position : t -> Mentat_protocol.Position.t option
  (** [head_position t] is the position immediately after the last committed
      fact of the current head — the last entry of the materialized projection,
      read in O(1) — or [None] when the head projects no fact yet. It is the
      attach point a [`Now] feed subscribes after: a feed subscribed from it
      catches up nothing and tails only future facts. *)

  val projected_length : t -> int
  (** [projected_length t] is the number of emitted facts in the materialized
      projection. Paired with {!projected_entry} it is the O(1) random-access
      reader a bounded transcript read ({!Mentat_protocol.Transcript}) carves
      over the shared array without re-folding the journal. *)

  val projected_entry :
    t -> int -> Mentat_protocol.Position.t * Mentat_protocol.Fact.t
  (** [projected_entry t i] is the [i]th position-tagged fact of the
      materialized projection, in O(1). [i] must be in
      [\[0, projected_length t)]. *)

  val await : t -> (unit -> 'a option) -> 'a
  (** [await t check] runs [check ()] until it returns [Some v], suspending on
      the hub's broadcast between runs — the waiter is registered before each
      check, so an advance between check and wait is never lost. [check] must
      not block. *)
end

type t
(** The type for one pull feed over a hub. *)

val subscribe : ?from:Mentat_protocol.Position.t -> Hub.t -> t
(** [subscribe ?from hub] is a feed positioned after [from] ([None] reads the
    whole journal). [from] is not checked here: the first {!next} resolves it
    against the hub's materialized projection — the one membership check — so a
    foreign or forged [from] surfaces structurally at that first {!next} and
    never silently skips. *)

val next : t -> (Mentat_client.Feed.outcome, Mentat_protocol.Error.t) result
(** [next t] blocks until the feed's next update, closure, or a structured
    failure, yielding the client-owned {!Mentat_client.Feed.outcome} — the one
    delivery vocabulary this seam and its wrapper share. Committed facts drain
    before progress. It is [Ok Closed] after {!close} — terminal for this feed
    only. A threaded [from] position that names no committed fact of this
    session's feed is [Error (Invalid_position _)]. *)

val close : t -> unit
(** [close t] releases the feed's observation resources and wakes a blocked
    {!next}. It cancels nothing. *)
