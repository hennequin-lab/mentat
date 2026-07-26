(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Bounded transcript reads — the tail view and backward pages.

    Two bounded reads over the one projection ({!Projection}): a {!Tail.t} is
    the feed head, the session's pending decision, and the last page of
    committed facts — a frontend's O(view) first paint instead of O(history); a
    {!Page.t} is a bounded page of facts strictly before a position — one older
    page of a scroll-up. Both carry the position to continue backward from, so
    paging is one read direction over the already-stable monotone sequence and
    needs no new state.

    Both are provably consistent with the replayed feed because one projector
    serves them: they carve a bounded sub-range of the same position-tagged
    facts {!Projection.all} yields, so a page served from a live hub's
    materialized array and one folded cold from the journal are byte-identical.
    The carving reads a projection through a random-access reader
    ([~length]/[~get]) so the live path stays O(page) over the hub array and the
    cold path is one O(J) fold then the same carve.

    These compose {!Position.t}, {!Fact.t}, and the session's own decision value
    whole; the module re-encodes nothing, per the firewall law. *)

(** {1:page Backward pages} *)

module Page : sig
  (** A bounded page of committed facts, oldest first, with its backward
      continuation. *)

  type t = {
    facts : (Position.t * Fact.t) list;
        (** The page's committed facts in feed order, oldest first. *)
    before : Position.t option;
        (** The position to continue backward from: [Some p] is passed as the
            next {!before} read's position; [None] when the page reaches the
            feed's first fact and nothing older remains. *)
  }

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same facts, in order, and
      the same continuation. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a page for diagnostics. Not stable storage syntax. *)

  val jsont : t Jsont.t
  (** [jsont] maps a page to a JSON object carrying the wire envelope [v], the
      ordered [facts] (each a [{position, fact}] pair), and the optional
      [before] continuation, omitted at the feed's beginning. Strict: unknown
      members and an unexpected [v] are rejected. *)

  val recent : length:int -> get:(int -> Position.t * Fact.t) -> n:int -> t
  (** [recent ~length ~get ~n] is the last [n] facts of a projection of [length]
      position-tagged facts read by [get] — an O(1) random-access reader over a
      hub's materialized array or a cold journal fold — oldest first. [before]
      is the first returned fact's position when older facts remain, else
      [None]. It reads at most [n] entries; [n] at most [0] yields an empty
      page. *)

  val before :
    owner:Mentat_session.Id.t ->
    length:int ->
    get:(int -> Position.t * Fact.t) ->
    Position.t ->
    n:int ->
    (t, Position.Invalid.t) result
  (** [before ~owner ~length ~get p ~n] is the last [n] facts strictly before
      [p] in the projection [owner] owns, oldest first. [p] is
      membership-validated exactly as {!Projection.after}: a candidate naming
      another session is [Error (Foreign_session _)], and one whose sequence
      names no emitted fact of this feed is [Error (Not_in_feed _)] — neither a
      token this projector produced, so a forged position never returns a
      silently truncated page. [before] is the first returned fact's position
      when older facts remain, else [None]. *)
end

(** {1:tail The tail view} *)

module Tail : sig
  (** The tail view — a session's head position, its pending decision, and the
      last page of committed facts — one bounded read for a frontend's first
      paint. *)

  val default_n : int
  (** [default_n] is the page size a read serves when the caller names none:
      [100] facts. *)

  val max_n : int
  (** [max_n] is the largest page a read serves: [1000] facts. A responder
      clamps a larger request to this cap, so a caller cannot force an
      O(history) read. *)

  type t = {
    head : Position.t option;
        (** The feed's head position — the SSE resume token and the cache
            validator that a new head invalidates — or [None] when the session
            has projected no fact. *)
    pending : Mentat_session.Decision.Requested.t option;
        (** The session's open decision when the head awaits one — the same
            session-owned packed value the feed and
            {!Mentat_client.pending_decision} carry — else [None]. *)
    page : Page.t;
        (** The last {!default_n} (or the requested [n]) committed facts, oldest
            first, with the backward continuation for the next older page. *)
  }

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same head, pending
      decision, and page. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a tail for diagnostics. Not stable storage syntax. *)

  val jsont : t Jsont.t
  (** [jsont] maps a tail to a JSON object carrying the wire envelope [v], the
      optional [head] and [pending] (omitted when absent), and the [page].
      Strict decode. *)

  val of_projection :
    length:int ->
    get:(int -> Position.t * Fact.t) ->
    n:int ->
    pending:Mentat_session.Decision.Requested.t option ->
    t
  (** [of_projection ~length ~get ~n ~pending] is the tail over a projection of
      [length] position-tagged facts read by [get]: [head] is the last fact's
      position (or [None] when [length] is [0]), [page] is {!Page.recent} of
      size [n], and [pending] is threaded through from the session's suspension.
      It reads at most [n]+1 entries. *)
end
