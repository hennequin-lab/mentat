(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** One session's pull feed.

    A feed is the frontend's cancellable, pull-based view of one session's
    committed facts and droppable progress. The consumer pulls on its own fiber
    ({!next}); the feed is a thin wrapper over the construction {!seam} — it
    forwards each pull and contains a seam fault, forking nothing — so a slow
    reader never runs on an engine or provider fiber and never blocks the
    producer. The committed-before-progress delivery order (never-skip catch-up)
    and the bounded, drop-oldest progress ring are the producer's obligations;
    this wrapper adds no buffer of its own. Feeds are per-session and
    independent: a frontend follows N sessions as N feeds with no order merged
    across them, since positions are per-session. *)

type from = [ `Beginning | `Now | `After of Mentat_protocol.Position.t ]
(** The type for where a feed attaches. [`Beginning] replays the whole journal
    then tails; [`Now] tails from the current head; [`After p] resumes strictly
    after committed position [p]. *)

(** The type for one feed poll outcome. [Closed] is the feed's own local
    delivery fact — never a versioned {!Mentat_protocol.Error.t}; there is no
    [Feed_closed] wire value. *)
type outcome = Item of Mentat_protocol.Update.t | Closed

type seam = {
  next : unit -> (outcome, Mentat_protocol.Error.t) result;
      (** Blocks the pulling fiber cancellably until the next update, [Closed],
          or a structured error at catch-up (a [from] naming no committed fact
          of this feed is {!Mentat_protocol.Error.Invalid_position}). The
          producer owns delivery: committed facts before progress with no gaps,
          and bounded drop-oldest progress. [Ok Closed] after {!close},
          terminal. *)
  close : unit -> unit;
      (** Idempotent; releases the underlying observation and unblocks a blocked
          {!next} with [Ok Closed]. Cancels nothing. *)
}
(** The raw pull the construction driver returns for one followed session. It
    carries the full delivery contract; this library's {!subscribe} adds only
    switch binding and fault containment, kept off the engine seam. Constructed
    by the engine bridge supplied through the injected driver. *)

type t
(** The type for a wrapped feed. *)

val subscribe : sw:Eio.Switch.t -> seam -> t
(** [subscribe ~sw seam] wraps [seam] into a feed bound to [sw]: it registers
    [Eio.Switch.on_release sw] so releasing [sw] closes the feed, and forks
    nothing. This is construction glue; {!Mentat_client.follow_session} is the
    frontend entry point. *)

val next : t -> (outcome, Mentat_protocol.Error.t) result
(** [next t] forwards the seam's next pull on the caller's fiber: committed
    facts in order with no gaps before any progress, then droppable progress,
    exactly as the producer delivers them. [Ok Closed] after {!close}, terminal
    for this feed. A catch-up position error surfaces as
    {!Mentat_protocol.Error.Invalid_position}. A {!seam} whose [next] raises
    rather than returning an [Error] is contained: the feed surfaces a terminal
    {!Mentat_protocol.Error.Unavailable} once, then [Ok Closed] — a broken seam
    fails only its own feed, never the subscribing switch or its siblings.

    The consumer must keep pulling. Committed facts are never dropped; pausing a
    feed is {!close} plus a later re-follow, not ceasing to call [next]. *)

val close : t -> unit
(** [close t] is idempotent; it closes the underlying {!seam}, which wakes a
    blocked {!next} with [Ok Closed]. It cancels nothing. Undelivered committed
    facts are not drained — [Closed] is immediate — but stay recoverable by
    re-following at [`After] the last delivered position. *)
