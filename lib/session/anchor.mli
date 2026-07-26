(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Stable, replay-valid rewind positions.

    An anchor names a durable {!Turn.Id.t} and which edge of that turn a cut
    falls on. It is content-independent and transfer-survivable: the same turn
    id names the same boundary in any copy of the events. The event-log prefix
    length an anchor implies is {e derived} on demand by
    {!Mentat_session.resolve_anchor}, never stored in the anchor.

    Only turn boundaries are anchorable. A cut inside an active turn would
    replay to a dangling active turn, so intra-turn positions have no anchor.

    Anchors gain a codec only when a wire transport carries the rewind flow
    across processes; today rewind is an in-process, typed crossing, so this
    module exposes none. *)

(** {1:types Types} *)

(** The type for the boundary edge a cut falls on. *)
type edge =
  | Before
      (** The state just before the turn started. Rewinding here drops the turn
          and every later event. *)
  | After
      (** The state just after the turn finished. Rewinding here keeps the turn
          and drops every later event. Requires the turn to have finished. *)

type t
(** The type for a rewind anchor: a {!Turn.Id.t} paired with an {!edge}.

    Anchors carry no event count, session id, or store cursor. Two anchors are
    equal iff they name the same turn and the same edge. *)

(** {1:constructors Constructors} *)

val before_turn : Turn.Id.t -> t
(** [before_turn id] anchors just before turn [id] started. *)

val after_turn : Turn.Id.t -> t
(** [after_turn id] anchors just after turn [id] finished. *)

(** {1:queries Queries} *)

val turn : t -> Turn.Id.t
(** [turn t] is the turn [t] names. *)

val edge : t -> edge
(** [edge t] is the boundary edge [t] names. *)

(** {1:predicates Predicates and formatting} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] name the same turn and edge. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] for diagnostics. The output is not stable storage
    syntax. *)
