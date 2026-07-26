(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Feed updates.

    Updates carry either a committed fact at its resume position or droppable
    progress. *)

(** The type for one feed item: a committed fact at its position, or a droppable
    pulse. Dropping every [Progress] value changes no fact and no query result;
    no completion is reported only through progress. *)
type t =
  | Committed of { position : Position.t; fact : Fact.t }
      (** A durable projected fact paired with the resume position emitted for
          it. Applications normally thread this value back to a feed; its public
          v1 shape does not by itself prove feed membership. *)
  | Progress of Progress.t
      (** A droppable, non-authoritative pulse with no resume position. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same update, by
    {!Position.equal}, {!Fact.equal}, and {!Progress.equal}. *)

val jsont : t Jsont.t
(** [jsont] maps updates to versioned JSON objects by a per-arm tag, rejecting
    unknown tags, unknown members, and an unexpected envelope version. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats an update for diagnostics. The output is not stable syntax. *)
