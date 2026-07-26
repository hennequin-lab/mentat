(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Feed positions.

    A position names the point immediately after one committed projection in one
    session's feed. The v1 encoding is the owning session id plus the
    per-session journal sequence of the event whose fact the position follows;
    both components are stable across process, host, and working directory,
    encoding no pid, host, or path. Construction and accessors expose that v1
    representation to protocol implementations and diagnostics. They do not
    establish feed membership: {!Projection.after} is the one authoritative
    validator (no [compare] — resumption threads a token back rather than
    imposing an order across feeds). *)

type t
(** The type for feed positions. Invariant: the sequence is non-negative. *)

val make : session:Mentat_session.Id.t -> seq:int -> t
(** [make ~session ~seq] constructs a candidate for the position immediately
    after a fact projected from [session]'s journal event at zero-based sequence
    [seq]. The value is not a resume token until {!Projection.after} validates
    membership.

    Raises [Invalid_argument] if [seq] is negative. *)

val session : t -> Mentat_session.Id.t
(** [session t] is the session named by [t]. Reading it is useful for protocol
    diagnostics; callers must not treat it as a membership check.
    {!Projection.after} rejects a foreign position structurally. *)

val seq : t -> int
(** [seq t] is [t]'s zero-based journal sequence. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] name the same point of the same
    session's feed. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a position for diagnostics. The output is not stable storage
    syntax. *)

val jsont : t Jsont.t
(** [jsont] maps positions to JSON objects, rejecting unknown members and a
    negative sequence on decode. Decoding validates the token's local shape;
    {!Projection.after} validates membership in a concrete feed. *)

module Invalid : sig
  (** Position membership failures — why a threaded position is not a legitimate
      resume token for a feed. Produced by {!Projection.after} and carried by
      {!Error.Invalid_position}; there is no second isomorphic error. *)

  (** The type for a rejected position. [nonrec] makes {!Not_in_feed} carry the
      outer feed position type, not this failure type. *)
  type nonrec t =
    | Foreign_session of {
        expected : Mentat_session.Id.t;
        actual : Mentat_session.Id.t;
      }  (** The candidate names [actual], not the expected session. *)
    | Not_in_feed of t
        (** The position names no fact-emitting event of its own session — a
            forged-future sequence past the journal head, or a sequence whose
            event projects no fact. The projector emits positions only at
            emitted facts, so neither is a token this seam produced. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same failure. *)

  val message : t -> string
  (** [message t] is a human-readable summary. Presentation only. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats {!message} output. *)

  val jsont : t Jsont.t
  (** [jsont] maps a failure to a JSON object by a per-arm tag, rejecting
      unknown tags and members on decode. *)
end
