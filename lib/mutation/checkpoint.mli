(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Durable boundary capture facts.

    A checkpoint records that the workspace adapter captured (or failed to
    capture) the workspace at a boundary. Identity {e is} the boundary: each
    boundary occurs at most once per turn or revert, so a continuation process
    constructing the same boundary re-derives the same id and detects the
    existing capture through {!State}.

    There is deliberately no ad-hoc, unkeyed boundary: an arm not keyed by its
    turn or revert would let two captures collide on one identity and break the
    re-derivation guarantee. Ad-hoc user checkpoints, should a consumer name
    them, enter as a distinct boundary arm keyed by an externally-minted id —
    never as an unkeyed escape hatch. *)

module Id : sig
  type t
  (** The type for checkpoint identifiers, derived deterministically from the
      boundary —
      [H("mentat.mutation.checkpoint.v1", boundary tag, turn or revert id)]. A
      checkpoint constructor accepts a boundary, never an id.

      Invariant: an identifier's stable textual form is non-empty. *)

  val of_string : string -> t
  (** [of_string s] is [s] as a checkpoint id.

      Raises [Invalid_argument] if [s] is empty. *)

  val to_string : t -> string
  (** [to_string id] is [id]'s stable string representation. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same checkpoint id. *)

  val compare : t -> t -> int
  (** [compare a b] orders ids by their stable string representations,
      compatibly with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an id for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps checkpoint ids to JSON strings, validating the non-empty
      invariant of {!of_string} on decode. *)
end

(** The type for capture boundaries — the checkpoint's identity. *)
type boundary =
  | Before_turn_tools of Mentat_session.Turn.Id.t
      (** The conservative capture before the turn's first tool callback. The
          engine takes it regardless of the tool's declared permissions, so no
          write escapes a capture on the ground that it was not expected. *)
  | After_recovery of Mentat_session.Turn.Id.t
      (** The fresh conservative capture a recovered turn takes before its next
          tool callback. Distinct from {!Before_turn_tools} because a pre-crash
          capture cannot attest the post-crash workspace — the crash window may
          hold writes completed after that capture — so a recovered turn takes a
          fresh capture and never reuses the pre-crash one. One per turn: the
          recovery point is its identity. *)
  | After_turn of Mentat_session.Turn.Id.t
      (** Closes the turn's opaque-writer attribution window. *)
  | Before_revert of Revert_data.Id.t
      (** Captured before applying the named revert, so the revert is itself
          undoable. *)

module Snapshot : sig
  type t
  (** The type for the abstract durable snapshot handle: backend identity and
      backend reference, serialized by this codec, interpreted only by the
      workspace adapter that minted it. No field of it is a matching surface. *)

  val make : backend:string -> reference:string -> t
  (** [make ~backend ~reference] is the snapshot handle the adapter mints.

      Raises [Invalid_argument] if [backend] or [reference] is empty. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same handle. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a handle for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps handles to JSON objects. *)
end

module Capture : sig
  (** Durable capture outcomes. *)

  module Failure : sig
    type t
    (** The type for the structured capture failure the adapter minted: a closed
        failure kind on the wire plus a presentation-only message — never a free
        backend string and never a matching surface. *)

    val make : message:string -> t
    (** [make ~message] is a capture failure with diagnostic [message].

        Raises [Invalid_argument] if [message] is empty. *)

    val message : t -> string
    (** [message t] is the diagnostic. Presentation only. *)

    val equal : t -> t -> bool
    (** [equal a b] is [true] iff [a] and [b] are the same failure. *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats a failure for diagnostics. *)

    val jsont : t Jsont.t
    (** [jsont] maps failures to JSON objects under a closed kind tag, rejecting
        unknown kinds. *)
  end

  (** The type for capture outcomes. *)
  type t =
    | Available of { snapshot : Snapshot.t; excluded : int }
        (** The capture succeeded; [excluded] counts paths the backend could not
            cover — coverage honesty for partial snapshots. *)
    | Degraded of { failure : Failure.t }  (** The capture failed. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same outcome. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an outcome for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps outcomes to JSON objects by a per-arm tag, rejecting unknown
      tags and negative exclusion counts. *)
end

type t
(** The type for checkpoint facts. There is no session member — the ledger is
    per-session, so session identity is a storage-lookup input — and no root
    member: identity {e is} the boundary, and a capture at a boundary covers the
    whole workspace. Which roots the snapshot reaches is the adapter's, carried
    inside the opaque {!Snapshot.t}; per-root coverage gaps surface as the
    capture's [excluded] count, never as parallel per-root facts one boundary
    identity would conflate. *)

val make : boundary:boundary -> capture:Capture.t -> t
(** [make ~boundary ~capture] is the validated checkpoint for [capture] at
    [boundary]. The workspace adapter uses it to record a capture. The id
    derives internally from [boundary]; there is no caller-supplied id.

    Raises [Invalid_argument] if an [Available] capture carries a negative
    exclusion count. *)

val id : t -> Id.t
(** [id t] is [t]'s boundary-derived id. Deterministic in the boundary:
    [Before_turn_tools t] and [After_turn t] occur at most once per turn, and
    [Before_revert r] at most once per revert id. *)

val boundary : t -> boundary
(** [boundary t] is [t]'s capture boundary. *)

val capture : t -> Capture.t
(** [capture t] is [t]'s capture outcome. *)

val snapshot : t -> Snapshot.t option
(** [snapshot t] is [Some s] iff the capture is [Available]. A [Degraded]
    checkpoint captured no snapshot, so it cannot serve as a revert base or be
    referenced by a later event. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same fact. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a checkpoint for diagnostics. *)

val jsont : t Jsont.t
(** [jsont] maps checkpoint facts to JSON objects. The id is not serialized: it
    is a pure function of the boundary, so {!id} recomputes it on demand and a
    stored id could only agree with — or contradict — its derivation. *)

(** {1:derivation Identifier derivation} *)

val id_of_boundary : boundary -> Id.t
(** [id_of_boundary b] is the identifier of every checkpoint constructed at [b].
    It is a pure projection: it performs no capture and constructs no
    checkpoint. {!State.checkpoint_at} uses the same derivation for
    boundary-keyed lookup, and {!id} is [id_of_boundary] of a checkpoint's own
    boundary. *)
