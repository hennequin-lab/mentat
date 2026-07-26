(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The durable task board.

    A board is the whole task list a turn replaces atomically. Its items carry
    an owner, content, status, priority, and position. The board is a
    self-validating fact: it enforces unique ids, contiguous per-owner
    positions, and at most one in-progress task per owner. Replay stores each
    replacement whole; there is no per-item mutation event. *)

(** {1:ids Identifiers} *)

module Id : sig
  type t
  (** The type for stable task identifiers, supplied by the model and unique per
      board.

      Invariant: an identifier's stable textual form is non-empty. *)

  val of_string : string -> t
  (** [of_string s] is [s] as a task id.

      Raises [Invalid_argument] if [s] is empty. *)

  val to_string : t -> string
  (** [to_string id] is [id]'s stable string representation. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same task id. *)

  val compare : t -> t -> int
  (** [compare a b] orders ids by their stable string representations,
      compatibly with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an id for diagnostics. The output is not stable storage
      syntax. *)

  val jsont : t Jsont.t
  (** [jsont] maps task ids to JSON strings, validating the non-empty invariant
      of {!of_string} on decode. *)
end

(** {1:owners Owners} *)

module Owner : sig
  (** The type for a task owner: the main session. *)
  type t = Main  (** The main session owns the task. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same owner. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an owner for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps owners to JSON values by a per-arm tag. *)
end

(** {1:enums Status and priority} *)

module Status : sig
  (** The type for a task's lifecycle status. *)
  type t = Pending | In_progress | Completed | Cancelled

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same status. *)

  val to_string : t -> string
  (** [to_string t] is [t]'s stable lowercase tag. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a status for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps statuses to their stable tags, rejecting unknown tags. *)
end

module Priority : sig
  (** The type for a task's priority. *)
  type t = High | Medium | Low

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same priority. *)

  val to_string : t -> string
  (** [to_string t] is [t]'s stable lowercase tag. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a priority for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps priorities to their stable tags, rejecting unknown tags. *)
end

(** {1:errors Errors} *)

module Error : sig
  (** Task and board construction errors. *)
  type t =
    | Empty_content  (** An item's content was empty. *)
    | Negative_position  (** An item's position was negative. *)
    | Duplicate_id of Id.t  (** A task id appeared more than once. *)
    | Non_contiguous_positions
        (** An owner's positions were not contiguous from zero. *)
    | Multiple_in_progress  (** An owner had more than one in-progress task. *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an error for diagnostics. *)
end

(** {1:items Items} *)

module Item : sig
  type t = private {
    id : Id.t;
    owner : Owner.t;
    content : string;
    status : Status.t;
    priority : Priority.t;
    position : int;
  }
  (** The type for one task. Invariant: [content] is non-empty and [position] is
      non-negative. *)

  val make :
    id:Id.t ->
    owner:Owner.t ->
    content:string ->
    ?status:Status.t ->
    ?priority:Priority.t ->
    position:int ->
    unit ->
    (t, Error.t) result
  (** [make ~id ~owner ~content ?status ?priority ~position ()] is a task.

      [status] defaults to {!Status.Pending} and [priority] to
      {!Priority.Medium}. It is a structured {!Error.t} if [content] is empty or
      [position] is negative. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same item. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an item for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps items to JSON values, validating the same invariants as
      {!make} on decode. *)
end

(** {1:boards Boards} *)

module Board : sig
  type t
  (** The type for a task board. Invariant: ids are unique across the board;
      each owner's positions are contiguous from zero; and no owner has more
      than one in-progress task. *)

  val empty : t
  (** [empty] is the board with no tasks. *)

  val make : Item.t list -> (t, Error.t) result
  (** [make items] is a board of [items].

      It is a structured {!Error.t} if ids are not unique, if an owner's
      positions are not contiguous from zero, or if an owner has more than one
      in-progress task. *)

  val items : t -> Item.t list
  (** [items t] is [t]'s items, in the order supplied to {!make}. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same board. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a board for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps boards to JSON values, validating the same invariants as
      {!make} on decode. *)
end
