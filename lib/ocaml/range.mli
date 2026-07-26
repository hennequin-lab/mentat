(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Source ranges over {!Position.t} endpoints. *)

type t
(** A half-open source range.

    [start] is included and [end_] is excluded. Empty ranges are valid and
    represent points. *)

val make : start:Position.t -> end_:Position.t -> t
(** [make ~start ~end_] is the half-open range from [start] to [end_].

    Raises [Invalid_argument] if [end_] is before [start]. *)

val point : Position.t -> t
(** [point p] is an empty range at [p]. *)

val start : t -> Position.t
(** [start t] is [t]'s included start position. *)

val end_ : t -> Position.t
(** [end_ t] is [t]'s excluded end position. *)

val contains : outer:t -> t -> bool
(** [contains ~outer t] is [true] iff [t] lies within [outer], that is [outer]'s
    start is at or before [t]'s start and [t]'s end is at or before [outer]'s
    end. *)

val compare : t -> t -> int
(** [compare a b] orders ranges by start position, then by end position. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have equal start and end positions. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] as ["start-end"]. *)
