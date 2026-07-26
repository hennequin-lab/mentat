(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Source positions. *)

type t
(** A source position.

    Lines are 1-based. Columns are 0-based byte offsets in the line. This
    matches OCaml compiler locations and keeps backend adapters lossless. *)

val make : line:int -> column:int -> t
(** [make ~line ~column] is a source position.

    Raises [Invalid_argument] if [line < 1] or [column < 0]. *)

val line : t -> int
(** [line t] is [t]'s 1-based line number. *)

val column : t -> int
(** [column t] is [t]'s 0-based column, a byte offset in the line. *)

val compare : t -> t -> int
(** [compare a b] orders positions by line, then by column. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have the same line and column. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] as ["line:column"]. *)
