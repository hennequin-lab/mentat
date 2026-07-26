(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** OCaml module names. *)

type t
(** An OCaml compilation-unit name. *)

val make : string -> t
(** [make name] is an OCaml module name.

    [name] must be an ASCII OCaml module identifier: it starts with an uppercase
    letter and continues with letters, digits, underscores, or apostrophes.
    Raises [Invalid_argument] otherwise. *)

val compare : t -> t -> int
(** [compare a b] orders module names lexicographically. *)

val to_string : t -> string
(** [to_string t] is [t] as a string. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] as its module name. *)
