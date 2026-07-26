(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Headless handling of permission reviews.

    This is permission-domain vocabulary consumed by hosts that cannot ask a
    reviewer interactively. It applies only when {!Policy.decide} requests
    review: a host may remain blocked for a reviewer, or answer that permission
    request with a denial. It does not turn policy denials into reviews and it
    says nothing about other session decision kinds. *)

(** The type for unattended permission-review handling. *)
type t = Block | Deny

val all : t list
(** [all] lists the values in stable declaration order. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same handling. *)

val to_string : t -> string
(** [to_string t] is [t]'s stable spelling, ["block"] or ["deny"]. *)

val of_string : string -> t option
(** [of_string s] parses ["block"] or ["deny"], and is [None] otherwise. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats {!to_string} output. *)
