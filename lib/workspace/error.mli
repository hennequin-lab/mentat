(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Workspace construction and admission failures.

    These structured errors distinguish a contradictory root set from a current
    directory whose root key is outside that set. They are pure address-model
    failures: path placement and binding use {!Resolve_error}, while filesystem
    observation failures belong to the host layer. Match on {!type:t} for
    recovery; {!message} and {!pp} are display diagnostics. *)

type t =
  | Conflicting_root of { existing : Root.t; duplicate : Root.t }
      (** [Conflicting_root { existing; duplicate }] means [duplicate] shares a
          durable key with [existing] but names a different logical directory,
          or shares its logical directory but uses a different key.

          [existing] is the first admitted root that conflicts with [duplicate].
      *)
  | Root_not_in_workspace of Root.Key.t
      (** [Root_not_in_workspace key] means a supplied current directory names
          [key], but the workspace admits no root with that identity. *)

val message : t -> string
(** [message error] is a human-readable diagnostic for [error]. The text is for
    display, not programmatic matching. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same error. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf error] formats [error] for diagnostics. *)
