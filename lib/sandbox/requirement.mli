(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** How strong a run demands its confinement to be.

    A requirement is the [sandbox.require] field's type and the gate the
    executable applies after sealing and {e before} loading credentials or
    creating a session, so an unenforceable run fails closed before touching
    authority state. {!Mentat_sandbox.admits} decides whether a sealed sandbox's
    evidence meets a requirement; this module is the vocabulary it decides over.
*)

(** The type for enforceability requirements. *)
type t =
  | Off  (** any posture is accepted, including a refused confinement *)
  | Enforced_or_external
      (** an enforced confinement or a declared external boundary is required *)
  | Enforced
      (** an enforced confinement is required; external does not satisfy it *)

val all : t list
(** [all] is [[Off; Enforced_or_external; Enforced]]. *)

val of_string : string -> t option
(** [of_string s] is the requirement spelled by [s] (["off"],
    ["enforced-or-external"], or ["enforced"]), or [None]. *)

val to_string : t -> string
(** [to_string t] is [t]'s configuration spelling. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same requirement. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t]'s configuration spelling. *)

module Rejection : sig
  (** Why a sealed sandbox fails a requirement, as returned by
      {!Mentat_sandbox.admits}. *)

  type t =
    | Unenforceable of Error.t
        (** a confined run's backend could not enforce it *)
    | External_not_enforced
        (** a declared external boundary does not satisfy {!Enforced} *)

  val message : t -> string
  (** [message t] is a human-readable diagnostic for [t]. It is not a stable
      matching key. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats {!message}. *)
end
