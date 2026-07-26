(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The sealed command sandbox — internal implementation of {!Mentat_sandbox.t}.

    The facade {!Mentat_sandbox} re-exports this module's value and operations
    and owns their public contract; the docs here are terse maintainer notes.
    The value is pure: a route constructor composes a policy with the resolver's
    backend outcome once, fixing evidence, escalation, and identity at seal
    time, and names the checks the effect twin performs ({!obligations}) rather
    than performing any I/O. *)

type t
(** A sealed command sandbox. *)

(** The sealed escalation stance, fixed from the policy's writable roots
    independent of the enforcement outcome (a refused-but-writable seal is still
    [Available]). *)
type escalation =
  | Available
      (** workspace-write shaped: escapable after a separate approval *)
  | Denied of Error.t  (** read-only shaped: no escalation *)
  | Ignored  (** unconfined or external: escalation is moot *)

module Obligation : sig
  (** A filesystem existence check the twin discharges before launch. *)

  type kind =
    | Exists
        (** checked with [lstat]: some entry must still exist. Existence only —
            it does not detect a kind change such as a directory replaced by a
            symlink, and does not follow a swapped symlink to its target *)
    | Directory  (** checked with [stat] + [S_DIR]: an existing directory *)

  type t

  val path : t -> Lpath.Abs.t
  val kind : t -> kind
  val pp : Format.formatter -> t -> unit
end

val confined :
  backend:(Backend.t, Error.t) result -> mutates:bool -> Policy.t -> t
(** [confined ~backend ~mutates policy] seals a confined route; see
    {!Mentat_sandbox.confined}. [Ok] lowers the profile from [policy]; [Error]
    refuses every command. Pure and total. *)

val direct : t
(** The intentionally unconfined route. *)

val external_ : t
(** The declared-external route. *)

val policy : t -> Policy.t option
(** [None] for the direct and external routes. *)

val evidence : t -> Evidence.t
(** The sealed posture, fixed at seal time; not per-command. *)

val escalated_evidence : t -> Evidence.t
(** The evidence an escalated command reports: always [Evidence.not_requested].
*)

val escalation : t -> escalation
val identity : t -> Identity.t

val obligations : t -> Obligation.t list
(** The existence checks the twin discharges before any launch; see
    {!Mentat_sandbox.obligations}. [[]] for direct, external, and refused
    routes. *)

val lower_argv :
  t -> cwd:Lpath.Abs.t -> string list -> (string list, Error.t) result
(** The pure per-command lowering; see {!Mentat_sandbox.lower_argv}.
    Precondition: [cwd] is realpath-resolved and {!obligations} discharged. *)

val lower_escalated_argv :
  t -> cwd:Lpath.Abs.t -> string list -> (string list, Error.t) result
(** As {!lower_argv} but drops confinement when {!escalation} is [Available]:
    never emits an enforcing prefix and applies no cwd containment; see
    {!Mentat_sandbox.lower_escalated_argv}. *)
