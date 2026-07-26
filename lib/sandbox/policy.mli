(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Pure command confinement policies.

    A policy is the complete inert description of one {e confined} process
    route: an ordered set of path clauses, a default for paths no clause names,
    and network access. Unconfined routes are not policies — direct and external
    execution are constructors of the sealed sandbox ({!Mentat_sandbox.direct},
    {!Mentat_sandbox.external_}), because they carry no confinement to describe.

    Constructing a policy enforces nothing and touches no filesystem: it is a
    value the resolver seals ({!Mentat_sandbox.confined}) before a command can
    use it. Every observer is total — a policy always has these fields — which
    is what lets the generators and lowering consume it without option-shaped
    dead ends. *)

module Network : sig
  (** Command network access. *)

  type t = Restricted | Enabled

  val all : t list
  (** [all] is [[Restricted; Enabled]]. *)

  val of_string : string -> t option
  (** [of_string s] is the network access spelled by [s], or [None] if [s] is
      neither ["restricted"] nor ["enabled"]. *)

  val to_string : t -> string
  (** [to_string t] is [t]'s configuration spelling. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] grant the same network access. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t]'s configuration spelling. *)
end

module Access : sig
  (** What one clause grants over the path it names and everything beneath it.
  *)

  type t =
    | Read  (** readable, not writable *)
    | Write  (** readable and writable *)
    | Deny  (** neither, whichever clause would otherwise admit it *)

  val compare : t -> t -> int
  (** [compare a b] orders by how much each takes away, [Read < Write < Deny].
      It is the tie-break when two clauses name one path, so the stronger
      survives. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same access. *)

  val to_string : t -> string
  (** [to_string t] is [t]'s diagnostic spelling. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t]'s diagnostic spelling. *)
end

type reads_default =
  | All  (** a path no clause names is readable *)
  | Denied  (** a path no clause names is unreachable *)

type t
(** The type for confinement policies. *)

val make :
  entries:(Lpath.Abs.t * Access.t) list ->
  reads_default:reads_default ->
  network:Network.t ->
  t
(** [make ~entries ~reads_default ~network] is a confinement policy.

    {b Resolution law.} A path's access is the access of the {e deepest} clause
    whose path is a prefix of it; where two clauses name one path, the stronger
    wins ({!Access.compare}). A path no clause covers takes [reads_default] and
    is never writable. This is the reference implementation's rule, and it is
    what makes a carveout an ordinary clause — a [Read] beneath a [Write] —
    rather than an exception threaded through the clause that contains it.

    {b Normalization law} (upheld as an invariant, so no observer or generator
    re-checks): duplicate paths collapse to their strongest access, and clauses
    are ordered shallowest-first. Both backends resolve by letting the last
    clause touching a path win — SBPL by rule order, bubblewrap by mount order —
    so emitting in this order implements the resolution law without either
    generator having to know it. Nothing is dropped: unlike a
    membership-filtered carveout, a clause is meaningful wherever it lies. *)

val entries : t -> (Lpath.Abs.t * Access.t) list
(** [entries t] are the clauses in emission order, shallowest first. *)

val reads_default : t -> reads_default
(** [reads_default t] is the access of a path no clause names. *)

val network : t -> Network.t
(** [network t] is the network access. *)

val writable_roots : t -> Lpath.Abs.t list
(** [writable_roots t] are the paths granted {!Access.Write}, in emission order.
    A path beneath one of them may still be denied or read-only by a deeper
    clause. *)

val readable_roots : t -> Lpath.Abs.t list
(** [readable_roots t] are the paths granted {!Access.Read} or {!Access.Write},
    in emission order. *)

val denied_paths : t -> Lpath.Abs.t list
(** [denied_paths t] are the denied paths, in emission order. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] describe the same confinement. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] for diagnostics. The output is not stable storage
    syntax. *)
