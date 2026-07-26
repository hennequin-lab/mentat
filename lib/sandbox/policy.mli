(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Pure command confinement policies.

    A policy is the complete inert description of one {e confined} process
    route: readable and writable roots, protected write carveouts, network
    access, and the ephemeral private scratch root. Unconfined routes are not
    policies — direct and external execution are constructors of the sealed
    sandbox ({!Mentat_sandbox.direct}, {!Mentat_sandbox.external_}), because
    they carry no confinement to describe.

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

type reads =
  | All
  | Only of Lpath.Abs.t list
      (** Filesystem read access. [Only roots] confines reads to [roots]. *)

type t
(** The type for confinement policies. *)

val make :
  scratch:Lpath.Abs.t ->
  reads:reads ->
  writable_roots:Lpath.Abs.t list ->
  protected_paths:Lpath.Abs.t list ->
  denied_paths:Lpath.Abs.t list ->
  network:Network.t ->
  t
(** [make ~scratch ~reads ~writable_roots ~protected_paths ~denied_paths
     ~network] is a confinement policy with normalized lists.

    [scratch] is the per-run ephemeral private writable root the resolver mints;
    the generators grant it write access and it is included in [Only] read
    roots. It is {e not} folded into {!writable_roots}, and the durable
    {!Mentat_sandbox.Identity} normalizes it out.

    {b Normalization law} (upheld as an invariant, so no observer or generator
    re-checks): writable roots and the scratch are included in the [Only] read
    roots; duplicate and redundant descendant roots collapse (a root strictly
    within another is dropped); order is canonical; protected paths outside the
    writable roots are discarded. [protected_paths] are host-resolved data: this
    module owns no version-control or authority-metadata name and confines
    whatever absolute paths it is handed. *)

val scratch : t -> Lpath.Abs.t
(** [scratch t] is the ephemeral private writable root. *)

val reads : t -> reads
(** [reads t] is the read scope. *)

val writable_roots : t -> Lpath.Abs.t list
(** [writable_roots t] is the writable roots in canonical order. The scratch is
    not among them; it is a separate field the generators also grant. *)

val protected_paths : t -> Lpath.Abs.t list
(** [protected_paths t] is the protected absolute paths in canonical order. *)

val denied_paths : t -> Lpath.Abs.t list
(** [denied_paths t] is the denied absolute paths in canonical order — neither
    readable nor writable, whichever read or write root would otherwise admit
    them. Both generators lower denials last, so a denial beats every other
    grant in the policy. *)

val network : t -> Network.t
(** [network t] is the network access. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] describe the same confinement,
    including the scratch root and the denied paths. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] for diagnostics. The output is not stable storage
    syntax. *)
