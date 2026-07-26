(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The closed set of built-in enforcing platforms, as pure data.

    A backend names which generator lowers a policy; it carries no probe and no
    closure. The effect twin ([mentat.workspace_io]) probes availability and
    injects the outcome — [(Backend.t, Error.t) result] — into
    {!Mentat_sandbox.confined}, which generates the enforcement profile from the
    policy it seals. This library never probes.

    Because the resolver injects a {e selector} and not a built profile,
    {!Mentat_sandbox.confined} can only generate a profile from the policy it is
    sealing: a profile lowered from one policy paired with a different policy —
    so that evidence, obligations, and identity describe a policy the command
    does not run under — is unrepresentable, not merely rejected at runtime.

    This module is the one owner of backend metadata: the stable {!id} strings
    {!Evidence} and {!Identity} speak, and the fixed absolute {!executable}
    paths the enforcing prefixes start with. *)

type t =
  | Seatbelt
  | Bubblewrap
      (** A built-in enforcing platform: macOS Seatbelt or Linux Bubblewrap. *)

val id : t -> string
(** [id t] is the stable backend identity {!Evidence} records:
    ["macos-seatbelt"] for {!Seatbelt}, ["linux-bubblewrap"] for {!Bubblewrap}.
*)

val of_id : string -> t option
(** [of_id s] is the backend whose {!id} is [s], or [None]. It is the decoder
    {!Identity.jsont} composes. *)

val executable : t -> string
(** [executable t] is the fixed absolute path of [t]'s enforcing executable:
    ["/usr/bin/sandbox-exec"] for {!Seatbelt}, ["/usr/bin/bwrap"] for
    {!Bubblewrap}. Absolute, so [PATH] cannot inject a substitute. *)

val all : t list
(** [all] is every backend, in a stable order. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same backend. *)
