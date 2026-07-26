(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The authority that resolved a decision.

    A principal names {e who} answered a {!Decision}. Every resolution records
    one, so an audit of a durable journal attributes each decision to its true
    author — an unattended-policy denial is attributed to the policy, never to
    the user. The type is closed and versioned so a future principal (a remote
    reviewer, a device route) is an additive, loud change rather than a silent
    free string. *)

type t = private
  | Local_user  (** The local interactive user. *)
  | Unattended_policy
      (** An unattended host policy. It may only deny a permission;
          {!Decision.resolve} enforces this. *)

val local_user : t
(** [local_user] is the local interactive user. *)

val unattended_policy : t
(** [unattended_policy] is an unattended host policy. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same principal. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a principal for diagnostics. *)

val jsont : t Jsont.t
(** [jsont] maps a principal to its stable tag (["local_user"] or
    ["unattended_policy"]) and rejects any other tag on decode, so a widened
    principal set fails loudly on old readers. *)
