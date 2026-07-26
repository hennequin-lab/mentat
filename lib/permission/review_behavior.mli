(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Per-run handling of review outcomes.

    Review behavior is permission value vocabulary consumed by the host shell
    that interprets policy decisions: what a run does with accesses whose policy
    outcome is review. It is run posture, not policy — it never changes pure
    evaluation ({!Policy.decide}), and denials are never bypassed. *)

(** The type for per-run review behavior. *)
type t =
  | Enforce  (** Review outcomes ask for reviewer input. *)
  | Bypass
      (** Review outcomes proceed without a permission boundary. Denials are
          never bypassed. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same behavior. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t]'s stable spelling, [enforce] or [bypass]. *)

val jsont : t Jsont.t
(** [jsont] maps behaviors to and from the JSON strings ["enforce"] and
    ["bypass"]. Any other value is a decoding error. *)
