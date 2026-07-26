(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Token usage reported by a model request.

    Usage is provider-reported accounting data. Lanes are disjoint and
    non-negative so callers own their billing, display, and aggregation policy.
*)

type t = private {
  input : int;
  output : int;
  reasoning : int;
  cache_read : int;
  cache_write : int;
}
(** The type for token usage.

    All counts are non-negative and lanes are disjoint:

    - [input] is non-cached input;
    - [cache_read] is cached input read;
    - [cache_write] is cached input written;
    - [output] is visible output;
    - [reasoning] is non-visible reasoning output. *)

val make :
  input:int ->
  output:int ->
  ?reasoning:int ->
  ?cache_read:int ->
  ?cache_write:int ->
  unit ->
  t
(** [make ~input ~output ?reasoning ?cache_read ?cache_write ()] is token usage.
    Optional lanes default to [0].

    Raises [Invalid_argument] if any count is negative. *)

val zero : t
(** [zero] is usage with every lane set to [0]. *)

val add : t -> t -> t
(** [add a b] is the lane-wise sum of [a] and [b]. It is associative,
    commutative, and has identity {!zero}; use it to accumulate usage across
    turns.

    {b Note.} [add] combines the usage of {e distinct} turns. It is not the way
    to reconcile the repeated {e cumulative} usage snapshots a provider may emit
    within one stream: those are collapsed to a single total inside the adapter,
    and summing them would over-count.

    Raises [Invalid_argument] if any lane overflows. *)

val input_total : t -> int
(** [input_total t] is [t.input + t.cache_read + t.cache_write].

    Raises [Invalid_argument] if the total overflows. *)

val output_total : t -> int
(** [output_total t] is [t.output + t.reasoning].

    Raises [Invalid_argument] if the total overflows. *)

val sum_lanes : t -> int
(** [sum_lanes t] is the sum of every disjoint lane in [t].

    Raises [Invalid_argument] if the total overflows. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have the same counts. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] for diagnostics. *)

val jsont : t Jsont.t
(** [jsont] maps usage values to and from JSON objects.

    Decoding errors if any lane is negative. *)
