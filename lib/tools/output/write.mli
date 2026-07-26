(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Compact result quantities for [write_file].

    The line count describes the complete contents written, not diff additions;
    mutation facts remain the owner of actual change statistics. *)

(** The type for a completed or unchanged whole-file write. *)
type t = private
  | Wrote of { lines : int }  (** Complete contents were written. *)
  | Unchanged  (** Requested contents already matched. *)

val wrote : lines:int -> t
(** [wrote ~lines] is a write of [lines] logical lines.

    Raises [Invalid_argument] if [lines] is negative. *)

val unchanged : t
(** [unchanged] is a write whose requested contents already matched. *)

val jsont : t Jsont.t
(** [jsont] maps write facts to their closed version-1 JSON shape. *)
