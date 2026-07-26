(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Compact aggregate facts for multi-file update tools.

    Paths, diffs, receipts, and revert data remain owned by mutation facts and
    are not duplicated here. *)

(** Whether the reported changes were applied or only previewed. *)
type disposition =
  | Applied  (** The update ran. *)
  | Previewed  (** The update was only planned. *)

type t
(** The type for one update result. File, line, and skipped counts are
    non-negative; line counts may be unknown. *)

val make :
  disposition:disposition ->
  files:int ->
  additions:int option ->
  deletions:int option ->
  skipped:int ->
  t
(** [make ~disposition ~files ~additions ~deletions ~skipped] is an aggregate
    update result.

    Raises [Invalid_argument] if any present count is negative. *)

val disposition : t -> disposition
(** [disposition t] is whether the changes were applied or previewed. *)

val files : t -> int
(** [files t] is the number of changed files. *)

val additions : t -> int option
(** [additions t] is the total added-line count, if known. *)

val deletions : t -> int option
(** [deletions t] is the total deleted-line count, if known. *)

val skipped : t -> int
(** [skipped t] is the number of candidate files omitted from the update. *)

val jsont : t Jsont.t
(** [jsont] maps update facts to their closed version-1 JSON shape. *)
