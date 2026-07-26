(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Canonical-list checks shared by the event and revert constructors.
    Library-internal. *)

val strictly_sorted : ('a -> 'a -> int) -> 'a list -> bool
(** [strictly_sorted compare items] is [true] iff [items] is duplicate-free and
    sorted by [compare] — the canonical decode form for id and path lists. *)

val unique_paths : Mentat_workspace.Path.t list -> bool
(** [unique_paths paths] is [true] iff no path occurs twice in [paths]. *)
