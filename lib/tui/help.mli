(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The composer's shortcut sheet.

    The sheet appears below the composer when [?] is pressed on an empty draft.
    It shows every active composer shortcut in semantic sections. *)

val view : palette:Theme.Palette.t -> unit -> 'msg Mosaic.t
(** [view ()] is the complete shortcut sheet. Mosaic aligns each section's key
    and action tracks and wraps whole sections when they do not fit side by
    side. *)
