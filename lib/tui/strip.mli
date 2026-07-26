(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Transient status rows above the composer.

    The strip announces the verbose-reasoning lens and prompts queued behind an
    active turn. It is fixed chrome rather than transcript content, so its rows
    never enter scrollback. The caller mounts the returned list between other
    transient activity and the composer; an empty list contributes no gap. *)

val view :
  palette:Theme.Palette.t ->
  verbose:bool ->
  queued:string list ->
  'msg Mosaic.t list
(** [view ~verbose ~queued] is the active status rows in display order:

    - when [verbose] is [true], a row with warning-styled ["◎ verbose"] and the
      faint hint [" ctrl+o closes"];
    - for each prompt in [queued], a row with muted ["↥ queued · \"<preview>\""]
      and the faint hint [" (↑ edits)"], preserving list order.

    Each row occupies one full-width layout line with a two-column left inset. A
    prompt preview uses only its first newline-delimited line. It is the row's
    only shrinking segment and truncates with an ellipsis as space contracts,
    preserving the marker, quotes, and edit hint. The row clips at its boundary
    if the fixed text alone exceeds the available space. *)
