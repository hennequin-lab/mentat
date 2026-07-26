(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The review nav pane: a directory-grouped tree of changed files, each file's
    CR comments always visible as children beneath it.

    Path-ordered (directories then files sorted), one level of [▾ <dir>]
    grouping with no nested collapsing. The pane renders the selection the
    view's cursor holds and, when [on_click] is supplied, emits a cursor target
    per row. Pure: it reads the review view and the CR occurrence list and
    produces rows; the screen owns the cursor. *)

val nav_step :
  Mentat_review.View.t ->
  crs:Mentat_review.Cr.View.t list ->
  [ `Next | `Previous | `First | `Last ] ->
  Mentat_review.Cursor.t option
(** [nav_step view ~crs direction] is the cursor for the nav row reached by
    moving [direction] from the currently selected row, or [None] when the nav
    is empty. Movement is across the nav's own rows — a file scope or a CR — so
    it steps between files (and their CR children) and never onto a diff hunk,
    which the nav does not show. A cursor anywhere inside a file selects that
    file's row, so a step from the diff still lands on the neighbouring file. *)

val view :
  palette:Theme.Palette.t ->
  ?width:int ->
  ?height:int ->
  ?focused:bool ->
  ?dimmed:bool ->
  ?on_click:(Mentat_review.Cursor.t -> 'a) ->
  crs:Mentat_review.Cr.View.t list ->
  Mentat_review.View.t ->
  'a Mosaic.t list
(** [view ~palette ?width ?height ?focused ?dimmed ?on_click ~crs view] is the
    windowed nav rows for [view]: a [▾ dir] group per directory, then each
    file's [❯ ] cursor (accent when [focused], muted otherwise, faint when
    [dimmed]), [[ ]]/[[✓]] mark, middle-ellipsised basename, and right-aligned
    [A]/[M]/[D] status letter, with the file's CR children ([! …] for malformed
    ones) beneath it. [crs] are in occurrence order, so a CR's list index is its
    {!Mentat_review.Cursor.Cr} index. The list is windowed to [height] rows with
    [↑/↓ N more] overflow markers. When [on_click] is supplied, each selectable
    row reports its {!Mentat_review.Cursor.t} on a left mouse-down. *)
