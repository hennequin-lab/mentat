(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Responsive transcript and activity composition.

    [Pane] declares two intrinsic layouts and delegates branch selection,
    allocation, clipping, and resize handling to Mosaic. At viewport widths of
    at least 110 cells, non-empty activity occupies a bounded column beside a
    transcript with an 80-cell minimum. Empty activity leaves the transcript at
    full width with no orphan divider. Below that breakpoint, the transcript
    takes the fluid region and narrow activity follows it in the surrounding
    column.

    The module receives complete Mosaic subtrees. It never measures their text,
    predicts their height, grants row budgets, or derives widths from terminal
    dimensions. Activity modules own their semantic presentation; Mosaic owns
    the resulting geometry and viewports. *)

val frame :
  ?tail:'msg Mosaic.t list ->
  palette:Theme.Palette.t ->
  left:'msg Mosaic.t ->
  wide_activity:'msg Mosaic.t list ->
  narrow_activity:'msg Mosaic.t list ->
  unit ->
  'msg Mosaic.t
(** [frame ?tail ~left ~wide_activity ~narrow_activity ()] is the responsive
    transcript region.

    [tail] rows belong to the transcript column: they render beneath [left]
    inside it on both branches, so on the wide branch the activity column's rule
    spans them and the region's bottom edge meets whatever follows the frame. A
    growing [left] pins them to that bottom edge. Defaults to none.

    The wide branch places the transcript column beside non-empty
    [wide_activity]. The activity column occupies 33 percent of the viewport
    within inclusive 30- and 45-cell bounds; its left border is the shared
    vertical rule. [left] grows through all remaining width and does not shrink
    below 80 cells. Empty [wide_activity] leaves the column at full width and
    draws no rule.

    The narrow branch places the transcript column above [narrow_activity]. The
    transcript grows and shrinks through the height left by the intrinsic
    activity rows.

    Mosaic selects exactly one branch before reconciliation and layout. The
    other branch is not mounted and cannot capture focus or input. The stable
    frame and transcript keys preserve compatible Mosaic-owned widget state,
    including transcript scroll position, across breakpoint crossings. *)
