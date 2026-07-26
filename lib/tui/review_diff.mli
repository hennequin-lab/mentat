(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The review diff pane: the focused file's unified diff with a scope line
    above it, inside a scroll box.

    Rebased onto the thin waist: the pane renders one
    {!Mentat_review.File_diff.t} — the body the screen fetched for the cursor's
    focused path — plus the {!Mentat_review.Cr.View.t} list, reading orientation
    from the {!Mentat_review.View.t} cursor. The cursor's hunk/line carries the
    gutter cursor (accent when the diff pane holds focus, muted otherwise) and a
    subtle line-highlight wash. A CR anchored outside any hunk gets a
    synthesized context-only view. Auto-scroll keeps the cursor line on screen
    via a one-shot {!Mosaic.scroll_box} reveal keyed on the cursor. Pure.

    The waist carries no per-hunk reviewed state, so hunk quieting degrades to
    whole-file coverage ([View.File.reviewed]) and the focused hunk's scope line
    reports [View.focus_reviewed]. *)

val view :
  palette:Theme.Palette.t ->
  ?width:int ->
  ?height:int ->
  ?focused:bool ->
  ?dimmed:bool ->
  ?layout_pref:Diff_view.layout_pref ->
  ?compose_anchor:string * int ->
  ?on_line_click:(Mentat_review.Scope.t -> 'a) ->
  crs:Mentat_review.Cr.View.t list ->
  file_diff:Mentat_review.File_diff.t option ->
  full_context:bool ->
  Mentat_review.View.t ->
  'a Mosaic.t list
(** [view ~palette ?width ?height ?focused ?dimmed ?layout_pref ?compose_anchor
     ?on_line_click ~crs ~file_diff ~full_context view] is the scope line plus
    the scroll box for the file the cursor selects. [file_diff] is the body the
    screen fetched for the cursor's focused path ([None] while a fetch is in
    flight or when the cursor sits on a CR in an unchanged file). Hunks render
    at the body's context; [full_context] recomputes the whole file from its
    before/after texts. [width] is the diff region's column width and, with
    [layout_pref] (default {!Diff_view.Auto}), resolves the split/unified layout
    used for both rendering and auto-scroll. [compose_anchor] is a
    [(path, line)] the compose dialog will land on, given a stronger [warning]
    wash; [on_line_click] reports the exact source line a click hits as a
    {!Mentat_review.Scope.Line}. *)
