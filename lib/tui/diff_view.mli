(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** One diff renderer over Mosaic's split/unified diff renderable.

    A thin wrapper that lowers a structured or textual diff for one file to a
    {!Mosaic.Diff.Patch.t}, resolves a responsive layout, threads the syntax
    seam through a palette-keyed highlight memo, and forwards every decoration
    Mosaic's diff takes. It computes no theme and holds no state; the caller
    owns scrolling, chrome, and the choice of theme variant.

    Intra-line word emphasis is a pure function of the patch, computed once at
    lowering time by {!Textdiff.word_spans} and stored on the {!source}.
    {!render} lowers each stored span to a {!Mosaic.Diff.line_span} coloured by
    its side and forwards them; the [emphasis] flag disables it. *)

type layout_pref =
  | Auto
  | Unified
  | Split
      (** The [tui.diff_layout] policy. [Auto] picks {!Mosaic.Diff.Split} when
          the diff region is wide enough, else {!Mosaic.Diff.Unified}. *)

type source
(** A lowered, decoration-free diff for one file: the {!Mosaic.Diff.Patch.t} and
    the per-line emphasis spans, both computed once. Constructors never fail. *)

val of_hunks : Textdiff.Hunk.t list -> source
(** [of_hunks hunks] lowers structured hunks byte-exactly, carrying each line's
    raw bytes and newline flag, and computes word emphasis for each changed
    before/after pair. *)

val of_unified : string -> source
(** [of_unified s] parses the first unified-diff file patch in [s]. A
    well-formed body becomes a patch source; a header-only or omission-note body
    — an empty create or delete, a size-capped ["[diff omitted: …]"], an
    edit-distance-capped render — becomes a note source that renders the text
    verbatim, so evidence is never dropped. Total. Its fidelity is
    display-level, not byte-level: the string is already display-escaped and
    carries no emphasis spans. *)

val of_patch : Mosaic.Diff.Patch.t -> source
(** [of_patch patch] wraps a patch a caller built directly — a synthesized
    context-only window — as a decoration-free source with no emphasis. *)

val patch : source -> Mosaic.Diff.Patch.t
(** [patch source] is [source]'s patch, for layout-correct scroll math with
    {!Mosaic.Diff.source_line_row}. A note source yields the empty patch. *)

val resolve : layout_pref -> width:int -> Mosaic.Diff.layout
(** [resolve pref ~width] is the single layout decision, so {!render} and scroll
    math agree. [Auto] is {!Mosaic.Diff.Split} at [width >= 120], [Split] at
    [width >= 100] (a hard floor below which split columns are unreadable), and
    [Unified] always. *)

val render :
  layout:Mosaic.Diff.layout ->
  theme:Mosaic.Diff.theme ->
  text_style:Mosaic.Ansi.Style.t ->
  added_emphasis:Mosaic.Ansi.Color.t ->
  removed_emphasis:Mosaic.Ansi.Color.t ->
  palette:Theme.Palette.t ->
  ?language:string ->
  ?emphasis:bool ->
  ?line_signs:Mosaic.Diff.line_sign list ->
  ?line_highlights:Mosaic.Diff.line_highlight list ->
  ?on_line_click:(Mosaic.Diff.line_hit -> 'msg option) ->
  ?wrap:Mosaic.Text_surface.wrap ->
  ?size:Mosaic.dimension Mosaic.size ->
  source ->
  'msg Mosaic.t
(** [render ~layout ~theme ~text_style ~added_emphasis ~removed_emphasis
     ~palette source] is a bare diff node.

    [theme] and [text_style] are forwarded straight to {!Mosaic.diff}. [palette]
    builds and memoizes the syntax token style keyed on its physical identity
    and [language], so an unchanged frame reuses one physical [highlight] record
    and a [/theme] swap rebuilds it. [language] defaults to plain text;
    [emphasis] defaults to [true] and colours each stored span by its side —
    [added_emphasis] on added lines, [removed_emphasis] on removed lines —
    forwarding them as {!Mosaic.diff}'s [line_spans]. [line_signs],
    [line_highlights], [on_line_click], [wrap] (default [`Word]), and [size]
    pass through. A note source renders its text verbatim in [text_style]. *)
