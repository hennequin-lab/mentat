(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Completion rows shared by the slash palette, file mentions, and prompt
    history.

    This module defines Mentat's completion-row anatomy and delegates sizing,
    clipping, scrolling, selection, and activation to Mosaic's table widget. The
    table occupies at most five rows, and Mosaic keeps its controlled selection
    visible within that allocation. Callers own the completion data, selected
    index, and messages produced by selection and activation. *)

(** {1:rows Rows} *)

type segment
(** The type for a styled semantic part of a completion row. *)

val segment : ?style:Mosaic.Ansi.Style.t -> string -> segment
(** [segment ?style text] is [text] drawn in [style]. [style] defaults to the
    terminal's default text style. Mosaic sizes and clips the text to the
    allocated column. *)

type row
(** The type for a completion row.

    A row has a primary value, such as a command, path, or prior prompt; an
    optional flexible detail; and an optional trailing hint. *)

val row : ?detail:segment -> ?hint:segment -> segment -> row
(** [row ?detail ?hint primary] is a completion row whose [primary] identifies
    the choice. [detail], when present, receives the flexible share of the
    available width. [hint], when present, remains a trailing intrinsic column.
*)

(** {1:views Views} *)

val view :
  palette:Theme.Palette.t ->
  selected:int ->
  on_select:(int -> 'msg) ->
  on_activate:(int -> 'msg) ->
  row list ->
  'msg Mosaic.t
(** [view ~selected ~on_select ~on_activate rows] renders [rows] as a
    borderless, headerless selection table of at most five visible rows. Mosaic
    scrolls automatically to keep the selection visible and wraps keyboard
    navigation at the ends. [view] clamps [selected] to the available rows
    before constructing the table. Selection changes produce [on_select index];
    Enter produces [on_activate index].

    The selected row is prefixed by Mentat's accent cursor. The table does not
    take focus automatically, so a composer may retain text-entry focus while
    controlling [selected]; mouse focus and explicit focus still enable the
    table's own navigation. *)

val note : palette:Theme.Palette.t -> string -> 'msg Mosaic.t
(** [note text] is an inert muted completion-state row, indented under the
    selection cursor. Mosaic clips [text] to its allocated width. *)

val error : palette:Theme.Palette.t -> string -> 'msg Mosaic.t
(** [error text] is an inert error-colored completion-state row prefixed by
    {!Theme.problem}. Mosaic clips [text] to its allocated width. *)
