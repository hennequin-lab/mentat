(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The conversation switcher rendered below the transcript or in the side pane.

    Callers own the ordered delegation rows, their placement, and optional
    keyboard selection. Mosaic owns column allocation, overflow, viewport
    clipping, pointer hit testing, and keeping the selected row visible. *)

open Mosaic

(** A conversation in the switcher's stable, caller-defined row order.

    Threads render one per row in the order the caller supplies, as a depth
    tree: [main] and its direct children sit flush at depth 0, while a child
    delegated by another child nests under it with a branch connector on its
    task. The cursor and status glyph occupy the same two leading columns on
    every row, so [main]'s glyph aligns with each thread's regardless of depth.
*)
type row =
  | Main  (** The parent conversation, rendered as [◯ main]. *)
  | Thread of {
      glyph : string;
          (** The status glyph, such as a running or completed mark. *)
      style : Ansi.Style.t;  (** The style applied to [glyph]. *)
      kind : string;  (** The child's actor kind, such as [subagent]. *)
      task : string;  (** The single-line task summary. *)
      facts : string list;  (** Trailing facts, in display order. *)
      depth : int;
          (** The nesting depth. [0] is a direct child of [main]; each deeper
              level is a child delegated by a delegated child, indented under
              its parent with a branch connector. *)
    }

(** The switcher's semantic placement in the conversation layout. *)
type placement =
  | Below_transcript
      (** The inline strip, including each thread's actor kind. *)
  | Agents_pane
      (** The agents pane. Its section heading supplies the actor context, so
          thread rows omit the redundant kind while retaining the main label,
          task summary, and facts. *)

(** An exact table-row interaction. Indices refer to the [rows] supplied to
    {!view}; callers should ignore an index after the row set changes. *)
type msg =
  | Select_index of int  (** Pointer or keyboard selection changed. *)
  | Activate_index of int  (** Enter or left click activated a row. *)
  | Hover_index of int option
      (** The pointer entered a row, or left all data rows. *)

val view :
  palette:Theme.Palette.t ->
  placement:placement ->
  ?can_open:bool ->
  ?activate_hint:string ->
  ?hovered:int option ->
  rows:row list ->
  selected:int option ->
  unit ->
  msg t list
(** [view ~placement ?can_open ?hovered ~rows ~selected ()] renders the
    conversation switcher in [placement]. Placement changes only redundant row
    chrome; it never changes row order or interaction.

    [selected = None] is the compact, unfocused glance. [Some index] is the
    focused browser; [view] clamps the index to [rows], then Mosaic scrolls the
    table to keep that controlled row visible. The glance and browser have
    declarative maximum heights of four and ten rows respectively; the complete
    row set is always passed to Mosaic. Empty [rows] render no node.

    When more rows exist than the glance shows, a muted [↓ N more] line follows
    the table so the static overview stays honest about the hidden threads. The
    focused browser omits it and relies on the table's own scroll indicator,
    which tracks the moving viewport. A caller sizing the strip to a fixed
    height therefore reserves one extra row for the glance overflow line.

    [can_open] defaults to [true]. When true, the [activate_hint] takes the
    place of the selected row's facts in {!Agents_pane}; it defaults to
    ["enter to open"], and a caller whose activation does something else (such
    as re-attaching a lost child feed) supplies the honest verb. The pane is too
    narrow to seat the label, its facts, and the hint together, and a focused
    row's glyph already states its status, so the hint is shown in the facts'
    place rather than beside them. The inline {!Below_transcript} placement
    never yields a hint, so every row keeps its facts; Enter and pointer
    activation remain available. [hovered] gives that exact row the active text
    style without selecting it. Pointer and keyboard selection emit
    {!Select_index}; Enter and left click emit {!Activate_index}; pointer row
    transitions emit {!Hover_index}. Wheel events remain available to an
    enclosing scroll surface. *)
