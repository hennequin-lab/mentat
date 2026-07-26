(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Live rendering of the durable session task board.

    A task-board fact replaces the whole board. This module projects that fact
    into one intrinsic Mosaic column. Mosaic owns text wrapping and the column's
    allocated height; the host decides whether the column belongs in a scroll
    region. The chat host retains the latest board once it has content, across
    turn settlement, interruption, and completion; only a whole-board
    replacement with no items removes the live tenant. The projection
    deliberately flattens task ownership: it shows the work itself without owner
    tags, ids, priorities, or positions. *)

val view :
  ?count_header:bool ->
  palette:Theme.Palette.t ->
  Mentat_session.Task.Board.t ->
  'msg Mosaic.t list
(** [view ?count_header board] is [[]] when [board] is empty and otherwise a
    singleton intrinsic board column suitable for direct composition.

    [count_header] defaults to [true]. When present, the column starts with the
    muted count line [◻ N tasks · N done · N running]. A host whose section
    header already carries these facts may pass [false].

    Open work is grouped by status while retaining encounter order within each
    group: in-progress tasks render first as accent [◼] items and pending tasks
    follow as default [◻] items. Item text wraps through Mosaic's text layout.
    While open work remains, completed and cancelled tasks fold into one
    trailing muted [… N done] digest that keeps the live strip compact. When no
    open work remains, that digest expands so every terminal task renders as a
    muted checked [☒] item, keeping a finished board's whole list visible. This
    folding is a status-semantic choice and never depends on allocated rows.
    Owners are not rendered; a board may consequently contribute one in-progress
    item per owner. *)

val strip_rule : palette:Theme.Palette.t -> 'msg Mosaic.t
(** [strip_rule] is the intrinsic full-width dotted rule that bounds the task
    board when it is hosted in the narrow status strip. The wide pane supplies
    its own vertical frame and does not use this rule. *)
