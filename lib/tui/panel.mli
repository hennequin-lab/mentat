(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Shared panel chrome and key classification.

    A panel replaces the composer and footer below the transcript. {!view} draws
    the full-width boundary, name chip, caller-owned content, and panel-specific
    hint row. {!classify} implements the common filter law so panel surfaces
    agree on which keys narrow a filter and which remain actions. *)

(** {1:types Key classes} *)

(** The type for panel actions. *)
type action =
  | Enter  (** Resolve the current selection. *)
  | Escape  (** Close or cancel the current panel state. *)
  | Tab  (** Promote or switch the current surface. *)
  | Left  (** Move or adjust left. *)
  | Right  (** Move or adjust right. *)
  | Up  (** Move the selection up. *)
  | Down  (** Move the selection down. *)
  | Backspace  (** Remove the preceding filter character. *)
  | Ctrl_d  (** The [ctrl+d] chord. *)
  | Other
      (** A key or chord not recognized above. Modal panels normally ignore this
          case. *)

(** The type for keys interpreted under the panel filter law. *)
type key =
  | Printable of string
      (** A character at or above [U+0020], other than [U+007F], with no ctrl,
          alt, or super modifier, encoded as UTF-8. Shift does not turn a
          character into an action. *)
  | Digit of int
      (** A bare decimal digit in the range [0] to [9]. A panel can interpret it
          as a numbered selection while its filter is empty and as filter text
          otherwise. *)
  | Action of action  (** A navigation key, control key, or chord. *)

(** {1:input Input} *)

val classify : Matrix.Input.Key.event -> key
(** [classify event] classifies [event] under the panel filter law.

    Bare characters are {!Printable}, except decimal digits, which are {!Digit},
    and ASCII controls, which are {!Action} {!Other}. [ctrl+d] is {!Action}
    {!Ctrl_d}; [ctrl+p] and [ctrl+n] are aliases for {!Up} and {!Down}. Page Up
    and Page Down also step by one row as {!Up} and {!Down}. Backspace and
    Delete are both {!Backspace}. All remaining keys and modified characters are
    {!Action} {!Other}. *)

(** {1:views View} *)

val view :
  palette:Theme.Palette.t ->
  frame:Mosaic.Ansi.Color.t ->
  name:string ->
  filter:string ->
  hint:string list ->
  content:'msg Mosaic.t ->
  'msg Mosaic.t
(** [view ~frame ~name ~filter ~hint ~content] is a panel column that grows and
    shrinks within the height allocated by its parent, with this row geometry:

    - an intrinsic full-width top border of {!Theme.panel_boundary} glyphs in
      [frame];
    - a [name] chip in [frame], followed by a faint [filter] when non-empty;
    - one blank row, then a body containing [content];
    - one blank row, then the faint [hint] fragments separated by
      {!Theme.separator}.

    The boundary, chip, blank rows, and hint keep their intrinsic heights. The
    body grows and shrinks to receive all remaining height and has a zero
    minimum size, so [content] can be a flex-filling {!Mosaic.scroll_box}
    without calculating a row budget for the panel chrome.

    The chip and hint rows have two columns of horizontal padding. Their text
    participates in Mosaic layout and truncates when the allocation is narrow.
    The caller supplies only hints for actions that are available in the current
    state. *)
