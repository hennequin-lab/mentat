(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Shared full-screen chrome and neutral key classification.

    A screen owns the complete application region. {!view} draws its top rule,
    optional filter line, caller-owned content, and pinned hint row. {!classify}
    leaves printable characters neutral so each screen can apply its own letter
    keymap while browsing and treat the same characters as text while filtering.
*)

(** {1:types Key classes} *)

(** The type for screen actions. *)
type action =
  | Enter  (** Confirm the current selection. *)
  | Escape  (** Clear or close the current screen state. *)
  | Tab  (** Switch or promote the current surface. *)
  | Up  (** Move the selection up. *)
  | Down  (** Move the selection down. *)
  | Left  (** Move or adjust left. *)
  | Right  (** Move or adjust right. *)
  | Backspace  (** Remove the preceding filter character. *)
  | Page_up  (** Move the focused viewport up by one measured page. *)
  | Page_down  (** Move the focused viewport down by one measured page. *)
  | Other
      (** A key or chord not recognized above. A modal screen normally ignores
          this case. *)

(** The type for keys interpreted by a concrete screen. *)
type key =
  | Char of string
      (** A character at or above [U+0020], other than [U+007F], with no ctrl,
          alt, or super modifier, encoded as UTF-8. Digits and shifted
          characters remain characters. *)
  | Action of action  (** A navigation key, control key, or chord. *)

type filter = { query : string; matches : int }
(** The type for an open filter line. [query] is the entered text and [matches]
    is the number of content rows it retains. *)

(** {1:input Input} *)

val classify : Matrix.Input.Key.event -> key
(** [classify event] classifies [event] without applying a screen's letter
    keymap or filter state.

    Bare printable characters are {!Char}; ASCII controls are {!Action}
    {!Other}. [ctrl+p] and [ctrl+n] are aliases for {!Up} and {!Down}. Backspace
    and Delete are both {!Backspace}. Page Up and Page Down remain distinct page
    actions. All remaining keys and modified characters are {!Action} {!Other}.
*)

(** {1:views View} *)

val view :
  palette:Theme.Palette.t ->
  frame:Mosaic.Ansi.Color.t ->
  name:string ->
  fact:string ->
  filter:filter option ->
  hint:string list ->
  content:'msg Mosaic.t ->
  'msg Mosaic.t
(** [view ~frame ~name ~fact ~filter ~hint ~content] is a screen column that
    grows and shrinks within the height allocated by its parent, with this
    geometry:

    - a top row containing a filled [name] chip, an intrinsic flexing rule in
      [frame], and a right-aligned faint [fact];
    - when [filter] is [Some f], a bare [/]-prefixed filter line showing
      [f.query] and [f.matches];
    - one blank row, then a body containing [content];
    - one blank row, then the pinned hint row.

    The rule, filter, blank rows, and hint keep their intrinsic heights. The
    body grows and shrinks to receive all remaining height and has a zero
    minimum size, so [content] can fill or scroll within the body without
    calculating a row budget for the screen chrome. Filter, fact, and hint text
    participate in Mosaic layout and truncate when the allocation is narrow. The
    caller supplies only hints for actions available in the current state. *)
