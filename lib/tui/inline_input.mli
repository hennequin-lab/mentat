(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Controlled single-line input embedded in decision dialogs.

    The adapter owns only the current text. {!Mosaic.input} owns editing,
    grapheme-aware cursor motion and deletion, selection, paste, horizontal
    cursor reveal, and submit event recognition. The enclosing dialog owns
    cancellation, validation, and conversion to a domain answer. *)

(** {1:types Types} *)

type t
(** The type for inline-input text state. Values contain valid UTF-8 on one
    logical line and no terminal control characters. *)

type msg
(** The type for controlled edit and submit messages emitted by {!view}. *)

(** The result of interpreting one key while the field owns input. *)
type outcome =
  | Stay of t
      (** The field remains open with the returned state. This includes ignored
          keys. *)
  | Submit of string
      (** Enter submitted the field. The string has leading and trailing
          whitespace removed with {!String.trim}. *)
  | Cancel  (** Escape cancelled the field. *)

(** {1:constructors Constructors} *)

val empty : t
(** [empty] is an empty field with its cursor at the start. *)

val of_text : string -> t
(** [of_text text] is a field initialized from [text] with its cursor at the
    end. Malformed UTF-8 and terminal controls are normalized exactly as for an
    edit; tabs and line boundaries become spaces. *)

val value : t -> string
(** [value input] is the normalized, single-line text currently owned by
    [input]. *)

(** {1:input Input} *)

val key : Matrix.Input.Key.event -> t -> outcome
(** [key event t] is {!Cancel} for Escape and [Stay t] for every other key.
    Non-cancellation keys must reach the focused Mosaic input rendered by
    {!view}; they are not interpreted twice by this adapter. *)

val update : msg -> t -> outcome
(** [update message t] folds a message emitted by {!view}. An edit returns
    [Stay] with Mosaic's complete current value after malformed UTF-8 and
    terminal controls are replaced with [U+FFFD] and line separators and tabs
    become spaces. A submit returns the current controlled value through
    {!Submit} after removing leading and trailing whitespace with
    {!String.trim}. *)

(** {1:view View} *)

val view : palette:Theme.Palette.t -> t -> msg Mosaic.t
(** [view t] is the ruled, focused single-line editor. The field fills the width
    offered by its parent. Mosaic owns rule length, intrinsic measurement,
    grapheme editing, selection, horizontal overflow, and keeping the cursor
    visible. Its decorative side insets yield before the editor in a narrow
    allocation. *)
