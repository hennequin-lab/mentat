(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Reviewer-question dialogs.

    A dialog presents a {!Mentat_session.Question.t} as either a numbered
    single-choice list or a bare prompt. Every question also has a permanent
    {!Theme.own_answer} row for entering a free-text reply. Fold keyboard input
    that is not consumed by its Mosaic widgets with {!key}, route widget
    messages with {!update}, and render the current state with {!view}.

    Choice replies retain their zero-based index through
    {!Mentat_session.Question.Answer.choice}; display normalization never
    changes the answer identity. *)

(** {1:types Types} *)

type t
(** The type for immutable question-dialog state.

    The state contains the current selection, an optional inline free-answer
    editor, and an optional validation message. *)

(** The result of folding one key. *)
type outcome =
  | Stay
      (** The dialog remains open. The returned state may have a different
          selection, editor value, or validation message. *)
  | Answer of Mentat_session.Question.Answer.t
      (** The dialog resolved to a typed choice or free-text answer. *)
  | Flash of string
      (** The key was rejected with the given user-facing validation message.
          The same message is present in the returned state's view. *)

type msg
(** The type for exact choice-table and inline-input messages emitted by
    {!view}. *)

(** {1:constructors Constructors} *)

val make : Mentat_session.Question.t -> t
(** [make question] is a dialog for [question]. The first choice is selected
    when choices are present; otherwise the free-answer row is selected.

    Prompts retain line breaks. Carriage-return line endings are canonicalized
    to line feeds. Choice labels are kept to one display row by replacing line
    breaks and tabs with spaces. Other terminal control characters and malformed
    UTF-8 are rendered as [U+FFFD]. A prompt with no visible, non-whitespace
    grapheme is shown as ["[blank prompt]"], and such a choice is shown as
    ["[blank choice]"]. These display transformations do not alter the choice
    index returned by {!key}. *)

(** {1:input Input} *)

val key : Matrix.Input.Key.event -> t -> t * outcome
(** [key event t] folds [event] into [t].

    While the choice list owns input:

    - bare digits [1] through [9] move to the corresponding one-based row when
      it exists, but do not answer;
    - Up and Down move by one row and wrap across the choices and the trailing
      free-answer row;
    - Enter answers with the selected choice, or opens the free-answer editor
      when that row is selected;
    - Escape opens the free-answer editor directly;
    - all other keys leave the dialog open unchanged.

    While the editor owns input, its focused {!Mosaic.input} consumes editing,
    cursor, selection, paste, and Enter events and emits them through {!update}.
    Only Escape reaches [key]; it closes the editor and returns to the choice
    list without resolving the question. *)

val editing : t -> bool
(** [editing t] is [true] iff the focused free-answer editor owns input. *)

val update : msg -> t -> t * outcome
(** [update message t] folds a message emitted by {!view} into [t]. Choice
    selection uses the exact zero-based row index emitted by Mosaic. Activation
    first selects that row and applies the same choice or custom-answer
    transition as Enter in {!key}.

    An inline edit replaces the controlled value. An inline submit trims
    surrounding whitespace and returns a free answer. A submission with no
    visible, non-whitespace grapheme—including one containing only zero-width
    joiners, spaces, or combining marks—returns [Flash "type an answer"] and
    keeps the editor open. A stale inline message is ignored when the editor is
    closed. *)

(** {1:views View} *)

val view : palette:Theme.Palette.t -> t -> msg Mosaic.t
(** [view t] is the complete accent-framed question panel. While the choice list
    owns input, choices and the permanent free-answer row form a controlled,
    borderless table with a declarative maximum height of nine rows. The
    selected row has an explicit cursor marker in addition to its color so plain
    terminal snapshots retain the selection. Mosaic owns row ellipsis,
    selected-row visibility, scrolling, and narrow allocation.

    While the free-answer editor owns input, the choice table is absent. The
    prompt, focused {!Inline_input}, validation issue, and editor-specific hints
    therefore describe one stable interaction mode regardless of available
    height. Both modes compose intrinsically and perform no terminal-size
    arithmetic. *)
