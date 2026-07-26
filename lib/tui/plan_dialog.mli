(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Plan-review dialogs.

    A dialog presents one {!Mentat_session.Plan.t}, its optional title, and four
    explicit decisions: approve in the current context, approve in a fresh
    context, revise with feedback, or keep planning. Fold keyboard input not
    consumed by its Mosaic widgets with {!key}, route widget messages with
    {!update}, and render the current state with {!view}.

    The proposal passed to {!make} remains the owner value. Approval carries
    that exact value; display sanitization never reconstructs or edits it. *)

(** {1:types Types} *)

type t
(** The type for immutable plan-dialog state.

    The state contains the owner plan, the current decision selection, the body
    disclosure state, an optional revision editor, and an optional validation
    message. *)

(** The result of folding one key. *)
type outcome =
  | Stay
      (** The dialog remains open. The returned state may have a different
          selection, disclosure state, editor value, or validation message. *)
  | Answer of Mentat_session.Plan.Answer.t
      (** The dialog resolved to a typed plan answer. *)
  | Flash of string
      (** The key was rejected with the given user-facing validation message.
          The same message is present in the returned state's view. *)

type msg
(** The type for exact decision-table and inline-input messages emitted by
    {!view}. *)

(** {1:constructors Constructors} *)

val make : Mentat_session.Plan.t -> t
(** [make plan] is a collapsed dialog for [plan] with approval in the current
    context selected.

    Carriage-return line endings in display copies are canonicalized to line
    feeds. A displayed title occupies one logical line; its line breaks and tabs
    become spaces. Other terminal control characters and malformed UTF-8 are
    rendered as [U+FFFD]. An absent title is shown as ["[untitled plan]"], a
    present title with no visible non-whitespace grapheme as
    ["[blank plan title]"], and a body with no such grapheme as
    ["[blank plan body]"]. These display transformations do not alter [plan]. *)

(** {1:input Input} *)

val key : Matrix.Input.Key.event -> t -> t * outcome
(** [key event t] folds [event] into [t].

    While the decision list owns input:

    - bare digits [1] through [4] move to the corresponding one-based row but do
      not resolve the dialog;
    - Up and Down move by one row and wrap;
    - Enter resolves the selected row. The first two rows return
      {!Mentat_session.Plan.Answer.approve} with [context] [`Current] and
      [`Fresh], respectively, the exact owner plan as [body], and no feedback;
    - the third row opens the revision editor;
    - the fourth row and Escape return
      {!Mentat_session.Plan.Answer.keep_planning};
    - Ctrl+O toggles between the collapsed and expanded body;
    - all other keys leave the dialog open unchanged.

    While the revision editor owns input, its focused {!Mosaic.input} consumes
    editing, cursor, selection, paste, and Enter events and emits them through
    {!update}. Only Escape reaches [key]; it cancels the editor and returns to
    the decision list without resolving the plan. *)

val editing : t -> bool
(** [editing t] is [true] iff the focused revision editor owns input. *)

val update : msg -> t -> t * outcome
(** [update message t] folds a message emitted by {!view} into [t]. Choice
    selection uses the exact zero-based row index emitted by Mosaic. Activation
    first selects that row and applies the same approval, revision, or
    keep-planning transition as Enter in {!key}.

    An inline edit replaces the controlled value. An inline submit trims
    surrounding whitespace and returns {!Mentat_session.Plan.Answer.revise}. A
    submission with no visible, non-whitespace grapheme—including one containing
    only zero-width joiners, spaces, or combining marks—returns
    [Flash "describe what should change"] and keeps the editor open. A stale
    inline message is ignored when the editor is closed. *)

(** {1:views View} *)

val view : palette:Theme.Palette.t -> t -> msg Mosaic.t
(** [view t] is the complete plan-blue review panel. The body is a scrollable,
    word-wrapped surface whose collapsed form has a declarative maximum height
    of eight rows; Ctrl+O lets it grow within the panel's allocation. Decisions
    form a controlled, borderless table. Mosaic owns text wrapping, ellipsis,
    selected-row visibility, scrolling, and narrow allocation. The title,
    validation issue, and active {!Inline_input} compose intrinsically around
    those surfaces; the view performs no terminal-size arithmetic.

    The decision hints distinguish digit selection from Enter resolution and
    expose the body's Ctrl+O disclosure state. Revision hints retain the input,
    submission, and cancellation controls. Both sets use compact, intrinsic
    labels so they remain meaningful when Mosaic allocates a narrow panel. *)
