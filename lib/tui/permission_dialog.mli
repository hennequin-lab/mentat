(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Permission-review dialogs.

    A dialog presents the exact access subset captured by one
    {!Mentat_permission.Policy.Review.t} and resolves it directly to a typed
    {!Mentat_permission.Answer.t}. The review remains the authority owner:
    request display text, request items, and change metadata are presentation
    evidence only and are never used to reconstruct policy or access identity.

    The available authority is deliberately narrow: allow this operation once,
    remember the exact reviewed accesses for this conversation, deny, or deny
    with a free-text message telling the model what to do differently. Family
    rules are not offered because a review carries no explicit reviewer-visible
    candidate rule. *)

(** {1:types Types} *)

type t
(** The type for immutable permission-dialog state.

    The state retains the owner review unchanged, together with the current
    decision selection, evidence-disclosure state, and an optional guidance
    editor. *)

type msg
(** A dialog input produced by the controlled decision table or guidance editor
    in {!view}. *)

(** The result of folding one key. *)
type outcome =
  | Stay
      (** The dialog remains open. The returned state may have a different
          selection, disclosure state, or guidance-editor value. *)
  | Answer of { answer : Mentat_permission.Answer.t; message : string option }
      (** The dialog resolved to a typed permission answer. [message] is [Some]
          only for a guided deny — the reviewer's free-text guidance; it is
          [None] for a bare deny and for every allow. *)

(** {1:constructors Constructors} *)

val make : Mentat_permission.Policy.Review.t -> t
(** [make review] is a collapsed dialog for [review] with allow-once selected.

    The owner [review] and all access identities remain unchanged. Display
    copies of its request metadata, access fields, and item metadata are
    repaired to valid UTF-8. Carriage-return line endings are canonicalized,
    tabs become two spaces, and terminal controls become [U+FFFD]. Fixed labels
    with no visible non-whitespace grapheme use an explicit bracketed fallback;
    this affects presentation only. A change diff renders through the shared
    diff renderer, which owns its own display fidelity. *)

(** {1:input Input} *)

val key : Matrix.Input.Key.event -> t -> t * outcome
(** [key event t] folds [event] into [t].

    While the decision list owns input:

    - bare digits [1] through [4] select the corresponding one-based decision
      without answering;
    - Up and Down move by one decision and wrap;
    - Enter or table activation resolves the selected decision: the first three
      rows return {!Mentat_permission.Answer.once},
      {!Mentat_permission.Answer.exact_for_conversation}, or
      {!Mentat_permission.Answer.deny} with no message; the fourth row opens the
      guidance editor;
    - bare [y] returns {!Mentat_permission.Answer.once}, bare [a] returns
      {!Mentat_permission.Answer.exact_for_conversation}, and bare [d], bare
      [n], and Escape return a bare {!Mentat_permission.Answer.deny};
    - Ctrl+O toggles evidence disclosure.

    While the guidance editor owns input, its focused {!Mosaic.input} consumes
    editing, cursor, selection, paste, and Enter events and emits them through
    {!update}. Only Escape reaches [key]; it cancels the editor and returns to
    the decision list without denying. A non-blank submit returns a deny with
    that message; a blank submit returns a bare deny.

    Bare decision mnemonics are ASCII case-insensitive. Alt-, Control-, or
    Super-modified letters have no decision meaning, except for Ctrl+O. *)

val editing : t -> bool
(** [editing t] is [true] iff the focused guidance editor owns input. *)

val update : msg -> t -> t * outcome
(** [update message t] folds a message emitted by {!view} into [t]. Choice
    selection uses the exact zero-based row index emitted by Mosaic; activation
    first selects that row and then applies the same transition as Enter in
    {!key}.

    A guidance edit replaces the controlled editor value. A guidance submit
    returns a deny carrying the trimmed message, or a bare deny when the message
    has no visible non-whitespace grapheme. A stale editor message is ignored
    when the editor is closed. *)

(** {1:views View} *)

val view : palette:Theme.Palette.t -> t -> msg Mosaic.t
(** [view t] is the complete accent-framed permission panel.

    Every request, access, item, count, and diff entry supplied by the owner
    review remains in the evidence scroll container. Evidence is ordered by
    decision value: requested action, affected items and diffs, exact access
    mechanics, then requester provenance. Collapsed disclosure caps that
    container's height and retains the headline and aggregate change count;
    expanded disclosure yields those repeated summary rows so the evidence can
    consume their space. The borderless controlled decision table retains all
    four numbered decisions and scrolls its selected row into view; while the
    guidance editor is open it replaces that table with a focused
    {!Inline_input} surface. The pinned hints distinguish digit selection from
    Enter resolution and expose the evidence disclosure state, and switch to the
    editor controls while a message is being typed; the direct decision
    mnemonics documented by {!key} remain available without displacing those
    essential controls in a narrow allocation. Mosaic owns text wrapping,
    allocated-width truncation, and viewport visibility; the view performs no
    terminal-size arithmetic. *)
