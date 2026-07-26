(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The CR compose dialog: a compact opaque box floating over the dimmed panes.

    The dialog holds a [draft] carrying the CR grammar itself (parsing happens
    on submit, in the screen). The input is a real {!Mosaic.textarea} — the
    native editor, with its own cursor and the usual editing keys (word motions,
    ctrl+w word-delete, kill-to-line); each keystroke mirrors into [draft]
    through the [on_edit] callback and enter fires [on_submit]. A parse or write
    problem renders as a [! …] line under the input with the draft preserved. *)

(** The type for what the compose targets. *)
type target =
  | Add of { path : Lpath.Rel.t; line : int }
      (** Insert a new CR before [line] of [path]. *)
  | Edit of Mentat_review.Cr.View.t
      (** Rewrite the CR the view addresses. The screen submits by the view's
          {!Mentat_review.Cr.Ref.t}, which the responder re-resolves against a
          fresh snapshot, so a background refresh cannot repoint the edit. *)
  | Resolve of Mentat_review.Cr.View.t
      (** Resolve the CR the view addresses (prefilled with the [XCR] form).
          Addressed by ref like {!Edit}. *)

type t
(** The type for a compose session: its target, draft, and any problem line. *)

val make : target:target -> draft:string -> t
(** [make ~target ~draft] is a fresh compose session, no problem shown. *)

val target : t -> target
(** [target t] is [t]'s target. *)

val draft : t -> string
(** [draft t] is [t]'s current draft text. *)

val with_draft : t -> string -> t
(** [with_draft t draft] replaces the draft and clears any problem. *)

val with_problem : t -> string -> t
(** [with_problem t message] shows [message] as the [! …] line, draft kept. *)

val dialog_width : int
(** [dialog_width] is the dialog's fixed column width. *)

val height : t -> int
(** [height t] is the dialog's row count (title + input + padding, plus an error
    line when a problem shows), so the panel can center it with an explicit
    inset. *)

val view :
  palette:Theme.Palette.t ->
  ?width:int ->
  ?on_edit:(string -> 'a) ->
  ?on_submit:(string -> 'a) ->
  t ->
  'a Mosaic.t
(** [view ?on_edit ?on_submit t] is the dialog: a muted title naming the target
    line, the editable {!Mosaic.textarea} (a faint placeholder when empty), and
    a [! …] problem line when present, over an opaque {!Theme.Palette.overlay}
    background. [on_edit] tags each edited value and [on_submit] the value at
    enter; without them the input renders but reports nothing. *)
