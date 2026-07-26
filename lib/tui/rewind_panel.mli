(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The rewind-to-message picker.

    A modal overlay listing the conversation's prior user turns, newest first,
    so the user can jump back to one, edit it, and resubmit. It follows the
    house {!Panel} idiom exactly as {!Sessions_panel} does: the shared
    {!Panel.classify} filter law, digit [1]–[9] quick-select on an empty filter,
    [Esc] to close, and the {!Panel.view} chrome.

    The panel is a pure view over targets the shell supplies from its own folded
    transcript — it performs no I/O and knows nothing about stores, clients, or
    anchors. Selecting a target returns only its {!Turn.Id.t}; the shell
    resolves that to an anchor and seeds the composer. *)

module Target : sig
  type t
  (** The type for one selectable prior user turn: the turn it anchors, the user
      text that began it, and the fold-time instant it was observed. *)

  val make : turn:Mentat_session.Turn.Id.t -> text:string -> at:float -> t
  (** [make ~turn ~text ~at] is a target for user turn [turn] whose submitted
      text was [text], first observed at Unix instant [at] in seconds. *)

  val turn : t -> Mentat_session.Turn.Id.t
  (** [turn target] is the turn [target] anchors. *)

  val text : t -> string
  (** [text target] is the user text that began [target]'s turn. *)

  val at : t -> float
  (** [at target] is the fold-time instant [target] was observed, in seconds. *)
end

(** {1:state State} *)

type t
(** The type for immutable picker state: the complete target list, the current
    filter, and the selected visible row. *)

type msg
(** The type for panel input messages, produced by {!key}. *)

(** The type for actions interpreted by the shell. *)
type event =
  | Stay  (** Keep the panel open; the returned state may have changed. *)
  | Close  (** Close the picker without arming a rewind. *)
  | Arm of Mentat_session.Turn.Id.t  (** Arm a rewind to the selected turn. *)

val make : Target.t list -> t
(** [make targets] opens a picker over [targets] in the newest-first display
    order given. The first target is selected. *)

(** {1:input Input} *)

val key : Matrix.Input.Key.event -> msg option
(** [key event] is the input message classified under {!Panel.classify}, or
    [None] for a key the modal surface deliberately swallows. *)

val update : msg -> t -> t * event
(** [update message t] folds one input into [t].

    [Esc] returns {!Close}. [Enter] returns {!Arm} for the selection when a
    visible target exists. [Up] and [Down] wrap the selection over the visible
    targets. Printable input narrows the filter and selects its first match;
    [Backspace] deletes one grapheme and reselects. With an empty filter, digits
    [1]–[9] immediately arm that one-based visible target when it exists; [0]
    and out-of-range digits do nothing. With a non-empty filter every digit
    narrows it. [Tab] and every other input return {!Stay}. *)

(** {1:view View} *)

val view :
  palette:Theme.Palette.t ->
  now:float ->
  frame:Mosaic.Ansi.Color.t ->
  t ->
  msg Mosaic.t
(** [view ~now ~frame t] renders the complete picker in the {!Panel.view}
    chrome. Each row shows a one-based quick-select digit, a relative age
    against [now], and the turn's first visible line, truncated by Mosaic. The
    selected row carries the accent marker. An empty filter or an empty match
    set renders an honest note. Text is normalized before rendering and cannot
    inject terminal controls. *)
