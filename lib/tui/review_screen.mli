(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The review screen as a thin-client TEA component over the review waist.

    [Review_screen] is the whole review surface: the two-pane nav+diff split,
    the line cursor, marks and verdict, and the CR compose dialog. It holds no
    authoritative review state — the review owner does, server-side. The screen
    renders the last {!Mentat_review.View.t} summary, the focused file's
    {!Mentat_review.File_diff.t} body, and the {!Mentat_review.Cr.View.t}
    occurrence list, and expresses every state change as a {!command} the
    application runs through [Mentat_client]: a navigation or mark is one
    {!Apply} plus a {!Query_state} re-read, a CR edit is one {!Compose} plus a
    re-read of the affected projections.

    {1 Embedding contract}

    The application embeds a surface holding {!t}, routes keys through {!key},
    folds messages through {!update}, interprets the returned {!command}s, and
    tags {!view}'s messages through [inject]. The screen owns its keyboard, so
    an unclaimed key is discarded; ctrl+c remains the shell's global chord.
    {!Stay} forwards commands, {!Close} returns to chat, and {!Task_mentat}
    submits the agent review turn. {!create} starts the open flow; the
    application builds the asynchronous completion messages below. *)

(** {1 Commands} *)

(** The review-state effects the screen requests. The application interprets
    each through the client's review query and command surface. Cursor, marks,
    and verdict live server-side, so a change is an {!Apply}/{!Compose} followed
    by a {!Query_state} (and, when the focus or content moved, {!Query_diff} /
    {!Query_crs}) re-read. *)
type command =
  | Query_state of Mentat_review.Scope.t
      (** Re-read {!Mentat_review.View.t} at a focus scope via
          [Mentat_client.review_state]. The screen reads [Scope.Feature] after a
          server-side cursor move to learn the new cursor, then refines with the
          cursor's own scope so [focus_reviewed] reports it. *)
  | Query_diff of Lpath.Rel.t
      (** Read [path]'s diff body via [Mentat_client.review_diff]. *)
  | Query_crs  (** Re-read the CR views via [Mentat_client.review_crs]. *)
  | Apply of Mentat_review.Command.t
      (** Submit one mark, verdict, or cursor move via [Mentat_client.review].
      *)
  | Compose of Mentat_review.Cr.Edit.t
      (** Submit one CR add, replace, or remove via
          [Mentat_client.review_compose]. *)

(** {1 Messages} *)

type msg
(** The type for review messages. Most are internal (produced by {!key} and
    {!view}); the application constructs only the asynchronous completions
    below. *)

val state_loaded : (Mentat_review.View.t, string) result -> msg
(** [state_loaded result] reports a [Query_state] completion. *)

val diff_loaded :
  Lpath.Rel.t -> (Mentat_review.File_diff.t option, string) result -> msg
(** [diff_loaded path result] reports the [Query_diff] completion for [path]. A
    completion whose [path] is no longer the focus is dropped. *)

val crs_loaded : (Mentat_review.Cr.View.t list, string) result -> msg
(** [crs_loaded result] reports a [Query_crs] completion. *)

val command_done : (unit, string) result -> msg
(** [command_done result] reports an [Apply] completion. Success is silent (the
    re-read follows); a failure surfaces as a notice. *)

val compose_done : (unit, string) result -> msg
(** [compose_done result] reports a [Compose] completion. A failure re-opens the
    draft with the problem when the dialog is still open. *)

type verb =
  [ `Toggle
  | `Verdict
  | `Help
  | `Compose of [ `Add | `Edit | `Resolve ]
  | `Remove
  | `Next_hunk
  | `Prev_hunk
  | `Next_cr
  | `Prev_cr ]
(** A review verb the shell resolves through the command registry and folds with
    {!verb}: toggle the reviewed mark, toggle the verdict, toggle help, start a
    comment add/edit/resolve, remove a comment, or jump between hunks and
    comments. These are not focus-contextual; the focus-aware movement keys stay
    the screen's own vocabulary. *)

val verb : verb -> msg
(** [verb v] is the message applying review verb [v]. The shell resolves its
    chord through {!Command} and folds it only while the screen is not
    {!composing}. *)

(** {1 Lifecycle} *)

type t
(** The type for the review screen: loading, failed, or open. Immutable. *)

(** The shell-owned outcome of an {!update}: what the shell does with the
    surface after folding a message. *)
type event =
  | Stay of command list  (** Remain open; run these commands. *)
  | Close of command list  (** Run the commands, then return to chat. *)

val create : ?focus:Lpath.Rel.t -> unit -> t * command list
(** [create ?focus ()] is a fresh review opening against the review owner's
    configured base, together with the initial commands to read the state and
    the CR views. Once the state is read, the cursor lands on the [focus] file
    when it is among the changed files, else advances onto the first unit. *)

val update : msg -> t -> t * event
(** [update msg t] folds [msg] into [t], returning the new state and the
    shell-owned {!event}. Pure: it decides which commands to run, never runs
    one. *)

(** {1 Keyboard} *)

val key : t -> Matrix.Input.Key.event -> msg option
(** [key t ev] is the message for key [ev], or [None] for a key the review owns
    but does nothing with. It classifies the focus-contextual navigation only;
    the review verbs resolve through the registry (see {!verb}). While the
    compose dialog is open, printables and backspace fold into the draft; esc
    cancels; enter submits. ctrl+c is never seen here — the shell intercepts it
    as the global quit chord. *)

val composing : t -> bool
(** [composing t] is [true] iff the CR compose dialog is open. While composing,
    the shell leaves every key to the textarea and never resolves a {!verb}. *)

(** {1 View} *)

val view :
  palette:Theme.Palette.t ->
  ?width:int ->
  ?height:int ->
  ?layout_pref:Diff_view.layout_pref ->
  inject:(msg -> 'a) ->
  t ->
  'a Mosaic.t
(** [view ~palette ?width ?height ?layout_pref ~inject t] renders the review at
    the given terminal size. [layout_pref] (default {!Diff_view.Auto}) is the
    [tui.diff_layout] policy the diff pane resolves against its own width.
    [inject] tags the screen's own messages (nav/line clicks) into the shell's
    message type. *)
