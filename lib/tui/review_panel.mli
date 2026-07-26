(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The review screen frame: the two-pane split, header, bottom legend, help
    table, and the view-local orientation the screen folds keys into.

    A persistent two-pane split — a directory-grouped nav on the left, the
    focused file's diff on the right, a full-height rule between — following the
    panel contract (rule, header, hint) but waiving the one-column law for this
    surface. This module is the pure view plus the focus transitions the screen
    calls; key routing and effects live in {!Review_screen}. The state is
    exposed concretely so the screen reads its orientation fields directly. *)

(** The focused pane. [Queue] is nav focus, [Diff] is diff focus. *)
type depth = Queue | Diff

type notice = { text : string; warning : bool }
(** A refresh/settle notice shown in place of the bottom legend until the next
    keypress. *)

type state = {
  depth : depth;  (** Which pane the movement keys drive. *)
  full_context : bool;  (** Whether the diff shows whole-file context. *)
  notice : notice option;  (** The active notice, if any. *)
  help : bool;  (** Whether the key table replaces the body. *)
  compose : Review_compose.t option;  (** The open compose dialog, if any. *)
}
(** The view-local orientation for one open review. The review state is the
    view's, held in {!Review_screen}; this holds only what the cursor does not.
*)

val init : state
(** [init] is the fresh orientation: nav focus, no notice, no dialog. *)

(** {1 Transitions} *)

val enter :
  state -> Mentat_review.View.t -> Mentat_review.Cr.View.t list -> state option
(** [enter state view crs] focuses the diff pane (enter on a nav row); [None]
    when already there or the cursor has no file to show. *)

val back : state -> state option
(** [back state] steps the esc ladder: diff focus returns to nav; nav focus
    returns [None] so the screen closes. *)

val toggle_focus : state -> state
(** [toggle_focus state] flips focus between the two panes (tab). *)

val set_compose : state -> Review_compose.t option -> state
(** [set_compose state compose] opens or closes the compose dialog. *)

val toggle_help : state -> state
(** [toggle_help state] flips the key table. *)

val toggle_context : state -> state
(** [toggle_context state] flips whole-file diff context. *)

val set_notice : state -> text:string -> warning:bool -> state
(** [set_notice state ~text ~warning] shows a notice in place of the legend. *)

val clear_notice : state -> state
(** [clear_notice state] drops any notice. *)

(** {1 Views} *)

val view :
  palette:Theme.Palette.t ->
  ?width:int ->
  ?height:int ->
  ?range:string ->
  ?layout_pref:Diff_view.layout_pref ->
  ?on_click:(Mentat_review.Cursor.t -> 'a) ->
  ?on_line_click:(Mentat_review.Scope.t -> 'a) ->
  ?on_compose_edit:(string -> 'a) ->
  ?on_compose_submit:(string -> 'a) ->
  crs:Mentat_review.Cr.View.t list ->
  file_diff:Mentat_review.File_diff.t option ->
  state ->
  Mentat_review.View.t ->
  'a Mosaic.t
(** [view ~palette ?width ?height ?range ?on_click ?on_line_click
     ?on_compose_edit ?on_compose_submit ~crs ~file_diff state view] renders the
    screen: the top rule, the [Review  <range>] header with progress and
    verdict, the two-pane split (nav + diff, or a single focused pane below 80
    columns, or the key table when [state.help]), the bottom legend or notice,
    and the compose dialog floated over the dimmed panes when open. [file_diff]
    is the body fetched for the cursor's focused path. [on_click] /
    [on_line_click] report nav-row and diff-line selections; [on_compose_edit] /
    [on_compose_submit] carry the compose input's edits and enter. *)

val loading_view :
  palette:Theme.Palette.t -> ?width:int -> ?height:int -> unit -> _ Mosaic.t
(** [loading_view ~palette ()] is the rule + [Review  computing…] header held
    while the first view loads, so the frame does not pop in. *)

val error_view :
  palette:Theme.Palette.t ->
  ?width:int ->
  ?height:int ->
  message:string ->
  unit ->
  _ Mosaic.t
(** [error_view ~palette ~message ()] is the rule + [Review] header + a
    [! message] error line + an [esc close] affordance (the load-failure state).
*)
