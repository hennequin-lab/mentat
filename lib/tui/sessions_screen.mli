(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The full-screen saved-session browser.

    The shell owns every effect. It supplies exact session-owned summaries with
    {!loaded}, reports listing failures with {!failed}, and interprets the typed
    {!event} returned by {!update}. The screen never reads a store, client,
    clock, or workspace and never mints a session identifier.

    Rows remain {!Mentat_session.Summary.t} values throughout. Search uses
    {!Mentat_session.Summary.matches}; titles, lifecycle, phase, recency,
    preview, cwd, turn count, and fork lineage are projected only at render or
    interaction time through their owner accessors. *)

(** {1:state State} *)

type t
(** The type for immutable browser state.

    State is loading, failed, or ready. A failed refresh retains an earlier
    ready snapshot, including its filter, exact selected identifier, and inline
    edit. That retained snapshot remains usable while the failure is visible. A
    lifecycle mutation temporarily locks its exact row; a successful refresh or
    correlated failure unlocks it without changing the selection. Selection
    never denotes a row index: refreshes preserve it iff the same identifier
    remains visible, otherwise the first visible summary is selected. *)

type msg
(** The type for input messages. Values are produced by {!key}, {!verb}, and the
    native results table's selection and activation callbacks. *)

type verb = [ `Fork | `Rename | `Archive | `Restore | `Delete ]
(** A browsing action the shell resolves through the command registry and folds
    with {!verb}: fork, rename, archive, restore, or delete the selected row. *)

val verb : verb -> msg
(** [verb v] is the message applying browsing verb [v] to the selected row. The
    shell resolves the verb's chord through {!Command} and folds this only while
    {!browsing}; the effect is exactly the browsing letter it replaced. *)

val browsing : t -> bool
(** [browsing t] is [true] iff [t] is browsing with the filter closed — the only
    state in which a {!verb} applies. The filter, rename, and confirmation
    states keep every key as the screen's own vocabulary. *)

(** The type for shell-owned outcomes. No constructor performs an effect. *)
type event =
  | Stay
      (** Keep the browser open. The returned state can still contain a filter,
          selection, edit, confirmation, or validation change. *)
  | Close  (** Return to the surface beneath the browser. *)
  | Resume of Mentat_session.Id.t
      (** Resume the selected active session from its beginning. *)
  | Fork of Mentat_session.Id.t
      (** Fork the selected idle active session. The identifier is the source;
          the shell mints the destination identifier before calling the client
          lifecycle operation. *)
  | Rename of { id : Mentat_session.Id.t; title : string }
      (** Persist [title] for [id]. [title] is normalized to inert one-line
          UTF-8, trimmed of surrounding Unicode whitespace, and contains at
          least one non-whitespace Unicode scalar. The returned state visibly
          locks [id] until {!loaded} or {!mutation_failed}. *)
  | Archive of Mentat_session.Id.t
      (** Archive the selected idle active session. This reversible action has
          its own key and never shares the delete confirmation path. The
          returned state visibly locks the selected row. *)
  | Restore of Mentat_session.Id.t
      (** Restore the selected archived session. The returned state visibly
          locks the selected row. *)
  | Delete of Mentat_session.Id.t
      (** Permanently delete the selected non-deleted idle session. This event
          is returned only for the second [d] of the in-row confirmation. The
          returned state visibly locks the selected row. *)

val loading : t
(** [loading] is a newly opened browser waiting for its first session query. *)

val promoted : filter:string -> select:Mentat_session.Id.t option -> t
(** [promoted ~filter ~select] is a loading browser promoted from
    {!Sessions_panel.Promote}.

    [filter] is normalized to inert one-line UTF-8. A non-empty filter starts
    open. After the first {!loaded}, [select] is retained iff that exact
    identifier matches the filter; otherwise the first match is selected. *)

val loaded : Mentat_session.Summary.t list -> t -> t
(** [loaded summaries t] installs a successful query response in [t].

    Summaries are ordered with {!Mentat_session.Summary.compare_recency};
    duplicate identifiers are discarded. An existing filter and exact selected
    identifier are preserved. An inline edit is preserved only while its target
    still exists, remains visible, and still admits that action. A pending
    lifecycle mutation is unlocked because the response is the shell's
    correlated post-mutation refresh. A successful response clears an earlier
    listing failure. *)

val failed : string -> t -> t
(** [failed message t] records a failed initial query or refresh. [message] is
    normalized to inert one-line UTF-8 before display. A successful snapshot in
    [t] remains visible and interactive; an initial failure keeps any pending
    promotion so a later {!loaded} can still honor it. A failed refresh also
    unlocks a pending lifecycle row: the mutation has already settled, while
    only its follow-up listing failed. *)

val mutation_failed : string -> t -> t
(** [mutation_failed message t] visibly reports a correlated lifecycle failure
    over the retained browser rows and unlocks the pending row. [message] is
    normalized as for {!failed}. A later successful {!loaded} clears the
    diagnostic. *)

(** {1:input Input} *)

val key : Matrix.Input.Key.event -> msg option
(** [key event] is the browser message classified by {!Screen.classify}, or
    [None] for an unsupported key. Unsupported keys stay inside the modal
    surface rather than becoming application chords. *)

val update : msg -> t -> t * event
(** [update message t] folds one input into [t].

    While browsing with the filter closed:
    - Up and Down wrap over visible summaries; Enter resumes an active row.
    - [/] opens the filter.
    - A {!verb} forks or renames an idle active row ([`Fork]/[`Rename]),
      archives an idle active row ([`Archive]), restores an archived row
      ([`Restore]), or opens a destructive in-row confirmation for an idle,
      non-deleted row ([`Delete]). Its chord is resolved by the shell through
      the command registry.
    - The first [`Delete] verb opens the confirmation; only a second [d] returns
      {!Delete}. Any other key cancels that confirmation.
    - Escape returns {!Close}.

    With the filter open, every printable character narrows with
    {!Mentat_session.Summary.matches}, Backspace removes one extended grapheme
    cluster, movement and Enter retain their browsing meanings, and Escape
    clears and closes the filter without leaving the screen.

    During rename, printable input and Backspace edit one-line Unicode text,
    Escape cancels, and Enter returns {!Rename} only for a visibly nonblank
    title. A title containing only Unicode whitespace keeps the editor open and
    displays a structured inline validation issue. After rename, archive,
    restore, or the confirmed delete returns its event, the exact row is visibly
    pending and every key returns {!Stay} until {!loaded} or {!mutation_failed}
    unlocks it. Unsupported actions return {!Stay}. *)

(** {1:view View} *)

val view :
  palette:Theme.Palette.t ->
  home:Lpath.Abs.t option ->
  now:Mentat_session.Time.t ->
  frame:Mosaic.Ansi.Color.t ->
  t ->
  msg Mosaic.t
(** [view ~home ~now ~frame t] renders the complete browser screen. [home]
    determines home-relative cwd spelling; [None] keeps absolute paths.

    Ready summaries are one-to-one rows in a native {!Mosaic.table}. Its columns
    show normalized title, lifecycle, phase, turn count, age relative to [now],
    and elapsed-recency group ([today], [this week], [older]). Mosaic truncates
    overflowing cells and automatically scrolls the exact selected row into
    view. The selected summary's scrollable detail shows its normalized preview
    and home-relative cwd, plus exact parent identifier and copied-event count
    when fork lineage exists; parent titles are never guessed. Future timestamps
    read [just now]. Lifecycle mutations replace the selected row's identity and
    detail with a pending label and suppress conflicting actions. Loading,
    initial failure, retained-refresh failure, mutation failure, empty,
    no-match, rename-validation, and delete-confirmation states are explicit
    scrollable regions.

    {!Screen.view} grows and shrinks within the parent allocation; child content
    flexes within its body without deriving a second viewport. Every displayed
    remote field is normalized before reaching Mosaic. *)
