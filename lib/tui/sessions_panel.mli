(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The saved-session quick switcher.

    The shell owns querying: it supplies session-owned summaries with {!loaded},
    reports query failures with {!failed}, and interprets the {!event} returned
    by {!update}. The panel performs no I/O and does not know about stores,
    hosts, transports, or clients.

    The panel retains every distinct summary. It filters those summaries with
    {!Mentat_session.Summary.matches}, so the session owner remains the
    authority for which metadata is searchable. Mosaic owns the measured
    viewport over that complete result. *)

(** {1:state State} *)

type t
(** The type for immutable quick-switch state.

    State is initially loading, then ready or failed. A refresh failure keeps an
    earlier ready snapshot, including its filter and selected session, and that
    retained snapshot remains interactive while the failure is shown. *)

type msg
(** The type for panel input messages. Values are produced by {!key} and the
    rendered session table's selection and activation callbacks. *)

(** The type for actions interpreted by the shell. *)
type event =
  | Stay
      (** Keep the panel open. The returned state can still contain a filter or
          selection change. *)
  | Close  (** Close the panel without changing the active session. *)
  | Resume of Mentat_session.Id.t  (** Resume the selected saved session. *)
  | Promote of { filter : string; select : Mentat_session.Id.t option }
      (** Open the full session browser with the current filter and visible
          selection. [select] is [None] when the filter has no match. *)

val loading : t
(** [loading] is a newly opened panel waiting for its first query response. *)

val loaded : Mentat_session.Summary.t list -> t -> t
(** [loaded summaries t] installs a successful query response in [t].

    Every distinct summary is retained in
    {!Mentat_session.Summary.compare_recency} order. For duplicate session
    identifiers, the newest summary is retained. The current filter is
    preserved. Its selected session is also preserved when that identifier
    remains visible after the refresh; otherwise the first visible summary is
    selected. A successful response clears an earlier failure. *)

val failed : string -> t -> t
(** [failed message t] records a failed query or refresh. [message] is
    normalized to inert one-line UTF-8 before display. When [t] already has a
    successful snapshot, its rows, filter, and selection are retained. *)

(** {1:input Input} *)

val key : Matrix.Input.Key.event -> msg option
(** [key event] is the input message classified under {!Panel.classify}, or
    [None] for an unsupported key. Page Up and Page Down deliberately return
    [None]: the focused Mosaic table owns measured viewport paging and reports
    the exact resulting selection through its typed callback. Unsupported keys
    stay inside the modal surface rather than becoming shell chords. *)

val update : msg -> t -> t * event
(** [update message t] folds one input into [t].

    Escape returns {!Close}. On a ready or retained snapshot, Up and Down wrap
    over visible summaries; Enter returns {!Resume} for the selection; and Tab
    returns {!Promote}. Printable input narrows the filter and selects its first
    match. Backspace deletes one extended grapheme cluster and selects the first
    match of the shortened filter. With an empty filter, digits [1] through [9]
    immediately resume that one-based result when it exists; [0] and
    out-of-range digits do nothing. With a non-empty filter every digit narrows
    it. All other input returns {!Stay}. *)

(** {1:view View} *)

val view :
  palette:Theme.Palette.t ->
  now:Mentat_session.Time.t ->
  frame:Mosaic.Ansi.Color.t ->
  t ->
  msg Mosaic.t
(** [view ~now ~frame t] renders the complete quick-switch panel.

    Ready summaries appear in a Mosaic table with a flexible title column and
    intrinsic phase and age columns. The table owns truncation, highlighting,
    mouse activation, and scrolling the selected row into view. Printable table
    keys remain filter input; bare arrows, Page Up, Page Down, and Enter travel
    through the table's typed callbacks. Page movement uses the currently
    measured visible-row count and clamps at the first or last summary. Future
    timestamps read [just now].

    Loading, failure, empty, and no-match states render in scroll boxes. A
    failed refresh shows its diagnostic above the retained table. Titles,
    errors, and the filter are normalized before rendering and cannot inject
    terminal controls. The panel performs no terminal measurement; Mosaic owns
    all viewport geometry. *)
