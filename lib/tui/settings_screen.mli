(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Effective settings and truthful session-scoped controls.

    The screen consumes owner values without re-projecting them into frontend
    records: {!Mentat_config.Resolved.View.t} supplies credential-free values,
    origins, and warnings; {!Mentat_session.Session_view.t} supplies optional
    status and usage; and account readiness remains the exact list of
    owner-produced {!Mentat_provider.Account.Discovery.t} values. {!Snapshot.t}
    contributes launch version, workspace, and sandbox facts together with the
    last-started model, effort, and known context window.

    The screen performs no I/O. The shell runs configuration and readiness
    queries, folds their results with {!configuration_loaded},
    {!readiness_loaded} and the matching failure functions, and interprets typed
    {!event} values with [Mentat_client]. Model selection and permission review
    are session overlays effective on the next turn; this surface never edits a
    config file. Sandbox remains launch configuration and a status fact, not a
    live-session control. *)

(** {1:state State} *)

type tab = Command.settings_tab =
  | Config
  | Status
  | Usage
      (** The three settings pages. This is {!Command.settings_tab}: the tab an
          {!Command.Open_settings} fate names is the tab this screen opens on.
      *)

type t
(** The type for immutable settings-screen state.

    Configuration and readiness load independently. A failed refresh retains the
    last successful owner value and displays the failure alongside it. Selection
    of a configuration entry is keyed by its owner-provided config key, so
    refreshes do not silently move it when entries are reordered. A permission
    choice is a local, visibly unconfirmed candidate until the shell folds its
    correlated result; a rejected candidate remains available for retry but is
    never presented as owner state. *)

type msg
(** The type for input messages. Values are produced by {!key} and by the
    focused Mosaic table callbacks used for measured config navigation. *)

type mutation
(** The type for opaque mutation correlations minted by {!update}.

    A value identifies one request for one exact active session. Callers cannot
    construct or inspect it; they return it unchanged to {!mutation_finished}
    with the matching client result. *)

(** The type for pure shell-owned outcomes. No constructor performs an effect.
*)
type event =
  | Stay
      (** Keep the screen open. The returned state can still contain changed
          navigation, filter, control, input, or validation state. *)
  | Close  (** Return to the surface beneath the settings screen. *)
  | Open_model_panel
      (** Open model selection for the active session. The model panel returns
          the exact selector consumed by [Mentat_client.set_model]. *)
  | Set_permission_review of {
      request : mutation;
      review : Mentat_permission.Review_behavior.t;
    }
      (** Ask the shell to set [review] for the active session's next turn, then
          fold the exact client result with {!mutation_finished}. *)

val loading :
  tab:tab ->
  snapshot:Snapshot.t ->
  session:Mentat_session.Session_view.t option ->
  t
(** [loading ~tab ~snapshot ~session] is a newly opened screen on [tab]. Both
    owner queries are pending. [snapshot] supplies launch presentation facts
    plus the exact last-started model and effort when a turn exists; [session],
    when present, is the exact active-session detail projection used for status,
    usage, and mutation availability. *)

val set_session : Mentat_session.Session_view.t option -> t -> t
(** [set_session session t] replaces the optional active-session projection.
    Configuration, readiness, tab, and filters are preserved. A local permission
    candidate is preserved only when [session] has the same exact
    {!Mentat_session.Id.t}; changing or removing the session clears it so a
    candidate cannot leak between sessions. Passing [None] immediately disables
    every session-scoped mutation. *)

val mutation_finished :
  mutation -> (unit, Mentat_protocol.Error.t) result -> t -> t
(** [mutation_finished request result t] folds the exact client result for
    [request]. It changes [t] only when [request] is the currently pending
    request and its captured session id is the exact id of [t]'s active session.
    A duplicate, superseded, late, or cross-session result is ignored.

    [Ok ()] clears the submitted local permission candidate and displays an
    exact next-turn acceptance acknowledgement. [Error error] retains the
    candidate and every owner snapshot, and displays
    [Mentat_protocol.Error.diagnostic error] without inspecting diagnostic text.
    Either settlement clears the in-flight state. The acknowledgement or failure
    is a transient acknowledgement of the last settled mutation: it is dismissed
    as soon as the user navigates—switching page, moving the selection, or
    opening the filter—or starts another mutation, and also when the exact
    session changes or the screen is reconstructed. A rejected candidate is
    retained independently so the change can be retried without the failure text
    lingering. A multi-line failure is mounted in a dedicated
    {!Mosaic.scroll_box}; Mosaic owns its wrapping, clipping, scrollbar, and
    mouse scrolling, so no diagnostic line is parsed, counted, or discarded by
    screen state. *)

val configuration_loaded : Mentat_config.Resolved.View.t -> t -> t
(** [configuration_loaded configuration t] installs a successful
    [Mentat_client.configuration] result and clears its earlier failure. The
    exact view is retained; entries are read only through owner accessors at
    filter and render time. The selected entry is preserved by config key when
    still visible. *)

val configuration_failed : string -> t -> t
(** [configuration_failed message t] records a failed configuration query.
    [message] is normalized to inert one-line UTF-8 before display. A previous
    successful configuration remains visible and filterable. *)

val readiness_loaded : Mentat_provider.Account.Discovery.t list -> t -> t
(** [readiness_loaded readiness t] installs a successful
    [Mentat_client.account_readiness] result and clears its earlier failure.
    Rows are ordered by provider identifier for display; known accounts derive
    their displayed phase from [Account.phase], while resolution failures remain
    provider-local diagnostics. Duplicates are retained so no owner-produced
    discovery disappears. *)

val readiness_failed : string -> t -> t
(** [readiness_failed message t] records a failed readiness query after
    normalizing [message]. A previous successful response remains visible. *)

(** {1:input Input} *)

val key : Matrix.Input.Key.event -> msg option
(** [key event] is the input message classified by {!Screen.classify}, or [None]
    for an unsupported key. Unsupported keys remain inside this modal screen
    rather than becoming application chords. *)

val paste : string -> t -> t
(** [paste text t] appends normalized one-line [text] to the open config filter
    and is inert on the status and usage tabs and while the config filter is
    closed. *)

val update : msg -> t -> t * event
(** [update message t] folds one input into [t].

    - Tab cycles pages; the arrow keys never switch pages, so their meaning
      never depends on which row is selected. Escape closes an open filter
      before it closes the screen.
    - On config, Up and Down wrap over the two typed controls, structured owner
      warnings, and visible owner entries. Left and Right change the focused
      setting's value: on the permission control they choose a visibly
      unconfirmed candidate locally, and are inert on rows with no editable
      value. Enter applies the focused control—it opens model selection, or
      emits the permission candidate's exact request (selecting the leading
      value first when none is chosen). Choosing a candidate never contacts the
      engine, so it cannot round-trip and revert. [/] opens the config filter.
      Owner warnings and entries are selectable and filterable but never emit
      writes.
    - Status and usage are read-only; Tab switches pages there too.

    Every mutation is unavailable when there is no active session. No update
    edits config files or applies optimistic owner data. While a mutation is in
    flight, its exact target is visible and mutation-producing input is inert;
    navigation, filtering, tab switching, and closing remain available. The
    config table owns selection, activation, and Page Up/Page Down while it is
    mounted, and reports the selected owner row through [msg]. Status and usage
    scroll boxes own Page Up/Page Down directly. Both widget classes derive a
    page from their measured Mosaic allocation; page keys never become one-row
    screen actions or terminal-size arithmetic in [t]. Mouse wheels and visible
    scrollbars inspect the same overflowing regions. *)

(** {1:view View} *)

val view :
  palette:Theme.Palette.t -> frame:Mosaic.Ansi.Color.t -> t -> msg Mosaic.t
(** [view ~frame t] renders the complete settings screen.

    Config shows session-scoped controls followed by every structured warning
    and the effective owner entries. Warnings are individual selectable rows,
    not collapsed behind a count. Mosaic's selectable table owns column sizing,
    cell ellipsis, measured keyboard paging, and selected-row scrolling.
    Redacted values are conspicuous and never reconstructed. Every entry
    supplies its complete owner-derived {!Mentat_config.Origin.pp} value to the
    source cell; the table, rather than this module, decides how much fits.
    Retained-refresh failures precede the retained values. Status combines
    launch version, workspace, and sandbox facts with the last-started model,
    effort, and known context window, optional exact session detail, and exact
    account discoveries. Usage shows only provider-reported lanes and session
    counters—never price, quota, or cost guesses. Mutation progress and
    settlement are global: success names the exact accepted target; failure
    mounts the complete {!Mentat_protocol.Error.diagnostic} rendering in its own
    scroll box.

    Remote text, config values, origins, warnings, diagnostics, and editable
    text cross a terminal-safety normalization boundary. Mosaic text, flex,
    table, and scroll-box primitives then own wrapping, truncation, clipping,
    overflow, and keyboard page distance; this module does not measure terminal
    presentation or predict visible row budgets. {!Screen.view} grows and
    shrinks within the allocation supplied by the application shell. *)
