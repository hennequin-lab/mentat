(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Live, availability-aware model selection.

    The panel retains the exact {!Mentat_provider.Model_readiness.t} returned by
    {!Mentat_client.model_readiness}. It browses the owner's provider routes and
    model entries in their original order and derives presentation directly from
    their structured eligibility, availability, and actionability facts. It
    never accepts a raw catalog or constructs a frontend model mirror.

    The launch fallback or last-started current model comes from {!Snapshot.t}.
    Browsed activation returns the exact typed selector owned by the selected
    {!Mentat_provider.Model.t}; set-by-name remains available while readiness is
    loading or unavailable. The shell interprets {!Set_model} with
    {!Mentat_client.set_model} and {!Reload} by issuing a fresh readiness query.
*)

(** {1:state State} *)

type t
(** The immutable panel state.

    State retains the current snapshot, the exact successful readiness owner
    value, selector/filter input, selected visible entry, the live
    reasoning-effort override, and any owner-produced parse error. A refresh
    failure retains the last successful owner value. Set-by-name entry remains
    interactive while readiness is loading or unavailable. *)

type msg
(** A panel input produced by {!key} or by the interactive model catalog table
    in {!view}. *)

(** The type for actions interpreted by the shell. *)
type event =
  | Stay
      (** Keep the panel open. The returned state can contain changed selector
          input, model selection, or a parse diagnostic. *)
  | Close  (** Close the panel without changing the selected model. *)
  | Reload
      (** Ask the shell to re-run {!Mentat_client.model_readiness}. The returned
          state marks the request as refreshing, clears a prior refresh or
          interaction error, and keeps its current owner snapshot visible;
          without a successful snapshot it returns to loading. The panel
          performs no I/O. *)
  | Set_model of {
      selector : Mentat_provider.Selector.t;
      reasoning_effort : Mentat_llm.Request.Options.Reasoning_effort.t option;
    }
      (** Ask the shell to set the exact browsed or owner-parsed selector and
          its reasoning effort — for the active session through
          {!Mentat_client.set_model}, or, before any session exists, staged for
          the session the first prompt starts. [reasoning_effort] is the level
          the effort control left on the activated row, or [None] to follow the
          model's own default — also for a by-name selection, which carries no
          effort context. The panel performs no I/O and never persists the
          selection. *)

val loading : Snapshot.t -> t
(** [loading snapshot] is a newly opened panel whose current model is rendered
    from [snapshot]. It is the launch fallback before any turn and the exact
    last-started selection afterwards. The model-readiness query has not
    completed; set-by-name entry is available immediately. *)

val loaded : Mentat_provider.Model_readiness.t -> t -> t
(** [loaded readiness t] installs the exact model-readiness owner value in [t].

    Provider routes and model entries retain owner order. The selector input and
    owner-produced parse diagnostic are preserved; a prior selection refusal is
    cleared because its facts may have changed. The visible selection stays on
    the same typed selector across refresh when possible, otherwise it moves to
    the current snapshot model when visible and then to the first visible entry.
    An earlier query failure is cleared. *)

val failed : string -> t -> t
(** [failed message t] records a failed model-readiness query. [message] is
    normalized to inert one-line UTF-8 before display. A previous successful
    owner value is retained and remains browsable. Without a previous success,
    set-by-name remains available. *)

val refuse_selection : string -> t -> t
(** [refuse_selection message t] records a shell-supplied refusal for a
    selection the panel emitted but the current context cannot apply, such as an
    activation while a previous selection is still being applied. [message] is
    displayed as a panel-local issue and normalized to inert one-line UTF-8; any
    further input clears it. This keeps the panel open with a visible reason
    rather than silently dropping the activation. *)

(** {1:input Input} *)

val key : Matrix.Input.Key.event -> msg option
(** [key event] is a keyboard message for [event], or [None] for an unsupported
    key. [Ctrl+R] is recognized before {!Panel.classify} and produces the
    refresh action. Model-table navigation and activation are emitted by the
    Mosaic widget itself. Up and Down move by one entry; Page Up and Page Down
    move through the table's measured viewport. *)

val paste : string -> t -> t
(** [paste text t] appends [text] to the selector input. Malformed UTF-8 and
    terminal controls are normalized to inert inline text. The visible model
    selection returns to the first match and a previous parse diagnostic is
    cleared. *)

val update : msg -> t -> t * event
(** [update message t] folds one panel input into [t].

    Printable characters and decimal digits append to the selector input;
    Backspace deletes one extended grapheme cluster; Escape returns {!Close};
    and [Ctrl+R] returns {!Reload}. Left and Right walk the selected row model's
    {!Mentat_provider.Model.supported_reasoning} levels, wrapping at the ends;
    landing on the model's own default records "follow the default" rather than
    a pin, and a model that supports no effort ignores them. Enter activates the
    selected visible owner entry, or parses the complete input with
    {!Mentat_provider.Selector.of_string} when no entry matches. A successful
    by-name parse returns {!Set_model} with no effort; a parse error remains
    structured for display. Activating an eligible, actionable row returns
    {!Set_model} with the exact {!Mentat_provider.Model.selector} from its owner
    entry and the live effort when that model supports it. An ineligible,
    provider-blocked, or model-unavailable row remains visible but returns
    {!Stay} with a structured panel-local explanation. *)

(** {1:view View} *)

val view :
  palette:Theme.Palette.t ->
  frame:Mosaic.Ansi.Color.t ->
  rows:int ->
  t ->
  msg Mosaic.t
(** [view ~frame ~rows t] renders the complete model panel. [rows] is the screen
    height; below a small-screen threshold the panel drops its decorative gaps
    and the Current line so the picker table keeps a browsable number of rows.

    The panel shows the current snapshot model and a controlled set-by-name
    field in every remote state. A successful readiness value becomes a quiet
    picker table in exact provider/model order: a selection cursor, the provider
    name shown once per contiguous provider run, the model name, and a single
    muted status clause that names why a row cannot be picked and is empty when
    the row is ready. Blocked and locked rows are muted whole; the current exact
    selector carries a trailing ✓. Below the table an effort line names the
    selected row model's reasoning level, tags the model default, and cautions a
    top-tier level; a model that supports no effort states so instead. Filtering
    matches provider identifiers and display names, model identifiers and
    display names, and canonical selectors without reordering matches. A
    retained refresh failure is shown without replacing the table; an in-flight
    refresh shows a concise status above the retained rows.

    Mosaic owns every allocation-sensitive behavior: columns truncate at their
    allocated width, the table scrolls its selection into view and shows an edge
    indicator, the controlled input keeps its insertion point visible, and long
    diagnostics wrap inside scroll containers. The view performs no terminal
    size arithmetic, row budgeting, display-width calculation, wrapping,
    slicing, or viewport management. *)
