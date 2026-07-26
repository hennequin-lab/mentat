(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Session-goal inspection and user controls.

    The screen retains the exact {!Mentat_session.Goal.t} supplied by the
    session detail projection. It derives only presentation and the legal pause,
    edit, resume, and clear controls. Goal declaration, completion, blocking,
    continuation admission, and accounting remain engine concerns.

    The screen performs no client call. The application interprets {!event}
    values with [Mentat_client], returns the same opaque {!mutation} through
    {!mutation_finished}, and installs authoritative state with {!set_goal}
    after [Fact.Journal_goal] refreshes [Session_view.goal]. *)

(** {1:state State} *)

type t
(** The type for immutable goal-screen state.

    The owner projection is distinctly loading, unavailable, or loaded with an
    optional goal. A failed refresh retains an earlier loaded projection. The
    remaining state is a selected legal action, an optional edit or budget
    field, a destructive-clear confirmation, a keyed Mosaic detail-page request,
    and at most one correlated mutation. Display sanitization never alters the
    owner projection or a command payload. *)

type msg
(** The type for input messages. Values are produced by {!key}, the controlled
    Mosaic action table, and the focused inline editor. *)

type mutation
(** The type for opaque mutation correlations minted by {!update}.

    A mutation identifies one request for the exact goal id visible when the
    user confirmed an action. Callers cannot construct or inspect it and must
    return it unchanged through {!mutation_finished}. *)

(** A goal command requested by the screen. Every constructor carries the exact
    goal id displayed when the action was confirmed. *)
type command =
  | Pause of Mentat_session.Goal.Id.t  (** Pause the displayed active goal. *)
  | Edit of { goal : Mentat_session.Goal.Id.t; objective : string }
      (** Replace the displayed unfinished goal's objective. [objective] is
          non-empty and differs from the displayed objective's normalized
          single-line editing copy. *)
  | Resume of { goal : Mentat_session.Goal.Id.t; budget : int option }
      (** Resume the displayed paused, blocked, or budget-limited goal. [budget]
          is a nonnegative replacement budget when present; [None] retains the
          current budget. *)
  | Clear of Mentat_session.Goal.Id.t
      (** Clear the displayed unfinished goal after explicit confirmation. *)

(** The type for pure shell-owned outcomes. No constructor performs an effect.
*)
type event =
  | Stay
      (** Keep the goal screen open. The returned state can still contain a
          changed selection, editor, validation issue, or confirmation. *)
  | Close  (** Return to the conversation surface. *)
  | Request of { mutation : mutation; command : command }
      (** Submit [command] and return [mutation] unchanged with its client
          result. The screen locks further actions until the result fails or an
          authoritative goal refresh supersedes the displayed projection. *)

val make : Mentat_session.Goal.t option -> t
(** [make goal] is a goal screen displaying [goal]. [None] is an honest
    read-only state: no goal exists and the screen offers no creation action. A
    goal is declared instead from the composer with [/goal <objective>], which
    rides the next prompt; this screen only inspects and controls the result. *)

val loading : t
(** [loading] is a goal screen waiting for its first session-detail query. It is
    distinct from [make None], which means the query proved that no goal is
    declared. *)

val set_goal : Mentat_session.Goal.t option -> t -> t
(** [set_goal goal t] installs the latest exact session projection.

    An equal projection preserves local navigation and mutation state. A changed
    projection resets editors and confirmations, clears mutation feedback,
    rebuilds the legal action selection, and retires a pending request whether
    the change came from that request or a concurrent goal transition. A late
    completion for the retired request is ignored. *)

val load_failed : Mentat_protocol.Error.t -> t -> t
(** [load_failed error t] records a failed session-detail query. If [t] has a
    loaded projection, that exact projection remains visible and interactive
    with the complete structured diagnostic above it; any pending request is
    retired because its authoritative result is unknown. An initial failure is
    an unavailable read-only screen. A later {!set_goal} clears the failure. *)

val mutation_finished :
  mutation -> (unit, Mentat_protocol.Error.t) result -> t -> t
(** [mutation_finished request result t] folds the exact client result for
    [request]. A duplicate, superseded, or late result is ignored.

    [Ok ()] acknowledges only command admission and keeps the screen locked
    while it waits for an authoritative {!set_goal}; it never changes the goal
    projection. [Error error] unlocks the current projection and displays the
    complete structured diagnostic without parsing its text. *)

(** {1:input Input} *)

val key : Matrix.Input.Key.event -> msg option
(** [key event] is the screen message classified by {!Screen.classify}, or
    [None] for an unsupported key. While an inline editor is focused, the
    application routes only Escape through this function; Mosaic owns editing,
    cursor motion, paste, and submission. *)

val editing : t -> bool
(** [editing t] is [true] iff the objective or resume-budget inline editor owns
    input. *)

val update : msg -> t -> t * event
(** [update message t] folds one input into [t].

    In the action list, Up and Down wrap, digits select without activating, and
    Enter activates the selected legal action. PageUp and PageDown issue keyed
    one-viewport requests to the detail scroll box; Mosaic applies them using
    its measured allocation and acknowledges each request exactly once. Pause
    emits immediately; edit opens a required objective field prefilled with the
    current objective's normalized single-line editing copy; resume opens an
    optional nonnegative token-budget field; clear opens a destructive
    confirmation. Escape closes the screen from the action list, cancels an
    editor or confirmation, and may close a screen whose request remains in
    flight.

    Unicode-whitespace-only or unchanged objective submissions and malformed or
    negative budgets remain in their editor with a visible validation issue. An
    unchanged comparison uses the same single-line normalization as the
    prefilled editor, so merely confirming a multiline objective cannot
    accidentally flatten it. A blank resume budget emits
    [Resume { budget = None; _ }]. Clear emits only after a second Enter while
    the explicit confirmation is visible. While a request is in flight,
    action-producing input is inert. *)

(** {1:view View} *)

val view :
  palette:Theme.Palette.t -> frame:Mosaic.Ansi.Color.t -> t -> msg Mosaic.t
(** [view ~frame t] renders the complete goal screen.

    Loading, loaded-without-goal, initial-failure, and retained-refresh-failure
    are visually distinct. A loaded goal displays its exact objective, lifecycle
    detail, goal id, continuation-turn count, token budget, consumed tokens, and
    remaining tokens from owner accessors. Only actions legal for the exact
    status are mounted. Completed and cleared goals are read-only. Rejected
    commands retain the projection and show the complete structured diagnostic
    in a scrollable region. State-aware hints advertise detail paging whenever a
    loaded or unavailable projection may overflow.

    Remote text crosses a terminal-safety normalization boundary before display.
    Mosaic text, flex, table, input, and scroll-box primitives own wrapping,
    truncation, clipping, selected-row visibility, and narrow-height allocation;
    this module performs no terminal measurement or row budgeting.
    {!Screen.view} supplies the full-screen chrome and pinned state-aware hints.
*)
