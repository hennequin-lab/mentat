(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Session goals.

    A goal is a durable objective the engine pursues across turns. Its lifecycle
    is a sequence of {!Update.t} facts; its accounting — tokens used and
    continuation-turn count — is {e derived} from goal-continuation turns and
    never stored. At most one goal exists per session. Whether to continue
    pursuing a goal on a new turn is engine policy, not a recorded fact. *)

(** {1:ids Identifiers} *)

module Id : sig
  type t
  (** The type for stable goal identifiers, minted by the engine at turn
      admission, content-derived from the declaring turn.

      Invariant: an identifier's stable textual form is non-empty. *)

  val of_string : string -> t
  (** [of_string s] is [s] as a goal id.

      Raises [Invalid_argument] if [s] is empty. *)

  val to_string : t -> string
  (** [to_string id] is [id]'s stable string representation. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same goal id. *)

  val compare : t -> t -> int
  (** [compare a b] orders ids by their stable string representations,
      compatibly with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an id for diagnostics. The output is not stable storage
      syntax. *)

  val jsont : t Jsont.t
  (** [jsont] maps goal ids to JSON strings, validating the non-empty invariant
      of {!of_string} on decode. *)
end

(** {1:status Status} *)

module Status : sig
  (** The type for a goal's derived lifecycle status. Assembled by replay from
      the {!Update.t} sequence; never stored. *)
  type t =
    | Active
    | Paused
    | Blocked of { reason : string option }
    | Budget_limited
    | Completed of { summary : string option }
    | Cleared

  val is_terminal : t -> bool
  (** [is_terminal t] is [true] iff [t] is {!Completed} or {!Cleared} — a goal
      that admits no further transition. *)

  val pausable : t -> bool
  (** [pausable t] is [true] iff a goal in status [t] may be paused: only
      {!Active}. A fork or rewind pauses the inherited goal in its reset suffix
      only when it is pausable. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same status. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a status for diagnostics. *)
end

(** {1:updates Updates} *)

module Update : sig
  (** The type for a durable goal lifecycle update. Transition legality is
      checked at replay ({!State.apply}); constructors check only local shape.
  *)
  type t = private
    | Declare of { id : Id.t; objective : string; token_budget : int option }
    | Pause of { id : Id.t }
    | Resume of { id : Id.t; token_budget : int option }
    | Edit of { id : Id.t; objective : string }
    | Clear of { id : Id.t }
    | Complete of { id : Id.t; summary : string option }
    | Block of { id : Id.t; reason : string option }
    | Budget_limited of { id : Id.t }

  val declare : id:Id.t -> objective:string -> ?token_budget:int -> unit -> t
  (** [declare ~id ~objective ?token_budget ()] declares a goal.

      Raises [Invalid_argument] if [objective] is empty or [token_budget] is
      negative. *)

  val pause : id:Id.t -> t
  (** [pause ~id] pauses goal [id]. *)

  val resume : id:Id.t -> ?token_budget:int -> unit -> t
  (** [resume ~id ?token_budget ()] resumes goal [id], optionally resetting its
      budget.

      Raises [Invalid_argument] if [token_budget] is negative. *)

  val edit : id:Id.t -> objective:string -> t
  (** [edit ~id ~objective] edits goal [id]'s objective.

      Raises [Invalid_argument] if [objective] is empty. *)

  val clear : id:Id.t -> t
  (** [clear ~id] clears goal [id]. *)

  val complete : id:Id.t -> ?summary:string -> unit -> t
  (** [complete ~id ?summary ()] completes goal [id].

      Raises [Invalid_argument] if [summary] is present and empty. *)

  val block : id:Id.t -> ?reason:string -> unit -> t
  (** [block ~id ?reason ()] blocks goal [id].

      Raises [Invalid_argument] if [reason] is present and empty. *)

  val budget_limited : id:Id.t -> t
  (** [budget_limited ~id] marks goal [id] as budget-limited. *)

  val id : t -> Id.t
  (** [id t] is the goal id [t] targets. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same update. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an update for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps updates to JSON values by a per-arm tag, validating local
      invariants on decode. *)
end

(** {1:projections Projections} *)

type t
(** The type for a goal projection: id, objective, status, budget, and derived
    accounting. Assembled by replay; never stored. *)

val make :
  id:Id.t ->
  objective:string ->
  status:Status.t ->
  token_budget:int option ->
  tokens_used:int ->
  continuation_turns:int ->
  t
(** [make ~id ~objective ~status ~token_budget ~tokens_used ~continuation_turns]
    is a validated goal projection for replay and transport. The accounting
    arguments are derived from goal-continuation turns.

    Raises [Invalid_argument] if [objective] is empty; [token_budget],
    [tokens_used], or [continuation_turns] is negative; a present blocked reason
    or completion summary is empty; or [tokens_used] is non-zero while
    [continuation_turns] is zero. *)

val id : t -> Id.t
(** [id t] is [t]'s goal id. *)

val objective : t -> string
(** [objective t] is [t]'s current objective. *)

val status : t -> Status.t
(** [status t] is [t]'s current status. *)

val token_budget : t -> int option
(** [token_budget t] is [t]'s token budget, if any. *)

val tokens_used : t -> int
(** [tokens_used t] is the total tokens consumed by [t]'s goal-continuation
    turns. Derived, not stored. *)

val continuation_turns : t -> int
(** [continuation_turns t] is the number of {!Turn.Origin.Goal_continuation}
    turns pursuing [t]. Derived, not stored. *)

val remaining_tokens : t -> int option
(** [remaining_tokens t] is [token_budget t] less {!tokens_used}, clamped at
    zero, or [None] when there is no budget. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same projection. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a goal for diagnostics. *)

val jsont : t Jsont.t
(** [jsont] maps goal projections to JSON objects with discriminated status
    arms. Decoding validates {!make} and rejects members that do not belong to
    the selected status arm. *)
