(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Correlating a mutation event log against its session document.

    Private to the store. Before a mutation log is trusted, every event is
    checked to reference a turn and tool claim the session actually records, in
    a state consistent with the event's kind. This is the store's half of the
    mutation/session arrow: the mutation library validates event ordering and
    diff derivation, while this module validates that the events name real
    session structure. A failure is a corruption diagnostic, not a recoverable
    condition. The store re-exports this type and its projections through
    {!Mutation.Error.Correlation}. *)

(** The type for a correlation failure: an event that names session structure
    the document does not support. *)
type t =
  | Unknown_turn of Mentat_session.Turn.Id.t
      (** An event references a turn the document does not record. *)
  | Turn_without_tool_claim of Mentat_session.Turn.Id.t
      (** A tool boundary names a turn that opened no tool claim. *)
  | Turn_not_settled of Mentat_session.Turn.Id.t
      (** An after-turn boundary references a turn still without an outcome. *)
  | Unknown_claim of Mentat_session.Tool_claim.Id.t
      (** An event references a tool claim the document does not record. *)
  | Claim_turn_mismatch of {
      claim : Mentat_session.Tool_claim.Id.t;
      expected : Mentat_session.Turn.Id.t;
      actual : Mentat_session.Turn.Id.t;
    }  (** An event's claim belongs to a different turn than it references. *)
  | Claim_not_open of Mentat_session.Tool_claim.Id.t
      (** A live append references a claim that is not the open suspension. *)
  | Recovery_without_ambiguity of Mentat_session.Turn.Id.t
      (** An after-recovery boundary names a turn with no prior ambiguous claim.
      *)
  | Before_tools_after_ambiguity of Mentat_session.Turn.Id.t
      (** A before-tools boundary follows a turn's ambiguous claim. *)

val message : t -> string
(** [message t] is the human-readable diagnostic for [t]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats {!message} output. *)

type mode =
  | History
  | Live
      (** [History] validates structure alone — the referenced turns and claims
          exist and agree — as required to replay a persisted log. [Live]
          additionally requires the referenced claim or tool boundary to be the
          session's currently open suspension, as required of an append to a
          running session. *)

val check :
  mode:mode ->
  Mentat_session.t ->
  Mentat_mutation.Event.t list ->
  (unit, t) result
(** [check ~mode session events] is [Ok ()] iff every event in [events]
    correlates with [session] under [mode], or the first [Error] otherwise. *)
