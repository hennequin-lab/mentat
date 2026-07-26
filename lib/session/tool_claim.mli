(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Durable tool claims — the two-claim staged lifecycle.

    An executable tool call is claimed before it runs and settled when it
    finishes. A staged call yields two claims over one model call: a
    {!Mentat_tool.Stage.Prepare} claim whose {!Settled.Prepared} settlement
    leaves the model call pending, then a {!Mentat_tool.Stage.Run} claim whose
    {!Settled.Returned} settlement consumes it. A plain call yields one
    {!Mentat_tool.Stage.Direct} claim. Each claim has exactly one settlement; a
    claim commits before its effect starts, so a crash settles
    {!Settled.Ambiguous}. The claim blesses a speculative effect: a write tool
    mutates the workspace after its claim commits but before the settlement
    records what it did, so the byte write is speculative and the durable claim,
    never the write, is the ordering point — an apply left unsettled is always
    covered by a committed claim a successor settles ({!Settled.Ambiguous} when
    the process died mid-effect). The stage vocabulary is the shared
    {!Mentat_tool.Stage.t}. *)

(** {1:ids Identifiers} *)

module Id : sig
  type t
  (** The type for a tool-claim identifier. It is content-derived from the
      claim's turn, call id, and stage ({!Started.id}); a caller cannot supply
      an unrelated id.

      Invariant: an identifier's stable textual form is non-empty. *)

  val of_string : string -> t
  (** [of_string s] is [s] as a tool-claim id.

      Raises [Invalid_argument] if [s] is empty. *)

  val to_string : t -> string
  (** [to_string id] is [id]'s stable string representation. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same tool-claim id. *)

  val compare : t -> t -> int
  (** [compare a b] orders ids by their stable string representations,
      compatibly with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an id for diagnostics. The output is not stable storage
      syntax. *)

  val jsont : t Jsont.t
  (** [jsont] maps tool-claim ids to JSON strings, validating the non-empty
      invariant of {!of_string} on decode. *)
end

(** {1:started Started claims} *)

module Started : sig
  type t
  (** The type for a durable started tool claim.

      State replay requires [turn] to be the active unfinished turn with no
      other open suspension, and a {!Mentat_tool.Stage.Run} claim requires its
      {!Mentat_tool.Stage.Prepare} sibling to have settled {!Settled.Prepared}.
  *)

  val make :
    turn:Turn.Id.t ->
    stage:Mentat_tool.Stage.t ->
    call:Mentat_llm.Tool.Call.t ->
    input:Jsont.json ->
    requests:Mentat_permission.Request.t list ->
    t
  (** [make ~turn ~stage ~call ~input ~requests] is a started claim. Its id is
      derived from [turn], {!Mentat_llm.Tool.Call.id} [call], and [stage].
      [call] is the exact model call, including its original input and provider
      signature. [input] is the canonical decoded input
      ({!Mentat_tool.Call.input}); [requests] are this stage's permission
      requests (a run claim carries the final requests derived from the prepared
      payload). *)

  val id : t -> Id.t
  (** [id t] is [t]'s content-derived claim id:
      [H("mentat.session.tool.v1", turn, call_id, stage)]. *)

  val turn : t -> Turn.Id.t
  (** [turn t] is the turn that owns [t]. *)

  val stage : t -> Mentat_tool.Stage.t
  (** [stage t] is [t]'s stage. *)

  val call : t -> Mentat_llm.Tool.Call.t
  (** [call t] is the exact model tool call [t] claims. *)

  val input : t -> Jsont.json
  (** [input t] is the canonical decoded call input. *)

  val requests : t -> Mentat_permission.Request.t list
  (** [requests t] is [t]'s stage permission requests. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same started claim. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats [t] for diagnostics. The output is not stable storage syntax.
  *)

  val jsont : t Jsont.t
  (** [jsont] maps started claims to JSON values (the id is derived). Replay
      validity is checked by {!State.apply}. *)
end

(** {1:settled Settled claims} *)

module Settled : sig
  (** The type for a claim's terminal outcome. *)
  type outcome =
    | Prepared of Mentat_tool.Prepared.t
        (** Terminal for a {!Mentat_tool.Stage.Prepare} claim; the model call
            stays pending. The prepared value owns its final requests. *)
    | Returned of Mentat_tool.Output.t Mentat_tool.Result.t
        (** The erased terminal result. Replay recovers Completed / Failed /
            Interrupted and the output's durable projections verbatim, then
            lowers the model-visible answer from them through
            [Mentat_tool.Result.to_model]. Consumes the model call.

            That lowering is the one deliberate exception to storing no derived
            fact: the model-visible result is {e derived} on demand rather than
            stored a second time, yet the lowering is itself part of the durable
            contract. Changing it changes what replay feeds the model and
            requires a session-document version change, on the same footing as
            the version-pinned ambiguous spelling and
            {!Plan.Approval.to_model_text}. *)
    | Ambiguous
        (** The callback may have run; nothing was recorded. Consumes the call
            with a synthetic ambiguous answer. *)

  type t
  (** The type for a durable tool-claim settlement. *)

  val prepared : id:Id.t -> Mentat_tool.Prepared.t -> t
  (** [prepared ~id payload] settles a prepare claim with [payload]. [id] is the
      opener's {!Started.id}. *)

  val returned : id:Id.t -> Mentat_tool.Output.t Mentat_tool.Result.t -> t
  (** [returned ~id result] settles a claim with the erased terminal [result].
  *)

  val ambiguous : id:Id.t -> t
  (** [ambiguous ~id] settles a claim as ambiguous after a crash. *)

  val id : t -> Id.t
  (** [id t] is the claim id [t] settles. *)

  val outcome : t -> outcome
  (** [outcome t] is [t]'s terminal outcome. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same settlement. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats [t] for diagnostics. The output is not stable storage syntax.
  *)

  val jsont : t Jsont.t
  (** [jsont] maps settlements to JSON values by a per-arm tag. Replay validity
      is checked by {!State.apply}. *)
end
