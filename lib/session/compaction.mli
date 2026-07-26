(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Durable model-replay compactions.

    A compaction is a summary fact that establishes a context boundary. It
    stores only genuinely-new bytes — the [summary_messages] — and a count
    [summarized_upto] of full-transcript messages the summary replaces. Replay
    keeps the whole transcript and {e derives} the model view: the summary
    followed by the retained tail [full[summarized_upto..]]. Two views, one
    journal, nothing deleted. *)

module Reason : sig
  (** Durable reasons for installing a compaction. *)

  type t =
    | User_requested  (** The user explicitly requested compaction. *)
    | Context_pressure
        (** The host compacted before a model request because the projected
            request was near the context limit. *)
    | Context_overflow
        (** The host compacted after a provider reported context overflow. *)
    | Model_downshift
        (** The host compacted because the next request uses a model with a
            smaller context window. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same reason. *)

  val to_string : t -> string
  (** [to_string r] is [r]'s stable lowercase tag: ["user_requested"],
      ["context_pressure"], ["context_overflow"], or ["model_downshift"]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a reason for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps reasons to their stable tags, rejecting unknown tags. *)
end

module Context_tokens : sig
  type t
  (** The type for projected replay context size around a compaction. Both
      counts are non-negative estimates.

      Owed consumer: the protocol's compaction fact rendering, which surfaces a
      "compacted ~N → ~M tokens" line. [before] is populated at install from the
      pre-install replay usage; [after] is reserved for a future context
      projection over the retained model view. *)

  val make : ?before:int -> ?after:int -> unit -> t
  (** [make ?before ?after ()] is context-size metadata: projected replay input
      tokens before and after installing the compaction.

      Raises [Invalid_argument] if no count is present, or if any present count
      is negative. *)

  val before : t -> int option
  (** [before t] is the projected replay input token count before compaction, if
      known. *)

  val after : t -> int option
  (** [after t] is the projected replay input token count after compaction, if
      known. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same estimate. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats context estimates for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps context estimates to JSON values, validating the same
      non-empty and non-negative invariant as {!make}. *)
end

type t
(** The type for a durable compaction.

    The fact is also the honest-success closer of the summary call's own
    provider claim: the summarizer request is billable, so it is claimed like
    any other provider call, and installing the compaction settles that claim.
    "Summarized but not installed" is unrepresentable — a crash mid-summary
    leaves an open provider claim that ordinary recovery settles ambiguous.

    Invariant: [summarized_upto] is non-negative and [summary_messages] forms a
    request-ready transcript. Installing the compaction through {!State.apply}
    additionally requires [provider_claim] to name the currently open provider
    suspension (and clears it), [summarized_upto] to be within the full
    transcript, the derived model view to be request-ready, and the
    [(request_digest, reason)] key to be unique. *)

val make :
  reason:Reason.t ->
  provider_claim:Provider_request.Id.t ->
  request_digest:Mentat_digest.t ->
  summary_messages:Mentat_llm.Message.t list ->
  summarized_upto:int ->
  ?usage:Mentat_llm.Usage.t ->
  ?context:Context_tokens.t ->
  unit ->
  t
(** [make ~reason ~provider_claim ~request_digest ~summary_messages
     ~summarized_upto ?usage ?context ()] is a compaction.

    [provider_claim] is the summary call's provider claim this fact closes.
    [request_digest] is the summarization request digest, the first half of the
    [(request_digest, reason)] idempotency key. [summary_messages] are the new,
    request-ready summary bytes. [summarized_upto] is the count of
    full-transcript messages the summary replaces. [usage] is the summarizer
    response's provider-reported usage, which {!Mentat_session.metrics} folds
    into the session total. [context] is the optional projected context size.

    Raises [Invalid_argument] if [summarized_upto] is negative or if
    [summary_messages] is not a request-ready transcript. *)

val reason : t -> Reason.t
(** [reason t] is the reason [t] was installed. *)

val provider_claim : t -> Provider_request.Id.t
(** [provider_claim t] is the summary call's provider claim [t] closes. *)

val request_digest : t -> Mentat_digest.t
(** [request_digest t] is [t]'s summarization request digest. *)

val summary_messages : t -> Mentat_llm.Message.t list
(** [summary_messages t] is [t]'s request-ready summary prefix. *)

val summarized_upto : t -> int
(** [summarized_upto t] is the count of full-transcript messages [t] replaces.
*)

val usage : t -> Mentat_llm.Usage.t option
(** [usage t] is the summarizer response's provider-reported usage, if known. *)

val context : t -> Context_tokens.t option
(** [context t] is [t]'s projected context-size metadata, if known. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same compaction. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a compaction for diagnostics. The output is not stable storage
    syntax. *)

val jsont : t Jsont.t
(** [jsont] maps compactions to JSON values, validating the same summary and
    boundary invariants as {!make} on decode. *)
