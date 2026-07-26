(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Durable provider-request claims.

    Every model call is claimed before it is issued and settled when it returns.
    The claim commits before the effect starts, so a crash between claim and
    response settles {!Settled.Ambiguous} at recovery. Structurally, every
    assistant message is the settlement of a provider claim:
    {!Settled.Responded} subsumes an appended response and
    {!Settled.Interrupted} subsumes retained interrupted prose. A call that
    finished and did not succeed settles {!Settled.Failed} with the provider
    layer's own diagnostic — proven, not ambiguous. *)

(** {1:ids Identifiers} *)

module Id : sig
  type t
  (** The type for a provider-request identifier. It is content-derived from the
      claim's turn and request digest ({!Started.id}); a caller cannot supply an
      unrelated id.

      Invariant: an identifier's stable textual form is non-empty. *)

  val of_string : string -> t
  (** [of_string s] is [s] as a provider-request id.

      Raises [Invalid_argument] if [s] is empty. *)

  val to_string : t -> string
  (** [to_string id] is [id]'s stable string representation. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same id. *)

  val compare : t -> t -> int
  (** [compare a b] orders ids by their stable string representations,
      compatibly with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an id for diagnostics. The output is not stable storage
      syntax. *)

  val jsont : t Jsont.t
  (** [jsont] maps provider-request ids to JSON strings, validating the
      non-empty invariant of {!of_string} on decode. *)
end

(** {1:started Started claims} *)

module Started : sig
  type t
  (** The type for a durable provider-request claim.

      State replay requires [turn] to be the active unfinished turn with no
      other open suspension. *)

  val make : turn:Turn.Id.t -> request_digest:Mentat_digest.t -> t
  (** [make ~turn ~request_digest] is a claim of the request whose
      {!Mentat_llm.Request.digest} is [request_digest], during [turn]. Its id is
      derived from these fields, so a crash re-drive recomputes the same id and
      a duplicate claim is detected structurally. The request itself is derived
      from the transcript and contract, never stored. *)

  val id : t -> Id.t
  (** [id t] is [t]'s content-derived claim id:
      [H("mentat.session.provider.v1", turn, request_digest)]. *)

  val turn : t -> Turn.Id.t
  (** [turn t] is the turn that owns [t]. *)

  val request_digest : t -> Mentat_digest.t
  (** [request_digest t] is the digest of the claimed request. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same started claim. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats [t] for diagnostics. The output is not stable storage syntax.
  *)

  val jsont : t Jsont.t
  (** [jsont] maps started claims to JSON values (turn and request digest; the
      id is derived). Replay validity is checked by {!State.apply}. *)
end

(** {1:settled Settled claims} *)

module Settled : sig
  (** The type for a provider claim's honest outcome. *)
  type outcome =
    | Responded of Mentat_llm.Response.t
        (** The provider responded; the response is appended to the transcript.
        *)
    | Interrupted of { text : string }
        (** A cooperative cancel retained visible prose, proven quiescent. *)
    | Failed of Mentat_llm.Error.t
        (** A known provider failure (auth rejected, quota, model gone, …): the
            call finished and did not succeed. It carries the provider layer's
            structured diagnostic, appends nothing, and the turn then settles
            [Failed]. Distinct from [Ambiguous] — the outcome is proven, never a
            crash — and from [Interrupted] — the failure is the provider's, not
            the user's intent. *)
    | Ambiguous
        (** A crash occurred between claim and response. Nothing is appended and
            the turn may then only settle interrupted. *)

  type t
  (** The type for a durable provider-request settlement. *)

  val responded : id:Id.t -> Mentat_llm.Response.t -> t
  (** [responded ~id response] settles the claim [id] refers to. [id] is the
      opener's {!Started.id}. *)

  val interrupted : id:Id.t -> text:string -> t
  (** [interrupted ~id ~text] settles claim [id] with retained interrupted
      prose.

      Raises [Invalid_argument] if [text] is empty or whitespace-only. *)

  val failed : id:Id.t -> Mentat_llm.Error.t -> t
  (** [failed ~id error] settles claim [id] as a known provider failure carrying
      [error], the smallest durable diagnostic the LLM/provider layer owns. *)

  val ambiguous : id:Id.t -> t
  (** [ambiguous ~id] settles claim [id] as ambiguous after a crash. *)

  val id : t -> Id.t
  (** [id t] is the claim id [t] settles. *)

  val outcome : t -> outcome
  (** [outcome t] is [t]'s honest outcome. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same settlement. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats [t] for diagnostics. The output is not stable storage syntax.
  *)

  val jsont : t Jsont.t
  (** [jsont] maps settlements to JSON values by a per-arm tag. Replay validity
      is checked by {!State.apply}. *)
end
