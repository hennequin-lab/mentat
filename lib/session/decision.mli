(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The one decision lifecycle.

    A decision is a blocked call awaiting a typed answer: a permission review, a
    reviewer question, or a plan proposal. The {!Request.t} constructor names
    the decision's kind; the client answer arrives kind-erased, so {!resolve}
    checks the answer's tag against the request dynamically and replay re-runs
    the check. {!resolve} is the single construction path for a {!Resolved.t}:
    it validates the answer against the request and the pending call, enforces
    the principal restriction, and derives whether the call proceeds or is
    consumed. First-valid-answer-wins is a replay law: a second resolution for a
    resolved id is rejected. *)

(** {1:ids Identifiers} *)

module Id : sig
  type t
  (** The type for stable decision identifiers. A decision id is content-derived
      from its turn, blocked call, and stage; the constructor minting it lives
      in {!Requested.make}, so a caller cannot supply an unrelated id.

      Invariant: an identifier's stable textual form is non-empty. *)

  val of_string : string -> t
  (** [of_string s] is [s] as a decision id.

      Raises [Invalid_argument] if [s] is empty. *)

  val to_string : t -> string
  (** [to_string id] is [id]'s stable string representation. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same decision id. *)

  val compare : t -> t -> int
  (** [compare a b] orders ids by their stable string representations,
      compatibly with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an id for diagnostics. The output is not stable storage
      syntax. *)

  val jsont : t Jsont.t
  (** [jsont] maps decision ids to JSON strings, validating the non-empty
      invariant of {!of_string} on decode. *)
end

(** {1:requests Requests} *)

module Request : sig
  (** The type for a decision's request payload. The constructor names the
      decision's kind: a permission review, a reviewer question, or a plan
      proposal. The kind is not a static witness — the answer arrives
      kind-erased, so {!resolve} checks the answer's tag against the request
      dynamically and replay re-runs the check. The permission arm stores the
      upstream {!Mentat_permission.Policy.Review.t} directly, which owns its own
      reconstruction and codec. *)
  type t =
    | Permission of Mentat_permission.Policy.Review.t
    | Question of Question.t
    | Plan of Plan.t

  val tag : t -> string
  (** [tag r] is [r]'s stable lowercase tag: ["permission"], ["question"], or
      ["plan"]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same request. *)

  val jsont : t Jsont.t
  (** [jsont] maps a request to JSON, dispatching on {!tag}. *)
end

(** {1:requested Requested decisions} *)

module Requested : sig
  type t
  (** A decision request: its blocked call, stage, and {!Request.t} payload,
      with a content-derived id. *)

  val make :
    turn:Turn.Id.t ->
    call:Mentat_llm.Tool.Call.t ->
    stage:Mentat_tool.Stage.t ->
    Request.t ->
    t
  (** [make ~turn ~call ~stage request] is a decision blocking [call] during
      [turn] at [stage]. Its id is derived from [turn], [call]'s id, and
      [stage], keeping a staged tool's prepare- and run-stage permission
      decisions distinct. *)

  val id : t -> Id.t
  (** [id t] is [t]'s content-derived decision id. *)

  val turn : t -> Turn.Id.t
  (** [turn t] is the turn [t] blocks. *)

  val call : t -> Mentat_llm.Tool.Call.t
  (** [call t] is the exact model tool call [t] blocks, including its input and
      optional signature. *)

  val call_id : t -> string
  (** [call_id t] is the id of {!call}. *)

  val name : t -> string
  (** [name t] is the name of {!call}. *)

  val stage : t -> Mentat_tool.Stage.t
  (** [stage t] is the lifecycle stage [t] belongs to. *)

  val tag : t -> string
  (** [tag t] is [t]'s request tag ({!Request.tag}). *)

  val request : t -> Request.t
  (** [request t] is [t]'s request payload. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same request. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats [t] for diagnostics. The output is not stable storage syntax.
  *)

  val jsont : t Jsont.t
  (** [jsont] maps requests to JSON values, storing the exact blocked call under
      ["call"] and dispatching on the request tag. The former split
      ["call_id"]/["name"] shape is rejected. Replay validity is checked by
      {!State.apply}. *)
end

(** {1:answers Answers} *)

module Answer : sig
  (** The client-submitted answer, its kind erased — the value the protocol
      decodes before the pending request is known. {!resolve} checks it against
      the request. *)
  type t =
    | Permission of {
        answer : Mentat_permission.Answer.t;
        message : string option;
      }
        (** A permission answer and the reviewer's optional free-text guidance.
            [message] is [Some] only for a guided
            {!Mentat_permission.Answer.deny} — the reason appended to the
            model-visible denial result; it is [None] for a bare deny and for
            every allow. The message rides here, at the decision layer, because
            the pure permission answer stays LLM-free. *)
    | Question of Question.Answer.t
    | Plan of Plan.Answer.t

  val tag : t -> string
  (** [tag a] is [a]'s stable lowercase tag, matching {!Request.tag}. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same answer. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an answer for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps answers to JSON values by a per-arm tag. *)
end

(** {1:handling Handling} *)

(** The validated effect of resolving a blocked call. This value is derived by
    {!resolve}; it is not stored in the session document. *)
type handling =
  | Proceed_permission
      (** A permitted call continues from the stage recorded by the request. *)
  | Proceed_plan of Plan.Approval.t
      (** The exact approved plan completes the planning turn and becomes the
          one typed value replay may admit into Build. *)
  | Consume of Mentat_llm.Tool.Result.t
      (** The decision consumes the call with this exact model-visible result.
          Permission denials, question answers, and non-approving plan answers
          use this arm. The derived spellings are a version-pinned part of the
          durable contract, since {!resolve} re-derives them at replay: a bare
          permission deny is the fixed error text
          ["The user denied permission to run this command."]; a guided deny
          appends its reviewer message as
          ["The user denied permission to run this command. They said: " ^ m]; a
          plan keep-planning the fixed text
          ["The plan was not approved; continue planning."]; a plan revise its
          feedback verbatim; and a question answer its free text — or, for a
          choice, the selected option text — verbatim. Changing any spelling
          changes replayed model input and requires a session-document version
          change, as with {!Plan.Approval.to_model_text}. *)

(** {1:resolved Resolutions} *)

module Resolved : sig
  type t
  (** The type for a durable decision resolution: the decision id, the answering
      {!Principal.t}, and the erased answer. It stores no {!handling}; replay
      derives that value again through {!resolve}. *)

  val id : t -> Id.t
  (** [id t] is the decision id [t] resolves. *)

  val answered_by : t -> Principal.t
  (** [answered_by t] is the principal that resolved [t]. *)

  val answer : t -> Answer.t
  (** [answer t] is the erased answer, for projection. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same resolution. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats [t] for diagnostics. The output is not stable storage syntax.
  *)

  val jsont : t Jsont.t
  (** [jsont] maps resolutions to JSON values. An unknown principal fails
      decode. Replay validity is checked by {!State.apply}. *)
end

(** {1:resolving Resolving} *)

module Resolve_error : sig
  (** The type for why a resolution is rejected. *)
  type t =
    | Tag_mismatch of { expected : string; got : string }
        (** The answer's tag does not match the pending request's tag. *)
    | Call_mismatch of {
        expected : string;
        got : string;
        expected_name : string;
        got_name : string;
      }
        (** The supplied call is not the one the request blocks. [expected] and
            [got] are the call ids; [expected_name] and [got_name] the tool
            names. All four may be equal when the unrendered input or signature
            differs; the diagnostic deliberately does not expose either value. A
            same-id name mismatch remains legible through the name fields. *)
    | Invalid_choice of { index : int; count : int }
        (** A choice answer's index is out of range for the question. *)
    | Unattended_not_permitted
        (** An {!Principal.Unattended_policy} answer was not a permission
            denial; an unattended policy may only deny. *)

  val message : t -> string
  (** [message t] is a human-readable diagnostic for [t]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an error for diagnostics. *)
end

val resolve :
  Requested.t ->
  call:Mentat_llm.Tool.Call.t ->
  by:Principal.t ->
  Answer.t ->
  (Resolved.t * handling, Resolve_error.t) result
(** [resolve requested ~call ~by answer] is the single construction path for a
    {!Resolved.t} and its justified {!handling}. It checks that [call] equals
    the exact blocked call (including input and signature), [answer]'s tag
    against [requested], a choice answer's index against the question, and the
    unattended-principal restriction ({!Principal.Unattended_policy} may only
    deny a permission). The resolution stores only the answer; replay re-runs
    these checks and derives the handling again. *)

val matches : Requested.t -> Resolved.t -> bool
(** [matches requested resolved] is [true] iff [resolved] resolves [requested],
    by id. *)
