(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Plan proposals and their reviewer answers.

    A plan is the payload of a {!Decision} of kind [Plan]: a proposed body with
    an optional title. Plans have no ids, no status, and no supersession — plan
    history is journal order, and the latest proposal is current. A reviewer
    replies with a {!Answer.t}: keep planning, revise, or approve. *)

module Error : sig
  (** Plan construction errors. *)
  type t = Empty_body | Empty_title

  val message : t -> string
  (** [message e] is a human-readable diagnostic. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an error for diagnostics. *)
end

type t = private { title : string option; body : string }
(** The type for a plan proposal. Invariant: [body] is non-empty; [title], when
    present, is non-empty. *)

val make : ?title:string -> body:string -> unit -> (t, Error.t) result
(** [make ?title ~body ()] is a plan proposal, or a structured {!Error.t} if
    [body] is empty or [title] is present and empty. *)

val title : t -> string option
(** [title t] is [t]'s optional title. *)

val body : t -> string
(** [body t] is [t]'s non-empty proposal body. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same plan. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a plan for diagnostics. *)

val jsont : t Jsont.t
(** [jsont] maps plans to JSON values, validating the same invariants as {!make}
    on decode. *)

(** {1:answers Answers} *)

module Approval : sig
  type plan = t
  (** [plan] is the enclosing {!Plan.t}, the exact approved plan body. *)

  type t
  (** The type for an approved plan and its Build-context choice.

      This is the single durable value carried from the review answer through
      decision handling, replay, and Build admission. [feedback], when present,
      is non-empty. *)

  val body : t -> plan
  (** [body t] is the exact, possibly reviewer-edited plan that was approved. *)

  val context : t -> [ `Current | `Fresh ]
  (** [context t] is [`Current] when Build keeps the planning model transcript,
      and [`Fresh] when Build starts from a new model-context boundary. *)

  val feedback : t -> string option
  (** [feedback t] is the reviewer's optional non-empty implementation guidance.
  *)

  val to_model_text : t -> string
  (** [to_model_text t] is the canonical non-empty model-visible approval: [t]'s
      exact body, optional title, and optional implementation feedback. It is
      used both to settle the planning tool call and to seed a fresh Build
      context. Changing this projection changes replayed model input and
      requires a session-document version change. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] approve the same plan, context, and
      feedback. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an approval for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps approvals to JSON values, validating the non-empty feedback
      invariant on decode. *)
end

module Answer : sig
  type plan = t
  (** [plan] is the enclosing {!Plan.t}, the body an approval carries. *)

  (** The type for a reviewer's reply to a plan proposal. *)
  type t = private
    | Keep_planning  (** Keep planning; the proposal stands. *)
    | Revise of string  (** Send the plan back with non-empty feedback. *)
    | Approve of Approval.t
        (** Approve, admitting a Build turn. {!Approval.body} is the exact
            (possibly edited) plan the Build turn carries, durable so its input
            replays. {!Approval.context} chooses whether the Build turn
            continues the current transcript or starts fresh;
            {!Approval.feedback} is optional implementation guidance. *)

  val keep_planning : t
  (** [keep_planning] is {!Keep_planning}. *)

  val revise : string -> t
  (** [revise feedback] sends the plan back.

      Raises [Invalid_argument] if [feedback] is empty. *)

  val approve :
    body:plan -> context:[ `Current | `Fresh ] -> ?feedback:string -> unit -> t
  (** [approve ~body ~context ?feedback ()] approves [body].

      Raises [Invalid_argument] if [feedback] is present and empty. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same answer. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an answer for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps answers to JSON values by a per-arm tag, validating local
      invariants on decode. *)
end
