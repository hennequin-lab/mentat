(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Decision-request dialogs.

    A dialog presents one exact {!Mentat_session.Decision.Requested.t} through
    the permission, plan, or reviewer-question form selected by its durable
    request tag. Fold keyboard input not consumed by a child Mosaic widget with
    {!key}, fold child messages with {!update}, and render the current form with
    {!view}.

    A resolving key returns a typed {!Mentat_session.Decision.Answer.t}. The
    client/session boundary validates that answer against {!requested}; this
    module neither calls {!Mentat_session.Decision.resolve} nor synthesizes
    transcript echoes. *)

(** {1:types Types} *)

type t
(** The type for immutable decision-dialog state.

    The state retains the exact requested decision for id, blocked-call, and
    lifecycle-stage routing. Form edits never reconstruct or replace that owner
    value. *)

(** The result of folding one key. *)
type outcome =
  | Stay
      (** The dialog remains open. The returned state may contain updated form
          selection, disclosure, editor, or validation state. *)
  | Resolve of Mentat_session.Decision.Answer.t
      (** The form resolved to an answer carrying the same durable tag as the
          request. The caller submits it through the client decision path and
          keeps durable result rendering fact-owned. *)
  | Flash of string
      (** The key was rejected with the given user-facing validation message.
          The message is already present in the returned form's view. *)

type msg
(** The type for exact child-form messages emitted by {!view}. *)

(** {1:constructors Constructors} *)

val make : Mentat_session.Decision.Requested.t -> t
(** [make requested] is a dialog for every request arm:

    - permission requests use {!Permission_dialog} over the exact
      {!Mentat_permission.Policy.Review.t};
    - plan requests use {!Plan_dialog};
    - reviewer questions use {!Question_dialog}.

    No legacy pending-boundary, owner, host-tool fallback, or undecodable
    question shape is accepted. *)

(** {1:queries Queries} *)

val requested : t -> Mentat_session.Decision.Requested.t
(** [requested t] is the exact owner value passed to {!make}. Its id, blocked
    call, and lifecycle stage identify the decision submitted by the caller. *)

(** {1:input Input} *)

val key : Matrix.Input.Key.event -> t -> t * outcome
(** [key event t] folds [event] into the active form. A form answer is packed
    into the matching {!Mentat_session.Decision.Answer.t} constructor. Question
    validation failures are returned as {!Flash}; a permission denial may carry
    an optional free-text guidance message, but has no local failure text. *)

val update : msg -> t -> t * outcome
(** [update message t] folds a message emitted by the active child form into
    [t]. A stale message for a different form is ignored with {!Stay}. Answers
    retain the durable permission, plan, or reviewer-question tag owned by [t].
*)

val editing : t -> bool
(** [editing t] is [true] iff a focused reviewer-question, plan, or permission
    guidance editor currently owns input. *)

(** {1:views View} *)

val view : palette:Theme.Palette.t -> t -> msg Mosaic.t
(** [view t] is the complete geometry-free panel produced by the active form.
    Mosaic owns width allocation, wrapping, truncation, and selection
    visibility. Child messages are tagged with their exact form before they
    cross this module boundary. *)
