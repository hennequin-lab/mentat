(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Effectful provider interpreters: the effectful side of the [mentat.llm]
    boundary.

    Clients interpret checked {!Request.t} values, emit {!Event.t} while
    producing a terminal {!Response.t}, and classify failures as {!Error.t}.
    Authentication, transport, retries, protocol translation, and provider
    replay projection belong to provider adapter libraries. A client never hands
    transport ownership to its caller: it consumes provider output before
    returning.

    Construct clients with {!make} and consume them with {!response}. *)

type cancellation = unit -> bool
(** The type for cooperative cancellation polls.

    The callback must be cheap and monotone: once it returns [true], it keeps
    returning [true]. A plain predicate keeps [mentat.llm] free of any runtime,
    fiber, or signal. *)

type accepts = Model.t -> bool
(** The type for model compatibility predicates.

    The predicate is stable for the client's lifetime and is checked before
    {!run}. *)

type run =
  cancelled:cancellation ->
  on_event:(Event.t -> unit) ->
  Request.t ->
  (Response.t, Error.t) result
(** The type for one-request provider interpreters.

    A [run] calls [on_event] for live events in order and returns the terminal
    response, consuming and closing provider output before returning. Because it
    delivers the terminal by returning, no event can follow the terminal — the
    "zero-or-more events then exactly one terminal" law is enforced by
    construction. {!Error.phase} distinguishes setup failures from failures
    while consuming provider output; a [Stream] failure need not have emitted an
    event. An implementation also releases its transport when [on_event] raises:
    the exception propagates to the caller, but no transport is left open. This
    is exactly what a provider adapter implements. *)

type t
(** The type for provider clients. *)

val make : provider:Provider.t -> ?accepts:accepts -> run:run -> unit -> t
(** [make ~provider ?accepts ~run ()] is a provider client backed by [run].

    [accepts] defaults to accepting models whose provider is [provider]. A
    custom predicate may further restrict accepted APIs or model ids. *)

val provider : t -> Provider.t
(** [provider t] is the provider interpreted by [t]. *)

val accepts : t -> Model.t -> bool
(** [accepts t model] is [true] iff [t] can interpret [model]. *)

val response :
  ?cancelled:cancellation ->
  ?on_event:(Event.t -> unit) ->
  t ->
  Request.t ->
  (Response.t, Error.t) result
(** [response ?cancelled ?on_event t request] interprets [request] with [t] and
    returns the terminal provider response.

    [cancelled] defaults to a function returning [false]; [on_event] to a no-op
    that observes no events and does not change the response. Errors distinguish
    startup from post-handoff failures with {!Error.phase}.

    Returns an error with kind {!Error.Invalid_request}, without calling {!run},
    if [Request.model request] is not accepted by [t]. [response] is a pure
    dispatcher: when the model is accepted it forwards to {!run}, which owns
    releasing provider transport before returning — including when [on_event]
    raises, in which case the exception propagates out of [response] with no
    transport left open. *)
