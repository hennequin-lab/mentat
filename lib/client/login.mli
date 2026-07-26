(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** One interactive login's typed progress and settlement.

    The interactive browser and device login flows are the single flows whose
    progress is not carried on any session feed, so they get their own pull
    handle. The frontend pulls {!next} for each step and opens any yielded url
    itself (a frontend-local OS spawn, never a client call). {!cancel} is
    idempotent. The non-interactive api-key method is not a login: it is the
    direct {!Mentat_client.save_api_key} save, because a secret cannot ride a
    pull-only {!step}. *)

(** The type for one login step. Progress and settlement reuse their canonical
    provider-owned values; the client introduces no copied progress or phase
    vocabulary. *)
type step =
  | Progress of Mentat_provider.Auth.Login.Progress.t
      (** Ephemeral provider-owned progress for the live frontend. Values may
          contain authorization capabilities and must not be logged, transcribed
          into a session, or stored. *)
  | Saved of Mentat_provider.Account.t
      (** The interactive flow durably saved a credential and returned its exact
          credential-free account observation. *)
  | Cancelled
      (** Explicit cancellation won before a local credential existed. *)

type steps = {
  next : unit -> (step, Mentat_protocol.Error.t) result;
      (** Pull the next step, blocking the caller's fiber cancellably. *)
  cancel : unit -> unit;  (** Idempotent cancel of the underlying flow. *)
}
(** The raw step-pull the construction driver's [Accounts.login] responder
    returns. The executable cannot mint the client-abstract {!t} (no public
    constructor), so it returns this record and {!attach} boxes it. *)

type t
(** The type for one live interactive login. *)

val attach : sw:Eio.Switch.t -> steps -> t
(** [attach ~sw steps] boxes [steps] into a login handle and registers
    [Eio.Switch.on_release sw] so releasing [sw] cancels it. Construction glue;
    {!Mentat_client.login} is the frontend entry point. *)

val next : t -> (step, Mentat_protocol.Error.t) result
(** [next t] pulls the next login step, blocking cancellably. Progress may be
    pulled repeatedly. The first [Saved], [Cancelled], or [Error _] result is
    terminal: every later call returns that exact result without pulling the
    underlying producer again. If the producer raises, cancellation is
    re-raised; any other exception becomes a terminal operation-labelled
    {!Mentat_protocol.Error.Unavailable} without exposing exception text. *)

val cancel : t -> unit
(** [cancel t] cancels the login. Idempotent: the underlying flow's cancel runs
    at most once. *)
