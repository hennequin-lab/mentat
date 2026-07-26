(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Private interactive authentication orchestration.

    The public runtime retains a [Login] authority facade because this module
    accepts the private {!Runtime.t}. Progress remains UI-agnostic and typed;
    settled save/logout facts use the pure provider vocabulary. *)

module Progress = Mentat_provider.Auth.Login.Progress
(** Canonical ephemeral login progress. *)

type terminal =
  | Saved of Mentat_provider.Account.t
      (** The provider minted a credential and durable settling completed. *)
  | Cancelled
      (** Explicit cancellation won before a local credential existed. *)

val run :
  Runtime.t ->
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  provider:Mentat_llm.Provider.t ->
  method_id:Mentat_provider.Auth.Login.Id.t ->
  ?name:Mentat_provider.Credential.Name.t ->
  ?base_url:string ->
  ?auth_base_url:string ->
  ?cancel:unit Eio.Promise.t ->
  progress:(Progress.t -> unit) ->
  unit ->
  (terminal, Error.t) result
(** [run t …] interprets one declared interactive protocol and emits typed
    progress. It returns [Saved] after the exact persisted credential has been
    checked, or [Cancelled] only when cancellation wins first. Dispatch,
    protocol, store, and applicable base-URL failures are structured {!Error.t}
    values. OAuth expiry timestamps use the runtime clock at the protocol effect
    that produces them. [name] defaults to
    [Mentat_provider.Credential.Name.default]. *)

val save_api_key :
  Runtime.t ->
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  provider:Mentat_llm.Provider.t ->
  ?name:Mentat_provider.Credential.Name.t ->
  ?base_url:string ->
  key:string ->
  unit ->
  (Mentat_provider.Account.t, Error.t) result
(** [save_api_key t … ~key ()] validates the provider, API-key capability, key
    text, and applicable API base URL before mutation. It observes the candidate
    under the provider/name lock, then cancellation-protects the sole durable
    edit. Empty or invalid UTF-8 key text is a structured [Error]. *)

val logout :
  Runtime.t ->
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  provider:Mentat_llm.Provider.t ->
  ?name:Mentat_provider.Credential.Name.t ->
  ?revoke:bool ->
  ?auth_base_url:string ->
  ?process:Mentat_provider.Credential.t list ->
  environment:(string * string) list ->
  unit ->
  (Mentat_provider.Account.Logout.t, Error.t) result
(** [logout t …] validates the provider before mutation and returns discovery
    derived from the exact post-settlement store snapshot plus the supplied
    process/environment snapshots. Revocation conditionally removes only the
    observed secret. [name] defaults to
    [Mentat_provider.Credential.Name.default], [revoke] to [false], and
    [process] to [[]]. Cancellation during remote revocation can leave provider
    state unknown; only a returned value attests local settlement. *)
