(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The private registration record: a pure declaration paired, exactly once,
    with the Eio-aware driver that serves it.

    One ordinary product type unifies the account operations
    [{build_client, check, refresh, revoke}] with the login's provider-defined
    handlers and the local-model artifact capability. Every built-in provider is
    one {!registration}; [declarations] and every driver operation derive from
    the single list, so catalog and driver cannot disagree in any constructed
    runtime. *)

type observation = {
  problems : Mentat_provider.Account.Problem.t list;
  profile : Mentat_provider.Account.Profile.t option;
  org : Mentat_provider.Account.Org.t option;
  models : string list option;
}
(** A live provider observation, lowered by [Account.check] into a [checked]
    account fact. *)

type device_flow =
  http:Oauth_flow.Http.t ->
  sw:Eio.Switch.t ->
  now:int64 ->
  auth_base_url:string option ->
  (Oauth_flow.Device_code.t, Oauth_flow.Error.t) result
(** A provider-defined device-authorization flow (OpenAI ChatGPT's), keyed by
    its login-protocol id. The function requests the initial device state; the
    shared poll loop drives it thereafter. *)

type artifact_driver = {
  status : Mentat_llm.Model.t -> Artifact.Status.t option;
  prepare :
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    cancelled:(unit -> bool) ->
    observe:(Artifact.Progress.t -> unit) ->
    Mentat_llm.Model.t ->
    (unit, Mentat_llm.Error.t) result;
  download :
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    force:bool ->
    observe:(Artifact.Progress.t -> unit) ->
    Mentat_llm.Model.t ->
    Artifact.Download_outcome.t;
}
(** A local-model artifact capability: passive status, transparent first-use
    prepare, and the explicit force-able download. *)

type driver = {
  build_client :
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    ?base_url:string ->
    Mentat_provider.Credential.t option ->
    (Mentat_llm.Client.t, Error.t) result;
  check :
    (sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    ?base_url:string ->
    Mentat_provider.Credential.t ->
    observation)
    option;
  refresh :
    (sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    now:int64 ->
    ?auth_base_url:string ->
    Mentat_provider.Credential.Secret.t ->
    ( Mentat_provider.Credential.Secret.t,
      Mentat_provider.Account.Problem.t )
    result)
    option;
  revoke :
    (sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    ?auth_base_url:string ->
    Mentat_provider.Credential.Secret.t ->
    (unit, Mentat_provider.Account.Problem.t) result)
    option;
  provider_defined : Mentat_provider.Auth.Login.Id.t -> device_flow option;
  artifact : artifact_driver option;
}
(** The effectful side of one provider. [check]/[refresh]/[revoke] are absent
    for providers that do not observe or refresh. A check lowers every ordinary
    provider or transport outcome into an {!observation}; fiber cancellation and
    implementation faults escape. [provider_defined] is empty except for
    OpenAI's ChatGPT device flow; [artifact] is present only for local
    providers. *)

type registration = { declaration : Mentat_provider.t; driver : driver }
(** A pure declaration paired with its driver. *)
