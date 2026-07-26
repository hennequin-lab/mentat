(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type observation = {
  problems : Mentat_provider.Account.Problem.t list;
  profile : Mentat_provider.Account.Profile.t option;
  org : Mentat_provider.Account.Org.t option;
  models : string list option;
}

type device_flow =
  http:Oauth_flow.Http.t ->
  sw:Eio.Switch.t ->
  now:int64 ->
  auth_base_url:string option ->
  (Oauth_flow.Device_code.t, Oauth_flow.Error.t) result

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

type registration = { declaration : Mentat_provider.t; driver : driver }
