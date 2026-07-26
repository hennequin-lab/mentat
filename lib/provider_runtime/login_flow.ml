(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

module Provider = Mentat_provider
module Auth = Provider.Auth
module Login = Auth.Login
module Protocol = Login.Protocol
module Credential = Provider.Credential
module Account = Provider.Account
module Store = Credential.Store
module Flow = Oauth_flow
module Progress = Login.Progress

type terminal = Saved of Account.t | Cancelled

let callback_timeout_s = 300.0
let clock env = Eio.Stdenv.clock env
let timestamp env = Eio.Time.now (clock env) |> Float.floor |> Int64.of_float

let diagnostic ~hints text =
  Mentat_diagnostic.of_text_or ~fallback:"authentication failed" ~hints text

let login_error ?(headless = false) ~provider message =
  let hints =
    if headless then
      [ "on a remote or headless machine, use a device-code login" ]
    else []
  in
  Error.Login { provider; diagnostic = diagnostic ~hints message }

let flow_error ~provider error =
  let headless =
    match error with
    | Flow.Error.Timeout _ | Flow.Error.Network _ -> true
    | Flow.Error.Invalid_request _ | Flow.Error.Protocol _
    | Flow.Error.Rejected _ | Flow.Error.Not_refreshable ->
        false
  in
  login_error ~headless ~provider (Flow.Error.message error)

let invalid_base_url ~provider value =
  let invalid message = Error (Error.Invalid_base_url { provider; message }) in
  match Uri.of_string value with
  | exception Invalid_argument _ -> invalid "could not parse URL"
  | uri -> (
      match (Uri.scheme uri, Uri.host uri) with
      | Some ("http" | "https"), Some host when not (String.is_empty host) ->
          Ok ()
      | _ -> invalid "expected an absolute http(s) URL")

let validate_check_base_url registration ~provider = function
  | Some value when Option.is_some registration.Driver.driver.Driver.check ->
      invalid_base_url ~provider value
  | None | Some _ -> Ok ()

(* Endpoint rerooting.

   A [MENTAT_<PROVIDER>_AUTH_BASE_URL] override reroots every protocol endpoint
   onto the override's scheme, host, and port. *)

let reroot_uri ~root uri =
  Uri.with_scheme uri (Uri.scheme root) |> fun uri ->
  Uri.with_host uri (Uri.host root) |> fun uri ->
  Uri.with_port uri (Uri.port root)

let reroot_protocol ~root = function
  | Protocol.Api_key -> Protocol.Api_key
  | Protocol.OAuth2_device_code spec ->
      Protocol.OAuth2_device_code
        {
          spec with
          device_endpoint = reroot_uri ~root spec.device_endpoint;
          token_endpoint = reroot_uri ~root spec.token_endpoint;
        }
  | Protocol.OAuth2_authorization_code spec ->
      Protocol.OAuth2_authorization_code
        {
          spec with
          authorization_endpoint = reroot_uri ~root spec.authorization_endpoint;
          token_endpoint = reroot_uri ~root spec.token_endpoint;
        }
  | Protocol.Provider_defined _ as protocol -> protocol
  | Protocol.External _ as protocol -> protocol

let reroot protocol = function
  | None -> protocol
  | Some root -> reroot_protocol ~root:(Uri.of_string root) protocol

(* Settling. *)

let settle registration t ~sw ~env ?base_url ~provider ~name secret =
  let store = Runtime.store t in
  Credential_store.with_credential_lock store ~env ~provider ~name (fun () ->
      let credential =
        Credential.make ~provider
          ~source:(Credential.Source.store ~name ())
          secret
      in
      let account =
        Account_ops.check registration ~sw ~env ?base_url credential
      in
      let* () =
        Eio.Cancel.protect (fun () ->
            Credential_store.edit store ~env ~f:(fun snapshot ->
                let committed = Store.set ~provider ~name secret snapshot in
                (committed, ())))
      in
      Ok account)
  |> Result.map_error (fun error -> Error.Store error)

let save_api_key t ~sw ~env ~provider ?(name = Credential.Name.default)
    ?base_url ~key () =
  match Runtime.registration_for t provider with
  | None -> Error (Error.Unknown_provider provider)
  | Some registration ->
      let provider_kinds =
        registration.Driver.declaration |> Provider.auth |> Auth.accepted_kinds
      in
      let kind = Credential.Kind.Api_key in
      if not (List.exists (Credential.Kind.equal kind) provider_kinds) then
        Error
          (Error.Credential
             {
               provider;
               error =
                 Provider.Credential_error.Unsupported_credential_kind
                   {
                     source = Credential.Source.store ~name ();
                     kind;
                     accepted = provider_kinds;
                   };
             })
      else begin
        let* secret =
          match Credential.Secret.api_key key with
          | secret -> Ok secret
          | exception Invalid_argument _ ->
              Error
                (login_error ~provider
                   "API key must be non-empty valid UTF-8 text")
        in
        let* () = validate_check_base_url registration ~provider base_url in
        settle registration t ~sw ~env ?base_url ~provider ~name secret
      end

(* [race ?cancel body] runs [body], preempted by [cancel]: the first to settle
   wins, and a resolved [cancel] returns [`Cancelled] immediately. *)
let race ?cancel body =
  match cancel with
  | None -> body ()
  | Some cancel ->
      Eio.Fiber.first
        (fun () ->
          Eio.Promise.await cancel;
          `Cancelled)
        body

(* Browser flow. *)

let browser registration t ~sw ~env ~provider ~name ?base_url ?cancel ~progress
    ~client ~authorization_endpoint ~token_endpoint ~redirect_uri ~scope ~extra
    ~pkce () =
  Mirage_crypto_rng_unix.use_default ();
  let random n = Mirage_crypto_rng.generate n in
  match
    Flow.Authorization_code.start ~random ~client ~authorization_endpoint
      ~token_endpoint ~redirect_uri ~scope ~extra ~pkce
  with
  | Error error -> Error (flow_error ~provider error)
  | Ok started -> (
      let authorization_uri =
        Flow.Authorization_code.authorization_uri started
      in
      let redirect_uri = Flow.Authorization_code.redirect_uri started in
      progress (Progress.Browser_url authorization_uri);
      let provider_label =
        match Provider.display_name registration.Driver.declaration with
        | Some name -> name
        | None -> Mentat_llm.Provider.id provider
      in
      let acquired =
        race ?cancel (fun () ->
            let on_ready () = progress (Progress.Listening { redirect_uri }) in
            let accept = Flow.Authorization_code.accepts_callback started in
            match
              Flow.Local_callback.await_once ~stdenv:env
                ~provider:provider_label ~on_ready ~accept ~redirect_uri
                ~timeout_s:callback_timeout_s ()
            with
            | Error error -> `Failed error
            | Ok callback -> (
                match Flow.Http.tls_client ~stdenv:env with
                | Error error -> `Failed error
                | Ok http -> (
                    match
                      Flow.Authorization_code.complete_secret ~http ~sw started
                        ~callback
                        ~now:(fun () -> timestamp env)
                        ~profile:
                          (if
                             Mentat_llm.Provider.equal provider
                               Mentat_llm_openai.provider
                           then Flow.Authorization_code.Openai_chatgpt
                           else Flow.Authorization_code.Generic)
                    with
                    | Error error -> `Failed error
                    | Ok secret -> `Authorized secret)))
      in
      match acquired with
      | `Cancelled -> Ok Cancelled
      | `Failed error -> Error (flow_error ~provider error)
      | `Authorized secret ->
          let* saved =
            settle registration t ~sw ~env ?base_url ~provider ~name secret
          in
          Ok (Saved saved))

(* Device flows. *)

let drive_device registration t ~sw ~env ~provider ~name ?base_url ?cancel
    ~progress ~http started =
  let challenge = Flow.Device_code.challenge started in
  progress
    (Progress.Device_challenge
       {
         url = challenge.Flow.Device_code.verification_uri;
         user_code = challenge.Flow.Device_code.user_code;
         expires_in = max 0 (Flow.Device_code.expires_in started);
       });
  (* Settling runs after the cancel race. Once the provider has minted a secret,
     this function never reports [Cancelled] after persisting it. *)
  let polled =
    race ?cancel (fun () ->
        let rec poll authorization =
          let delay =
            Flow.Device_code.next_poll_delay_s ~now:(timestamp env)
              authorization
          in
          if delay > 0 then Eio.Time.sleep (clock env) (Float.of_int delay);
          match
            Flow.Device_code.poll ~http ~sw ~now:(timestamp env) authorization
          with
          | Error error -> `Failed error
          | Ok (Flow.Device_code.Authorized secret) -> `Authorized secret
          | Ok (Flow.Device_code.Pending authorization) -> poll authorization
          | Ok Flow.Device_code.Expired -> `Expired
        in
        poll started)
  in
  match polled with
  | `Cancelled -> Ok Cancelled
  | `Failed error -> Error (flow_error ~provider error)
  | `Expired ->
      Error (login_error ~provider "device code expired — run the login again")
  | `Authorized secret ->
      let* saved =
        settle registration t ~sw ~env ?base_url ~provider ~name secret
      in
      Ok (Saved saved)

let run_device registration t ~env ~provider ~name ?base_url ?cancel ~progress
    ~start () =
  Eio.Switch.run @@ fun sw ->
  let* http =
    Flow.Http.tls_client ~stdenv:env |> Result.map_error (flow_error ~provider)
  in
  let* started =
    start ~http ~sw ~now:(timestamp env)
    |> Result.map_error (flow_error ~provider)
  in
  drive_device registration t ~sw ~env ~provider ~name ?base_url ?cancel
    ~progress ~http started

(* Run. *)

let run t ~sw ~env ~provider ~method_id ?(name = Credential.Name.default)
    ?base_url ?auth_base_url ?cancel ~progress () =
  match Runtime.registration_for t provider with
  | None -> Error (Error.Unknown_provider provider)
  | Some registration -> (
      match
        Auth.login_by_id
          (Provider.auth registration.Driver.declaration)
          method_id
      with
      | None ->
          Error
            (login_error ~provider
               ("unknown auth method: " ^ Login.Id.to_string method_id))
      | Some login -> (
          let protocol = Login.protocol login in
          match protocol with
          | Protocol.Api_key ->
              Error
                (login_error ~provider
                   "this provider's API-key login is entered directly, not run")
          | Protocol.External _ ->
              Error
                (login_error ~provider
                   "this provider's login is completed outside Mentat")
          | Protocol.OAuth2_authorization_code _ | Protocol.OAuth2_device_code _
          | Protocol.Provider_defined _ -> (
              let* () =
                validate_check_base_url registration ~provider base_url
              in
              let* () =
                match auth_base_url with
                | None -> Ok ()
                | Some value -> invalid_base_url ~provider value
              in
              match reroot protocol auth_base_url with
              | Protocol.OAuth2_authorization_code
                  {
                    client;
                    authorization_endpoint;
                    token_endpoint;
                    redirect_uri;
                    scope;
                    extra;
                    pkce;
                  } ->
                  browser registration t ~sw ~env ~provider ~name ?base_url
                    ?cancel ~progress ~client ~authorization_endpoint
                    ~token_endpoint ~redirect_uri ~scope ~extra ~pkce ()
              | Protocol.OAuth2_device_code
                  { client; device_endpoint; token_endpoint; scope; extra } ->
                  run_device registration t ~env ~provider ~name ?base_url
                    ?cancel ~progress
                    ~start:(fun ~http ~sw ~now ->
                      Flow.Device_code.start_oauth2 ~http ~sw ~now ~client
                        ~device_endpoint ~token_endpoint ~scope ~extra)
                    ()
              | Protocol.Provider_defined { protocol = protocol_id; _ } ->
                  let device_flow =
                    match
                      registration.Driver.driver.Driver.provider_defined
                        protocol_id
                    with
                    | Some device_flow -> device_flow
                    | None -> assert false
                  in
                  run_device registration t ~env ~provider ~name ?base_url
                    ?cancel ~progress
                    ~start:(fun ~http ~sw ~now ->
                      device_flow ~http ~sw ~now ~auth_base_url)
                    ()
              | Protocol.Api_key | Protocol.External _ -> assert false)))

let logout = Account_ops.logout
