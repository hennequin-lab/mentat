(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

module Provider = Mentat_provider
module Account = Provider.Account
module Credential = Provider.Credential
module Secret = Credential.Secret
module Name = Credential.Name
module Store = Credential.Store
module Problem = Account.Problem

let timestamp env =
  Eio.Time.now (Eio.Stdenv.clock env) |> Float.floor |> Int64.of_float

let discovery_of_resolution ~provider = function
  | Ok (Some credential) -> Account.Discovery.Known (Account.present credential)
  | Ok None -> Account.Discovery.Known (Account.missing ~provider)
  | Error error -> Account.Discovery.Resolution_failed { provider; error }

let discover_declaration declaration ~process ~environment ~store ?name () =
  let provider = Provider.id declaration in
  discovery_of_resolution ~provider
    (Provider.resolve_credential declaration ~process ~environment ~store ?name
       ())

let load t ?(process = []) ~environment () =
  match Credential_store.load (Runtime.store t) with
  | Error store_error -> Error store_error
  | Ok store ->
      Ok
        (List.map
           (fun declaration ->
             discover_declaration declaration ~process ~environment ~store ())
           (Provider.Catalog.declarations (Runtime.catalog t)))

let credential_fingerprint credential =
  Option.bind credential Credential.fingerprint

let check t registration ~sw ~env ?base_url ?auth_base_url credential =
  match registration.Driver.driver.Driver.check with
  | None -> Account.present credential
  | Some check_route ->
      let { Driver.problems; profile; org; listing } =
        check_route ~sw ~env ?base_url ?auth_base_url (Some credential)
      in
      Runtime.remember_listing t
        ~provider:(Credential.provider credential)
        ~fingerprint:(credential_fingerprint (Some credential))
        ~base_url ~auth_base_url listing;
      Account.checked credential ~at:(timestamp env) ?profile ?org ~problems
        ?models:(Option.map Provider.Listing.ids listing) ()

let selected_declarations t providers =
  Provider.Catalog.declarations (Runtime.catalog t)
  |> List.filter (fun declaration ->
      match providers with
      | None -> true
      | Some providers ->
          List.exists
            (Mentat_llm.Provider.equal (Provider.id declaration))
            providers)

(* One refresh job per checkable provider whose credential resolves — [None]
   for a required-auth provider with no candidate (skipped, not failed) and
   for a provider whose resolution errors (its account row already carries the
   failure). *)
let listing_job t ~sw ~env ~base_url ~auth_base_url ~process ~environment
    ~store declaration =
  let ( let*? ) = Option.bind in
  let provider = Provider.id declaration in
  let*? registration = Runtime.registration_for t provider in
  let*? check_route = registration.Driver.driver.Driver.check in
  let*? credential =
    match
      Provider.resolve_credential declaration ~process ~environment ~store ()
    with
    | Error _ -> None
    | Ok None when Provider.Auth.required (Provider.auth declaration) -> None
    | Ok credential -> Some credential
  in
  let base_url = base_url provider in
  let auth_base_url = auth_base_url provider in
  Some
    (fun () ->
      let observation =
        check_route ~sw ~env ?base_url ?auth_base_url credential
      in
      Runtime.remember_listing t ~provider
        ~fingerprint:(credential_fingerprint credential)
        ~base_url ~auth_base_url observation.Driver.listing)

let refresh_listings t ~sw ~env ?providers ?(base_url = fun _ -> None)
    ?(auth_base_url = fun _ -> None) ?(process = []) ~environment () =
  match Credential_store.load (Runtime.store t) with
  | Error store_error -> Error store_error
  | Ok store ->
      selected_declarations t providers
      |> List.filter_map
           (listing_job t ~sw ~env ~base_url ~auth_base_url ~process
              ~environment ~store)
      |> Eio.Fiber.all;
      Ok ()

let listings t ?providers ?(process = []) ?(base_url = fun _ -> None)
    ?(auth_base_url = fun _ -> None) ~environment () =
  match Credential_store.load (Runtime.store t) with
  | Error store_error -> Error store_error
  | Ok store ->
      Ok
        (selected_declarations t providers
        |> List.filter_map (fun declaration ->
            let provider = Provider.id declaration in
            match
              Provider.resolve_credential declaration ~process ~environment
                ~store ()
            with
            | Error _ -> None
            | Ok credential ->
                Runtime.listing_for t ~provider
                  ~fingerprint:(credential_fingerprint credential)
                  ~base_url:(base_url provider)
                  ~auth_base_url:(auth_base_url provider)
                |> Option.map (fun listing -> (provider, listing))))

let refresh_window_s = 300L

let near_expiry ~now secret =
  match Secret.expires_at secret with
  | None -> false
  | Some at -> Int64.compare at (Int64.add now refresh_window_s) <= 0

let stored_name credential =
  match Credential.source credential with
  | Credential.Source.Store name -> Some name
  | Credential.Source.Process | Credential.Source.Env _ -> None

let same_secret a b = Secret.equal (Credential.secret a) (Credential.secret b)

let refresh t ~sw ~env ~now ?(force = false) ?auth_base_url expected =
  let provider = Credential.provider expected in
  let expected_secret = Credential.secret expected in
  match Runtime.registration_for t provider with
  | None -> Error (Error.Unknown_provider provider)
  | Some registration -> (
      match
        (stored_name expected, registration.Driver.driver.Driver.refresh)
      with
      | None, _ | _, None -> Ok (Some expected)
      | Some _, Some _
        when (not force)
             && ((not (Secret.has_refresh_token expected_secret))
                || not (near_expiry ~now expected_secret)) ->
          Ok (Some expected)
      | Some name, Some refresh_route -> (
          let store = Runtime.store t in
          let outcome =
            Credential_store.with_credential_lock store ~env ~provider ~name
              (fun () ->
                let* snapshot =
                  Credential_store.with_store_lock store ~env (fun () ->
                      let* current_store = Credential_store.load store in
                      Ok (Store.credential current_store ~provider ~name ()))
                in
                match snapshot with
                | None -> Ok (`Current None)
                | Some current when not (same_secret current expected) ->
                    Ok (`Current (Some current))
                | Some current -> (
                    let secret = Credential.secret current in
                    if
                      (not (Secret.has_refresh_token secret))
                      || ((not force) && not (near_expiry ~now secret))
                    then Ok (`Current (Some current))
                    else
                      let settle decide =
                        Credential_store.with_store_lock store ~env (fun () ->
                            let* current_store = Credential_store.load store in
                            match
                              Store.credential current_store ~provider ~name ()
                            with
                            | None -> Ok (`Current None)
                            | Some present
                              when not (same_secret present current) ->
                                Ok (`Current (Some present))
                            | Some present -> decide current_store present)
                      in
                      match
                        refresh_route ~sw ~env ~now ?auth_base_url secret
                      with
                      | Error problem ->
                          settle (fun _store present ->
                              if Problem.transient problem then
                                Ok (`Current (Some present))
                              else Ok (`Fatal problem))
                      | Ok replacement ->
                          Eio.Cancel.protect (fun () ->
                              settle (fun current_store _present ->
                                  let committed =
                                    Store.set ~provider ~name replacement
                                      current_store
                                  in
                                  let* () =
                                    Credential_store.save store ~env committed
                                  in
                                  Ok
                                    (`Committed
                                       (Store.credential committed ~provider
                                          ~name ()))))))
          in
          match outcome with
          | Error store_error -> Error (Error.Store store_error)
          | Ok (`Current credential) -> Ok credential
          | Ok (`Committed credential) -> Ok credential
          | Ok (`Fatal problem) ->
              Error
                (Error.Blocked_credential { provider; problems = [ problem ] }))
      )

let invalid_base_url ~provider value =
  let invalid message = Error (Error.Invalid_base_url { provider; message }) in
  match Uri.of_string value with
  | exception Invalid_argument _ -> invalid "could not parse URL"
  | uri -> (
      match (Uri.scheme uri, Uri.host uri) with
      | Some ("http" | "https"), Some host when not (String.is_empty host) ->
          Ok ()
      | _ -> invalid "expected an absolute http(s) URL")

let current_discovery registration ~process ~environment ~store ~name =
  discover_declaration registration.Driver.declaration ~process ~environment
    ~store ~name ()

let ordinary_logout t ~env ~registration ~provider ~name ~process ~environment =
  let store = Runtime.store t in
  Credential_store.with_credential_lock store ~env ~provider ~name (fun () ->
      let* committed =
        Credential_store.with_store_lock store ~env (fun () ->
            let* snapshot = Credential_store.load store in
            match Store.credential snapshot ~provider ~name () with
            | None -> Ok snapshot
            | Some _ ->
                Eio.Cancel.protect (fun () ->
                    let committed = Store.remove ~provider ~name snapshot in
                    let* () = Credential_store.save store ~env committed in
                    Ok committed))
      in
      let current =
        current_discovery registration ~process ~environment ~store:committed
          ~name
      in
      Ok Account.Logout.{ current; revocation = None })
  |> Result.map_error (fun error -> Error.Store error)

let revoking_logout t ~sw ~env ~auth_base_url ~registration ~provider ~name
    ~process ~environment =
  let revoke_route = registration.Driver.driver.Driver.revoke in
  let* () =
    match (revoke_route, auth_base_url) with
    | Some _, Some value -> invalid_base_url ~provider value
    | None, (None | Some _) | Some _, None -> Ok ()
  in
  let store = Runtime.store t in
  Credential_store.with_credential_lock store ~env ~provider ~name (fun () ->
      let* snapshot_store, snapshot =
        Credential_store.with_store_lock store ~env (fun () ->
            let* snapshot_store = Credential_store.load store in
            Ok
              ( snapshot_store,
                Store.credential snapshot_store ~provider ~name () ))
      in
      match snapshot with
      | None ->
          let current =
            current_discovery registration ~process ~environment
              ~store:snapshot_store ~name
          in
          Ok
            Account.Logout.
              { current; revocation = Some Account.Logout.Not_stored }
      | Some snapshot ->
          let secret = Credential.secret snapshot in
          let remote =
            match revoke_route with
            | None -> Account.Logout.Unsupported
            | Some route -> (
                match route ~sw ~env ?auth_base_url secret with
                | Ok () -> Account.Logout.Revoked
                | Error Problem.Unsupported -> Account.Logout.Unsupported
                | Error problem -> Account.Logout.Failed problem)
          in
          Eio.Cancel.protect (fun () ->
              let* revocation, committed =
                Credential_store.with_store_lock store ~env (fun () ->
                    let* current_store = Credential_store.load store in
                    match Store.credential current_store ~provider ~name () with
                    | Some current when not (same_secret current snapshot) ->
                        Ok
                          ( Account.Logout.Settled
                              { remote; local = Account.Logout.Superseded },
                            current_store )
                    | None ->
                        Ok
                          ( Account.Logout.Settled
                              { remote; local = Account.Logout.Removed },
                            current_store )
                    | Some _ ->
                        let committed =
                          Store.remove ~provider ~name current_store
                        in
                        let* () = Credential_store.save store ~env committed in
                        Ok
                          ( Account.Logout.Settled
                              { remote; local = Account.Logout.Removed },
                            committed ))
              in
              let current =
                current_discovery registration ~process ~environment
                  ~store:committed ~name
              in
              Ok Account.Logout.{ current; revocation = Some revocation }))
  |> Result.map_error (fun error -> Error.Store error)

let logout t ~sw ~env ~provider ?(name = Name.default) ?(revoke = false)
    ?auth_base_url ?(process = []) ~environment () =
  match Runtime.registration_for t provider with
  | None -> Error (Error.Unknown_provider provider)
  | Some registration ->
      if revoke then
        revoking_logout t ~sw ~env ~auth_base_url ~registration ~provider ~name
          ~process ~environment
      else
        ordinary_logout t ~env ~registration ~provider ~name ~process
          ~environment
