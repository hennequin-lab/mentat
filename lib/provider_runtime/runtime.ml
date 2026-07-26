(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Auth = Mentat_provider.Auth
module Login = Auth.Login

type t = {
  registrations : Driver.registration list;
  catalog : Mentat_provider.Catalog.t;
  store : Credential_store.t;
}

let invalid message = invalid_arg ("Mentat_provider_runtime.create: " ^ message)

(* The coverage check: constructing a [t] proves that every declaration and its
   driver agree. Providers are unique, and every declared provider-defined login
   protocol resolves to a driver handler. The shared interpreter serves the
   standard OAuth and API-key protocols, so those need no per-driver handler. *)
let check_coverage registrations =
  let seen = Hashtbl.create 8 in
  List.iter
    (fun { Driver.declaration; driver } ->
      let id = Mentat_llm.Provider.id (Mentat_provider.id declaration) in
      if Hashtbl.mem seen id then invalid ("provider declared twice: " ^ id);
      Hashtbl.add seen id ();
      List.iter
        (fun login ->
          match Login.protocol login with
          | Login.Protocol.Provider_defined { protocol; _ } -> (
              match driver.Driver.provider_defined protocol with
              | Some _ -> ()
              | None ->
                  invalid
                    (Printf.sprintf
                       "provider %s declares login protocol %s with no driver \
                        handler"
                       id
                       (Login.Id.to_string protocol)))
          | Login.Protocol.Api_key | Login.Protocol.OAuth2_device_code _
          | Login.Protocol.OAuth2_authorization_code _
          | Login.Protocol.External _ ->
              ())
        (Auth.logins (Mentat_provider.auth declaration)))
    registrations

let create ~config_dir =
  let registrations = Builtin.registrations in
  check_coverage registrations;
  let declarations =
    List.map (fun registration -> registration.Driver.declaration) registrations
  in
  let catalog = Mentat_provider.Catalog.make declarations in
  { registrations; catalog; store = Credential_store.make ~config_dir }

let catalog t = t.catalog

let registration_for t provider =
  List.find_opt
    (fun r ->
      Mentat_llm.Provider.equal
        (Mentat_provider.id r.Driver.declaration)
        provider)
    t.registrations

let store t = t.store
