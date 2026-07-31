(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Unit suite for [mentat_provider_runtime], the effectful
   provider leaf. External network is never exercised. One focused account test
   uses an invalid endpoint identity and the reserved [.invalid] DNS namespace
   to pin TLS setup and resolver failure lowering. The remaining credential,
   store, login, and account flows use a fresh temp [config_dir] and the ollama
   provider, whose driver declares no live account check. A live OAuth handshake
   and provider transport remain blackbox-through-the-binary territory. *)

(* {2 The contract-law map}

   Where each contract law is pinned. "blackbox-only" means the law is only
   observable through the real binary (effectful auth / a live transport / the
   dune dependency graph) and feeds the adversarial review, not this unit suite.

   - L1 (ports-only): blackbox-only. The exclusion (no archive above this leaf
     links a transport/TLS/OAuth/auth.json) is a dune dependency-test fact, not a
     runtime assertion. Review-input.
   - L2 (no port naming): type-level. [Runtime.client] returns a
     [Mentat_llm.Client.t] and names no engine port; [client_builds_with_env] and
     [client_optional_bare] bind that type. Compiler-checked, so unreachable as a
     value assertion.
   - L3 (single registration / coverage): [create_declarations],
     [create_coverage_sweep], [create_performs_no_io]. [create_coverage_sweep]
     pins that the only [Provider_defined] login across the whole built-in list
     is OpenAI's, so it subsumes a per-provider coverage check. The negative
     drift cases (a provider declared
     twice, a Provider_defined login with no handler) are unreachable-in-unit:
     [create] only ever accepts the frozen built-in list, so a malformed list
     cannot be injected. Review-input (no genuine defect found; the suite is
     fully green, so there is no disabled section).
   - L4 (secret confinement): [secret_never_formatted] (property over the
     account formatters), [secret_confinement_io_path] (the save/load path),
     [error_taxonomy_messages] (the error/store formatters carry no secret).
   - L5 (call non-blocking on interactive auth): blackbox-only. Observing the
     built client's one reactive refresh needs a live [call]. Review-input.
   - L6 (refresh atomicity): refresh is private client policy. Client
     construction exercises the non-stored short circuit; network mint-vs-write
     cancellation and replacement races remain blackbox work rather than public
     unit seams.
   - L7 (login orchestration centralized / cancel): [login_run_rejections] pins
     the non-runnable-method dispatch. The device/browser poll+callback+cancel
     loop needs the fake issuer. Blackbox-only for the loop; review-input.
   - L8 (store confinement): [store_load_missing_file], [store_corrupt_json],
     [store_is_directory], [store_perm_across_rewrite],
     [store_concurrent_saves_converge] pin the atomic + 0o600 + lock + decode
     file machinery. That bytes reach *only* auth.json is the L1 dependency fact.

*)

open Windtrap
module Runtime = Mentat_provider_runtime
module Provider = Mentat_provider
module Credential = Provider.Credential
module Account = Provider.Account
module Llm = Mentat_llm

let provider_t = Testable.make ~pp:Llm.Provider.pp ~equal:Llm.Provider.equal

let problem_t =
  Testable.make ~pp:Account.Problem.pp ~equal:Account.Problem.equal

(* {2 Helpers} *)

let with_runtime name f =
  Eio_main.run @@ fun env ->
  let raw = Filename.temp_dir ("mentat-provider-runtime-" ^ name) "" in
  let config_dir = Eio.Path.( / ) (Eio.Stdenv.fs env) raw in
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote raw)))
    (fun () -> f env raw (Runtime.create ~config_dir))

let now env = Int64.of_float (Eio.Time.now (Eio.Stdenv.clock env))

let ok_error msg = function
  | Ok value -> value
  | Error error -> failf "%s: %a" msg Runtime.Error.pp error

let save_ok result = ok_error "save" result

let known_account msg = function
  | Account.Discovery.Known account -> account
  | Account.Discovery.Resolution_failed { provider; error } ->
      failf "%s: %a: %a" msg Llm.Provider.pp provider
        Provider.Credential_error.pp error

let ok_discoveries msg = function
  | Ok discoveries -> discoveries
  | Error error -> failf "%s: %a" msg Runtime.Store_error.pp error

let accounts_of_discoveries msg discoveries =
  List.map
    (function
      | Account.Discovery.Known account -> account
      | Account.Discovery.Resolution_failed { provider; error } ->
          failf "%s: %a: %a" msg Llm.Provider.pp provider
            Provider.Credential_error.pp error)
    discoveries

let load_accounts msg result =
  result |> ok_discoveries msg |> accounts_of_discoveries msg

let provider_ids decls =
  List.map (fun d -> Llm.Provider.id (Provider.id d)) decls

let account_for provider accounts =
  List.find (fun a -> Llm.Provider.equal (Account.provider a) provider) accounts

let contains ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec loop i =
    if i + n > h then false
    else if String.equal (String.sub haystack i n) needle then true
    else loop (i + 1)
  in
  n = 0 || loop 0

let write_file path text =
  Out_channel.with_open_text path (fun oc -> Out_channel.output_string oc text)

let with_early_close_peer env f =
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen (Eio.Stdenv.net env) ~sw ~backlog:1 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix path -> failf "expected TCP listening socket, got Unix path %S" path
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Switch.run @@ fun connection_sw ->
      let flow, _ = Eio.Net.accept ~sw:connection_sw socket in
      Eio.Flow.close flow;
      `Stop_daemon);
  f ~sw port

(* {2 create, declarations, coverage} *)

let create_declarations () =
  with_runtime "declarations" @@ fun _env _raw t ->
  let decls = Provider.Catalog.declarations (Runtime.catalog t) in
  let expected =
    List.map Llm.Provider.id
      [
        Mentat_llm_openai.provider;
        Mentat_llm_anthropic.provider;
        Mentat_llm_google.provider;
        Mentat_llm_local.provider;
        Mentat_llm_ollama.provider;
      ]
  in
  equal (list string) (provider_ids decls) expected

let create_coverage_sweep () =
  (* Sweep all six declarations: the only [Provider_defined] login protocol
     across the whole built-in list is OpenAI's "openai_chatgpt". create
     succeeded, so its handler resolves — the L3 coverage proof for every
     provider, not just OpenAI. *)
  with_runtime "coverage-sweep" @@ fun _env _raw t ->
  let provider_defined_ids =
    List.concat_map
      (fun d ->
        List.filter_map
          (fun login ->
            match Provider.Auth.Login.protocol login with
            | Provider.Auth.Login.Protocol.Provider_defined { protocol; _ } ->
                Some (Provider.Auth.Login.Id.to_string protocol)
            | Provider.Auth.Login.Protocol.Api_key
            | Provider.Auth.Login.Protocol.OAuth2_device_code _
            | Provider.Auth.Login.Protocol.OAuth2_authorization_code _
            | Provider.Auth.Login.Protocol.External _ ->
                None)
          (Provider.Auth.logins (Provider.auth d)))
      (Provider.Catalog.declarations (Runtime.catalog t))
  in
  equal (list string) provider_defined_ids [ "openai_chatgpt" ]

let create_performs_no_io () =
  (* create binds the store path but reads and writes nothing: pointed at a
     nonexistent nested config dir, it creates no file or directory. *)
  Eio_main.run @@ fun env ->
  let raw = Filename.temp_dir "mentat-provider-runtime-noio" "" in
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote raw)))
    (fun () ->
      let nested = Filename.concat raw "does/not/exist" in
      let config_dir = Eio.Path.( / ) (Eio.Stdenv.fs env) nested in
      let (_ : Runtime.t) = Runtime.create ~config_dir in
      is_false (Sys.file_exists (Filename.concat raw "does")))

(* {2 Credential resolution and connectivity} *)

let load_resolves_env () =
  with_runtime "load-env" @@ fun _env _raw t ->
  let accounts =
    load_accounts "load"
      (Runtime.discover_accounts t
         ~environment:[ ("OPENAI_API_KEY", "sk-env-key") ]
         ())
  in
  (* OpenAI resolves from the environment; ollama has no source and is missing. *)
  let openai = account_for Mentat_llm_openai.provider accounts in
  is_true (Account.connected openai);
  is_true
    (match Account.source openai with
    | Some source ->
        Credential.Source.equal source (Credential.Source.env "OPENAI_API_KEY")
    | None -> false);
  let ollama = account_for Mentat_llm_ollama.provider accounts in
  is_false (Account.connected ollama)

let load_process_precedence () =
  with_runtime "load-process" @@ fun _env _raw t ->
  let process =
    [
      Credential.make ~provider:Mentat_llm_openai.provider
        ~source:Credential.Source.process
        (Credential.Secret.api_key "sk-process-key");
    ]
  in
  let accounts =
    load_accounts "load"
      (Runtime.discover_accounts t ~process
         ~environment:[ ("OPENAI_API_KEY", "sk-env-key") ]
         ())
  in
  let openai = account_for Mentat_llm_openai.provider accounts in
  is_true
    (match Account.source openai with
    | Some source -> Credential.Source.equal source Credential.Source.process
    | None -> false)

let load_empty_env_is_unset () =
  (* An empty environment value reads as unset (resolve_credential precedence),
     so with no store slot the provider is missing rather than connected. *)
  with_runtime "load-empty-env" @@ fun _env _raw t ->
  let accounts =
    load_accounts "load"
      (Runtime.discover_accounts t ~environment:[ ("OPENAI_API_KEY", "") ] ())
  in
  is_false (Account.connected (account_for Mentat_llm_openai.provider accounts))

let load_env_over_store () =
  (* Env outranks the store: seed the ollama store slot, then resolve with the
     env var also set — the resolved source is the env var. *)
  with_runtime "load-env-store" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  ignore
    (save_ok
       (Runtime.Login.save_api_key t ~sw ~env
          ~provider:Mentat_llm_ollama.provider ~key:"stored-key-xxxx" ()));
  let accounts =
    load_accounts "load"
      (Runtime.discover_accounts t
         ~environment:[ ("OLLAMA_API_KEY", "env-key-yyyy") ]
         ())
  in
  let ollama = account_for Mentat_llm_ollama.provider accounts in
  is_true
    (match Account.source ollama with
    | Some source ->
        Credential.Source.equal source (Credential.Source.env "OLLAMA_API_KEY")
    | None -> false)

let load_preserves_partial_resolution_failure () =
  with_runtime "load-partial" @@ fun _env _raw t ->
  let process =
    [
      Credential.make ~provider:Mentat_llm_openai.provider
        ~source:Credential.Source.process
        (Credential.Secret.bearer "wrong-kind-openai");
    ]
  in
  let discoveries =
    ok_discoveries "load"
      (Runtime.discover_accounts t ~process
         ~environment:[ ("ANTHROPIC_API_KEY", "anthropic-key") ]
         ())
  in
  let expected_providers =
    Provider.Catalog.declarations (Runtime.catalog t) |> List.map Provider.id
  in
  let discovered_providers =
    List.map
      (function
        | Account.Discovery.Known account -> Account.provider account
        | Account.Discovery.Resolution_failed { provider; _ } -> provider)
      discoveries
  in
  equal (list provider_t) expected_providers discovered_providers
    ~msg:"order and cardinality follow the catalog";
  (match List.hd discoveries with
  | Account.Discovery.Resolution_failed
      {
        provider;
        error =
          Provider.Credential_error.Unsupported_credential_kind
            { source; kind = Credential.Kind.Bearer; _ };
      } ->
      equal provider_t Mentat_llm_openai.provider provider;
      is_true (Credential.Source.equal source Credential.Source.process)
  | Account.Discovery.Known _ | Account.Discovery.Resolution_failed _ ->
      fail "OpenAI should retain its credential resolution failure");
  let anthropic =
    List.find
      (function
        | Account.Discovery.Known account ->
            Llm.Provider.equal (Account.provider account)
              Mentat_llm_anthropic.provider
        | Account.Discovery.Resolution_failed _ -> false)
      discoveries
  in
  match anthropic with
  | Account.Discovery.Known account -> is_true (Account.connected account)
  | Account.Discovery.Resolution_failed _ ->
      fail "Anthropic should remain healthy when OpenAI resolution fails"

(* {2 Client construction (no network — build only)} *)

let openai_model = Mentat_llm_openai.model "gpt-5.6-sol"
let ollama_model = Mentat_llm_ollama.model "qwen3-coder"

let client_builds_with_env () =
  with_runtime "client-env" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  let client =
    ok_error "client"
      (Runtime.client t ~sw ~env ~now:(now env) ~process:[]
         ~environment:[ ("OPENAI_API_KEY", "sk-env-key") ]
         openai_model)
  in
  is_true
    (Llm.Provider.equal (Llm.Client.provider client) Mentat_llm_openai.provider)

let client_missing_credential () =
  with_runtime "client-missing" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  match
    Runtime.client t ~sw ~env ~now:(now env) ~process:[] ~environment:[]
      openai_model
  with
  | Error (Runtime.Error.Missing_credential provider) ->
      is_true (Llm.Provider.equal provider Mentat_llm_openai.provider)
  | Error other ->
      failf "expected Missing_credential, got %a" Runtime.Error.pp other
  | Ok _ ->
      fail
        "expected Missing_credential for a mandatory provider with no \
         credential"

let client_preserves_selected_resolution_failure () =
  with_runtime "client-credential-error" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  let process =
    [
      Credential.make ~provider:Mentat_llm_openai.provider
        ~source:Credential.Source.process
        (Credential.Secret.bearer "wrong-kind-openai");
    ]
  in
  match
    Runtime.client t ~sw ~env ~now:(now env) ~process ~environment:[]
      openai_model
  with
  | Error
      (Runtime.Error.Credential
         {
           provider;
           error =
             Provider.Credential_error.Unsupported_credential_kind
               { source; kind = Credential.Kind.Bearer; _ };
         }) ->
      equal provider_t Mentat_llm_openai.provider provider;
      is_true (Credential.Source.equal source Credential.Source.process)
  | Error other ->
      failf "expected the selected credential error, got %a" Runtime.Error.pp
        other
  | Ok _ -> fail "expected the selected credential to be rejected"

let client_optional_bare () =
  with_runtime "client-bare" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  let client =
    ok_error "client"
      (Runtime.client t ~sw ~env ~now:(now env) ~process:[] ~environment:[]
         ollama_model)
  in
  is_true
    (Llm.Provider.equal (Llm.Client.provider client) Mentat_llm_ollama.provider)

let client_rejects_invalid_base_url_for_every_http_provider () =
  with_runtime "client-base-url" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  let routes =
    [
      (Mentat_llm_openai.provider, openai_model);
      ( Mentat_llm_anthropic.provider,
        Mentat_llm_anthropic.model "claude-sonnet-5" );
      (Mentat_llm_google.provider, Mentat_llm_google.model "gemini-3.6-flash");
      (Mentat_llm_ollama.provider, ollama_model);
    ]
  in
  List.iter
    (fun (provider, model) ->
      let credential =
        Credential.make ~provider ~source:Credential.Source.process
          (Credential.Secret.api_key "route-test-key")
      in
      match
        Runtime.client t ~sw ~env ~now:(now env) ~base_url:"///"
          ~process:[ credential ] ~environment:[] model
      with
      | Error (Runtime.Error.Invalid_base_url { provider = found; _ }) ->
          equal provider_t provider found
      | Error error ->
          failf "%a: expected Invalid_base_url, got %a" Llm.Provider.pp provider
            Runtime.Error.pp error
      | Ok _ ->
          failf "%a: invalid base URL built a client" Llm.Provider.pp provider)
    routes

(* {2 Login save / logout / run (ollama: no live check) } *)

let login_save_and_load () =
  with_runtime "login-save" @@ fun env raw t ->
  Eio.Switch.run @@ fun sw ->
  let account =
    save_ok
      (Runtime.Login.save_api_key t ~sw ~env
         ~provider:Mentat_llm_ollama.provider ~key:"ollama-secret" ())
  in
  is_true (Account.connected account);
  equal string (Account.Phase.to_string (Account.phase account)) "unchecked";
  (* The store file is written 0o600. *)
  let stat = Unix.stat (Filename.concat raw "auth.json") in
  equal int (stat.Unix.st_perm land 0o777) 0o600;
  (* The credential is persisted and read back. *)
  let accounts =
    load_accounts "load" (Runtime.discover_accounts t ~environment:[] ())
  in
  is_true (Account.connected (account_for Mentat_llm_ollama.provider accounts))

let login_save_replaces () =
  (* A second save under the same provider/name replaces the first: the resolved
     credential's fingerprint follows the latest secret. *)
  with_runtime "login-replace" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  let save suffix =
    ignore
      (save_ok
         (Runtime.Login.save_api_key t ~sw ~env
            ~provider:Mentat_llm_ollama.provider
            ~key:("ollama-secret-" ^ suffix)
            ()))
  in
  save "AAAA";
  save "BBBB";
  let accounts =
    load_accounts "load" (Runtime.discover_accounts t ~environment:[] ())
  in
  let ollama = account_for Mentat_llm_ollama.provider accounts in
  equal (option string) (Account.fingerprint ollama) (Some "BBBB")

let login_save_rejects_before_store_mutation () =
  with_runtime "login-save-reject" @@ fun env raw t ->
  Eio.Switch.run @@ fun sw ->
  let store_path = Filename.concat raw "auth.json" in
  let key = "rejected-api-key" in
  let unknown = Llm.Provider.make "unknown-save-provider" in
  (match Runtime.Login.save_api_key t ~sw ~env ~provider:unknown ~key () with
  | Error (Runtime.Error.Unknown_provider provider) ->
      equal provider_t unknown provider
  | Error error ->
      failf "unknown provider: expected Unknown_provider, got %a"
        Runtime.Error.pp error
  | Ok _ -> fail "unknown provider save should fail before settling");
  is_false
    (Sys.file_exists store_path)
    ~msg:"unknown provider save left the store untouched";
  (match
     Runtime.Login.save_api_key t ~sw ~env ~provider:Mentat_llm_local.provider
       ~key ()
   with
  | Error
      (Runtime.Error.Credential
         {
           provider;
           error =
             Provider.Credential_error.Unsupported_credential_kind
               { kind = Credential.Kind.Api_key; accepted = []; _ };
         }) ->
      equal provider_t Mentat_llm_local.provider provider
  | Error error ->
      failf "credentialless provider: expected Credential, got %a"
        Runtime.Error.pp error
  | Ok _ -> fail "credentialless provider save should fail before settling");
  is_false
    (Sys.file_exists store_path)
    ~msg:"credentialless provider save left the store untouched";
  (match
     Runtime.Login.save_api_key t ~sw ~env ~base_url:"LEAKMARKER-invalid-base"
       ~provider:Mentat_llm_openai.provider ~key ()
   with
  | Error (Runtime.Error.Invalid_base_url { provider; _ } as error) ->
      equal provider_t Mentat_llm_openai.provider provider;
      is_false (contains ~needle:"LEAKMARKER" (Runtime.Error.message error))
  | Error error ->
      failf "invalid API base: expected Invalid_base_url, got %a"
        Runtime.Error.pp error
  | Ok _ -> fail "invalid applicable API base URL should fail before settling");
  is_false
    (Sys.file_exists store_path)
    ~msg:"invalid applicable API base URL left the store untouched";
  let invalid_key label key =
    match
      Runtime.Login.save_api_key t ~sw ~env ~provider:Mentat_llm_ollama.provider
        ~key ()
    with
    | Error (Runtime.Error.Login { diagnostic; _ }) ->
        is_true
          ~msg:(label ^ " has a payload-free diagnostic")
          (contains ~needle:"valid UTF-8"
             (Mentat_diagnostic.to_string diagnostic))
    | Error error ->
        failf "%s: expected Login, got %a" label Runtime.Error.pp error
    | Ok _ -> failf "%s: invalid key was persisted" label
  in
  invalid_key "empty key" "";
  invalid_key "invalid UTF-8 key" (String.make 1 (Char.chr 0xFF));
  is_false
    (Sys.file_exists store_path)
    ~msg:"invalid key text left the store untouched"

let login_save_lowers_tls_setup_and_dns_failures () =
  with_runtime "login-save-network-setup" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  let save base_url =
    save_ok
      (Runtime.Login.save_api_key t ~sw ~env
         ~provider:Mentat_llm_openai.provider ~base_url ~key:"account-check-key"
         ())
  in
  let check label account =
    equal (list problem_t)
      ~msg:(label ^ " is a network observation")
      [ Account.Problem.Network ]
      (Account.problems account)
  in
  let invalid_identity = String.make 64 'a' ^ ".example" in
  check "invalid TLS identity" (save ("https://" ^ invalid_identity ^ "/v1"));
  check "DNS resolution failure" (save "https://does-not-exist.invalid/v1");
  with_early_close_peer env @@ fun ~sw:_ port ->
  check "early TLS handshake close"
    (save (Printf.sprintf "https://127.0.0.1:%d/v1" port))

let login_logout_removes () =
  with_runtime "login-logout" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  ignore
    (save_ok
       (Runtime.Login.save_api_key t ~sw ~env
          ~provider:Mentat_llm_ollama.provider ~key:"ollama-secret" ()));
  let logout =
    ok_error "logout"
      (Runtime.Login.logout t ~sw ~env ~provider:Mentat_llm_ollama.provider
         ~environment:[ ("OLLAMA_API_KEY", "env-key") ]
         ())
  in
  let current = known_account "logout current" logout.Account.Logout.current in
  is_true (Account.connected current);
  is_true
    (match Account.source current with
    | Some source ->
        Credential.Source.equal source (Credential.Source.env "OLLAMA_API_KEY")
    | None -> false);
  is_true (Option.is_none logout.Account.Logout.revocation);
  (* Without the env source, the stored credential is gone. *)
  let accounts =
    load_accounts "load" (Runtime.discover_accounts t ~environment:[] ())
  in
  is_false (Account.connected (account_for Mentat_llm_ollama.provider accounts))

let load_observes_save_and_logout_live () =
  with_runtime "load-live" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  let connected () =
    Runtime.discover_accounts t ~environment:[] ()
    |> load_accounts "load"
    |> account_for Mentat_llm_ollama.provider
    |> Account.connected
  in
  is_false (connected ()) ~msg:"initially missing";
  ignore
    (save_ok
       (Runtime.Login.save_api_key t ~sw ~env
          ~provider:Mentat_llm_ollama.provider ~key:"ollama-live-secret" ()));
  is_true (connected ()) ~msg:"the next load observes save";
  ignore
    (ok_error "logout"
       (Runtime.Login.logout t ~sw ~env ~provider:Mentat_llm_ollama.provider
          ~environment:[] ()));
  is_false (connected ()) ~msg:"the next load observes logout"

let login_logout_never_saved () =
  (* Ordinary logout returns the missing discovery without creating auth.json
     for a slot that was already absent. *)
  with_runtime "logout-never-saved" @@ fun env raw t ->
  Eio.Switch.run @@ fun sw ->
  let logout =
    ok_error "logout"
      (Runtime.Login.logout t ~sw ~env ~provider:Mentat_llm_ollama.provider
         ~environment:[] ())
  in
  let current = known_account "logout current" logout.Account.Logout.current in
  equal string (Account.Phase.to_string (Account.phase current)) "missing";
  is_true (Option.is_none logout.Account.Logout.revocation);
  is_false
    (Sys.file_exists (Filename.concat raw "auth.json"))
    ~msg:"absent ordinary logout did not create auth.json"

let login_logout_absent_slot_does_not_rewrite () =
  with_runtime "logout-absent-slot" @@ fun env raw t ->
  Eio.Switch.run @@ fun sw ->
  ignore
    (save_ok
       (Runtime.Login.save_api_key t ~sw ~env
          ~provider:Mentat_llm_ollama.provider ~key:"stored-credential" ()));
  let path = Filename.concat raw "auth.json" in
  let before = Unix.stat path in
  ignore
    (ok_error "logout"
       (Runtime.Login.logout t ~sw ~env ~provider:Mentat_llm_ollama.provider
          ~name:(Credential.Name.make "absent")
          ~environment:[] ()));
  let after = Unix.stat path in
  equal int ~msg:"absent slot keeps the store inode" before.Unix.st_ino
    after.Unix.st_ino;
  equal (Windtrap.float 0.) ~msg:"absent slot keeps the store mtime"
    before.Unix.st_mtime after.Unix.st_mtime

let login_logout_current_prefers_process () =
  with_runtime "logout-process" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  ignore
    (save_ok
       (Runtime.Login.save_api_key t ~sw ~env
          ~provider:Mentat_llm_ollama.provider ~key:"stored-credential" ()));
  let process =
    [
      Credential.make ~provider:Mentat_llm_ollama.provider
        ~source:Credential.Source.process
        (Credential.Secret.api_key "process-credential");
    ]
  in
  let logout =
    ok_error "logout"
      (Runtime.Login.logout t ~sw ~env ~provider:Mentat_llm_ollama.provider
         ~process
         ~environment:[ ("OLLAMA_API_KEY", "environment-credential") ]
         ())
  in
  let current = known_account "logout current" logout.Account.Logout.current in
  is_true
    (match Account.source current with
    | Some source -> Credential.Source.equal source Credential.Source.process
    | None -> false)

let login_logout_revoke_no_route () =
  (* A missing revoke route and an explicit unsupported response are the same
     public fact. The observed local secret is still removed. *)
  with_runtime "logout-revoke" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  ignore
    (save_ok
       (Runtime.Login.save_api_key t ~sw ~env
          ~provider:Mentat_llm_ollama.provider ~key:"ollama-secret" ()));
  let logout =
    ok_error "logout"
      (Runtime.Login.logout t ~sw ~env ~provider:Mentat_llm_ollama.provider
         ~revoke:true ~environment:[] ())
  in
  let current = known_account "logout current" logout.Account.Logout.current in
  equal string (Account.Phase.to_string (Account.phase current)) "missing";
  match logout.Account.Logout.revocation with
  | Some
      (Account.Logout.Settled
         { remote = Account.Logout.Unsupported; local = Account.Logout.Removed })
    ->
      ()
  | Some _ -> fail "expected Settled{unsupported; removed}"
  | None -> fail "expected revocation settlement with ~revoke:true"

let login_logout_revoke_not_stored () =
  with_runtime "logout-revoke-absent" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  let logout =
    ok_error "logout"
      (Runtime.Login.logout t ~sw ~env ~provider:Mentat_llm_ollama.provider
         ~revoke:true ~environment:[] ())
  in
  match logout.Account.Logout.revocation with
  | Some Account.Logout.Not_stored -> ()
  | Some (Account.Logout.Settled _) -> fail "expected Not_stored"
  | None -> fail "expected revocation settlement with ~revoke:true"

let login_logout_rejects_before_store_mutation () =
  with_runtime "logout-preflight" @@ fun env raw t ->
  Eio.Switch.run @@ fun sw ->
  let store_path = Filename.concat raw "auth.json" in
  let unknown = Llm.Provider.make "unknown-logout-provider" in
  (match
     Runtime.Login.logout t ~sw ~env ~provider:unknown ~environment:[] ()
   with
  | Error (Runtime.Error.Unknown_provider provider) ->
      equal provider_t unknown provider
  | Error error ->
      failf "unknown provider: expected Unknown_provider, got %a"
        Runtime.Error.pp error
  | Ok _ -> fail "unknown provider logout should fail before mutation");
  is_false (Sys.file_exists store_path);
  (match
     Runtime.Login.logout t ~sw ~env ~provider:Mentat_llm_openai.provider
       ~revoke:true ~auth_base_url:"///" ~environment:[] ()
   with
  | Error (Runtime.Error.Invalid_base_url { provider; _ }) ->
      equal provider_t Mentat_llm_openai.provider provider
  | Error error ->
      failf "invalid auth base: expected Invalid_base_url, got %a"
        Runtime.Error.pp error
  | Ok _ -> fail "invalid applicable auth base should fail before mutation");
  is_false (Sys.file_exists store_path)

let login_run_rejections () =
  with_runtime "login-run" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  let run method_id =
    Runtime.Login.run t ~sw ~env ~provider:Mentat_llm_ollama.provider
      ~method_id:(Provider.Auth.Login.Id.make method_id)
      ~progress:(fun _ -> ())
      ()
  in
  (match run "no-such-method" with
  | Error (Runtime.Error.Login { provider; diagnostic }) ->
      equal provider_t Mentat_llm_ollama.provider provider;
      is_true
        (contains ~needle:"unknown auth method"
           (Mentat_diagnostic.to_string diagnostic))
  | Error error -> failf "expected Login error, got %a" Runtime.Error.pp error
  | Ok _ -> fail "unknown method should be a structured error");
  (* ollama's declared api-key login has no interactive run. *)
  match run "api-key" with
  | Error (Runtime.Error.Login _) -> ()
  | Error error -> failf "expected Login error, got %a" Runtime.Error.pp error
  | Ok _ -> fail "api-key run should be a structured error"

(* {2 Local artifact vocabulary} *)

module Artifact = Runtime.Artifact

let roundtrip_stable codec value =
  match Jsont_bytesrw.encode_string codec value with
  | Error message -> failf "encode: %s" message
  | Ok text -> (
      match Jsont_bytesrw.decode_string codec text with
      | Error message -> failf "decode %s: %s" text message
      | Ok value' -> (
          match Jsont_bytesrw.encode_string codec value' with
          | Error message -> failf "re-encode: %s" message
          | Ok text' -> equal string text text'))

(* The fixed-example round-trips are subsumed by the fuzz round-trips below,
   whose generators cover every arm of each artifact type. *)

(* Fuzz round-trips over the arms with windtrap generators. *)

let gen_int64 = Gen.map Int64.of_int (Gen.int_range 0 1_000_000_000)
let gen_text = Gen.string_of ~size:(Gen.int_range 0 16) (Gen.char_range ' ' '~')

let gen_phase =
  Gen.of_list Artifact.Progress.[ Checking; Downloading; Verifying; Ready ]

let gen_status =
  Gen.frequency
    [
      (1, Gen.map (fun path -> Artifact.Status.Installed { path }) gen_text);
      ( 2,
        Gen.map
          (fun ((path, size), source) ->
            Artifact.Status.Missing { path; size; source })
          (Gen.pair
             (Gen.pair gen_text (Gen.option gen_int64))
             (Gen.option gen_text)) );
      ( 1,
        Gen.map
          (fun message -> Artifact.Status.Unavailable { message })
          gen_text );
    ]

let gen_outcome =
  Gen.frequency
    [
      ( 1,
        Gen.map
          (fun p -> Artifact.Download_outcome.Already_installed p)
          gen_text );
      ( 1,
        Gen.of_list
          [
            Artifact.Download_outcome.Not_downloadable;
            Artifact.Download_outcome.Downloaded;
          ] );
      ( 1,
        Gen.map
          (fun (message, force_hint) ->
            Artifact.Download_outcome.Refused { message; force_hint })
          (Gen.pair gen_text Gen.bool) );
    ]

let gen_progress =
  Gen.map
    (fun ((model, label), ((received, total), phase)) ->
      {
        Artifact.Progress.provider = Mentat_llm_local.provider;
        model;
        label;
        received;
        total;
        phase;
      })
    (Gen.pair
       (Gen.pair gen_text gen_text)
       (Gen.pair (Gen.pair gen_int64 (Gen.option gen_int64)) gen_phase))

let pp_via codec ppf value =
  match Jsont_bytesrw.encode_string codec value with
  | Ok text -> Format.pp_print_string ppf text
  | Error message -> Format.fprintf ppf "<unencodable: %s>" message

let status_gen = Gen.with_pp (pp_via Artifact.Status.jsont) gen_status

let outcome_gen =
  Gen.with_pp (pp_via Artifact.Download_outcome.jsont) gen_outcome

let progress_gen = Gen.with_pp (pp_via Artifact.Progress.jsont) gen_progress

let artifact_status_fuzz =
  prop "artifact status round-trips (fuzz)" status_gen
    (roundtrip_stable Artifact.Status.jsont)

let artifact_outcome_fuzz =
  prop "artifact outcome round-trips (fuzz)" outcome_gen
    (roundtrip_stable Artifact.Download_outcome.jsont)

let artifact_progress_fuzz =
  prop "artifact progress round-trips (fuzz)" progress_gen
    (roundtrip_stable Artifact.Progress.jsont)

let artifact_strict_decode () =
  let decode_status = Jsont_bytesrw.decode_string Artifact.Status.jsont in
  let decode_outcome =
    Jsont_bytesrw.decode_string Artifact.Download_outcome.jsont
  in
  (* Unknown case tags are rejected, not silently mapped. *)
  is_true (Result.is_error (decode_status {|{"state":"bogus","path":"/x"}|}));
  is_true (Result.is_error (decode_outcome {|{"outcome":"bogus"}|}));
  (* Unknown members are rejected (error_unknown). *)
  is_true
    (Result.is_error
       (decode_status {|{"state":"installed","path":"/x","extra":1}|}));
  (* A well-formed document still decodes. *)
  is_true (Result.is_ok (decode_status {|{"state":"installed","path":"/x"}|}))

let artifact_summary () =
  equal string
    (Artifact.Status.summary (Artifact.Status.Installed { path = "/x" }))
    "installed: /x";
  equal string
    (Artifact.Status.summary
       (Artifact.Status.Missing { path = "/x"; size = None; source = None }))
    "missing - auto-download";
  equal string
    (Artifact.Status.summary
       (Artifact.Status.Missing
          { path = "/x"; size = Some 2_048L; source = None }))
    "missing - auto-download 2.0 KB";
  equal string
    (Artifact.Status.summary
       (Artifact.Status.Unavailable { message = "no manifest" }))
    "unavailable: no manifest"

let local_status_remote_is_none () =
  with_runtime "local-status" @@ fun _env _raw t ->
  (* A remote provider has no local artifact. *)
  equal (option string)
    (Option.map Artifact.Status.summary (Runtime.Local.status t openai_model))
    None

let local_download_remote_not_downloadable () =
  (* [download] on a remote provider is Not_downloadable, not an error. *)
  with_runtime "local-download-remote" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  let outcome =
    Runtime.Local.download t ~sw ~env ~force:false
      ~observe:(fun _ -> ())
      openai_model
  in
  match outcome with
  | Artifact.Download_outcome.Not_downloadable -> ()
  | other ->
      failf "expected Not_downloadable, got %s"
        (Format.asprintf "%a" (pp_via Artifact.Download_outcome.jsont) other)

(* {2 Error taxonomy and secret confinement} *)

let phantom_provider = Llm.Provider.make "phantom-provider"

let llm_kind =
  Testable.make
    ~pp:(fun ppf k -> Format.pp_print_string ppf (Llm.Error.label k))
    ~equal:( = )

let error_to_llm_total () =
  (* Every Error arm maps onto a provider-boundary error with the kind the .mli
     promises, and the provider is carried where one exists. *)
  let cases =
    [
      ( Runtime.Error.Credential
          {
            provider = Mentat_llm_openai.provider;
            error =
              Provider.Credential_error.Unsupported_credential_kind
                {
                  source = Credential.Source.process;
                  kind = Credential.Kind.Bearer;
                  accepted = [ Credential.Kind.Api_key; Credential.Kind.OAuth ];
                };
          },
        Llm.Error.Auth,
        Some Mentat_llm_openai.provider );
      ( Runtime.Error.Missing_credential Mentat_llm_openai.provider,
        Llm.Error.Auth,
        Some Mentat_llm_openai.provider );
      ( Runtime.Error.Blocked_credential
          {
            provider = Mentat_llm_openai.provider;
            problems = [ Account.Problem.Invalid_credential ];
          },
        Llm.Error.Auth,
        Some Mentat_llm_openai.provider );
      ( Runtime.Error.Invalid_base_url
          { provider = Mentat_llm_ollama.provider; message = "bad url" },
        Llm.Error.Transport,
        Some Mentat_llm_ollama.provider );
      ( Runtime.Error.Store (Runtime.Store_error.Io "io"),
        Llm.Error.Transport,
        None );
      ( Runtime.Error.Store (Runtime.Store_error.Decode "dec"),
        Llm.Error.Transport,
        None );
      ( Runtime.Error.Store (Runtime.Store_error.Locked "lock"),
        Llm.Error.Transport,
        None );
      ( Runtime.Error.Unknown_provider phantom_provider,
        Llm.Error.Provider,
        Some phantom_provider );
      ( Runtime.Error.Login
          {
            provider = Mentat_llm_openai.provider;
            diagnostic = Mentat_diagnostic.make "login failed";
          },
        Llm.Error.Auth,
        Some Mentat_llm_openai.provider );
    ]
  in
  List.iter
    (fun (error, kind, provider) ->
      let llm = Runtime.Error.to_llm error in
      equal llm_kind (Llm.Error.kind llm) kind;
      equal (option provider_t) (Llm.Error.provider llm) provider;
      is_true (String.length (Runtime.Error.message error) > 0))
    cases

let error_taxonomy_messages () =
  (* Store_error projections are the pass-through diagnostic strings, and the
     account/error formatters never invent a secret. *)
  equal string
    (Runtime.Store_error.message (Runtime.Store_error.Io "disk full"))
    "disk full";
  equal string
    (Runtime.Store_error.message (Runtime.Store_error.Decode "bad"))
    "bad";
  equal string
    (Runtime.Store_error.message (Runtime.Store_error.Locked "held"))
    "held";
  (* Blocked_credential names its problems without any secret. *)
  let blocked =
    Runtime.Error.Blocked_credential
      {
        provider = Mentat_llm_openai.provider;
        problems =
          [ Account.Problem.Invalid_credential; Account.Problem.Network ];
      }
  in
  is_true (contains ~needle:"openai" (Runtime.Error.message blocked));
  let diagnostic =
    Mentat_diagnostic.make ~hints:[ "try again" ] "authentication failed"
  in
  is_true
    (Mentat_diagnostic.equal diagnostic
       (Runtime.Error.diagnostic
          (Runtime.Error.Login
             { provider = Mentat_llm_openai.provider; diagnostic })));
  is_true
    (String.length
       (Runtime.Error.message (Runtime.Error.Store (Runtime.Store_error.Io "")))
    > 0)

let secret_body = "LEAKMARKER-SECRET-DO-NOT-DISCLOSE"

let leak_secret_gen =
  Gen.string_of ~size:(Gen.int_range 4 12) (Gen.char_range 'a' 'z')

let secret_never_formatted =
  prop "account formatters never emit a secret" leak_secret_gen (fun suffix ->
      (* A secret whose material begins with a recognizable marker: the only
         thing a formatter may show is a <=4-char fingerprint (the suffix), which
         can never contain the marker. *)
      let key = secret_body ^ "-" ^ suffix in
      let credential =
        Credential.make ~provider:Mentat_llm_ollama.provider
          ~source:(Credential.Source.store ())
          (Credential.Secret.api_key key)
      in
      let present = Account.present credential in
      let checked =
        Account.checked credential
          ~problems:[ Account.Problem.Invalid_credential ]
          ()
      in
      let rendered =
        [
          Format.asprintf "%a" Account.pp present;
          Format.asprintf "%a" Account.pp checked;
          Option.value (Account.fingerprint present) ~default:"";
          Format.asprintf "%a" Credential.Source.pp
            (Credential.source credential);
          Format.asprintf "%a" Credential.Kind.pp (Credential.kind credential);
        ]
      in
      List.iter
        (fun text ->
          if contains ~needle:"LEAKMARKER" text then
            failf "formatter leaked the secret marker: %s" text)
        rendered)

let secret_confinement_io_path () =
  (* The save/load path carries a real secret through the store; no account it
     yields, and no login outcome, exposes the marker. *)
  with_runtime "secret-io" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  let key = secret_body ^ "-tail" in
  let account =
    save_ok
      (Runtime.Login.save_api_key t ~sw ~env
         ~provider:Mentat_llm_ollama.provider ~key ())
  in
  let outcome_text = Format.asprintf "%a" Account.pp account in
  let accounts =
    load_accounts "load" (Runtime.discover_accounts t ~environment:[] ())
  in
  let account_text =
    String.concat " "
      (List.map (fun a -> Format.asprintf "%a" Account.pp a) accounts)
  in
  not_contains ~sub:"LEAKMARKER" outcome_text;
  not_contains ~sub:"LEAKMARKER" account_text

(* {2 Credential store faults} *)

let store_load_missing_file () =
  (* First run: no auth.json, so load succeeds and every provider is missing. *)
  with_runtime "store-missing" @@ fun _env _raw t ->
  let accounts =
    load_accounts "load" (Runtime.discover_accounts t ~environment:[] ())
  in
  is_true (List.for_all (fun a -> not (Account.connected a)) accounts)

let store_corrupt_json () =
  (* A corrupt auth.json surfaces a structured Store Decode error, never a raise. *)
  with_runtime "store-corrupt" @@ fun _env raw t ->
  write_file (Filename.concat raw "auth.json") "not json{";
  match Runtime.discover_accounts t ~environment:[] () with
  | Error (Runtime.Store_error.Decode _) -> ()
  | Error other ->
      failf "expected Store Decode, got %a" Runtime.Store_error.pp other
  | Ok _ -> fail "expected a decode error for corrupt auth.json"

let store_is_directory () =
  (* auth.json shadowed by a directory is a structured Store Io error. *)
  with_runtime "store-dir" @@ fun _env raw t ->
  Unix.mkdir (Filename.concat raw "auth.json") 0o700;
  match Runtime.discover_accounts t ~environment:[] () with
  | Error (Runtime.Store_error.Io _) -> ()
  | Error other ->
      failf "expected Store Io, got %a" Runtime.Store_error.pp other
  | Ok _ -> fail "expected an io error for a directory at auth.json"

let store_perm_across_rewrite () =
  (* The 0o600 mode is preserved across an atomic rewrite (temp + rename). *)
  with_runtime "store-perm" @@ fun env raw t ->
  Eio.Switch.run @@ fun sw ->
  let save suffix =
    ignore
      (save_ok
         (Runtime.Login.save_api_key t ~sw ~env
            ~provider:Mentat_llm_ollama.provider
            ~key:("ollama-secret-" ^ suffix)
            ()))
  in
  let perm () =
    (Unix.stat (Filename.concat raw "auth.json")).Unix.st_perm land 0o777
  in
  save "one";
  equal int (perm ()) 0o600;
  save "two";
  equal int (perm ()) 0o600

let store_concurrent_saves_converge () =
  (* Two fibers save distinct credential names concurrently. The store lock
     serializes the two load-set-save edits, so neither write is lost: both names
     are present afterward (each revoke settles Removed rather than Not_stored).
     Deterministic in outcome — no sleeps, no interleaving assumption. *)
  with_runtime "store-concurrent" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  let alpha = Credential.Name.make "alpha" in
  let beta = Credential.Name.make "beta" in
  let save name suffix =
    ignore
      (save_ok
         (Runtime.Login.save_api_key t ~sw ~env
            ~provider:Mentat_llm_ollama.provider ~name
            ~key:("ollama-secret-" ^ suffix)
            ()))
  in
  Eio.Fiber.both (fun () -> save alpha "aaaa") (fun () -> save beta "bbbb");
  let logout name =
    ok_error "logout"
      (Runtime.Login.logout t ~sw ~env ~provider:Mentat_llm_ollama.provider
         ~name ~revoke:true ~environment:[] ())
  in
  let was_stored result =
    match result.Account.Logout.revocation with
    | Some (Account.Logout.Settled _) -> true
    | Some Account.Logout.Not_stored | None -> false
  in
  is_true (was_stored (logout alpha)) ~msg:"alpha survived the concurrent save";
  is_true (was_stored (logout beta)) ~msg:"beta survived the concurrent save"

let concurrent_same_slot_saves_return_exact_accounts () =
  (* The per-credential lock spans each edit and confirming check. Each caller
     therefore receives the account for its own committed secret even though the
     final winner is intentionally nondeterministic. *)
  with_runtime "save-exact" @@ fun env _raw t ->
  Eio.Switch.run @@ fun sw ->
  let save suffix =
    Runtime.Login.save_api_key t ~sw ~env ~provider:Mentat_llm_ollama.provider
      ~key:("same-slot-secret-" ^ suffix)
      ()
    |> save_ok
  in
  let a = Eio.Fiber.fork_promise ~sw (fun () -> save "AAAA") in
  let b = Eio.Fiber.fork_promise ~sw (fun () -> save "BBBB") in
  let a = Eio.Promise.await_exn a in
  let b = Eio.Promise.await_exn b in
  equal (option string) (Account.fingerprint a) (Some "AAAA");
  equal (option string) (Account.fingerprint b) (Some "BBBB")

(* Media_support drift guard: the table is keyed by the string api ids the
   builtin adapters own, so assert each adapter's real [api] resolves to the
   media encodability its per-adapter test verifies (chat-completions user-only
   and anthropic/openai both-channels are pinned in test_llm_ollama and
   test_llm_openai). A table drift or an adapter api-id rename fails here. *)
let media_support_matches_adapters () =
  let module M = Provider.Media_support in
  let check name api ~user ~tool_result =
    equal bool ~msg:(name ^ " user content") user
      (M.encodable api M.User_content);
    equal bool ~msg:(name ^ " tool result") tool_result
      (M.encodable api M.Tool_result)
  in
  check "anthropic messages" Mentat_llm_anthropic.api ~user:true
    ~tool_result:true;
  check "openai responses" Mentat_llm_openai.api ~user:true ~tool_result:true;
  check "google gemini" Mentat_llm_google.api ~user:true ~tool_result:false;
  check "ollama chat-completions" Mentat_llm_ollama.api ~user:true
    ~tool_result:false;
  check "local chat-completions" Mentat_llm_local.api ~user:true
    ~tool_result:false;
  let unknown = Llm.Model.Api.make "unknown-vision-api" in
  is_false ~msg:"unknown api carries no user media"
    (M.encodable unknown M.User_content);
  is_false ~msg:"unknown api carries no tool-result media"
    (M.encodable unknown M.Tool_result)

let () =
  run "mentat.provider_runtime"
    [
      test "Media_support matches the builtin adapters"
        media_support_matches_adapters;
      (* create / coverage *)
      test "declarations are the six providers in order" create_declarations;
      test "create coverage sweeps all six declarations" create_coverage_sweep;
      test "create performs no I/O" create_performs_no_io;
      (* Credential resolution *)
      test "load resolves an environment credential" load_resolves_env;
      test "process credentials outrank the environment" load_process_precedence;
      test "an empty environment value reads as unset" load_empty_env_is_unset;
      test "environment outranks the store" load_env_over_store;
      test "one route failure preserves every provider discovery"
        load_preserves_partial_resolution_failure;
      (* Client construction *)
      test "client builds with an environment credential" client_builds_with_env;
      test "client fails closed with no mandatory credential"
        client_missing_credential;
      test "client returns the selected route credential error exactly"
        client_preserves_selected_resolution_failure;
      test "client builds a bare optional-auth client" client_optional_bare;
      test "every HTTP provider rejects an invalid configured base URL"
        client_rejects_invalid_base_url_for_every_http_provider;
      (* Login flows *)
      test "login save persists and reads back" login_save_and_load;
      test "login save replaces an existing secret" login_save_replaces;
      test "login save rejects invalid routes before mutating the store"
        login_save_rejects_before_store_mutation;
      test
        "login save lowers TLS setup, DNS, and peer failures to network \
         observations"
        login_save_lowers_tls_setup_and_dns_failures;
      test "login logout removes and reports env" login_logout_removes;
      test
        "account loads observe save and logout without rebuilding the runtime"
        load_observes_save_and_logout_live;
      test "logout of a never-saved provider is a no-op"
        login_logout_never_saved;
      test "logout of an absent slot does not rewrite the store"
        login_logout_absent_slot_does_not_rewrite;
      test "logout current reports the winning process source"
        login_logout_current_prefers_process;
      test "logout with revoke settles the local removal"
        login_logout_revoke_no_route;
      test "revoking logout of an empty slot is Not_stored"
        login_logout_revoke_not_stored;
      test "logout validates provider and auth base before mutation"
        login_logout_rejects_before_store_mutation;
      test "login run rejects non-runnable methods" login_run_rejections;
      (* Local artifacts *)
      artifact_status_fuzz;
      artifact_outcome_fuzz;
      artifact_progress_fuzz;
      test "artifact codecs reject unknown tags and fields"
        artifact_strict_decode;
      test "artifact summary" artifact_summary;
      test "local status is none for a remote provider"
        local_status_remote_is_none;
      test "local download of a remote provider is not-downloadable"
        local_download_remote_not_downloadable;
      (* Error taxonomy and secret confinement *)
      test "every error arm maps onto the port total" error_to_llm_total;
      test "error and store-error messages carry no secret"
        error_taxonomy_messages;
      secret_never_formatted;
      test "the save/load path never exposes a secret"
        secret_confinement_io_path;
      (* Credential store faults *)
      test "load of a missing store file is first-run empty"
        store_load_missing_file;
      test "a corrupt store file is a structured decode error"
        store_corrupt_json;
      test "a directory at auth.json is a structured io error"
        store_is_directory;
      test "0o600 is preserved across a rewrite" store_perm_across_rewrite;
      test "concurrent saves converge under the store lock"
        store_concurrent_saves_converge;
      test "same-slot saves return their exact committed accounts"
        concurrent_same_slot_saves_return_exact_accounts;
    ]
