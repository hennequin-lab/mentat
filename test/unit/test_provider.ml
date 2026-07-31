(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap
open Test_support
module Provider = Mentat_provider
module Auth = Provider.Auth
module Env = Auth.Env
module Login = Auth.Login
module Selector = Provider.Selector
module Model = Provider.Model
module Date = Model.Date
module Month = Model.Month
module Modality = Model.Modality
module Capability = Model.Capability
module Pricing = Model.Pricing
module Credential = Provider.Credential
module Secret = Credential.Secret
module Kind = Credential.Kind
module Name = Credential.Name
module Source = Credential.Source
module Store = Credential.Store
module Account = Provider.Account
module Profile = Account.Profile
module Org = Account.Org
module Problem = Account.Problem
module Credential_error = Provider.Credential_error
module Catalog = Provider.Catalog
module Selection = Provider.Selection
module Requirement = Selection.Requirement
module Model_readiness = Provider.Model_readiness
module Readiness_route = Model_readiness.Route
module Readiness_entry = Model_readiness.Entry
module Availability = Model_readiness.Availability
module Eligibility = Model_readiness.Eligibility
module Actionability = Model_readiness.Actionability
module Llm_model = Mentat_llm.Model
module Llm_provider = Mentat_llm.Provider
module Usage = Mentat_llm.Usage
module Options = Mentat_llm.Request.Options
module Reasoning = Options.Reasoning_effort
module Json = Jsont.Json

(* Providers, apis, and llm identities used throughout. *)
let openai = Llm_provider.make "openai"
let anthropic = Llm_provider.make "anthropic"
let google = Llm_provider.make "google"
let responses = Llm_model.Api.make "responses"
let messages = Llm_model.Api.make "messages"

let llm ?(provider = openai) ?(api = responses) id =
  Llm_model.make ~provider ~api ~id

(* Testables. Selector has no [pp]; render via [to_string]. *)
let provider_value = Testable.make ~pp:Llm_provider.pp ~equal:Llm_provider.equal
let llm_model_value = Testable.make ~pp:Llm_model.pp ~equal:Llm_model.equal
let model_value = Testable.make ~pp:Model.pp ~equal:Model.equal
let date_value = Testable.make ~pp:Date.pp ~equal:Date.equal
let month_value = Testable.make ~pp:Month.pp ~equal:Month.equal
let modality_value = Testable.make ~pp:Modality.pp ~equal:Modality.equal
let capability_value = Testable.make ~pp:Capability.pp ~equal:Capability.equal
let kind_value = Testable.make ~pp:Kind.pp ~equal:Kind.equal
let source_value = Testable.make ~pp:Source.pp ~equal:Source.equal
let name_value = Testable.make ~pp:Name.pp ~equal:Name.equal
let problem_value = Testable.make ~pp:Problem.pp ~equal:Problem.equal
let profile_value = Testable.make ~pp:Profile.pp ~equal:Profile.equal
let org_value = Testable.make ~pp:Org.pp ~equal:Org.equal
let phase_value = Testable.make ~pp:Account.Phase.pp ~equal:Account.Phase.equal
let float_value = Testable.make ~pp:Format.pp_print_float ~equal:Float.equal

let reasoning_value =
  Testable.make ~pp:Reasoning.pp ~equal:(fun a b ->
      String.equal (Reasoning.to_string a) (Reasoning.to_string b))

let selector_value =
  Testable.make
    ~pp:(fun ppf s -> Format.pp_print_string ppf (Selector.to_string s))
    ~equal:Selector.equal

(* A selector error carries no equality; compare structurally, ignoring the
   provider diagnostic message which comes from [Mentat_llm.Provider.make]. *)
let equal_selector_error a b =
  let open Selector.Error in
  match (a, b) with
  | Empty, Empty
  | Missing_separator, Missing_separator
  | Empty_provider, Empty_provider
  | Empty_model, Empty_model ->
      true
  | Invalid_provider a, Invalid_provider b -> String.equal a.input b.input
  | ( ( Empty | Missing_separator | Empty_provider | Empty_model
      | Invalid_provider _ ),
      _ ) ->
      false

let selector_error_value =
  Testable.make ~pp:Selector.Error.pp ~equal:equal_selector_error

let equal_catalog_error a b =
  let open Catalog.Error in
  match (a, b) with
  | Invalid_selector a, Invalid_selector b ->
      String.equal a.input b.input
      && equal_selector_error a.error b.error
      && List.equal Selector.equal a.candidates b.candidates
  | Unknown_provider a, Unknown_provider b ->
      Llm_provider.equal a.provider b.provider
      && List.equal Llm_provider.equal a.known b.known
  | Unknown_model a, Unknown_model b ->
      Llm_provider.equal a.provider b.provider
      && String.equal a.model b.model
      && List.equal String.equal a.known b.known
  | (Invalid_selector _ | Unknown_provider _ | Unknown_model _), _ -> false

let catalog_error_value =
  Testable.make ~pp:Catalog.Error.pp ~equal:equal_catalog_error

(* A structural view of a secret's payload, for exposing and comparing. *)
type secret_view =
  | Api_key_secret of string
  | Bearer_secret of string
  | OAuth_secret of {
      access_token : string;
      refresh_token : string option;
      expires_at : int64 option;
      account_id : string option;
    }

let pp_string_option ppf = function
  | None -> Format.pp_print_string ppf "None"
  | Some v -> Format.fprintf ppf "Some %S" v

let pp_int64_option ppf = function
  | None -> Format.pp_print_string ppf "None"
  | Some v -> Format.fprintf ppf "Some %Ld" v

let pp_secret_view ppf = function
  | Api_key_secret key -> Format.fprintf ppf "api_key(%S)" key
  | Bearer_secret token -> Format.fprintf ppf "bearer(%S)" token
  | OAuth_secret { access_token; refresh_token; expires_at; account_id } ->
      Format.fprintf ppf "oauth(%S,%a,%a,%a)" access_token pp_string_option
        refresh_token pp_int64_option expires_at pp_string_option account_id

let equal_secret_view a b =
  match (a, b) with
  | Api_key_secret a, Api_key_secret b -> String.equal a b
  | Bearer_secret a, Bearer_secret b -> String.equal a b
  | OAuth_secret a, OAuth_secret b ->
      String.equal a.access_token b.access_token
      && Option.equal String.equal a.refresh_token b.refresh_token
      && Option.equal Int64.equal a.expires_at b.expires_at
      && Option.equal String.equal a.account_id b.account_id
  | (Api_key_secret _ | Bearer_secret _ | OAuth_secret _), _ -> false

let secret_view_value =
  Testable.make ~pp:pp_secret_view ~equal:equal_secret_view

let secret_view secret =
  Secret.expose secret
    ~api_key:(fun ~key -> Api_key_secret key)
    ~bearer:(fun ~token -> Bearer_secret token)
    ~oauth:(fun ~access_token ~refresh_token ~expires_at ~account_id ->
      OAuth_secret { access_token; refresh_token; expires_at; account_id })

(* A structural view of a resolved credential: provider route, provenance, and
   the exposed secret. *)
type cred_view = {
  cv_provider : Llm_provider.t;
  cv_source : Source.t;
  cv_secret : secret_view;
}

let cred_view c =
  {
    cv_provider = Credential.provider c;
    cv_source = Credential.source c;
    cv_secret = secret_view (Credential.secret c);
  }

let pp_cred_view ppf v =
  Format.fprintf ppf "@[{provider=%a; source=%a; secret=%a}@]" Llm_provider.pp
    v.cv_provider Source.pp v.cv_source pp_secret_view v.cv_secret

let equal_cred_view a b =
  Llm_provider.equal a.cv_provider b.cv_provider
  && Source.equal a.cv_source b.cv_source
  && equal_secret_view a.cv_secret b.cv_secret

let cred_view_value = Testable.make ~pp:pp_cred_view ~equal:equal_cred_view

(* Assert a JSON codec rejects a value, optionally checking a message fragment. *)
let assert_decode_error ?contains msg codec json =
  match Json.decode codec json with
  | Ok _ -> failf "%s: expected decode error" msg
  | Error actual -> (
      match contains with
      | None -> ()
      | Some fragment ->
          is_true
            ~msg:(msg ^ ": message is " ^ actual)
            (String.includes ~affix:fragment actual))

let assert_round_trip ~equal msg codec value =
  is_true ~msg (equal value (value |> encode codec |> decode codec))

let rec json_contains_string needle = function
  | Jsont.String (value, _) -> String.includes ~affix:needle value
  | Jsont.Object (fields, _) ->
      List.exists (fun (_, value) -> json_contains_string needle value) fields
  | Jsont.Array (values, _) -> List.exists (json_contains_string needle) values
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ -> false

let modality_strings ms = List.map Modality.to_string ms
let capability_strings cs = List.map Capability.to_string cs

(* Selector. *)

let selector_group =
  group "selector"
    [
      test "make/of_string/to_string round-trip" (fun () ->
          let s = Selector.make ~provider:openai ~id:"gpt-5" in
          equal provider_value ~msg:"provider" openai (Selector.provider s);
          equal string ~msg:"id" "gpt-5" (Selector.id s);
          equal string ~msg:"to_string" "openai/gpt-5" (Selector.to_string s);
          equal
            (result selector_value selector_error_value)
            ~msg:"round-trip" (Ok s)
            (Selector.of_string (Selector.to_string s)));
      test "make raises on empty and non-trim-stable ids" (fun () ->
          expect_invalid_arg "empty id" (fun () ->
              Selector.make ~provider:openai ~id:"");
          expect_invalid_arg "leading whitespace" (fun () ->
              Selector.make ~provider:openai ~id:" gpt-5");
          expect_invalid_arg "trailing whitespace" (fun () ->
              Selector.make ~provider:openai ~id:"gpt-5 "));
      test "of_model bridges a canonical model to its selector" (fun () ->
          let m = llm "gpt-5" in
          equal selector_value ~msg:"of_model"
            (Selector.make ~provider:openai ~id:"gpt-5")
            (Selector.of_model m);
          equal string ~msg:"to_string" "openai/gpt-5"
            (Selector.to_string (Selector.of_model m)));
      test "of_string trims and splits on the first slash" (fun () ->
          let trimmed = Result.get_ok (Selector.of_string "  openai/gpt-5  ") in
          equal provider_value ~msg:"trimmed provider" openai
            (Selector.provider trimmed);
          equal string ~msg:"trimmed id" "gpt-5" (Selector.id trimmed);
          let nested =
            Result.get_ok (Selector.of_string "openai/family/gpt-5")
          in
          equal string ~msg:"remainder keeps later slashes" "family/gpt-5"
            (Selector.id nested);
          (* The id segment itself is trimmed, so both constructors maintain the
             same trim-stable invariant and round-trips are unconditional. *)
          let padded = Result.get_ok (Selector.of_string "openai/ gpt-5") in
          equal string ~msg:"id segment is trimmed" "gpt-5" (Selector.id padded);
          equal
            (result selector_value selector_error_value)
            ~msg:"padded input round-trips" (Ok padded)
            (Selector.of_string (Selector.to_string padded)));
      test "of_string reports each structured error" (fun () ->
          let case raw expected =
            equal
              (result selector_value selector_error_value)
              ~msg:raw (Error expected) (Selector.of_string raw)
          in
          case "   " Selector.Error.Empty;
          case "openai" Selector.Error.Missing_separator;
          case "/gpt-5" Selector.Error.Empty_provider;
          case "openai/" Selector.Error.Empty_model;
          match Selector.of_string "OpenAI/gpt-5" with
          | Error (Selector.Error.Invalid_provider { input; message }) ->
              equal string ~msg:"invalid provider segment" "OpenAI" input;
              is_true ~msg:"message names the failure"
                (String.includes ~affix:"invalid"
                   (Selector.Error.message
                      (Selector.Error.Invalid_provider { input; message })))
          | _ -> failf "expected Invalid_provider");
      test "jsont round-trips and rejects bad strings" (fun () ->
          let s = Selector.make ~provider:openai ~id:"gpt-5" in
          equal selector_value ~msg:"jsont round-trip" s
            (s |> encode Selector.jsont |> decode Selector.jsont);
          assert_decode_error ~contains:"provider/model"
            "jsont rejects no slash" Selector.jsont (Json.string "openai"));
      prop "of_string parses back any valid provider/model"
        (Gen.with_pp
           (fun ppf (p, m) -> Format.fprintf ppf "%S/%S" p m)
           (Gen.pair
              (let open Gen in
               let tail =
                 of_list
                   (List.init 26 (fun i -> Char.chr (Char.code 'a' + i))
                   @ List.init 10 (fun i -> Char.chr (Char.code '0' + i))
                   @ [ '-' ])
               in
               let+ head = char_range 'a' 'z'
               and+ rest = string_of ~size:(int_range 0 7) tail in
               String.make 1 head ^ rest)
              (Gen.string_of ~size:(Gen.int_range 1 10)
                 (Gen.of_list [ 'a'; 'z'; '0'; '9'; '-'; '.'; '_'; '/'; 'A' ]))))
        (fun (p, m) ->
          let raw = p ^ "/" ^ m in
          match Selector.of_string raw with
          | Error e -> failf "rejected %S: %s" raw (Selector.Error.message e)
          | Ok s ->
              equal string ~msg:"provider round-trips" p
                (Llm_provider.id (Selector.provider s));
              equal string ~msg:"id round-trips" m (Selector.id s);
              (* [make] on a trim-stable id round-trips unconditionally. *)
              let made = Selector.make ~provider:(Selector.provider s) ~id:m in
              equal
                (result selector_value selector_error_value)
                ~msg:"make/to_string/of_string" (Ok made)
                (Selector.of_string (Selector.to_string made)));
    ]

(* Auth. *)

let auth_env_group =
  group "auth env"
    [
      test "declarations carry their variable name and kind" (fun () ->
          let api_key = Env.api_key "OPENAI_API_KEY" in
          let bearer = Env.bearer "OPENAI_BEARER" in
          let oauth = Env.oauth_access_token "OPENAI_ACCESS_TOKEN" in
          equal string ~msg:"name" "OPENAI_API_KEY" (Env.name api_key);
          equal kind_value ~msg:"api key kind" Kind.Api_key (Env.kind api_key);
          equal kind_value ~msg:"bearer kind" Kind.Bearer (Env.kind bearer);
          equal kind_value ~msg:"oauth kind" Kind.OAuth (Env.kind oauth));
      test "constructors reject invalid variable names" (fun () ->
          expect_invalid_arg "empty" (fun () -> Env.api_key "");
          expect_invalid_arg "leading digit" (fun () -> Env.api_key "1TOKEN");
          expect_invalid_arg "hyphen" (fun () -> Env.bearer "OPENAI-KEY"));
      test "pp names the variable and kind only" (fun () ->
          equal string ~msg:"declaration formatting" "OPENAI_API_KEY:api_key"
            (Format.asprintf "%a" Env.pp (Env.api_key "OPENAI_API_KEY")));
    ]

let uri s = Uri.of_string s
let client = Oauth2.Client.make ~id:"client-id" ()

let auth_login_group =
  group "auth login"
    [
      test "progress carries presentation-neutral flow facts" (fun () ->
          let progress : Login.Progress.t =
            Login.Progress.Device_challenge
              {
                url = uri "https://auth.example/device";
                user_code = "ABCD-EFGH";
                expires_in = 600;
              }
          in
          match progress with
          | Login.Progress.Device_challenge { url; user_code; expires_in } ->
              equal string ~msg:"verification URL" "https://auth.example/device"
                (Uri.to_string url);
              equal string ~msg:"user code" "ABCD-EFGH" user_code;
              equal int ~msg:"expiry" 600 expires_in
          | Login.Progress.Browser_url _ | Login.Progress.Listening _ ->
              failf "expected device challenge");
      test "Login.Id.make raises while of_string returns an option" (fun () ->
          equal string ~msg:"make round-trip" "api-key"
            (Login.Id.to_string (Login.Id.make "api-key"));
          expect_invalid_arg "make rejects empty" (fun () -> Login.Id.make "");
          expect_invalid_arg "make rejects uppercase" (fun () ->
              Login.Id.make "Browser");
          equal (option string) ~msg:"of_string parses" (Some "device-code")
            (Option.map Login.Id.to_string (Login.Id.of_string "device-code"));
          equal (option string) ~msg:"of_string rejects uppercase" None
            (Option.map Login.Id.to_string (Login.Id.of_string "Browser"));
          equal (option string) ~msg:"of_string rejects empty" None
            (Option.map Login.Id.to_string (Login.Id.of_string "")));
      test "api-key and external protocols" (fun () ->
          let api_key = Login.api_key () in
          equal string ~msg:"default id" "api-key"
            (Login.Id.to_string (Login.id api_key));
          equal string ~msg:"default label" "API key" (Login.label api_key);
          (match Login.protocol api_key with
          | Login.Protocol.Api_key -> ()
          | _ -> failf "expected Api_key protocol");
          let external_ =
            Login.make ~id:(Login.Id.make "external") ~label:"External"
              (Login.Protocol.External
                 {
                   instructions = Some "configure externally";
                   credential_kind = Some Kind.OAuth;
                 })
          in
          match Login.protocol external_ with
          | Login.Protocol.External { instructions; credential_kind } ->
              equal (option string) ~msg:"external instructions"
                (Some "configure externally") instructions;
              equal (option kind_value) ~msg:"external kind" (Some Kind.OAuth)
                credential_kind
          | _ -> failf "expected External protocol");
      test "provider-defined protocol carries a checked id" (fun () ->
          let login =
            Login.make
              ~id:(Login.Id.make "device-code")
              ~label:"Device"
              (Login.Protocol.Provider_defined
                 {
                   protocol = Login.Id.make "openai-chatgpt";
                   credential_kind = Kind.OAuth;
                 })
          in
          match Login.protocol login with
          | Login.Protocol.Provider_defined { protocol; credential_kind } ->
              equal string ~msg:"checked protocol id" "openai-chatgpt"
                (Login.Id.to_string protocol);
              equal kind_value ~msg:"credential kind" Kind.OAuth credential_kind
          | _ -> failf "expected Provider_defined protocol");
      test "oauth2 device-code protocol keeps its named fields" (fun () ->
          let login =
            Login.oauth2_device_code
              ~id:(Login.Id.make "oauth-device")
              ~label:"OAuth device" ~client
              ~device_endpoint:(uri "https://auth.example/device")
              ~token_endpoint:(uri "https://auth.example/token")
              ~scope:[ "openid"; "profile" ]
              ~extra:[ ("audience", "mentat") ]
              ()
          in
          match Login.protocol login with
          | Login.Protocol.OAuth2_device_code
              { device_endpoint; token_endpoint; scope; extra; client = _ } ->
              equal string ~msg:"device endpoint" "https://auth.example/device"
                (Uri.to_string device_endpoint);
              equal string ~msg:"token endpoint" "https://auth.example/token"
                (Uri.to_string token_endpoint);
              equal (list string) ~msg:"scope" [ "openid"; "profile" ] scope;
              equal
                (list (pair string string))
                ~msg:"extra"
                [ ("audience", "mentat") ]
                extra
          | _ -> failf "expected OAuth2_device_code protocol");
      test "oauth2 authorization-code protocol keeps its named fields"
        (fun () ->
          let login =
            Login.oauth2_authorization_code
              ~id:(Login.Id.make "oauth-browser")
              ~label:"OAuth browser" ~client
              ~authorization_endpoint:(uri "https://auth.example/authorize")
              ~token_endpoint:(uri "https://auth.example/token")
              ~redirect_uri:(uri "http://localhost:1455/auth/callback")
              ~pkce:false
              ~scope:[ "email"; "offline_access" ]
              ~extra:[ ("prompt", "consent") ]
              ()
          in
          match Login.protocol login with
          | Login.Protocol.OAuth2_authorization_code
              {
                authorization_endpoint;
                token_endpoint;
                redirect_uri;
                scope;
                extra;
                pkce;
                client = _;
              } ->
              equal string ~msg:"authorization endpoint"
                "https://auth.example/authorize"
                (Uri.to_string authorization_endpoint);
              equal string ~msg:"token endpoint" "https://auth.example/token"
                (Uri.to_string token_endpoint);
              equal (option string) ~msg:"redirect"
                (Some "http://localhost:1455/auth/callback")
                (Option.map (fun u -> Uri.to_string u) redirect_uri);
              equal (list string) ~msg:"scope"
                [ "email"; "offline_access" ]
                scope;
              equal
                (list (pair string string))
                ~msg:"extra"
                [ ("prompt", "consent") ]
                extra;
              is_false ~msg:"pkce can be disabled" pkce
          | _ -> failf "expected OAuth2_authorization_code protocol");
      test "make rejects an empty label" (fun () ->
          expect_invalid_arg "empty label" (fun () ->
              Login.make ~id:(Login.Id.make "x") ~label:""
                Login.Protocol.Api_key));
    ]

let auth_declaration_group =
  group "auth declaration"
    [
      test "none accepts nothing" (fun () ->
          is_false ~msg:"none is not required" (Auth.required Auth.none);
          equal (list kind_value) ~msg:"none accepts no kinds" []
            (Auth.accepted_kinds Auth.none);
          equal (list string) ~msg:"none declares no env" []
            (List.map Env.name (Auth.env Auth.none));
          equal (list string) ~msg:"none declares no login" []
            (List.map
               (fun l -> Login.Id.to_string (Login.id l))
               (Auth.logins Auth.none)));
      test "accepted_kinds derives from declared methods, deduplicated"
        (fun () ->
          let auth =
            Auth.make
              ~env:[ Env.api_key "OPENAI_API_KEY"; Env.bearer "OPENAI_TOKEN" ]
              ~login:
                [
                  Login.api_key ();
                  Login.oauth2_device_code ~id:(Login.Id.make "device") ~client
                    ~device_endpoint:(uri "https://a/d")
                    ~token_endpoint:(uri "https://a/t") ();
                ]
              ()
          in
          (* env kinds first (Api_key, Bearer), then login kinds; the api-key
             login's Api_key is a duplicate and is dropped; the device login
             contributes OAuth. *)
          equal (list kind_value) ~msg:"accepted kinds in declaration order"
            [ Kind.Api_key; Kind.Bearer; Kind.OAuth ]
            (Auth.accepted_kinds auth));
      test "make defaults, ordering, and lookup" (fun () ->
          let auth =
            Auth.make
              ~env:[ Env.api_key "OPENAI_API_KEY"; Env.bearer "OPENAI_TOKEN" ]
              ~login:[ Login.api_key () ]
              ()
          in
          is_true ~msg:"declaring a method defaults to required"
            (Auth.required auth);
          is_false ~msg:"no method defaults to not required"
            (Auth.required (Auth.make ()));
          is_false ~msg:"optional auth keeps its methods"
            (Auth.required
               (Auth.make ~required:false ~login:[ Login.api_key () ] ()));
          equal (list string) ~msg:"env declaration order"
            [ "OPENAI_API_KEY"; "OPENAI_TOKEN" ]
            (List.map Env.name (Auth.env auth));
          equal (option string) ~msg:"login lookup by id" (Some "api-key")
            (Option.map
               (fun l -> Login.Id.to_string (Login.id l))
               (Auth.login_by_id auth (Login.Id.make "api-key")));
          equal (option string) ~msg:"login lookup miss" None
            (Option.map
               (fun l -> Login.Id.to_string (Login.id l))
               (Auth.login_by_id auth (Login.Id.make "missing"))));
      test "make raises on duplicates and impossible requirements" (fun () ->
          expect_invalid_arg "duplicate env names" (fun () ->
              Auth.make
                ~env:
                  [ Env.api_key "OPENAI_API_KEY"; Env.bearer "OPENAI_API_KEY" ]
                ());
          expect_invalid_arg "duplicate login ids" (fun () ->
              Auth.make
                ~login:[ Login.api_key (); Login.api_key ~label:"Other" () ]
                ());
          expect_invalid_arg "required with no method" (fun () ->
              Auth.make ~required:true ()));
    ]

(* Model. *)

let model_metadata_group =
  group "model metadata"
    [
      test "date construction, parsing, and comparison" (fun () ->
          let leap = Date.make ~year:2024 ~month:2 ~day:29 in
          equal string ~msg:"formats as ISO" "2024-02-29" (Date.to_string leap);
          equal (option date_value) ~msg:"parses" (Some leap)
            (Date.of_string "2024-02-29");
          equal (option date_value) ~msg:"invalid leap day is None" None
            (Date.of_string "2023-02-29");
          equal (option date_value) ~msg:"non-padded is None" None
            (Date.of_string "2024-2-29");
          equal (option date_value) ~msg:"year zero is None" None
            (Date.of_string "0000-01-01");
          expect_invalid_arg "invalid date" (fun () ->
              Date.make ~year:2023 ~month:2 ~day:29);
          expect_invalid_arg "month 13" (fun () ->
              Date.make ~year:2024 ~month:13 ~day:1);
          is_true ~msg:"chronological order"
            (Date.compare leap (Date.make ~year:2025 ~month:1 ~day:1) < 0);
          equal date_value ~msg:"jsont round-trip" leap
            (leap |> encode Date.jsont |> decode Date.jsont);
          assert_decode_error ~contains:"invalid date"
            "date jsont rejects garbage" Date.jsont (Json.string "2024/02/29"));
      test "month construction, parsing, and comparison" (fun () ->
          let cutoff = Month.make ~year:2025 ~month:8 in
          equal string ~msg:"formats as YYYY-MM" "2025-08"
            (Month.to_string cutoff);
          equal (option month_value) ~msg:"parses" (Some cutoff)
            (Month.of_string "2025-08");
          equal (option month_value) ~msg:"non-padded is None" None
            (Month.of_string "2025-8");
          equal (option month_value) ~msg:"day component is None" None
            (Month.of_string "2025-08-01");
          equal (option month_value) ~msg:"month zero is None" None
            (Month.of_string "2025-00");
          equal (option month_value) ~msg:"year zero is None" None
            (Month.of_string "0000-01");
          expect_invalid_arg "month 13" (fun () ->
              Month.make ~year:2025 ~month:13);
          is_true ~msg:"chronological order"
            (Month.compare cutoff (Month.make ~year:2026 ~month:1) < 0);
          equal month_value ~msg:"jsont round-trip" cutoff
            (cutoff |> encode Month.jsont |> decode Month.jsont);
          assert_decode_error ~contains:"invalid month"
            "month jsont rejects garbage" Month.jsont (Json.string "2025/08"));
      test "modality tags: built-ins, extensions, and ordering" (fun () ->
          let depth = Modality.extension "depth_map" in
          equal (option modality_value) ~msg:"built-in parses"
            (Some Modality.text)
            (Modality.of_string "text");
          equal (option modality_value) ~msg:"extension parses" (Some depth)
            (Modality.of_string "depth_map");
          equal string ~msg:"extension spelling" "depth_map"
            (Modality.to_string depth);
          equal (list string) ~msg:"sort by spelling"
            [ "depth_map"; "text"; "video" ]
            (modality_strings
               (List.sort Modality.compare
                  [ Modality.video; Modality.text; depth ]));
          equal (option modality_value) ~msg:"invalid is None" None
            (Modality.of_string "bad tag");
          expect_invalid_arg "reserved" (fun () -> Modality.extension "text");
          expect_invalid_arg "uppercase" (fun () -> Modality.extension "Text");
          equal modality_value ~msg:"jsont round-trip" depth
            (depth |> encode Modality.jsont |> decode Modality.jsont);
          assert_decode_error ~contains:"invalid modality"
            "modality rejects bad tag" Modality.jsont (Json.string "bad tag"));
      test "capability tags: built-ins, extensions, and ordering" (fun () ->
          let cu = Capability.extension "computer_use" in
          equal (option capability_value) ~msg:"built-in parses"
            (Some Capability.json_schema)
            (Capability.of_string "json_schema");
          equal (option capability_value) ~msg:"extension parses" (Some cu)
            (Capability.of_string "computer_use");
          equal (list string) ~msg:"sort by spelling"
            [ "computer_use"; "json_schema"; "tools" ]
            (capability_strings
               (List.sort Capability.compare
                  [ Capability.tools; cu; Capability.json_schema ]));
          equal (option capability_value) ~msg:"invalid is None" None
            (Capability.of_string "bad tag");
          expect_invalid_arg "reserved" (fun () -> Capability.extension "tools");
          expect_invalid_arg "leading digit" (fun () ->
              Capability.extension "2tools");
          equal capability_value ~msg:"jsont round-trip" cu
            (cu |> encode Capability.jsont |> decode Capability.jsont));
      test "status determines visibility and selectability" (fun () ->
          let m status = Model.make (llm "gpt-5") ~status () in
          is_true ~msg:"stable visible" (Model.visible (m Model.Stable));
          is_true ~msg:"stable selectable" (Model.selectable (m Model.Stable));
          is_true ~msg:"preview visible" (Model.visible (m Model.Preview));
          is_true ~msg:"preview selectable" (Model.selectable (m Model.Preview));
          is_true ~msg:"deprecated visible" (Model.visible (m Model.Deprecated));
          is_false ~msg:"deprecated not selectable"
            (Model.selectable (m Model.Deprecated));
          is_false ~msg:"unavailable hidden"
            (Model.visible (m (Model.Unavailable "retired")));
          is_false ~msg:"unavailable not selectable"
            (Model.selectable (m (Model.Unavailable "retired"))));
      test "accessors reflect construction" (fun () ->
          let released_on = Date.make ~year:2026 ~month:1 ~day:15 in
          let knowledge_cutoff = Month.make ~year:2025 ~month:8 in
          let rich =
            Model.make (llm "gpt-5") ~display_name:"GPT-5" ~family:"gpt-5"
              ~released_on ~knowledge_cutoff ~context_window:400_000
              ~max_output_tokens:128_000 ~default_reasoning:Reasoning.Medium
              ~supported_reasoning:
                [ Reasoning.High; Reasoning.Medium; Reasoning.Low ]
              ~input_modalities:[ Modality.image; Modality.text ]
              ~output_modalities:[ Modality.text ]
              ~capabilities:
                [
                  Capability.tools; Capability.json_schema; Capability.reasoning;
                ]
              ~status:Model.Preview ()
          in
          equal llm_model_value ~msg:"llm" (llm "gpt-5") (Model.llm rich);
          equal provider_value ~msg:"provider" openai (Model.provider rich);
          equal string ~msg:"api" "responses"
            (Llm_model.Api.id (Model.api rich));
          equal string ~msg:"id" "gpt-5" (Model.id rich);
          equal selector_value ~msg:"selector"
            (Selector.make ~provider:openai ~id:"gpt-5")
            (Model.selector rich);
          equal (option string) ~msg:"display name" (Some "GPT-5")
            (Model.display_name rich);
          equal (option string) ~msg:"family" (Some "gpt-5") (Model.family rich);
          equal (option date_value) ~msg:"released on" (Some released_on)
            (Model.released_on rich);
          equal (option month_value) ~msg:"knowledge cutoff"
            (Some knowledge_cutoff)
            (Model.knowledge_cutoff rich);
          equal (option int) ~msg:"context window" (Some 400_000)
            (Model.context_window rich);
          equal (option int) ~msg:"max output" (Some 128_000)
            (Model.max_output_tokens rich);
          equal (option reasoning_value) ~msg:"default reasoning"
            (Some Reasoning.Medium)
            (Model.default_reasoning rich);
          equal (list reasoning_value)
            ~msg:"supported reasoning is deterministic"
            [ Reasoning.Low; Reasoning.Medium; Reasoning.High ]
            (Model.supported_reasoning rich);
          equal (list string) ~msg:"input modalities sorted" [ "image"; "text" ]
            (modality_strings (Model.input_modalities rich));
          is_true ~msg:"input modality membership"
            (Model.has_input_modality Modality.image rich);
          equal (list string) ~msg:"capabilities sorted"
            [ "json_schema"; "reasoning"; "tools" ]
            (capability_strings (Model.capabilities rich));
          is_true ~msg:"capability membership"
            (Model.has_capability Capability.tools rich));
      test "make raises on invalid metadata" (fun () ->
          let m = llm "gpt-5" in
          expect_invalid_arg "empty display name" (fun () ->
              Model.make m ~display_name:"" ());
          expect_invalid_arg "empty family" (fun () ->
              Model.make m ~family:"" ());
          expect_invalid_arg "non-positive context window" (fun () ->
              Model.make m ~context_window:0 ());
          expect_invalid_arg "non-positive max output" (fun () ->
              Model.make m ~max_output_tokens:(-1) ());
          expect_invalid_arg "duplicate input modalities" (fun () ->
              Model.make m ~input_modalities:[ Modality.text; Modality.text ] ());
          expect_invalid_arg "duplicate capabilities" (fun () ->
              Model.make m
                ~capabilities:[ Capability.tools; Capability.tools ]
                ());
          expect_invalid_arg "duplicate reasoning" (fun () ->
              Model.make m
                ~supported_reasoning:[ Reasoning.Low; Reasoning.Low ]
                ());
          expect_invalid_arg "default reasoning not supported" (fun () ->
              Model.make m ~default_reasoning:Reasoning.Medium
                ~supported_reasoning:[ Reasoning.Low ] ()));
    ]

let rate ?input ?cached ?output ?cw5m ?cw1h () =
  Pricing.Rate.make ?input_per_million:input ?cached_input_per_million:cached
    ?output_per_million:output ?cache_write_5m_per_million:cw5m
    ?cache_write_1h_per_million:cw1h ()

let model_pricing_group =
  group "model pricing"
    [
      test "rate rejects negative and non-finite lanes" (fun () ->
          expect_invalid_arg "negative" (fun () -> rate ~input:(-0.01) ());
          expect_invalid_arg "nan" (fun () -> rate ~input:Float.nan ());
          expect_invalid_arg "infinite" (fun () ->
              rate ~input:Float.infinity ());
          equal (option float_value) ~msg:"input accessor" (Some 1.)
            (Pricing.Rate.input_per_million (rate ~input:1. ()));
          equal (option float_value) ~msg:"missing lane is unknown" None
            (Pricing.Rate.output_per_million (rate ~input:1. ())));
      test "tiered thresholds sort and select the greatest match" (fun () ->
          let default = rate ~input:1. ~output:2. () in
          let over_100 = rate ~input:3. () in
          let over_1000 = rate ~input:5. () in
          let pricing =
            Pricing.make default
              ~context_over:[ (1000, over_1000); (100, over_100) ]
          in
          equal (list int) ~msg:"thresholds sorted" [ 100; 1000 ]
            (List.map fst (Pricing.context_over pricing));
          let input_at ?context_tokens () =
            Pricing.Rate.input_per_million
              (Pricing.rate_for ?context_tokens pricing)
          in
          equal (option float_value) ~msg:"default without context" (Some 1.)
            (input_at ());
          equal (option float_value) ~msg:"threshold is strict" (Some 1.)
            (input_at ~context_tokens:100 ());
          equal (option float_value) ~msg:"lower threshold matches" (Some 3.)
            (input_at ~context_tokens:101 ());
          equal (option float_value) ~msg:"greatest match wins" (Some 5.)
            (input_at ~context_tokens:1001 ());
          expect_invalid_arg "negative threshold" (fun () ->
              Pricing.make default ~context_over:[ (-1, over_100) ]);
          expect_invalid_arg "duplicate threshold" (fun () ->
              Pricing.make default
                ~context_over:[ (100, over_100); (100, over_1000) ]);
          expect_invalid_arg "negative context tokens" (fun () ->
              Pricing.rate_for ~context_tokens:(-1) pricing));
      test "rate and pricing jsont round-trip and reject bad members" (fun () ->
          let r = rate ~input:1. ~cached:0.5 ~output:2. ~cw5m:1.25 () in
          equal bool ~msg:"rate jsont round-trip" true
            (Pricing.Rate.equal r
               (r |> encode Pricing.Rate.jsont |> decode Pricing.Rate.jsont));
          let pricing =
            Pricing.make r ~context_over:[ (1000, rate ~input:10. ()) ]
          in
          equal bool ~msg:"pricing jsont round-trip" true
            (Pricing.equal pricing
               (pricing |> encode Pricing.jsont |> decode Pricing.jsont));
          assert_decode_error ~contains:"finite" "rate rejects negative"
            Pricing.Rate.jsont
            (json_object [ ("input_per_million", Json.number (-1.)) ]);
          assert_decode_error "rate rejects unknown member" Pricing.Rate.jsont
            (json_object [ ("bogus", Json.number 1.) ]));
    ]

let usage ?(input = 0) ?(output = 0) ?(reasoning = 0) ?(cache_read = 0)
    ?(cache_write = 0) () =
  Usage.make ~input ~output ~reasoning ~cache_read ~cache_write ()

let model_cost_group =
  group "model cost"
    [
      test "a model without pricing has no cost" (fun () ->
          equal (option float_value) ~msg:"none" None
            (Model.cost (Model.make (llm "gpt-5") ()) Usage.zero));
      test "each lane bills against its rate" (fun () ->
          let model =
            Model.make (llm "gpt-5")
              ~pricing:
                (Pricing.make
                   (rate ~input:1. ~cached:0.5 ~output:2. ~cw5m:1.25 ()))
              ()
          in
          equal (option float_value) ~msg:"zero usage costs nothing" (Some 0.)
            (Model.cost model Usage.zero);
          (* input 1.0 + cache_read 0.5 + cache_write 1.25 + output 2.0 +
             reasoning 2.0 (billed at the output rate) = 6.75 *)
          equal (option float_value) ~msg:"lanes sum against their own rates"
            (Some 6.75)
            (Model.cost model
               (usage ~input:1_000_000 ~output:1_000_000 ~reasoning:1_000_000
                  ~cache_read:1_000_000 ~cache_write:1_000_000 ())));
      test "an unknown rate is never billed as free" (fun () ->
          let input_only =
            Model.make (llm "gpt-5")
              ~pricing:(Pricing.make (rate ~input:1. ()))
              ()
          in
          equal (option float_value) ~msg:"a spent lane with an unknown rate"
            None
            (Model.cost input_only
               (usage ~input:1_000_000 ~output:1_000_000 ()));
          equal (option float_value)
            ~msg:"an unspent unknown lane does not force None" (Some 1.)
            (Model.cost input_only (usage ~input:1_000_000 ~output:0 ())));
      test "the input side selects the price tier" (fun () ->
          let tiered =
            Model.make (llm "gpt-5")
              ~pricing:
                (Pricing.make (rate ~input:1. ())
                   ~context_over:[ (2_000_000, rate ~input:10. ()) ])
              ()
          in
          (* input_total 3M is over the 2M threshold, so the higher tier applies. *)
          equal (option float_value) ~msg:"tier chosen by input total"
            (Some 30.)
            (Model.cost tiered (usage ~input:3_000_000 ~output:0 ())));
      test "an overflowing input total still reports a cost" (fun () ->
          let model =
            Model.make (llm "gpt-5")
              ~pricing:(Pricing.make (rate ~input:1. ~output:2. ~cached:0.5 ()))
              ()
          in
          is_true ~msg:"overflow maps to max_int tier, not a crash"
            (Option.is_some
               (Model.cost model
                  (usage ~input:max_int ~cache_read:1 ~output:0 ()))));
      prop "cost is monotonic in a lane under a flat positive rate"
        (Gen.with_pp
           (fun ppf (a, b) -> Format.fprintf ppf "(%d,%d)" a b)
           (Gen.pair (Gen.int_range 0 5_000_000) (Gen.int_range 0 5_000_000)))
        (fun (a, b) ->
          let low, high = if a <= b then (a, b) else (b, a) in
          let model =
            Model.make (llm "gpt-5")
              ~pricing:(Pricing.make (rate ~input:2. ()))
              ()
          in
          match
            ( Model.cost model (usage ~input:low ()),
              Model.cost model (usage ~input:high ()) )
          with
          | Some cl, Some ch -> is_true ~msg:"monotonic" (cl <= ch)
          | _ -> fail "expected costs for both usages");
    ]

let model_rate_group =
  group "model rate per million"
    [
      test "a model without pricing reports no rate" (fun () ->
          let input, output =
            Model.rate_per_million (Model.make (llm "gpt-5") ())
          in
          equal (option float_value) ~msg:"no input rate" None input;
          equal (option float_value) ~msg:"no output rate" None output);
      test "the base-tier rate is exactly the isolated single-lane cost"
        (fun () ->
          let model =
            Model.make (llm "gpt-5")
              ~pricing:(Pricing.make (rate ~input:3. ~output:15. ()))
              ()
          in
          let input, output = Model.rate_per_million model in
          equal (option float_value) ~msg:"input rate" (Some 3.) input;
          equal (option float_value) ~msg:"output rate" (Some 15.) output;
          (* The base tier prices a request that stays below every threshold, so
             a one-million-token single lane costs exactly its reported rate. *)
          equal (option float_value) ~msg:"input lane cost is the input rate"
            input
            (Model.cost model (usage ~input:1_000_000 ~output:0 ()));
          equal (option float_value) ~msg:"output lane cost is the output rate"
            output
            (Model.cost model (usage ~input:0 ~output:1_000_000 ())));
      test "an unpriced lane stays None, agreeing with cost" (fun () ->
          let model =
            Model.make (llm "gpt-5")
              ~pricing:(Pricing.make (rate ~input:1. ()))
              ()
          in
          let input, output = Model.rate_per_million model in
          equal (option float_value) ~msg:"priced input" (Some 1.) input;
          equal (option float_value) ~msg:"unpriced output is unknown" None
            output;
          equal (option float_value) ~msg:"an unknown lane is never free" output
            (Model.cost model (usage ~input:0 ~output:1_000_000 ())));
      test "context tiers never shift the reported base rate" (fun () ->
          (* [rate_per_million] reads [Pricing.default], so an over-threshold
             tier with a different rate does not move it. *)
          let model =
            Model.make (llm "gpt-5")
              ~pricing:
                (Pricing.make
                   (rate ~input:2. ~output:8. ())
                   ~context_over:[ (1_000, rate ~input:20. ~output:80. ()) ])
              ()
          in
          let input, output = Model.rate_per_million model in
          equal (option float_value) ~msg:"base input" (Some 2.) input;
          equal (option float_value) ~msg:"base output" (Some 8.) output);
    ]

let model_jsont_group =
  group "model jsont"
    [
      test "a rich model round-trips through jsont" (fun () ->
          let rich =
            Model.make (llm "gpt-5") ~display_name:"GPT-5" ~family:"gpt-5"
              ~released_on:(Date.make ~year:2026 ~month:1 ~day:15)
              ~knowledge_cutoff:(Month.make ~year:2025 ~month:8)
              ~context_window:400_000 ~max_output_tokens:128_000
              ~default_reasoning:Reasoning.Medium
              ~supported_reasoning:[ Reasoning.Low; Reasoning.Medium ]
              ~input_modalities:[ Modality.text; Modality.image ]
              ~output_modalities:[ Modality.text ]
              ~capabilities:[ Capability.tools; Capability.reasoning ]
              ~pricing:(Pricing.make (rate ~input:1. ~output:2. ()))
              ~status:Model.Preview ()
          in
          equal model_value ~msg:"round-trip" rich
            (rich |> encode Model.jsont |> decode Model.jsont));
      test "defaults round-trip and unavailable carries its reason" (fun () ->
          let plain = Model.make (llm "gpt-5") () in
          equal model_value ~msg:"plain round-trip" plain
            (plain |> encode Model.jsont |> decode Model.jsont);
          let gone =
            Model.make (llm "gpt-5") ~status:(Model.Unavailable "retired") ()
          in
          equal model_value ~msg:"unavailable round-trip" gone
            (gone |> encode Model.jsont |> decode Model.jsont));
      test "jsont rejects malformed model objects" (fun () ->
          let base extra =
            json_object
              (( "model",
                 json_object
                   [
                     ("provider", Json.string "openai");
                     ("api", Json.string "responses");
                     ("id", Json.string "gpt-5");
                   ] )
              :: extra)
          in
          assert_decode_error ~contains:"member" "unknown member" Model.jsont
            (base [ ("bogus", Json.bool true) ]);
          assert_decode_error ~contains:"reason" "unavailable without reason"
            Model.jsont
            (base [ ("status", Json.string "unavailable") ]);
          assert_decode_error ~contains:"unavailable"
            "reason on non-unavailable" Model.jsont
            (base
               [
                 ("status", Json.string "stable");
                 ("status_reason", Json.string "why");
               ]));
    ]

(* Account. *)

let account_secret_group =
  group "account secret"
    [
      test "kinds, exposure, and state equality" (fun () ->
          let key = Secret.api_key "sk-test" in
          let token = Secret.bearer "bearer-test" in
          let oauth =
            Secret.oauth ~access_token:"access" ~refresh_token:"refresh"
              ~expires_at:123L ~account_id:"acct" ()
          in
          equal kind_value ~msg:"api key kind" Kind.Api_key (Secret.kind key);
          equal kind_value ~msg:"bearer kind" Kind.Bearer (Secret.kind token);
          equal kind_value ~msg:"oauth kind" Kind.OAuth (Secret.kind oauth);
          equal secret_view_value ~msg:"api key expose"
            (Api_key_secret "sk-test") (secret_view key);
          equal secret_view_value ~msg:"oauth expose"
            (OAuth_secret
               {
                 access_token = "access";
                 refresh_token = Some "refresh";
                 expires_at = Some 123L;
                 account_id = Some "acct";
               })
            (secret_view oauth);
          is_true ~msg:"api key state equality"
            (Secret.equal key (Secret.api_key "sk-test"));
          is_false ~msg:"oauth rotation changes state equality"
            (Secret.equal oauth (Secret.oauth ~access_token:"access-new" ()));
          is_true ~msg:"oauth carries a refresh token"
            (Secret.has_refresh_token oauth);
          is_false ~msg:"api key never carries a refresh token"
            (Secret.has_refresh_token key);
          equal (option int64) ~msg:"oauth expiry metadata" (Some 123L)
            (Secret.expires_at oauth));
      test "constructors reject empty or invalid-text material" (fun () ->
          expect_invalid_arg "api key" (fun () -> Secret.api_key "");
          expect_invalid_arg "bearer" (fun () -> Secret.bearer "");
          expect_invalid_arg "oauth access token" (fun () ->
              Secret.oauth ~access_token:"" ());
          expect_invalid_arg "oauth refresh token" (fun () ->
              Secret.oauth ~access_token:"a" ~refresh_token:"" ());
          expect_invalid_arg "oauth account id" (fun () ->
              Secret.oauth ~access_token:"a" ~account_id:"" ());
          let invalid = String.make 1 (Char.chr 0xFF) in
          expect_invalid_arg "API key UTF-8" (fun () -> Secret.api_key invalid);
          expect_invalid_arg "bearer UTF-8" (fun () -> Secret.bearer invalid);
          expect_invalid_arg "OAuth access UTF-8" (fun () ->
              Secret.oauth ~access_token:invalid ());
          expect_invalid_arg "OAuth refresh UTF-8" (fun () ->
              Secret.oauth ~access_token:"a" ~refresh_token:invalid ());
          expect_invalid_arg "OAuth account id UTF-8" (fun () ->
              Secret.oauth ~access_token:"a" ~account_id:invalid ());
          expect_invalid_arg "oauth negative expiry" (fun () ->
              Secret.oauth ~access_token:"a" ~expires_at:(-1L) ()));
    ]

let account_fingerprint_group =
  group "account fingerprint"
    [
      test "material fingerprints are a bounded suffix" (fun () ->
          equal (option string) ~msg:"api key last 4" (Some "f234")
            (Secret.fingerprint (Secret.api_key "sk-test-f234"));
          equal (option string) ~msg:"bearer last 4" (Some "edcb")
            (Secret.fingerprint (Secret.bearer "token-edcb"));
          equal (option string) ~msg:"5-char material has no fingerprint" None
            (Secret.fingerprint (Secret.api_key "short"));
          equal (option string) ~msg:"7-char material has no fingerprint" None
            (Secret.fingerprint (Secret.api_key "1234567"));
          equal (option string) ~msg:"8-char material has a fingerprint"
            (Some "5678")
            (Secret.fingerprint (Secret.api_key "12345678")));
      test "oauth fingerprint is a bounded suffix of the account id" (fun () ->
          (* The main defect this port fixes: the OAuth branch must not return the
             full account id. A stable account id yields only its last 4 chars. *)
          equal (option string) ~msg:"account id suffix, not the whole id"
            (Some "7654")
            (Secret.fingerprint
               (Secret.oauth ~access_token:"access-token-9999"
                  ~account_id:"acct-7654" ()));
          equal (option string) ~msg:"a short account id has no fingerprint"
            None
            (Secret.fingerprint
               (Secret.oauth ~access_token:"access-token-9999" ~account_id:"a1"
                  ()));
          equal (option string) ~msg:"falls back to the access token when no id"
            (Some "9999")
            (Secret.fingerprint
               (Secret.oauth ~access_token:"access-token-9999" ()));
          equal (option string) ~msg:"credential fingerprint is its secret's"
            (Some "f234")
            (Credential.fingerprint
               (Credential.make ~provider:openai ~source:Source.process
                  (Secret.api_key "sk-test-f234"))));
      prop "no fingerprint discloses more than 4 characters"
        (Gen.string_of ~size:(Gen.int_range 1 20)
           (Gen.of_list [ 'a'; 'b'; 'c'; '1'; '2'; '3'; '-'; '_' ]))
        (fun material ->
          let bounded = function
            | None -> true
            | Some s -> String.length s <= 4
          in
          is_true ~msg:"api key"
            (bounded (Secret.fingerprint (Secret.api_key material)));
          is_true ~msg:"bearer"
            (bounded (Secret.fingerprint (Secret.bearer material)));
          is_true ~msg:"oauth"
            (bounded
               (Secret.fingerprint
                  (Secret.oauth ~access_token:material ~account_id:material ()))));
    ]

let account_credential_group =
  group "account credential"
    [
      test "source construction, matching, and name" (fun () ->
          let env = Source.env "OPENAI_API_KEY" in
          (* The variant is private but matchable; destructuring is the way to
             branch on provenance. *)
          (match Source.process with
          | Source.Process -> ()
          | Source.Env _ | Source.Store _ -> failf "expected Process");
          (match env with
          | Source.Env name ->
              equal string ~msg:"env carries the variable" "OPENAI_API_KEY" name
          | Source.Process | Source.Store _ -> failf "expected Env");
          (match Source.store () with
          | Source.Store name ->
              equal name_value ~msg:"store carries the name" Name.default name
          | Source.Process | Source.Env _ -> failf "expected Store");
          equal (option string) ~msg:"process has no name" None
            (Source.name Source.process);
          equal (option string) ~msg:"env name is the variable"
            (Some "OPENAI_API_KEY") (Source.name env);
          equal (option string) ~msg:"default store name" (Some "default")
            (Source.name (Source.store ()));
          equal (option string) ~msg:"named store name" (Some "work")
            (Source.name (Source.store ~name:(Name.make "work") ()));
          expect_invalid_arg "empty env name" (fun () -> Source.env "");
          expect_invalid_arg "hyphen env name" (fun () -> Source.env "BAD-NAME"));
      test "credential name grammar" (fun () ->
          equal name_value ~msg:"default is 'default'" Name.default
            (Name.make "default");
          equal string ~msg:"storage spelling" "work.account"
            (Name.to_string (Name.make "work.account"));
          is_true ~msg:"names compare by spelling"
            (Name.compare (Name.make "personal") (Name.make "work") < 0);
          expect_invalid_arg "empty" (fun () -> Name.make "");
          expect_invalid_arg "slash" (fun () -> Name.make "bad/name"));
      test "credential accessors" (fun () ->
          let cred =
            Credential.make ~provider:openai
              ~source:(Source.store ~name:(Name.make "work") ())
              (Secret.oauth ~access_token:"access" ~refresh_token:"refresh" ())
          in
          equal provider_value ~msg:"provider" openai (Credential.provider cred);
          equal kind_value ~msg:"kind" Kind.OAuth (Credential.kind cred);
          equal source_value ~msg:"source"
            (Source.store ~name:(Name.make "work") ())
            (Credential.source cred);
          equal secret_view_value ~msg:"secret"
            (OAuth_secret
               {
                 access_token = "access";
                 refresh_token = Some "refresh";
                 expires_at = None;
                 account_id = None;
               })
            (secret_view (Credential.secret cred)));
      test "source jsont round-trips and is strict" (fun () ->
          List.iter
            (assert_round_trip ~equal:Source.equal "source round-trip"
               Source.jsont)
            [
              Source.process;
              Source.env "OPENAI_API_KEY";
              Source.store ~name:(Name.make "work") ();
            ];
          assert_decode_error "source rejects an unknown tag" Source.jsont
            (json_object [ ("type", Json.string "future") ]);
          assert_decode_error ~contains:"Unexpected member"
            "source rejects an unknown outer member" Source.jsont
            (json_object
               [ ("type", Json.string "process"); ("extra", Json.bool true) ]);
          assert_decode_error "source enforces environment-name syntax"
            Source.jsont
            (json_object
               [ ("type", Json.string "env"); ("name", Json.string "BAD-NAME") ]));
    ]

let credential_error_group =
  group "credential resolution error"
    [
      test "equality, jsont round-trip, and strict decoding" (fun () ->
          let error =
            Credential_error.Unsupported_credential_kind
              {
                source = Source.env "OPENAI_TOKEN";
                kind = Kind.Bearer;
                accepted = [ Kind.Api_key; Kind.OAuth ];
              }
          in
          is_true ~msg:"structural equality"
            (Credential_error.equal error
               (Credential_error.Unsupported_credential_kind
                  {
                    source = Source.env "OPENAI_TOKEN";
                    kind = Kind.Bearer;
                    accepted = [ Kind.Api_key; Kind.OAuth ];
                  }));
          assert_round_trip ~equal:Credential_error.equal
            "credential error round-trip" Credential_error.jsont error;
          let invalid_text =
            Credential_error.Invalid_credential_text
              { source = Source.env "OPENAI_API_KEY"; kind = Kind.Api_key }
          in
          is_true ~msg:"invalid text equality"
            (Credential_error.equal invalid_text
               (Credential_error.Invalid_credential_text
                  { source = Source.env "OPENAI_API_KEY"; kind = Kind.Api_key }));
          assert_round_trip ~equal:Credential_error.equal
            "invalid credential text round-trip" Credential_error.jsont
            invalid_text;
          assert_decode_error "credential error rejects an unknown tag"
            Credential_error.jsont
            (json_object [ ("type", Json.string "future") ]);
          assert_decode_error ~contains:"Unexpected member"
            "credential error rejects an unknown outer member"
            Credential_error.jsont
            (json_object
               [
                 ("type", Json.string "unsupported_credential_kind");
                 ("source", json_object [ ("type", Json.string "process") ]);
                 ("kind", Json.string "bearer");
                 ("accepted", Json.array [| Json.string "api_key" |]);
                 ("extra", Json.bool true);
               ]));
    ]

let account_store_group =
  group "account store"
    [
      test "lookup, ordering, and mutation" (fun () ->
          let work = Name.make "work" in
          let personal = Name.make "personal" in
          let store =
            Store.of_list
              [
                (openai, work, Secret.bearer "work-token");
                ( anthropic,
                  Name.default,
                  Secret.oauth ~access_token:"a-access" () );
                (openai, personal, Secret.api_key "sk-personal");
              ]
          in
          equal (list string) ~msg:"names sorted deterministically"
            [ "personal"; "work" ]
            (List.map Name.to_string (Store.names store ~provider:openai));
          equal (option secret_view_value) ~msg:"secret by name"
            (Some (Bearer_secret "work-token"))
            (Option.map secret_view
               (Store.secret store ~provider:openai ~name:work ()));
          equal (option source_value)
            ~msg:"credential projection carries the name"
            (Some (Source.store ~name:work ()))
            (Option.map Credential.source
               (Store.credential store ~provider:openai ~name:work ()));
          let replaced =
            Store.set ~provider:openai ~name:work (Secret.api_key "sk-new")
              store
          in
          equal (option secret_view_value) ~msg:"set replaces provider/name"
            (Some (Api_key_secret "sk-new"))
            (Option.map secret_view
               (Store.secret replaced ~provider:openai ~name:work ()));
          equal (list string) ~msg:"remove deletes one binding" [ "personal" ]
            (List.map Name.to_string
               (Store.names
                  (store |> Store.remove ~provider:openai ~name:work)
                  ~provider:openai));
          expect_invalid_arg "of_list rejects duplicate provider/name"
            (fun () ->
              Store.of_list
                [
                  (openai, Name.default, Secret.api_key "one");
                  (openai, Name.default, Secret.api_key "two");
                ]));
      test "jsont persists secret material and round-trips" (fun () ->
          let store =
            Store.empty
            |> Store.set ~provider:openai (Secret.api_key "sk-store")
            |> Store.set ~provider:anthropic
                 (Secret.oauth ~access_token:"access-store"
                    ~refresh_token:"refresh-store" ())
          in
          let json = encode Store.jsont store in
          is_true ~msg:"store JSON carries the api key"
            (json_contains_string "sk-store" json);
          is_true ~msg:"store JSON carries the oauth token"
            (json_contains_string "access-store" json);
          equal (list string) ~msg:"round-trip preserves names"
            [ "openai:default"; "anthropic:default" ]
            (json |> decode Store.jsont |> Store.bindings
            |> List.map (fun (p, n, _) ->
                Llm_provider.id p ^ ":" ^ Name.to_string n)
            |> List.sort String.compare |> List.rev));
      test "jsont rejects unsupported and contradictory shapes" (fun () ->
          let store_json ?(version = 1) credentials =
            json_object
              [
                ("version", Json.int version);
                ("credentials", json_object credentials);
              ]
          in
          assert_decode_error ~contains:"unsupported account store version"
            "unknown version" Store.jsont (store_json ~version:2 []);
          (* Contradictory per-kind fields: an api_key secret carrying an oauth
             field is rejected rather than silently accepted. *)
          assert_decode_error ~contains:"Unexpected member"
            "contradictory per-kind fields" Store.jsont
            (store_json
               [
                 ( "openai",
                   json_object
                     [
                       ( "default",
                         json_object
                           [
                             ("kind", Json.string "api_key");
                             ("api_key", Json.string "sk");
                             ("access_token", Json.string "leak");
                           ] );
                     ] );
               ]);
          let marker = "LEAKMARKER-CREDENTIAL-KIND" in
          let malformed =
            store_json
              [
                ( "openai",
                  json_object
                    [
                      ("default", json_object [ ("kind", Json.string marker) ]);
                    ] );
              ]
          in
          match Json.decode Store.jsont malformed with
          | Ok _ -> fail "unknown credential kind decoded"
          | Error message ->
              is_false ~msg:"decoder error is payload-free"
                (String.includes ~affix:marker message));
      test "jsont rejects a repeated provider or credential name" (fun () ->
          let secret key =
            json_object
              [ ("kind", Json.string "api_key"); ("api_key", Json.string key) ]
          in
          let store_json credentials =
            json_object
              [
                ("version", Json.int 1); ("credentials", json_object credentials);
              ]
          in
          (* Two entries for one key are a corrupt store, not a last-one-wins
             merge: silently dropping one would discard a live credential. *)
          assert_decode_error ~contains:"duplicate member"
            "a repeated provider is not collapsed" Store.jsont
            (store_json
               [
                 ("openai", json_object [ ("default", secret "sk-one") ]);
                 ("openai", json_object [ ("default", secret "sk-two") ]);
               ]);
          assert_decode_error ~contains:"duplicate member"
            "a repeated credential name is not collapsed" Store.jsont
            (store_json
               [
                 ( "openai",
                   json_object
                     [
                       ("default", secret "sk-one"); ("default", secret "sk-two");
                     ] );
               ]));
    ]

let account_facts_group =
  group "account profile/org/problem"
    [
      test "profile and org construction" (fun () ->
          let profile =
            Profile.make ~id:"acct" ~email:"user@example.com" ~name:"User" ()
          in
          equal profile_value ~msg:"profile equality" profile
            (Profile.make ~id:"acct" ~email:"user@example.com" ~name:"User" ());
          expect_invalid_arg "profile needs a field" (fun () -> Profile.make ());
          expect_invalid_arg "profile empty field" (fun () ->
              Profile.make ~email:"" ());
          let org = Org.make ~id:"org" ~name:"Engineering" () in
          equal org_value ~msg:"org equality" org
            (Org.make ~id:"org" ~name:"Engineering" ());
          expect_invalid_arg "org empty id" (fun () -> Org.make ~id:"" ());
          expect_invalid_arg "org empty name" (fun () ->
              Org.make ~id:"org" ~name:"" ()));
      test "problem vocabulary, ordering, and classification" (fun () ->
          let builtins =
            [
              Problem.Invalid_credential;
              Problem.Expired_credential;
              Problem.Refresh_failed;
              Problem.Revoked;
              Problem.Wrong_account;
              Problem.Wrong_organization;
              Problem.Rate_limited;
              Problem.Quota_exceeded;
              Problem.Network;
              Problem.Unsupported;
            ]
          in
          List.iter
            (fun p ->
              equal (option problem_value)
                ~msg:("round-trips " ^ Problem.to_string p)
                (Some p)
                (Problem.of_string (Problem.to_string p)))
            builtins;
          equal (option problem_value) ~msg:"unknown valid label"
            (Some (Problem.other "maintenance_window"))
            (Problem.of_string "maintenance_window");
          equal (option problem_value) ~msg:"invalid label is None" None
            (Problem.of_string "Bad-Label");
          expect_invalid_arg "other rejects reserved" (fun () ->
              Problem.other "network");
          List.iter
            (fun p ->
              is_true ~msg:("fatal " ^ Problem.to_string p) (Problem.fatal p))
            [
              Problem.Invalid_credential;
              Problem.Expired_credential;
              Problem.Refresh_failed;
              Problem.Revoked;
              Problem.Wrong_account;
              Problem.Wrong_organization;
            ];
          List.iter
            (fun p ->
              is_false
                ~msg:("non-fatal " ^ Problem.to_string p)
                (Problem.fatal p))
            [
              Problem.Rate_limited;
              Problem.Quota_exceeded;
              Problem.Network;
              Problem.Unsupported;
              Problem.other "x";
            ];
          is_true ~msg:"network transient" (Problem.transient Problem.Network);
          is_false ~msg:"quota not transient"
            (Problem.transient Problem.Quota_exceeded));
    ]

let account_status_group =
  group "account status"
    [
      test "phase and connectivity" (fun () ->
          let cred =
            Credential.make ~provider:openai
              ~source:(Source.env "OPENAI_API_KEY")
              (Secret.api_key "sk-secret-value")
          in
          let missing = Account.missing ~provider:openai in
          equal phase_value ~msg:"missing phase" Account.Phase.Missing
            (Account.phase missing);
          is_false ~msg:"missing is not connected" (Account.connected missing);
          equal (option source_value) ~msg:"missing source" None
            (Account.source missing);
          let present = Account.present cred in
          equal phase_value ~msg:"present phase" Account.Phase.Unchecked
            (Account.phase present);
          is_true ~msg:"present is connected" (Account.connected present);
          equal (option source_value) ~msg:"present source"
            (Some (Source.env "OPENAI_API_KEY"))
            (Account.source present);
          equal (option kind_value) ~msg:"present kind" (Some Kind.Api_key)
            (Account.credential_kind present);
          is_false ~msg:"present pp hides the secret"
            (String.includes ~affix:"sk-secret-value"
               (Format.asprintf "%a" Account.pp present));
          equal phase_value ~msg:"checked clean is ready" Account.Phase.Ready
            (Account.phase (Account.checked cred ()));
          is_true ~msg:"checked clean is connected"
            (Account.connected (Account.checked cred ()));
          equal phase_value ~msg:"transient degrades" Account.Phase.Degraded
            (Account.phase
               (Account.checked cred ~problems:[ Problem.Network ] ()));
          is_true ~msg:"degraded is still connected"
            (Account.connected
               (Account.checked cred ~problems:[ Problem.Network ] ()));
          equal phase_value ~msg:"fatal blocks" Account.Phase.Blocked
            (Account.phase
               (Account.checked cred ~problems:[ Problem.Revoked ] ()));
          is_false ~msg:"blocked is not connected"
            (Account.connected
               (Account.checked cred ~problems:[ Problem.Revoked ] ())));
      test "phase jsont round-trips and rejects unknown spellings" (fun () ->
          List.iter
            (fun phase ->
              equal phase_value
                ~msg:("round-trip " ^ Account.Phase.to_string phase)
                phase
                (phase |> encode Account.Phase.jsont
               |> decode Account.Phase.jsont))
            Account.Phase.[ Missing; Unchecked; Ready; Degraded; Blocked ];
          assert_decode_error "phase rejects unknown spelling"
            Account.Phase.jsont (Json.string "connected"));
      test "checked facts: dedup, ordering, and negative timestamp" (fun () ->
          let cred =
            Credential.make ~provider:openai ~source:Source.process
              (Secret.api_key "sk-test-1234")
          in
          let checked =
            Account.checked cred ~at:42L
              ~profile:(Profile.make ~email:"user@example.com" ())
              ~org:(Org.make ~id:"org" ())
              ~problems:
                [
                  Problem.Network;
                  Problem.Rate_limited;
                  Problem.Network;
                  Problem.other "maintenance";
                ]
              ~models:[ "gpt-b"; "gpt-a"; "gpt-b" ]
              ()
          in
          equal (option int64) ~msg:"timestamp" (Some 42L)
            (Account.checked_at checked);
          equal (list string) ~msg:"problems sorted and deduplicated"
            [ "maintenance"; "network"; "rate_limited" ]
            (List.map Problem.to_string (Account.problems checked));
          equal
            (option (list string))
            ~msg:"models sorted and deduplicated"
            (Some [ "gpt-a"; "gpt-b" ])
            (Account.models checked);
          is_true ~msg:"listed model available"
            (Account.model_available checked "gpt-a" = `Available);
          is_true ~msg:"unlisted model unavailable"
            (Account.model_available checked "gpt-z" = `Unavailable);
          is_true ~msg:"unchecked model availability is unknown"
            (Account.model_available (Account.present cred) "gpt-a" = `Unknown);
          expect_invalid_arg "negative timestamp" (fun () ->
              Account.checked cred ~at:(-1L) ()));
    ]

let account_codec_group =
  group "account codecs and outcomes"
    [
      test "account jsont preserves rich facts without credential material"
        (fun () ->
          let credential =
            Credential.make ~provider:openai
              ~source:(Source.store ~name:(Name.make "work") ())
              (Secret.oauth ~access_token:"secret-access-value"
                 ~refresh_token:"secret-refresh-value"
                 ~account_id:"account-1234" ())
          in
          let rich =
            Account.checked credential ~at:42L
              ~profile:
                (Profile.make ~id:"acct" ~email:"user@example.com" ~name:"User"
                   ())
              ~org:(Org.make ~id:"org" ~name:"Engineering" ())
              ~problems:[ Problem.Network; Problem.other "maintenance" ]
              ~models:[ "gpt-5"; "gpt-5-mini" ] ()
          in
          let values =
            [
              Account.missing ~provider:anthropic;
              Account.present credential;
              rich;
            ]
          in
          List.iter
            (assert_round_trip ~equal:Account.equal "account round-trip"
               Account.jsont)
            values;
          let json = encode Account.jsont rich in
          is_false ~msg:"account JSON excludes access-token material"
            (json_contains_string "secret-access-value" json);
          is_false ~msg:"account JSON excludes refresh-token material"
            (json_contains_string "secret-refresh-value" json);
          is_true ~msg:"account JSON retains only the bounded fingerprint"
            (json_contains_string "1234" json));
      test "account jsont rejects invalid status shapes" (fun () ->
          let provider = encode Llm_provider.jsont openai in
          assert_decode_error "account rejects an unknown status" Account.jsont
            (json_object
               [ ("provider", provider); ("status", Json.string "future") ]);
          assert_decode_error ~contains:"Unexpected member"
            "account rejects an unknown member" Account.jsont
            (json_object
               [
                 ("provider", provider);
                 ("status", Json.string "missing");
                 ("extra", Json.bool true);
               ]);
          assert_decode_error ~contains:"exactly 4"
            "account rejects an invalid fingerprint" Account.jsont
            (json_object
               [
                 ("provider", provider);
                 ("status", Json.string "present");
                 ("source", json_object [ ("type", Json.string "process") ]);
                 ("credential_kind", Json.string "api_key");
                 ("fingerprint", Json.string "bad");
               ]));
      test "discovery jsont preserves known and failed routes" (fun () ->
          let account = Account.missing ~provider:openai in
          let error =
            Credential_error.Unsupported_credential_kind
              {
                source = Source.process;
                kind = Kind.Bearer;
                accepted = [ Kind.Api_key ];
              }
          in
          let values =
            [
              Account.Discovery.Known account;
              Account.Discovery.Resolution_failed
                { provider = anthropic; error };
            ]
          in
          List.iter
            (assert_round_trip ~equal:Account.Discovery.equal
               "discovery round-trip" Account.Discovery.jsont)
            values;
          equal provider_value ~msg:"known provider" openai
            (Account.Discovery.provider (List.hd values));
          equal provider_value ~msg:"failed provider" anthropic
            (Account.Discovery.provider (List.nth values 1));
          assert_decode_error "discovery rejects an unknown tag"
            Account.Discovery.jsont
            (json_object [ ("type", Json.string "future") ]);
          assert_decode_error ~contains:"Unexpected member"
            "discovery rejects an unknown outer member" Account.Discovery.jsont
            (json_object
               [
                 ("type", Json.string "known");
                 ("account", encode Account.jsont account);
                 ("extra", Json.bool true);
               ]));
      test "logout jsont preserves post-state and revocation settlement"
        (fun () ->
          let current =
            Account.Discovery.Known (Account.missing ~provider:openai)
          in
          let values : Account.Logout.t list =
            [
              Account.Logout.{ current; revocation = None };
              Account.Logout.
                { current; revocation = Some Account.Logout.Not_stored };
              Account.Logout.
                {
                  current;
                  revocation =
                    Some
                      (Account.Logout.Settled
                         {
                           remote = Account.Logout.Revoked;
                           local = Account.Logout.Removed;
                         });
                };
              Account.Logout.
                {
                  current;
                  revocation =
                    Some
                      (Account.Logout.Settled
                         {
                           remote = Account.Logout.Unsupported;
                           local = Account.Logout.Superseded;
                         });
                };
              Account.Logout.
                {
                  current;
                  revocation =
                    Some
                      (Account.Logout.Settled
                         {
                           remote = Account.Logout.Failed Problem.Network;
                           local = Account.Logout.Removed;
                         });
                };
            ]
          in
          List.iter
            (assert_round_trip ~equal:Account.Logout.equal "logout round-trip"
               Account.Logout.jsont)
            values;
          assert_decode_error ~contains:"Unexpected member"
            "logout rejects an unknown member" Account.Logout.jsont
            (json_object
               [
                 ("current", encode Account.Discovery.jsont current);
                 ("extra", Json.bool true);
               ]);
          assert_decode_error "logout rejects an unknown remote tag"
            Account.Logout.jsont
            (json_object
               [
                 ("current", encode Account.Discovery.jsont current);
                 ( "revocation",
                   json_object
                     [
                       ("type", Json.string "settled");
                       ("remote", json_object [ ("type", Json.string "future") ]);
                       ("local", Json.string "removed");
                     ] );
               ]));
    ]

(* Declaration (top-level t). *)

let declaration_group =
  group "declaration"
    [
      test "accessors reflect construction" (fun () ->
          let model = Model.make (llm "gpt-5") () in
          let mini = Model.make (llm "gpt-5-mini") () in
          let auth = Auth.make ~env:[ Env.api_key "OPENAI_API_KEY" ] () in
          let decl =
            Provider.make openai ~display_name:"OpenAI" ~auth
              ~default_model:(Model.make (llm "gpt-5") ())
              [ mini; model ]
          in
          equal provider_value ~msg:"id" openai (Provider.id decl);
          equal (option string) ~msg:"display name" (Some "OpenAI")
            (Provider.display_name decl);
          equal (list kind_value) ~msg:"auth is carried" [ Kind.Api_key ]
            (Auth.accepted_kinds (Provider.auth decl));
          equal (list string) ~msg:"models keep declaration order"
            [ "gpt-5-mini"; "gpt-5" ]
            (List.map Model.id (Provider.models decl));
          equal (option llm_model_value) ~msg:"default is a member of models"
            (Some (llm "gpt-5"))
            (Option.map Model.llm (Provider.default_model decl));
          equal (option llm_model_value) ~msg:"model lookup"
            (Some (llm "gpt-5-mini"))
            (Option.map Model.llm (Provider.model decl (llm "gpt-5-mini")));
          equal (option llm_model_value) ~msg:"model lookup miss" None
            (Option.map Model.llm (Provider.model decl (llm "gpt-4"))));
      test "make raises on invalid declarations" (fun () ->
          let model = Model.make (llm "gpt-5") () in
          expect_invalid_arg "empty display name" (fun () ->
              Provider.make openai ~display_name:"" [ model ]);
          expect_invalid_arg "foreign model" (fun () ->
              Provider.make openai
                [
                  Model.make (llm ~provider:anthropic ~api:messages "claude") ();
                ]);
          expect_invalid_arg "duplicate model identity" (fun () ->
              Provider.make openai [ model; Model.make (llm "gpt-5") () ]);
          expect_invalid_arg "duplicate model id" (fun () ->
              Provider.make openai
                [ model; Model.make (llm ~api:messages "gpt-5") () ]);
          expect_invalid_arg "undeclared default" (fun () ->
              Provider.make openai
                ~default_model:(Model.make (llm "gpt-5-mini") ())
                [ model ]);
          expect_invalid_arg "unselectable default" (fun () ->
              Provider.make openai
                ~default_model:(Model.make (llm "gpt-5") ())
                [ Model.make (llm "gpt-5") ~status:(Model.Unavailable "x") () ]));
      test "resolve_dynamic validates provider and id, performing no I/O"
        (fun () ->
          let plain = Provider.make openai [ Model.make (llm "gpt-5") () ] in
          equal (option llm_model_value) ~msg:"no rule is None" None
            (Option.map Model.llm
               (Provider.resolve_dynamic plain "weights.gguf"));
          let dynamic =
            Provider.make openai
              ~dynamic:(fun id ->
                if String.ends_with ~suffix:".gguf" id then
                  Some (Model.make (llm id) ())
                else None)
              [ Model.make (llm "gpt-5") () ]
          in
          equal (option llm_model_value) ~msg:"synthesizes a model"
            (Some (llm "weights.gguf"))
            (Option.map Model.llm
               (Provider.resolve_dynamic dynamic "weights.gguf"));
          equal (option llm_model_value) ~msg:"declines" None
            (Option.map Model.llm
               (Provider.resolve_dynamic dynamic "weights.bin"));
          (* An inconsistent rule is a provider-author bug: the check raises. *)
          expect_invalid_arg "foreign-provider synthesis raises" (fun () ->
              Provider.resolve_dynamic
                (Provider.make openai
                   ~dynamic:(fun _ ->
                     Some
                       (Model.make
                          (llm ~provider:anthropic ~api:messages "claude")
                          ()))
                   [])
                "claude");
          expect_invalid_arg "different-id synthesis raises" (fun () ->
              Provider.resolve_dynamic
                (Provider.make openai
                   ~dynamic:(fun _ -> Some (Model.make (llm "other") ()))
                   [])
                "requested"));
    ]

(* resolve_credential — the precedence ladder. *)

let openai_decl =
  Provider.make openai
    ~auth:(Auth.make ~env:[ Env.api_key "OPENAI_API_KEY" ] ())
    [ Model.make (llm "gpt-5") () ]

let resolve ?name decl ~process ~environment ~store =
  Provider.resolve_credential decl ~process ~environment ~store ?name ()

let ok_cred msg = function
  | Ok v -> Option.map cred_view v
  | Error e -> failf "%s: %s" msg (Provider.Credential_error.message e)

let resolve_credential_group =
  group "resolve credential"
    [
      test "process wins over environment and store" (fun () ->
          let process =
            [
              Credential.make ~provider:openai ~source:Source.process
                (Secret.api_key "sk-process");
            ]
          in
          equal (option cred_view_value) ~msg:"process credential wins"
            (Some
               {
                 cv_provider = openai;
                 cv_source = Source.process;
                 cv_secret = Api_key_secret "sk-process";
               })
            (ok_cred "process"
               (resolve openai_decl ~process
                  ~environment:[ ("OPENAI_API_KEY", "sk-env") ]
                  ~store:
                    (Store.empty
                    |> Store.set ~provider:openai (Secret.api_key "sk-store")))));
      test "a process credential for another provider is skipped" (fun () ->
          equal (option cred_view_value) ~msg:"falls through to environment"
            (Some
               {
                 cv_provider = openai;
                 cv_source = Source.env "OPENAI_API_KEY";
                 cv_secret = Api_key_secret "sk-env";
               })
            (ok_cred "env"
               (resolve openai_decl
                  ~process:
                    [
                      Credential.make ~provider:anthropic ~source:Source.process
                        (Secret.api_key "sk-other");
                    ]
                  ~environment:[ ("OPENAI_API_KEY", "sk-env") ]
                  ~store:Store.empty)));
      test "an empty environment value is skipped" (fun () ->
          equal (option cred_view_value) ~msg:"falls through to store"
            (Some
               {
                 cv_provider = openai;
                 cv_source = Source.store ();
                 cv_secret = Api_key_secret "sk-store";
               })
            (ok_cred "store"
               (resolve openai_decl ~process:[]
                  ~environment:[ ("OPENAI_API_KEY", "") ]
                  ~store:
                    (Store.empty
                    |> Store.set ~provider:openai (Secret.api_key "sk-store")))));
      test "invalid environment text is a payload-free resolution error"
        (fun () ->
          let invalid = String.make 1 (Char.chr 0xFF) in
          match
            resolve openai_decl ~process:[]
              ~environment:[ ("OPENAI_API_KEY", invalid) ]
              ~store:
                (Store.empty
                |> Store.set ~provider:openai (Secret.api_key "sk-store"))
          with
          | Error
              (Provider.Credential_error.Invalid_credential_text
                 { source = Source.Env name; kind = Kind.Api_key }) ->
              equal string ~msg:"source names the declaration" "OPENAI_API_KEY"
                name;
              is_false ~msg:"error message carries no invalid bytes"
                (String.includes ~affix:invalid
                   (Provider.Credential_error.message
                      (Provider.Credential_error.Invalid_credential_text
                         {
                           source = Source.env "OPENAI_API_KEY";
                           kind = Kind.Api_key;
                         })))
          | Error error ->
              failf "expected Invalid_credential_text, got %s"
                (Provider.Credential_error.message error)
          | Ok _ -> fail "invalid environment text should not fall through");
      test "declared environment order breaks ties" (fun () ->
          let decl =
            Provider.make openai
              ~auth:
                (Auth.make
                   ~env:
                     [ Env.api_key "OPENAI_API_KEY"; Env.bearer "OPENAI_TOKEN" ]
                   ())
              [ Model.make (llm "gpt-5") () ]
          in
          equal (option cred_view_value) ~msg:"first declared env wins"
            (Some
               {
                 cv_provider = openai;
                 cv_source = Source.env "OPENAI_API_KEY";
                 cv_secret = Api_key_secret "sk-first";
               })
            (ok_cred "env order"
               (resolve decl ~process:[]
                  ~environment:
                    [
                      ("OPENAI_TOKEN", "second"); ("OPENAI_API_KEY", "sk-first");
                    ]
                  ~store:Store.empty)));
      test "each declared env kind decodes to its secret shape" (fun () ->
          let decl kind_env =
            Provider.make openai
              ~auth:(Auth.make ~env:[ kind_env ] ())
              [ Model.make (llm "gpt-5") () ]
          in
          equal (option cred_view_value) ~msg:"bearer env decodes"
            (Some
               {
                 cv_provider = openai;
                 cv_source = Source.env "OPENAI_TOKEN";
                 cv_secret = Bearer_secret "session-token";
               })
            (ok_cred "bearer"
               (resolve
                  (decl (Env.bearer "OPENAI_TOKEN"))
                  ~process:[]
                  ~environment:[ ("OPENAI_TOKEN", "session-token") ]
                  ~store:Store.empty));
          equal (option cred_view_value) ~msg:"oauth env decodes"
            (Some
               {
                 cv_provider = openai;
                 cv_source = Source.env "OPENAI_ACCESS_TOKEN";
                 cv_secret =
                   OAuth_secret
                     {
                       access_token = "access-token";
                       refresh_token = None;
                       expires_at = None;
                       account_id = None;
                     };
               })
            (ok_cred "oauth"
               (resolve
                  (decl (Env.oauth_access_token "OPENAI_ACCESS_TOKEN"))
                  ~process:[]
                  ~environment:[ ("OPENAI_ACCESS_TOKEN", "access-token") ]
                  ~store:Store.empty)));
      test "the named store slot resolves last" (fun () ->
          equal (option cred_view_value) ~msg:"named store slot"
            (Some
               {
                 cv_provider = openai;
                 cv_source = Source.store ~name:(Name.make "work") ();
                 cv_secret = Api_key_secret "sk-work";
               })
            (ok_cred "store name"
               (resolve openai_decl ~name:(Name.make "work") ~process:[]
                  ~environment:[]
                  ~store:
                    (Store.empty
                    |> Store.set ~provider:openai ~name:(Name.make "work")
                         (Secret.api_key "sk-work")))));
      test "no candidate resolves to Ok None" (fun () ->
          equal (option cred_view_value) ~msg:"nothing resolves" None
            (ok_cred "none"
               (resolve openai_decl ~process:[] ~environment:[]
                  ~store:Store.empty)));
      test
        "an unsupported high-precedence candidate errors, never falls through"
        (fun () ->
          (* A process bearer is not accepted; resolution errors rather than using
             the valid lower-precedence env credential. *)
          match
            resolve openai_decl
              ~process:
                [
                  Credential.make ~provider:openai ~source:Source.process
                    (Secret.bearer "bearer-process");
                ]
              ~environment:[ ("OPENAI_API_KEY", "sk-env") ]
              ~store:Store.empty
          with
          | Error
              (Provider.Credential_error.Unsupported_credential_kind
                 { kind; accepted; source }) ->
              equal kind_value ~msg:"rejected kind" Kind.Bearer kind;
              equal (list kind_value) ~msg:"accepted kinds" [ Kind.Api_key ]
                accepted;
              is_true ~msg:"source is the process candidate"
                (match source with
                | Source.Process -> true
                | Source.Env _ | Source.Store _ -> false)
          | _ -> failf "expected Unsupported_credential_kind");
      test "Auth.none accepts nothing" (fun () ->
          let decl = Provider.make openai [ Model.make (llm "gpt-5") () ] in
          equal (option cred_view_value) ~msg:"no candidates is Ok None" None
            (ok_cred "none-auth-empty"
               (resolve decl ~process:[] ~environment:[] ~store:Store.empty));
          match
            resolve decl
              ~process:
                [
                  Credential.make ~provider:openai ~source:Source.process
                    (Secret.api_key "sk-anything");
                ]
              ~environment:[] ~store:Store.empty
          with
          | Error
              (Provider.Credential_error.Unsupported_credential_kind
                 { accepted; _ }) ->
              equal (list kind_value)
                ~msg:"empty accepted set rejects everything" [] accepted
          | _ -> failf "expected Unsupported_credential_kind for Auth.none");
      prop "the highest-precedence rung commits, never falling through"
        (Gen.with_pp Kind.pp
           (Gen.of_list [ Kind.Api_key; Kind.Bearer; Kind.OAuth ]))
        (fun kind ->
          let secret =
            match kind with
            | Kind.Api_key -> Secret.api_key "sk-process-1"
            | Kind.Bearer -> Secret.bearer "bearer-process"
            | Kind.OAuth -> Secret.oauth ~access_token:"access-process" ()
          in
          let process =
            [ Credential.make ~provider:openai ~source:Source.process secret ]
          in
          (* openai_decl accepts only Api_key; a valid env and store are present. *)
          match
            resolve openai_decl ~process
              ~environment:[ ("OPENAI_API_KEY", "sk-env") ]
              ~store:
                (Store.empty
                |> Store.set ~provider:openai (Secret.api_key "sk-store"))
          with
          | Ok (Some c) ->
              (* An accepted process credential resolves; it is never the env or
                 store secret. *)
              is_true ~msg:"only the accepted kind resolves"
                (Kind.equal kind Kind.Api_key);
              is_true ~msg:"the process rung committed"
                (match Credential.source c with
                | Source.Process -> true
                | Source.Env _ | Source.Store _ -> false)
          | Error _ ->
              is_false ~msg:"an accepted kind must resolve"
                (Kind.equal kind Kind.Api_key)
          | Ok None -> fail "resolution fell through to None");
    ]

(* Catalog. *)

let catalog_group =
  group "catalog"
    [
      test "make raises on a duplicate provider" (fun () ->
          expect_invalid_arg "duplicate provider" (fun () ->
              Catalog.make
                [
                  Provider.make openai [ Model.make (llm "gpt-5") () ];
                  Provider.make openai [];
                ]));
      test "listing preserves order and hides non-visible models by default"
        (fun () ->
          let hidden =
            Model.make (llm "gpt-4") ~status:(Model.Unavailable "retired") ()
          in
          let openai_decl =
            Provider.make openai
              [
                Model.make (llm "gpt-5-mini") ();
                hidden;
                Model.make (llm "gpt-5") ();
              ]
          in
          let anthropic_decl =
            Provider.make anthropic
              [
                Model.make (llm ~provider:anthropic ~api:messages "claude-3") ();
              ]
          in
          let catalog = Catalog.make [ openai_decl; anthropic_decl ] in
          equal (list provider_value) ~msg:"declaration order"
            [ openai; anthropic ]
            (List.map Provider.id (Catalog.declarations catalog));
          equal (list string) ~msg:"visible listing hides unavailable"
            [ "gpt-5-mini"; "gpt-5"; "claude-3" ]
            (List.map Model.id (Catalog.models catalog));
          equal (list string) ~msg:"include_hidden lists everything"
            [ "gpt-5-mini"; "gpt-4"; "gpt-5"; "claude-3" ]
            (List.map Model.id (Catalog.models ~include_hidden:true catalog));
          equal
            (result (list string) catalog_error_value)
            ~msg:"models_for"
            (Ok [ "gpt-5-mini"; "gpt-5" ])
            (Catalog.models_for catalog openai |> Result.map (List.map Model.id));
          equal
            (result (list string) catalog_error_value)
            ~msg:"models_for unknown provider"
            (Error
               (Catalog.Error.Unknown_provider
                  { provider = google; known = [ openai; anthropic ] }))
            (Catalog.models_for catalog google |> Result.map (List.map Model.id)));
      test "resolution reaches hidden, deprecated, and dynamic models"
        (fun () ->
          let decl =
            Provider.make openai
              ~dynamic:(fun id ->
                if String.ends_with ~suffix:".gguf" id then
                  Some (Model.make (llm id) ())
                else None)
              [
                Model.make (llm "gpt-5") ();
                Model.make (llm "gpt-4") ~status:(Model.Unavailable "retired")
                  ();
              ]
          in
          let catalog = Catalog.make [ decl ] in
          equal
            (result llm_model_value catalog_error_value)
            ~msg:"resolves visible"
            (Ok (llm "gpt-5"))
            (Catalog.resolve catalog " openai/gpt-5 " |> Result.map Model.llm);
          equal
            (result llm_model_value catalog_error_value)
            ~msg:"resolves hidden"
            (Ok (llm "gpt-4"))
            (Catalog.resolve catalog "openai/gpt-4" |> Result.map Model.llm);
          equal
            (result llm_model_value catalog_error_value)
            ~msg:"resolves dynamic"
            (Ok (llm "weights.gguf"))
            (Catalog.resolve catalog "openai/weights.gguf"
            |> Result.map Model.llm);
          equal
            (result llm_model_value catalog_error_value)
            ~msg:"find takes a selector"
            (Ok (llm "gpt-5"))
            (Catalog.find catalog (Selector.make ~provider:openai ~id:"gpt-5")
            |> Result.map Model.llm));
      test "error vocabulary carries candidate and known hints" (fun () ->
          let decl =
            Provider.make openai
              [ Model.make (llm "gpt-5-mini") (); Model.make (llm "gpt-5") () ]
          in
          let anthropic_decl =
            Provider.make anthropic
              [
                Model.make (llm ~provider:anthropic ~api:messages "claude-3") ();
              ]
          in
          let catalog = Catalog.make [ decl; anthropic_decl ] in
          let sel p m = Selector.make ~provider:p ~id:m in
          equal
            (result llm_model_value catalog_error_value)
            ~msg:"invalid selector lists every declared selector"
            (Error
               (Catalog.Error.Invalid_selector
                  {
                    input = "openai";
                    error = Selector.Error.Missing_separator;
                    candidates =
                      [
                        sel openai "gpt-5-mini";
                        sel openai "gpt-5";
                        sel anthropic "claude-3";
                      ];
                  }))
            (Catalog.resolve catalog "openai" |> Result.map Model.llm);
          equal
            (result llm_model_value catalog_error_value)
            ~msg:"unknown provider lists known providers"
            (Error
               (Catalog.Error.Unknown_provider
                  { provider = google; known = [ openai; anthropic ] }))
            (Catalog.resolve catalog "google/gemini" |> Result.map Model.llm);
          equal
            (result llm_model_value catalog_error_value)
            ~msg:"unknown model lists the provider's model ids"
            (Error
               (Catalog.Error.Unknown_model
                  {
                    provider = openai;
                    model = "gpt-6";
                    known = [ "gpt-5-mini"; "gpt-5" ];
                  }))
            (Catalog.resolve catalog "openai/gpt-6" |> Result.map Model.llm));
      test "a bare id that matches a declared model id is suggested" (fun () ->
          let catalog =
            Catalog.make
              [ Provider.make openai [ Model.make (llm "gpt-5") () ] ]
          in
          match Catalog.resolve catalog "gpt-5" with
          | Error (Catalog.Error.Invalid_selector { candidates; _ }) ->
              equal (list selector_value) ~msg:"exact model-id match suggested"
                [ Selector.make ~provider:openai ~id:"gpt-5" ]
                candidates
          | _ -> failf "expected Invalid_selector");
      test "a provider's dynamic inconsistency raises through resolution"
        (fun () ->
          let catalog =
            Catalog.make
              [
                Provider.make openai
                  ~dynamic:(fun _ ->
                    Some
                      (Model.make
                         (llm ~provider:anthropic ~api:messages "claude")
                         ()))
                  [];
              ]
          in
          expect_invalid_arg "inconsistent rule is a programmer error"
            (fun () -> Catalog.resolve catalog "openai/claude"));
      test "empty catalog resolves nothing" (fun () ->
          match Catalog.resolve (Catalog.make []) "openai/gpt-5" with
          | Error (Catalog.Error.Unknown_provider { known = []; _ }) -> ()
          | _ -> failf "expected Unknown_provider with no known providers");
    ]

(* Model readiness. *)

let model_readiness_group =
  group "model readiness"
    [
      test
        "projection preserves declaration order and resolves checked dynamic \
         ids" (fun () ->
          let openai_stable =
            Model.make (llm "gpt-5") ~display_name:"GPT-5" ()
          in
          let openai_deprecated =
            Model.make (llm "gpt-4") ~status:Model.Deprecated ()
          in
          let openai_declaration =
            Provider.make openai
              ~auth:(Auth.make ~env:[ Env.api_key "OPENAI_API_KEY" ] ())
              [ openai_stable; openai_deprecated ]
          in
          let anthropic_static =
            Model.make
              (llm ~provider:anthropic ~api:messages "claude-static")
              ~display_name:"Claude static" ()
          in
          let dynamic_calls = ref [] in
          let anthropic_declaration =
            Provider.make anthropic
              ~auth:
                (Auth.make ~required:false
                   ~env:[ Env.api_key "ANTHROPIC_API_KEY" ]
                   ())
              ~dynamic:(fun id ->
                dynamic_calls := id :: !dynamic_calls;
                if String.equal id "claude-dynamic" then
                  Some
                    (Model.make
                       (llm ~provider:anthropic ~api:messages id)
                       ~family:"dynamic" ())
                else None)
              [ anthropic_static ]
          in
          let catalog =
            Catalog.make [ openai_declaration; anthropic_declaration ]
          in
          let openai_error =
            Credential_error.Unsupported_credential_kind
              {
                source = Source.process;
                kind = Kind.Bearer;
                accepted = [ Kind.Api_key ];
              }
          in
          let openai_failure =
            Account.Discovery.Resolution_failed
              { provider = openai; error = openai_error }
          in
          let anthropic_credential =
            Credential.make ~provider:anthropic ~source:Source.process
              (Secret.api_key "anthropic-secret")
          in
          let anthropic_account =
            Account.checked anthropic_credential
              ~models:[ "ignored"; "claude-dynamic"; "claude-static" ]
              ()
          in
          let readiness =
            Model_readiness.of_catalog catalog
              ~discoveries:
                [ Account.Discovery.Known anthropic_account; openai_failure ]
          in
          let routes = Model_readiness.routes readiness in
          equal (list provider_value) ~msg:"catalog provider order"
            [ openai; anthropic ]
            (List.map Readiness_route.provider routes);
          let openai_route = List.hd routes in
          is_true ~msg:"required auth is retained"
            (Readiness_route.auth_required openai_route);
          is_true ~msg:"resolution failure is retained"
            (Account.Discovery.equal openai_failure
               (Readiness_route.discovery openai_route));
          let openai_entries = Readiness_route.models openai_route in
          equal (list string) ~msg:"static model order" [ "gpt-5"; "gpt-4" ]
            (List.map
               (fun entry -> Model.id (Readiness_entry.model entry))
               openai_entries);
          equal (option string) ~msg:"static model metadata" (Some "GPT-5")
            (Model.display_name
               (Readiness_entry.model (List.hd openai_entries)));
          (match Readiness_entry.eligibility (List.nth openai_entries 1) with
          | Eligibility.Ineligible Model.Deprecated -> ()
          | Eligibility.Eligible
          | Eligibility.Ineligible
              (Model.Stable | Model.Preview | Model.Unavailable _) ->
              fail "deprecated model lost its structured eligibility reason");
          List.iter
            (fun entry ->
              (match Readiness_entry.availability entry with
              | Availability.Unknown -> ()
              | Availability.Available | Availability.Unavailable ->
                  fail "failed discovery leaves availability unknown");
              match Readiness_entry.actionability entry with
              | Actionability.Provider_blocked discovery ->
                  is_true ~msg:"entry retains the exact provider failure"
                    (Account.Discovery.equal openai_failure discovery)
              | Actionability.Actionable | Actionability.Model_unavailable ->
                  fail "resolution failure was lowered to an actionable model")
            openai_entries;
          let anthropic_route = List.nth routes 1 in
          is_false ~msg:"optional auth is retained"
            (Readiness_route.auth_required anthropic_route);
          let anthropic_entries = Readiness_route.models anthropic_route in
          equal (list string) ~msg:"dynamic models follow declared models"
            [ "claude-static"; "claude-dynamic" ]
            (List.map
               (fun entry -> Model.id (Readiness_entry.model entry))
               anthropic_entries);
          equal (list string) ~msg:"only undeclared checked ids reach resolver"
            [ "claude-dynamic"; "ignored" ]
            (List.rev !dynamic_calls);
          equal (option string) ~msg:"dynamic metadata comes from resolver"
            (Some "dynamic")
            (Model.family
               (Readiness_entry.model (List.nth anthropic_entries 1)));
          List.iter
            (fun entry ->
              (match Readiness_entry.availability entry with
              | Availability.Available -> ()
              | Availability.Unavailable | Availability.Unknown ->
                  fail "checked listed model is available");
              match Readiness_entry.actionability entry with
              | Actionability.Actionable -> ()
              | Actionability.Provider_blocked _
              | Actionability.Model_unavailable ->
                  fail "available optional-auth model is not actionable")
            anthropic_entries);
      test
        "eligibility, authentication, and checked availability stay independent"
        (fun () ->
          let stable = Model.make (llm "gpt-5") () in
          let required_catalog =
            Catalog.make
              [
                Provider.make openai
                  ~auth:(Auth.make ~env:[ Env.api_key "OPENAI_API_KEY" ] ())
                  [ stable ];
              ]
          in
          let missing = Account.missing ~provider:openai in
          let required =
            Model_readiness.of_catalog required_catalog
              ~discoveries:[ Account.Discovery.Known missing ]
          in
          let required_entry =
            required |> Model_readiness.routes |> List.hd
            |> Readiness_route.models |> List.hd
          in
          (match Readiness_entry.eligibility required_entry with
          | Eligibility.Eligible -> ()
          | Eligibility.Ineligible _ ->
              fail "missing auth changed static selection eligibility");
          (match Readiness_entry.actionability required_entry with
          | Actionability.Provider_blocked (Account.Discovery.Known account) ->
              equal phase_value ~msg:"exact missing account"
                Account.Phase.Missing (Account.phase account)
          | Actionability.Provider_blocked
              (Account.Discovery.Resolution_failed _)
          | Actionability.Actionable | Actionability.Model_unavailable ->
              fail "required missing auth did not block provider actionability");
          let optional_catalog =
            Catalog.make
              [
                Provider.make openai
                  ~auth:
                    (Auth.make ~required:false
                       ~env:[ Env.api_key "OPENAI_API_KEY" ]
                       ())
                  [ stable ];
              ]
          in
          let optional =
            Model_readiness.of_catalog optional_catalog
              ~discoveries:[ Account.Discovery.Known missing ]
          in
          let optional_entry =
            optional |> Model_readiness.routes |> List.hd
            |> Readiness_route.models |> List.hd
          in
          (match Readiness_entry.actionability optional_entry with
          | Actionability.Actionable -> ()
          | Actionability.Provider_blocked _ | Actionability.Model_unavailable
            ->
              fail "optional missing auth blocked an otherwise unknown model");
          let credential =
            Credential.make ~provider:openai ~source:Source.process
              (Secret.api_key "openai-secret")
          in
          let checked = Account.checked credential ~models:[ "other" ] () in
          let unavailable =
            Model_readiness.of_catalog optional_catalog
              ~discoveries:[ Account.Discovery.Known checked ]
          in
          let unavailable_entry =
            unavailable |> Model_readiness.routes |> List.hd
            |> Readiness_route.models |> List.hd
          in
          (match Readiness_entry.availability unavailable_entry with
          | Availability.Unavailable -> ()
          | Availability.Available | Availability.Unknown ->
              fail "checked entitlement is unavailable");
          match Readiness_entry.actionability unavailable_entry with
          | Actionability.Model_unavailable -> ()
          | Actionability.Actionable | Actionability.Provider_blocked _ ->
              fail "checked model absence remained actionable");
      test "codec round-trips exact provider and model readiness facts"
        (fun () ->
          let model =
            Model.make (llm "gpt-5") ~display_name:"GPT-5"
              ~capabilities:[ Capability.tools ] ()
          in
          let catalog =
            Catalog.make
              [
                Provider.make openai ~display_name:"OpenAI"
                  ~auth:(Auth.make ~env:[ Env.api_key "OPENAI_API_KEY" ] ())
                  [ model ];
              ]
          in
          let credential =
            Credential.make ~provider:openai ~source:Source.process
              (Secret.api_key "LEAKMARKER-model-readiness-secret")
          in
          let readiness =
            Model_readiness.of_catalog catalog
              ~discoveries:
                [
                  Account.Discovery.Known
                    (Account.checked credential ~models:[ "gpt-5" ] ());
                ]
          in
          assert_round_trip ~equal:Model_readiness.equal
            "model readiness round-trip" Model_readiness.jsont readiness;
          let json = encode Model_readiness.jsont readiness in
          is_false ~msg:"readiness JSON cannot contain credential material"
            (json_contains_string "LEAKMARKER" json);
          assert_decode_error ~contains:"Unexpected member"
            "model readiness rejects an unknown member" Model_readiness.jsont
            (json_object
               [ ("providers", json_array []); ("extra", Json.bool true) ]));
      test "projection rejects incomplete or duplicate discovery coverage"
        (fun () ->
          let catalog =
            Catalog.make
              [
                Provider.make openai [ Model.make (llm "gpt-5") () ];
                Provider.make anthropic [];
              ]
          in
          let openai_discovery =
            Account.Discovery.Known (Account.missing ~provider:openai)
          in
          expect_invalid_arg "missing provider discovery" (fun () ->
              Model_readiness.of_catalog catalog
                ~discoveries:[ openai_discovery ]);
          expect_invalid_arg "duplicate provider discovery" (fun () ->
              Model_readiness.of_catalog catalog
                ~discoveries:[ openai_discovery; openai_discovery ]));
    ]

(* Selection. *)

let selection_requirement_group =
  group "selection requirement"
    [
      test "none passes any selectable model and fails unselectable" (fun () ->
          let stable = Model.make (llm "gpt-5") () in
          let deprecated = Model.make (llm "old") ~status:Model.Deprecated () in
          is_true ~msg:"selectable passes"
            (Result.is_ok (Requirement.check Requirement.none stable));
          match Requirement.check Requirement.none deprecated with
          | Error (Requirement.Mismatch.Lifecycle Model.Deprecated) -> ()
          | _ -> failf "expected a Lifecycle mismatch");
      test "capability and reasoning constraints" (fun () ->
          let model =
            Model.make (llm "gpt-5") ~capabilities:[ Capability.reasoning ]
              ~input_modalities:[ Modality.text ]
              ~output_modalities:[ Modality.text ]
              ~supported_reasoning:[ Reasoning.Low; Reasoning.Medium ]
              ()
          in
          (match
             Requirement.check
               (Requirement.make ~capabilities:[ Capability.tools ] ())
               model
           with
          | Error (Requirement.Mismatch.Missing_capability c) ->
              equal capability_value ~msg:"missing capability" Capability.tools
                c
          | _ -> failf "expected Missing_capability");
          (* Reasoning membership is by rank: a supported effort passes, an
             unsupported one fails with the model's declared list. *)
          is_true ~msg:"supported reasoning passes"
            (Result.is_ok
               (Requirement.check
                  (Requirement.make ~reasoning_effort:Reasoning.Medium ())
                  model));
          (match
             Requirement.check
               (Requirement.make ~reasoning_effort:Reasoning.Max ())
               model
           with
          | Error
              (Requirement.Mismatch.Unsupported_reasoning
                 { requested; supported }) ->
              equal reasoning_value ~msg:"requested" Reasoning.Max requested;
              equal (list reasoning_value) ~msg:"supported list"
                [ Reasoning.Low; Reasoning.Medium ]
                supported
          | _ -> failf "expected Unsupported_reasoning");
          (* A model lacking the reasoning capability fails even if it lists the
             effort. *)
          let no_cap =
            Model.make (llm "gpt-5") ~supported_reasoning:[ Reasoning.Medium ]
              ()
          in
          match
            Requirement.check
              (Requirement.make ~reasoning_effort:Reasoning.Medium ())
              no_cap
          with
          | Error (Requirement.Mismatch.Unsupported_reasoning _) -> ()
          | _ -> failf "expected Unsupported_reasoning without the capability");
    ]

let ok_model msg = function
  | Ok model -> model
  | Error e -> failf "%s: %s" msg (Selection.Error.message e)

(* A preferred model is an already-resolved [Model.t]; the supported wiring is
   to resolve the selector through the catalog first. *)
let find_model catalog p m =
  match Catalog.find catalog (Selector.make ~provider:p ~id:m) with
  | Ok model -> model
  | Error e ->
      failf "find_model %s/%s: %s" (Llm_provider.id p) m
        (Catalog.Error.message e)

let no_provider_preferred _provider = false

let selection_main_group =
  group "selection main"
    [
      test "a preferred model wins without consulting provider preference"
        (fun () ->
          let catalog =
            Catalog.make
              [ Provider.make openai [ Model.make (llm "gpt-5") () ] ]
          in
          let model =
            ok_model "preferred"
              (Selection.main ~catalog
                 ~provider_preferred:(fun _ ->
                   fail "preferred selection consulted provider preference")
                 ~preferred:(find_model catalog openai "gpt-5")
                 ())
          in
          equal llm_model_value ~msg:"model" (llm "gpt-5") (Model.llm model));
      test "provider preference biases derived defaults, disjunctively"
        (fun () ->
          let catalog =
            Catalog.make
              [
                Provider.make openai
                  ~default_model:(Model.make (llm "gpt-5") ())
                  [ Model.make (llm "gpt-5") () ];
                Provider.make anthropic
                  ~default_model:
                    (Model.make
                       (llm ~provider:anthropic ~api:messages "claude")
                       ())
                  [
                    Model.make
                      (llm ~provider:anthropic ~api:messages "claude")
                      ();
                  ];
              ]
          in
          let anthropic_cred =
            Credential.make ~provider:anthropic ~source:Source.process
              (Secret.api_key "sk-anthropic")
          in
          (* Only anthropic is connected, so its default wins even though openai
             is declared first. Duplicate accounts aggregate: one blocked plus one
             ready still counts as connected. *)
          let accounts =
            [
              Account.checked anthropic_cred ~problems:[ Problem.Revoked ] ();
              Account.present anthropic_cred;
            ]
          in
          let provider_preferred provider =
            List.exists
              (fun account ->
                Llm_provider.equal (Account.provider account) provider
                && Account.connected account)
              accounts
          in
          let model =
            ok_model "connected"
              (Selection.main ~catalog ~provider_preferred ())
          in
          equal llm_model_value ~msg:"connected provider default"
            (llm ~provider:anthropic ~api:messages "claude")
            (Model.llm model));
      test "provider default and first eligible are usable fallbacks" (fun () ->
          let with_defaults =
            Catalog.make
              [
                Provider.make openai
                  ~default_model:(Model.make (llm "gpt-default") ())
                  [ Model.make (llm "gpt-default") () ];
              ]
          in
          let model =
            ok_model "provider default"
              (Selection.main ~catalog:with_defaults
                 ~provider_preferred:no_provider_preferred ())
          in
          equal llm_model_value ~msg:"provider default" (llm "gpt-default")
            (Model.llm model);
          let no_defaults =
            Catalog.make
              [ Provider.make openai [ Model.make (llm "gpt-first") () ] ]
          in
          let model =
            ok_model "first eligible"
              (Selection.main ~catalog:no_defaults
                 ~provider_preferred:no_provider_preferred ())
          in
          equal llm_model_value ~msg:"first eligible model" (llm "gpt-first")
            (Model.llm model));
      test "derived candidates that fail requirements are skipped" (fun () ->
          (* openai's default lacks tools; anthropic's default has them. With no
             connected account, the derived ladder skips openai and lands on
             anthropic rather than failing. *)
          let catalog =
            Catalog.make
              [
                Provider.make openai
                  ~default_model:(Model.make (llm "gpt-5") ())
                  [ Model.make (llm "gpt-5") () ];
                Provider.make anthropic
                  ~default_model:
                    (Model.make
                       (llm ~provider:anthropic ~api:messages "claude")
                       ~capabilities:[ Capability.tools ] ())
                  [
                    Model.make
                      (llm ~provider:anthropic ~api:messages "claude")
                      ~capabilities:[ Capability.tools ] ();
                  ];
              ]
          in
          let model =
            ok_model "skip"
              (Selection.main ~catalog ~provider_preferred:no_provider_preferred
                 ~requirements:
                   (Requirement.make ~capabilities:[ Capability.tools ] ())
                 ())
          in
          equal llm_model_value ~msg:"skipped to the eligible default"
            (llm ~provider:anthropic ~api:messages "claude")
            (Model.llm model));
      test "no eligible model yields No_model" (fun () ->
          let catalog =
            Catalog.make
              [
                Provider.make openai
                  [ Model.make (llm "gpt-5") ~status:Model.Deprecated () ];
              ]
          in
          match
            Selection.main ~catalog ~provider_preferred:no_provider_preferred ()
          with
          | Error Selection.Error.No_model -> ()
          | _ -> failf "expected No_model");
      test
        "a preferred model that fails requirements errors with a full \
         alternative" (fun () ->
          (* The preferred model has neither tools nor reasoning; the alternative
             must satisfy the WHOLE requirement set, not just the tools part. *)
          let only_tools =
            Model.make (llm "gpt-tools") ~capabilities:[ Capability.tools ] ()
          in
          let both =
            Model.make (llm "gpt-both")
              ~capabilities:[ Capability.tools; Capability.reasoning ]
              ~supported_reasoning:[ Reasoning.Medium ] ()
          in
          let preferred = Model.make (llm "gpt-plain") () in
          let catalog =
            Catalog.make
              [ Provider.make openai [ preferred; only_tools; both ] ]
          in
          let requirements =
            Requirement.make
              ~capabilities:[ Capability.tools; Capability.reasoning ]
              ~reasoning_effort:Reasoning.Medium ()
          in
          match
            Selection.main ~catalog ~provider_preferred:no_provider_preferred
              ~preferred:(find_model catalog openai "gpt-plain")
              ~requirements ()
          with
          | Error
              (Selection.Error.Requirement_mismatch { selector; alternative; _ })
            ->
              equal selector_value ~msg:"the failing selector"
                (Selector.make ~provider:openai ~id:"gpt-plain")
                selector;
              equal (option selector_value)
                ~msg:"alternative satisfies the full set"
                (Some (Selector.make ~provider:openai ~id:"gpt-both"))
                alternative
          | _ -> failf "expected Requirement_mismatch");
      test "capability and reasoning mismatches each yield an alternative"
        (fun () ->
          let make_case ~preferred_model ~alt_model ~requirements ~alt_id =
            let catalog =
              Catalog.make
                [ Provider.make openai [ preferred_model; alt_model ] ]
            in
            match
              Selection.main ~catalog ~provider_preferred:no_provider_preferred
                ~preferred:
                  (find_model catalog openai (Model.id preferred_model))
                ~requirements ()
            with
            | Error (Selection.Error.Requirement_mismatch { alternative; _ }) ->
                equal (option selector_value) ~msg:"alternative"
                  (Some (Selector.make ~provider:openai ~id:alt_id))
                  alternative
            | _ -> failf "expected Requirement_mismatch"
          in
          make_case
            ~preferred_model:(Model.make (llm "no-cap") ())
            ~alt_model:
              (Model.make (llm "has-cap") ~capabilities:[ Capability.tools ] ())
            ~requirements:
              (Requirement.make ~capabilities:[ Capability.tools ] ())
            ~alt_id:"has-cap";
          make_case
            ~preferred_model:(Model.make (llm "no-reason") ())
            ~alt_model:
              (Model.make (llm "reasoner")
                 ~capabilities:[ Capability.reasoning ]
                 ~supported_reasoning:[ Reasoning.High ] ())
            ~requirements:(Requirement.make ~reasoning_effort:Reasoning.High ())
            ~alt_id:"reasoner");
    ]

(* The purity claim — the catalog and selection run with no transport — is owned
   structurally by the suite's dependency graph: test_provider links only
   declaration/value libraries and no provider client, so no transport can be
   reached. The former runtime re-assertion is dropped; Selection.main is
   exercised throughout the selection groups. *)

let selection_small_group =
  group "selection small"
    [
      test "a selectable preferred small model wins over the main model"
        (fun () ->
          let catalog =
            Catalog.make
              [
                Provider.make openai
                  [
                    Model.make (llm "gpt-5") ();
                    Model.make (llm "gpt-5-mini") ();
                  ];
              ]
          in
          let main = find_model catalog openai "gpt-5" in
          let preferred = find_model catalog openai "gpt-5-mini" in
          let model = Selection.small ~main ~preferred () in
          equal llm_model_value ~msg:"preferred small wins" (llm "gpt-5-mini")
            (Model.llm model));
      test "no preferred small model falls back to the main model" (fun () ->
          let catalog =
            Catalog.make
              [ Provider.make openai [ Model.make (llm "gpt-5") () ] ]
          in
          let main = find_model catalog openai "gpt-5" in
          let model = Selection.small ~main () in
          equal llm_model_value ~msg:"fallback to main" (llm "gpt-5")
            (Model.llm model));
      test "an unselectable preferred small model falls back, never blocks"
        (fun () ->
          (* A deprecated configured small model is not selectable, yet the small
             role must never block a run the main model can start: it silently
             yields [main] rather than surfacing a requirement mismatch. *)
          let catalog =
            Catalog.make
              [
                Provider.make openai
                  [
                    Model.make (llm "gpt-5") ();
                    Model.make (llm "old-mini") ~status:Model.Deprecated ();
                  ];
              ]
          in
          let main = find_model catalog openai "gpt-5" in
          let preferred = find_model catalog openai "old-mini" in
          let model = Selection.small ~main ~preferred () in
          equal llm_model_value ~msg:"deprecated small falls back to main"
            (llm "gpt-5") (Model.llm model));
    ]

let () =
  run "mentat.provider"
    [
      selector_group;
      auth_env_group;
      auth_login_group;
      auth_declaration_group;
      model_metadata_group;
      model_pricing_group;
      model_cost_group;
      model_rate_group;
      model_jsont_group;
      account_secret_group;
      account_fingerprint_group;
      account_credential_group;
      credential_error_group;
      account_store_group;
      account_facts_group;
      account_status_group;
      account_codec_group;
      declaration_group;
      resolve_credential_group;
      catalog_group;
      model_readiness_group;
      selection_requirement_group;
      selection_main_group;
      selection_small_group;
    ]
