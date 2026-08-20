(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid fn message =
  invalid_arg ("Mentat_provider.Model_readiness." ^ fn ^ ": " ^ message)

let decode_error message = Jsont.Error.msg Jsont.Meta.none message

let decode_invalid_arg f =
  match f () with
  | value -> value
  | exception Invalid_argument message -> decode_error message

let equal_status a b =
  match (a, b) with
  | Model.Stable, Model.Stable
  | Model.Preview, Model.Preview
  | Model.Deprecated, Model.Deprecated ->
      true
  | Model.Unavailable a, Model.Unavailable b -> String.equal a b
  | (Model.Stable | Model.Preview | Model.Deprecated | Model.Unavailable _), _
    ->
      false

let pp_status ppf = function
  | Model.Stable -> Format.pp_print_string ppf "stable"
  | Model.Preview -> Format.pp_print_string ppf "preview"
  | Model.Deprecated -> Format.pp_print_string ppf "deprecated"
  | Model.Unavailable reason -> Format.fprintf ppf "unavailable(%s)" reason

module Availability = struct
  type t = Available | Unavailable | Unknown

  let equal a b =
    match (a, b) with
    | Available, Available | Unavailable, Unavailable | Unknown, Unknown -> true
    | (Available | Unavailable | Unknown), _ -> false

  let to_string = function
    | Available -> "available"
    | Unavailable -> "unavailable"
    | Unknown -> "unknown"

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

module Eligibility = struct
  type t = Eligible | Ineligible of Model.status

  let equal a b =
    match (a, b) with
    | Eligible, Eligible -> true
    | Ineligible a, Ineligible b -> equal_status a b
    | Eligible, Ineligible _ | Ineligible _, Eligible -> false

  let pp ppf = function
    | Eligible -> Format.pp_print_string ppf "eligible"
    | Ineligible status -> Format.fprintf ppf "ineligible(%a)" pp_status status
end

module Actionability = struct
  type t =
    | Actionable
    | Provider_blocked of Account.Discovery.t
    | Model_unavailable

  let equal a b =
    match (a, b) with
    | Actionable, Actionable | Model_unavailable, Model_unavailable -> true
    | Provider_blocked a, Provider_blocked b -> Account.Discovery.equal a b
    | (Actionable | Provider_blocked _ | Model_unavailable), _ -> false

  let pp_discovery ppf = function
    | Account.Discovery.Known account -> Account.pp ppf account
    | Account.Discovery.Resolution_failed { provider; error } ->
        Format.fprintf ppf "{provider=%a; resolution_failed=%a}"
          Mentat_llm.Provider.pp provider Credential_error.pp error

  let pp ppf = function
    | Actionable -> Format.pp_print_string ppf "actionable"
    | Provider_blocked discovery ->
        Format.fprintf ppf "provider_blocked(%a)" pp_discovery discovery
    | Model_unavailable -> Format.pp_print_string ppf "model_unavailable"
end

module Entry = struct
  type origin = Declared | Listed

  let equal_origin a b =
    match (a, b) with
    | Declared, Declared | Listed, Listed -> true
    | (Declared | Listed), _ -> false

  let pp_origin ppf = function
    | Declared -> Format.pp_print_string ppf "declared"
    | Listed -> Format.pp_print_string ppf "listed"

  type t = {
    model : Model.t;
    origin : origin;
    availability : Availability.t;
    eligibility : Eligibility.t;
    actionability : Actionability.t;
  }

  let model t = t.model
  let origin t = t.origin
  let availability t = t.availability
  let eligibility t = t.eligibility
  let actionability t = t.actionability

  let equal a b =
    Model.equal a.model b.model
    && equal_origin a.origin b.origin
    && Availability.equal a.availability b.availability
    && Eligibility.equal a.eligibility b.eligibility
    && Actionability.equal a.actionability b.actionability

  let pp ppf t =
    Format.fprintf ppf
      "@[<2>{model=%a; origin=%a; availability=%a; eligibility=%a; \
       actionability=%a}@]"
      Model.pp t.model pp_origin t.origin Availability.pp t.availability
      Eligibility.pp t.eligibility Actionability.pp t.actionability
end

module Route = struct
  type t = {
    provider : Mentat_llm.Provider.t;
    display_name : string option;
    auth_required : bool;
    discovery : Account.Discovery.t;
    listed : string list option;
    dynamic : string list;
    models : Entry.t list;
  }

  let provider t = t.provider
  let display_name t = t.display_name
  let auth_required t = t.auth_required
  let discovery t = t.discovery
  let listed t = t.listed
  let dynamic t = t.dynamic
  let models t = t.models

  let equal a b =
    Mentat_llm.Provider.equal a.provider b.provider
    && Option.equal String.equal a.display_name b.display_name
    && Bool.equal a.auth_required b.auth_required
    && Account.Discovery.equal a.discovery b.discovery
    && Option.equal (List.equal String.equal) a.listed b.listed
    && List.equal String.equal a.dynamic b.dynamic
    && List.equal Entry.equal a.models b.models

  let pp ppf t =
    Format.fprintf ppf
      "@[<2>{provider=%a; display_name=%a; auth_required=%b; discovery=%a; \
       models=[%a]}@]"
      Mentat_llm.Provider.pp t.provider
      Format.(pp_print_option pp_print_string)
      t.display_name t.auth_required Actionability.pp_discovery t.discovery
      Format.(
        pp_print_list ~pp_sep:(fun ppf () -> pp_print_string ppf "; ") Entry.pp)
      t.models
end

type t = { routes : Route.t list }

let routes t = t.routes
let equal a b = List.equal Route.equal a.routes b.routes

let pp ppf t =
  Format.fprintf ppf "@[<2>{providers=[%a]}@]"
    Format.(
      pp_print_list ~pp_sep:(fun ppf () -> pp_print_string ppf "; ") Route.pp)
    t.routes

let eligibility model =
  if Model.selectable model then Eligibility.Eligible
  else Eligibility.Ineligible (Model.status model)

let availability ~listed discovery model =
  match listed with
  | Some ids ->
      if List.exists (String.equal (Model.id model)) ids then
        Availability.Available
      else Availability.Unavailable
  | None -> (
      match discovery with
      | Account.Discovery.Resolution_failed _ -> Availability.Unknown
      | Account.Discovery.Known account -> (
          match Account.model_available account (Model.id model) with
          | `Available -> Availability.Available
          | `Unavailable -> Availability.Unavailable
          | `Unknown -> Availability.Unknown))

let actionability ~auth_required discovery availability =
  match discovery with
  | Account.Discovery.Resolution_failed _ ->
      Actionability.Provider_blocked discovery
  | Account.Discovery.Known account
    when auth_required && not (Account.connected account) ->
      Actionability.Provider_blocked discovery
  | Account.Discovery.Known _ -> (
      match availability with
      | Availability.Unavailable -> Actionability.Model_unavailable
      | Availability.Available | Availability.Unknown ->
          Actionability.Actionable)

let make_entry ~auth_required ~discovery ~origin model availability =
  {
    Entry.model;
    origin;
    availability;
    eligibility = eligibility model;
    actionability = actionability ~auth_required discovery availability;
  }

let provider_equal = Mentat_llm.Provider.equal

let discovery_for provider discoveries =
  List.find_opt
    (fun discovery ->
      provider_equal provider (Account.Discovery.provider discovery))
    discoveries

let check_listings declarations listings =
  let declared provider =
    List.exists
      (fun declaration -> provider_equal provider (Declaration.id declaration))
      declarations
  in
  let rec check_unique seen = function
    | [] -> ()
    | (provider, _) :: rest ->
        if not (declared provider) then
          invalid "of_catalog"
            ("listing has unknown provider " ^ Mentat_llm.Provider.id provider);
        if List.exists (provider_equal provider) seen then
          invalid "of_catalog"
            ("duplicate listing for provider " ^ Mentat_llm.Provider.id provider);
        check_unique (provider :: seen) rest
  in
  check_unique [] listings

let check_discoveries declarations discoveries =
  let declared provider =
    List.exists
      (fun declaration -> provider_equal provider (Declaration.id declaration))
      declarations
  in
  let rec check_unique seen = function
    | [] -> ()
    | discovery :: rest ->
        let provider = Account.Discovery.provider discovery in
        if not (declared provider) then
          invalid "of_catalog"
            ("discovery has unknown provider " ^ Mentat_llm.Provider.id provider);
        if List.exists (provider_equal provider) seen then
          invalid "of_catalog"
            ("duplicate discovery for provider "
            ^ Mentat_llm.Provider.id provider);
        check_unique (provider :: seen) rest
  in
  check_unique [] discoveries;
  List.iter
    (fun declaration ->
      let provider = Declaration.id declaration in
      if Option.is_none (discovery_for provider discoveries) then
        invalid "of_catalog"
          ("missing discovery for provider " ^ Mentat_llm.Provider.id provider))
    declarations

let undeclared declaration =
  let declared_ids = List.map Model.id (Declaration.models declaration) in
  fun id -> not (List.exists (String.equal id) declared_ids)

let dynamic_models declaration discovery =
  match discovery with
  | Account.Discovery.Resolution_failed _ -> []
  | Account.Discovery.Known account -> (
      match Account.models account with
      | None -> []
      | Some checked_ids ->
          checked_ids
          |> List.filter (undeclared declaration)
          |> List.filter_map (Declaration.resolve_dynamic declaration))

let listed_models declaration listing =
  Listing.models listing
  |> List.filter (fun listed -> undeclared declaration (Listing.Model.id listed))
  |> List.filter_map (Declaration.resolve_listed declaration)

let route_of_declaration declaration discovery listing =
  let provider = Declaration.id declaration in
  if not (provider_equal provider (Account.Discovery.provider discovery)) then
    invalid "of_catalog" "discovery provider contradicts declaration";
  let auth_required = Auth.required (Declaration.auth declaration) in
  let listed = Option.map Listing.ids listing in
  let dynamic =
    match listing with
    | Some listing -> listed_models declaration listing
    | None -> dynamic_models declaration discovery
  in
  let entry ~origin model =
    let availability = availability ~listed discovery model in
    make_entry ~auth_required ~discovery ~origin model availability
  in
  let models =
    List.map (entry ~origin:Entry.Declared) (Declaration.models declaration)
    @ List.map (entry ~origin:Entry.Listed) dynamic
  in
  {
    Route.provider;
    display_name = Declaration.display_name declaration;
    auth_required;
    discovery;
    listed;
    dynamic = List.map Model.id dynamic;
    models;
  }

let of_catalog ?(listings = []) catalog ~discoveries =
  let declarations = Catalog.declarations catalog in
  check_discoveries declarations discoveries;
  check_listings declarations listings;
  let listing_for provider =
    Option.map snd
      (List.find_opt (fun (p, _) -> provider_equal provider p) listings)
  in
  let routes =
    List.map
      (fun declaration ->
        let provider = Declaration.id declaration in
        match discovery_for provider discoveries with
        | Some discovery ->
            route_of_declaration declaration discovery (listing_for provider)
        | None -> assert false)
      declarations
  in
  { routes }

let check_optional_display_name = function
  | None -> ()
  | Some "" -> invalid "jsont" "provider display_name must not be empty"
  | Some _ -> ()

let check_route_models provider models =
  let rec loop seen = function
    | [] -> ()
    | model :: rest ->
        if not (provider_equal provider (Model.provider model)) then
          invalid "jsont" "model provider contradicts readiness route";
        let id = Model.id model in
        if List.exists (String.equal id) seen then
          invalid "jsont" ("duplicate model id " ^ id);
        loop (id :: seen) rest
  in
  loop [] models

let route_of_wire provider display_name auth_required discovery listed dynamic
    models =
  check_optional_display_name display_name;
  if not (provider_equal provider (Account.Discovery.provider discovery)) then
    invalid "jsont" "discovery provider contradicts readiness route";
  check_route_models provider models;
  let models =
    List.map
      (fun model ->
        let origin =
          if List.exists (String.equal (Model.id model)) dynamic then
            Entry.Listed
          else Entry.Declared
        in
        let availability = availability ~listed discovery model in
        make_entry ~auth_required ~discovery ~origin model availability)
      models
  in
  {
    Route.provider;
    display_name;
    auth_required;
    discovery;
    listed;
    dynamic;
    models;
  }

let route_jsont =
  let dec provider display_name auth_required discovery listed dynamic models =
    decode_invalid_arg (fun () ->
        route_of_wire provider display_name auth_required discovery listed
          dynamic models)
  in
  Jsont.Object.map ~kind:"model readiness provider" dec
  |> Jsont.Object.mem "provider" Mentat_llm.Provider.jsont ~enc:Route.provider
  |> Jsont.Object.opt_mem "display_name" Jsont.string ~enc:Route.display_name
  |> Jsont.Object.mem "auth_required" Jsont.bool ~enc:Route.auth_required
  |> Jsont.Object.mem "discovery" Account.Discovery.jsont ~enc:Route.discovery
  |> Jsont.Object.opt_mem "listed" (Jsont.list Jsont.string) ~enc:Route.listed
  |> Jsont.Object.mem "dynamic" (Jsont.list Jsont.string) ~dec_absent:[]
       ~enc_omit:(fun dynamic -> dynamic = [])
       ~enc:Route.dynamic
  |> Jsont.Object.mem "models" (Jsont.list Model.jsont) ~enc:(fun route ->
      List.map Entry.model (Route.models route))
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let check_routes routes =
  let rec loop seen = function
    | [] -> ()
    | route :: rest ->
        let provider = Route.provider route in
        if List.exists (provider_equal provider) seen then
          invalid "jsont"
            ("duplicate provider " ^ Mentat_llm.Provider.id provider);
        loop (provider :: seen) rest
  in
  loop [] routes

let jsont =
  let dec routes =
    decode_invalid_arg (fun () ->
        check_routes routes;
        { routes })
  in
  Jsont.Object.map ~kind:"model readiness" dec
  |> Jsont.Object.mem "providers" (Jsont.list route_jsont) ~enc:routes
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
