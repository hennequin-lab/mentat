(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

type timestamp = int64

let invalid fn message =
  invalid_arg ("Mentat_provider.Credential." ^ fn ^ ": " ^ message)

let check_non_empty fn field = function
  | "" -> invalid fn (field ^ " must not be empty")
  | _ -> ()

let check_text fn field value =
  check_non_empty fn field value;
  if not (String.is_valid_utf_8 value) then
    invalid fn (field ^ " must be valid UTF-8 text")

let check_optional_text fn field = function
  | None -> ()
  | Some value -> check_text fn field value

let check_optional_non_negative_time fn field = function
  | None -> ()
  | Some value when Int64.compare value 0L >= 0 -> ()
  | Some _ -> invalid fn (field ^ " must not be negative")

let equal_option equal a b =
  match (a, b) with
  | None, None -> true
  | Some a, Some b -> equal a b
  | None, Some _ | Some _, None -> false

let valid_env_name name =
  let len = String.length name in
  let valid_first = function '_' -> true | c -> Char.Ascii.is_letter c in
  let valid_rest = function
    | '_' -> true
    | c -> Char.Ascii.is_letter c || Char.Ascii.is_digit c
  in
  let rec loop index =
    index = len
    || (valid_rest (String.unsafe_get name index) && loop (index + 1))
  in
  len > 0 && valid_first (String.unsafe_get name 0) && loop 1

let check_env_name fn name =
  if not (valid_env_name name) then invalid fn "name is invalid"

let valid_name name =
  let len = String.length name in
  let valid_char = function
    | '_' | '-' | '.' -> true
    | c -> Char.Ascii.is_letter c || Char.Ascii.is_digit c
  in
  let rec loop index =
    index = len
    || (valid_char (String.unsafe_get name index) && loop (index + 1))
  in
  len > 0 && loop 0

let check_name fn name =
  if not (valid_name name) then invalid fn "name is invalid"

module Kind = struct
  type t = Api_key | Bearer | OAuth

  let equal a b =
    match (a, b) with
    | Api_key, Api_key | Bearer, Bearer | OAuth, OAuth -> true
    | Api_key, (Bearer | OAuth)
    | Bearer, (Api_key | OAuth)
    | OAuth, (Api_key | Bearer) ->
        false

  let to_string = function
    | Api_key -> "api_key"
    | Bearer -> "bearer"
    | OAuth -> "oauth"

  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.enum ~kind:"credential kind"
      [ ("api_key", Api_key); ("bearer", Bearer); ("oauth", OAuth) ]
end

module Secret = struct
  type t =
    | Api_key of string
    | Bearer of string
    | OAuth of {
        access_token : string;
        refresh_token : string option;
        expires_at : timestamp option;
        account_id : string option;
      }

  let api_key key =
    check_text "Secret.api_key" "key" key;
    Api_key key

  let bearer token =
    check_text "Secret.bearer" "token" token;
    Bearer token

  let oauth ~access_token ?refresh_token ?expires_at ?account_id () =
    check_text "Secret.oauth" "access_token" access_token;
    check_optional_text "Secret.oauth" "refresh_token" refresh_token;
    check_optional_text "Secret.oauth" "account_id" account_id;
    check_optional_non_negative_time "Secret.oauth" "expires_at" expires_at;
    OAuth { access_token; refresh_token; expires_at; account_id }

  let kind = function
    | Api_key _ -> Kind.Api_key
    | Bearer _ -> Kind.Bearer
    | OAuth _ -> Kind.OAuth

  let equal a b =
    match (a, b) with
    | Api_key a, Api_key b | Bearer a, Bearer b -> String.equal a b
    | OAuth a, OAuth b ->
        String.equal a.access_token b.access_token
        && equal_option String.equal a.refresh_token b.refresh_token
        && equal_option Int64.equal a.expires_at b.expires_at
        && equal_option String.equal a.account_id b.account_id
    | Api_key _, (Bearer _ | OAuth _)
    | Bearer _, (Api_key _ | OAuth _)
    | OAuth _, (Api_key _ | Bearer _) ->
        false

  (* At most 4 trailing characters, and none if the material is short enough
     that a suffix would disclose most of it. Every fingerprint honours the
     "at most 4 characters of secret material" contract. *)
  let material_fingerprint material =
    let len = String.length material in
    if len < 8 then None else Some (String.sub material (len - 4) 4)

  let fingerprint = function
    | Api_key key -> material_fingerprint key
    | Bearer token -> material_fingerprint token
    | OAuth { account_id = Some account_id; _ } ->
        material_fingerprint account_id
    | OAuth { access_token; account_id = None; _ } ->
        material_fingerprint access_token

  let expires_at = function
    | Api_key _ | Bearer _ -> None
    | OAuth { expires_at; _ } -> expires_at

  let has_refresh_token = function
    | Api_key _ | Bearer _ -> false
    | OAuth { refresh_token; _ } -> Option.is_some refresh_token

  let expose t ~api_key ~bearer ~oauth =
    match t with
    | Api_key key -> api_key ~key
    | Bearer token -> bearer ~token
    | OAuth { access_token; refresh_token; expires_at; account_id } ->
        oauth ~access_token ~refresh_token ~expires_at ~account_id
end

module Name = struct
  type t = string

  let default = "default"

  let make name =
    check_name "Name.make" name;
    name

  let to_string t = t
  let equal = String.equal
  let compare = String.compare
  let pp ppf t = Format.pp_print_string ppf t

  let jsont =
    Jsont.map ~kind:"credential name"
      ~dec:(fun s -> decode_invalid_arg (fun () -> make s))
      ~enc:to_string Jsont.string
end

module Source = struct
  type t = Process | Env of string | Store of Name.t

  let process = Process

  let env name =
    check_env_name "Source.env" name;
    Env name

  let store ?(name = Name.default) () = Store name

  let name = function
    | Process -> None
    | Env name -> Some name
    | Store name -> Some (Name.to_string name)

  let equal a b =
    match (a, b) with
    | Process, Process -> true
    | Env a, Env b -> String.equal a b
    | Store a, Store b -> Name.equal a b
    | Process, (Env _ | Store _)
    | Env _, (Process | Store _)
    | Store _, (Process | Env _) ->
        false

  let pp ppf = function
    | Process -> Format.pp_print_string ppf "process"
    | Env name -> Format.fprintf ppf "env(%s)" name
    | Store name -> Format.fprintf ppf "store(%a)" Name.pp name

  let jsont =
    let process =
      Jsont.Object.map ~kind:"process credential source" Process
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "process" ~dec:Fun.id
    in
    let env =
      Jsont.Object.map ~kind:"environment credential source" (fun name ->
          decode_invalid_arg (fun () -> env name))
      |> Jsont.Object.mem "name" Jsont.string ~enc:(function
        | Env name -> name
        | Process | Store _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "env" ~dec:Fun.id
    in
    let store =
      Jsont.Object.map ~kind:"stored credential source" (fun name -> Store name)
      |> Jsont.Object.mem "name" Name.jsont ~enc:(function
        | Store name -> name
        | Process | Env _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "store" ~dec:Fun.id
    in
    let cases = List.map Jsont.Object.Case.make [ process; env; store ] in
    let enc_case = function
      | Process as source -> Jsont.Object.Case.value process source
      | Env _ as source -> Jsont.Object.Case.value env source
      | Store _ as source -> Jsont.Object.Case.value store source
    in
    Jsont.Object.map ~kind:"credential source" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

type t = {
  provider : Mentat_llm.Provider.t;
  source : Source.t;
  secret : Secret.t;
}

let make ~provider ~source secret = { provider; source; secret }
let provider t = t.provider
let source t = t.source
let kind t = Secret.kind t.secret
let fingerprint t = Secret.fingerprint t.secret
let secret t = t.secret

module Store = struct
  type binding = Mentat_llm.Provider.t * Name.t * Secret.t
  type t = { bindings : binding list }

  let empty = { bindings = [] }

  let compare_binding (provider_a, name_a, _) (provider_b, name_b, _) =
    match Mentat_llm.Provider.compare provider_a provider_b with
    | 0 -> Name.compare name_a name_b
    | order -> order

  let same_key provider name (binding_provider, binding_name, _) =
    Mentat_llm.Provider.equal provider binding_provider
    && Name.equal name binding_name

  let check_unique sorted =
    let rec loop = function
      | [] | [ _ ] -> ()
      | (provider, name, _) :: ((next_provider, next_name, _) :: _ as bindings)
        ->
          if
            Mentat_llm.Provider.equal provider next_provider
            && Name.equal name next_name
          then
            invalid "Store.of_list"
              ("duplicate credential "
              ^ Mentat_llm.Provider.id provider
              ^ "/" ^ Name.to_string name);
          loop bindings
    in
    loop sorted

  let of_list bindings =
    let sorted = List.sort compare_binding bindings in
    check_unique sorted;
    { bindings = sorted }

  let bindings ?provider t =
    match provider with
    | None -> t.bindings
    | Some provider ->
        List.filter
          (fun (binding_provider, _, _) ->
            Mentat_llm.Provider.equal provider binding_provider)
          t.bindings

  let names t ~provider =
    bindings ~provider t |> List.map (fun (_, name, _) -> name)

  let secret t ~provider ?(name = Name.default) () =
    match List.find_opt (same_key provider name) t.bindings with
    | None -> None
    | Some (_, _, secret) -> Some secret

  let credential t ~provider ?(name = Name.default) () =
    match secret t ~provider ~name () with
    | None -> None
    | Some secret ->
        let source = Source.store ~name () in
        Some (make ~provider ~source secret)

  let set ~provider ?(name = Name.default) secret t =
    let bindings =
      List.filter
        (fun binding -> not (same_key provider name binding))
        t.bindings
    in
    of_list ((provider, name, secret) :: bindings)

  let remove ~provider ?(name = Name.default) t =
    {
      bindings =
        List.filter
          (fun binding -> not (same_key provider name binding))
          t.bindings;
    }

  (* Secrets are serialized only within a store snapshot, so their (trusted,
     secret-bearing) encoding is private to this codec. Each case projects its
     material through [Secret.expose], the one eliminator that reaches it. *)
  let api_key_material secret =
    Secret.expose secret
      ~api_key:(fun ~key -> key)
      ~bearer:(fun ~token:_ -> assert false)
      ~oauth:(fun
          ~access_token:_ ~refresh_token:_ ~expires_at:_ ~account_id:_ ->
        assert false)

  let bearer_material secret =
    Secret.expose secret
      ~api_key:(fun ~key:_ -> assert false)
      ~bearer:(fun ~token -> token)
      ~oauth:(fun
          ~access_token:_ ~refresh_token:_ ~expires_at:_ ~account_id:_ ->
        assert false)

  let oauth_material secret =
    Secret.expose secret
      ~api_key:(fun ~key:_ -> assert false)
      ~bearer:(fun ~token:_ -> assert false)
      ~oauth:(fun ~access_token ~refresh_token ~expires_at ~account_id ->
        (access_token, refresh_token, expires_at, account_id))

  let secret_jsont =
    let api_key =
      Jsont.Object.map ~kind:"API key secret" (fun key ->
          decode_invalid_arg (fun () -> Secret.api_key key))
      |> Jsont.Object.mem "api_key" Jsont.string ~enc:api_key_material
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "api_key" ~dec:Fun.id
    in
    let bearer =
      Jsont.Object.map ~kind:"bearer secret" (fun token ->
          decode_invalid_arg (fun () -> Secret.bearer token))
      |> Jsont.Object.mem "token" Jsont.string ~enc:bearer_material
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "bearer" ~dec:Fun.id
    in
    let oauth =
      let dec access_token refresh_token expires_at account_id =
        decode_invalid_arg (fun () ->
            Secret.oauth ~access_token ?refresh_token ?expires_at ?account_id ())
      in
      Jsont.Object.map ~kind:"OAuth secret" dec
      |> Jsont.Object.mem "access_token" Jsont.string ~enc:(fun secret ->
          let access_token, _, _, _ = oauth_material secret in
          access_token)
      |> Jsont.Object.opt_mem "refresh_token" Jsont.string ~enc:(fun secret ->
          let _, refresh_token, _, _ = oauth_material secret in
          refresh_token)
      |> Jsont.Object.opt_mem "expires_at" Jsont.int64 ~enc:(fun secret ->
          let _, _, expires_at, _ = oauth_material secret in
          expires_at)
      |> Jsont.Object.opt_mem "account_id" Jsont.string ~enc:(fun secret ->
          let _, _, _, account_id = oauth_material secret in
          account_id)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "oauth" ~dec:Fun.id
    in
    let cases = List.map Jsont.Object.Case.make [ api_key; bearer; oauth ] in
    let enc_case secret =
      match Secret.kind secret with
      | Kind.Api_key -> Jsont.Object.Case.value api_key secret
      | Kind.Bearer -> Jsont.Object.Case.value bearer secret
      | Kind.OAuth -> Jsont.Object.Case.value oauth secret
    in
    Jsont.Object.map ~kind:"stored credential" Fun.id
    |> Jsont.Object.case_mem "kind" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let version_jsont =
    Jsont.map ~kind:"account store version"
      ~dec:(fun version ->
        if version <> 1 then
          decode_error
            ("unsupported account store version: " ^ string_of_int version))
      ~enc:(fun () -> 1)
      Jsont.int

  (* Bindings are sorted by provider then name, and a strict map decodes its
     members in name order, so grouping by provider round-trips the order. *)
  let to_credentials t =
    let rec loop = function
      | [] -> []
      | (provider, _, _) :: _ as bindings ->
          let same, rest =
            List.partition
              (fun (binding_provider, _, _) ->
                Mentat_llm.Provider.equal provider binding_provider)
              bindings
          in
          ( Mentat_llm.Provider.id provider,
            List.map
              (fun (_, name, secret) -> (Name.to_string name, secret))
              same )
          :: loop rest
    in
    loop t.bindings

  let of_credentials credentials =
    let bindings =
      List.concat_map
        (fun (provider_id, secrets) ->
          let provider =
            decode_invalid_arg (fun () -> Mentat_llm.Provider.make provider_id)
          in
          List.map
            (fun (name, secret) ->
              (provider, decode_invalid_arg (fun () -> Name.make name), secret))
            secrets)
        credentials
    in
    decode_invalid_arg (fun () -> of_list bindings)

  let jsont =
    let credentials_jsont =
      strict_string_map ~kind:"account store credentials"
        (strict_string_map ~kind:"provider credentials" secret_jsont)
    in
    Jsont.Object.map ~kind:"account store" (fun () credentials ->
        of_credentials credentials)
    |> Jsont.Object.mem "version" version_jsont ~enc:(fun _ -> ())
    |> Jsont.Object.mem "credentials" credentials_jsont ~enc:to_credentials
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end
