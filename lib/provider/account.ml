(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

type timestamp = int64

let invalid fn message =
  invalid_arg ("Mentat_provider.Account." ^ fn ^ ": " ^ message)

let check_non_empty fn field = function
  | "" -> invalid fn (field ^ " must not be empty")
  | _ -> ()

let check_optional_non_empty fn field = function
  | None -> ()
  | Some value -> check_non_empty fn field value

let check_optional_non_negative_time fn field = function
  | None -> ()
  | Some value when Int64.compare value 0L >= 0 -> ()
  | Some _ -> invalid fn (field ^ " must not be negative")

let equal_option equal a b =
  match (a, b) with
  | None, None -> true
  | Some a, Some b -> equal a b
  | None, Some _ | Some _, None -> false

module Profile = struct
  type t = { id : string option; email : string option; name : string option }

  let make ?id ?email ?name () =
    check_optional_non_empty "Profile.make" "id" id;
    check_optional_non_empty "Profile.make" "email" email;
    check_optional_non_empty "Profile.make" "name" name;
    if Option.is_none id && Option.is_none email && Option.is_none name then
      invalid "Profile.make" "at least one field is required";
    { id; email; name }

  let equal a b =
    equal_option String.equal a.id b.id
    && equal_option String.equal a.email b.email
    && equal_option String.equal a.name b.name

  let pp ppf t =
    Format.fprintf ppf "@[<2>{id=%a; email=%a; name=%a}@]"
      Format.(pp_print_option pp_print_string)
      t.id
      Format.(pp_print_option pp_print_string)
      t.email
      Format.(pp_print_option pp_print_string)
      t.name

  let jsont =
    let dec id email name =
      decode_invalid_arg (fun () -> make ?id ?email ?name ())
    in
    Jsont.Object.map ~kind:"account profile" dec
    |> Jsont.Object.opt_mem "id" Jsont.string ~enc:(fun t -> t.id)
    |> Jsont.Object.opt_mem "email" Jsont.string ~enc:(fun t -> t.email)
    |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun t -> t.name)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Org = struct
  type t = { id : string; name : string option }

  let make ~id ?name () =
    check_non_empty "Org.make" "id" id;
    check_optional_non_empty "Org.make" "name" name;
    { id; name }

  let equal a b =
    String.equal a.id b.id && equal_option String.equal a.name b.name

  let pp ppf t =
    Format.fprintf ppf "@[<2>{id=%s; name=%a}@]" t.id
      Format.(pp_print_option pp_print_string)
      t.name

  let jsont =
    let dec id name = decode_invalid_arg (fun () -> make ~id ?name ()) in
    Jsont.Object.map ~kind:"account organization" dec
    |> Jsont.Object.mem "id" Jsont.string ~enc:(fun t -> t.id)
    |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun t -> t.name)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Problem = struct
  type label = string

  type t =
    | Invalid_credential
    | Expired_credential
    | Refresh_failed
    | Revoked
    | Wrong_account
    | Wrong_organization
    | Rate_limited
    | Quota_exceeded
    | Network
    | Unsupported
    | Other of label

  let valid_label label =
    let len = String.length label in
    let valid_first = function 'a' .. 'z' -> true | _ -> false in
    let valid_rest = function
      | 'a' .. 'z' | '0' .. '9' | '_' -> true
      | _ -> false
    in
    let rec loop index =
      index = len
      || (valid_rest (String.unsafe_get label index) && loop (index + 1))
    in
    len > 0 && valid_first (String.unsafe_get label 0) && loop 1

  let to_string = function
    | Invalid_credential -> "invalid_credential"
    | Expired_credential -> "expired_credential"
    | Refresh_failed -> "refresh_failed"
    | Revoked -> "revoked"
    | Wrong_account -> "wrong_account"
    | Wrong_organization -> "wrong_organization"
    | Rate_limited -> "rate_limited"
    | Quota_exceeded -> "quota_exceeded"
    | Network -> "network"
    | Unsupported -> "unsupported"
    | Other label -> label

  let is_reserved = function
    | "invalid_credential" | "expired_credential" | "refresh_failed" | "revoked"
    | "wrong_account" | "wrong_organization" | "rate_limited" | "quota_exceeded"
    | "network" | "unsupported" ->
        true
    | _ -> false

  let check_other_label fn label =
    if (not (valid_label label)) || is_reserved label then
      invalid fn "label is invalid"

  let other label =
    check_other_label "Problem.other" label;
    Other label

  let check fn = function
    | Other label -> check_other_label fn label
    | Invalid_credential | Expired_credential | Refresh_failed | Revoked
    | Wrong_account | Wrong_organization | Rate_limited | Quota_exceeded
    | Network | Unsupported ->
        ()

  let of_string = function
    | "invalid_credential" -> Some Invalid_credential
    | "expired_credential" -> Some Expired_credential
    | "refresh_failed" -> Some Refresh_failed
    | "revoked" -> Some Revoked
    | "wrong_account" -> Some Wrong_account
    | "wrong_organization" -> Some Wrong_organization
    | "rate_limited" -> Some Rate_limited
    | "quota_exceeded" -> Some Quota_exceeded
    | "network" -> Some Network
    | "unsupported" -> Some Unsupported
    | label when valid_label label && not (is_reserved label) ->
        Some (Other label)
    | _ -> None

  let fatal = function
    | Invalid_credential | Expired_credential | Refresh_failed | Revoked
    | Wrong_account | Wrong_organization ->
        true
    | Rate_limited | Quota_exceeded | Network | Unsupported | Other _ -> false

  let transient = function
    | Network | Rate_limited -> true
    | Invalid_credential | Expired_credential | Refresh_failed | Revoked
    | Wrong_account | Wrong_organization | Quota_exceeded | Unsupported
    | Other _ ->
        false

  let compare a b = String.compare (to_string a) (to_string b)
  let equal a b = compare a b = 0
  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.map ~kind:"account problem"
      ~dec:(fun value ->
        match of_string value with
        | Some problem -> problem
        | None -> decode_error ("invalid account problem: " ^ value))
      ~enc:to_string Jsont.string
end

let normalize_problems fn problems =
  List.iter (Problem.check fn) problems;
  List.sort_uniq Problem.compare problems

type credential_summary = {
  source : Credential.Source.t;
  kind : Credential.Kind.t;
  fingerprint : string option;
}

type checked = {
  at : timestamp option;
  profile : Profile.t option;
  org : Org.t option;
  problems : Problem.t list;
  models : string list option;
}

type status =
  | Missing_status
  | Present_status of credential_summary
  | Checked_status of credential_summary * checked

type t = { provider : Mentat_llm.Provider.t; status : status }

module Phase = struct
  type t = Missing | Unchecked | Ready | Degraded | Blocked

  let to_string = function
    | Missing -> "missing"
    | Unchecked -> "unchecked"
    | Ready -> "ready"
    | Degraded -> "degraded"
    | Blocked -> "blocked"

  let equal a b =
    match (a, b) with
    | Missing, Missing
    | Unchecked, Unchecked
    | Ready, Ready
    | Degraded, Degraded
    | Blocked, Blocked ->
        true
    | (Missing | Unchecked | Ready | Degraded | Blocked), _ -> false

  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.enum ~kind:"account phase"
      (List.map
         (fun phase -> (to_string phase, phase))
         [ Missing; Unchecked; Ready; Degraded; Blocked ])
end

let summary_of_credential credential =
  {
    source = Credential.source credential;
    kind = Credential.kind credential;
    fingerprint = Credential.fingerprint credential;
  }

let missing ~provider = { provider; status = Missing_status }

let present credential =
  {
    provider = Credential.provider credential;
    status = Present_status (summary_of_credential credential);
  }

let checked credential ?at ?profile ?org ?(problems = []) ?models () =
  check_optional_non_negative_time "checked" "at" at;
  {
    provider = Credential.provider credential;
    status =
      Checked_status
        ( summary_of_credential credential,
          {
            at;
            profile;
            org;
            problems = normalize_problems "checked" problems;
            models = Option.map (List.sort_uniq String.compare) models;
          } );
  }

let provider t = t.provider

let credential_summary t =
  match t.status with
  | Missing_status -> None
  | Present_status summary | Checked_status (summary, _) -> Some summary

let checked_facts t =
  match t.status with
  | Missing_status | Present_status _ -> None
  | Checked_status (_, checked) -> Some checked

let source t = Option.map (fun summary -> summary.source) (credential_summary t)

let credential_kind t =
  Option.map (fun summary -> summary.kind) (credential_summary t)

let fingerprint t =
  Option.bind (credential_summary t) (fun summary -> summary.fingerprint)

let checked_at t = Option.bind (checked_facts t) (fun checked -> checked.at)
let profile t = Option.bind (checked_facts t) (fun checked -> checked.profile)
let org t = Option.bind (checked_facts t) (fun checked -> checked.org)

let problems t =
  match checked_facts t with None -> [] | Some checked -> checked.problems

let models t = Option.bind (checked_facts t) (fun checked -> checked.models)

let model_available t model =
  match models t with
  | None -> `Unknown
  | Some models ->
      if List.exists (String.equal model) models then `Available
      else `Unavailable

let phase t =
  match t.status with
  | Missing_status -> Phase.Missing
  | Present_status _ -> Phase.Unchecked
  | Checked_status (_, { problems = []; _ }) -> Phase.Ready
  | Checked_status (_, { problems; _ }) ->
      if List.exists Problem.fatal problems then Phase.Blocked
      else Phase.Degraded

let connected t =
  match phase t with
  | Phase.Ready | Phase.Degraded | Phase.Unchecked -> true
  | Phase.Missing | Phase.Blocked -> false

let equal_credential_summary a b =
  Credential.Source.equal a.source b.source
  && Credential.Kind.equal a.kind b.kind
  && equal_option String.equal a.fingerprint b.fingerprint

let equal_checked a b =
  equal_option Int64.equal a.at b.at
  && equal_option Profile.equal a.profile b.profile
  && equal_option Org.equal a.org b.org
  && List.equal Problem.equal a.problems b.problems
  && equal_option (List.equal String.equal) a.models b.models

let equal_status a b =
  match (a, b) with
  | Missing_status, Missing_status -> true
  | Present_status a, Present_status b -> equal_credential_summary a b
  | ( Checked_status (a_credential, a_checked),
      Checked_status (b_credential, b_checked) ) ->
      equal_credential_summary a_credential b_credential
      && equal_checked a_checked b_checked
  | (Missing_status | Present_status _ | Checked_status _), _ -> false

let equal a b =
  Mentat_llm.Provider.equal a.provider b.provider
  && equal_status a.status b.status

let fingerprint_jsont =
  Jsont.map ~kind:"account fingerprint"
    ~dec:(fun fingerprint ->
      if String.length fingerprint = 4 then fingerprint
      else decode_error "account fingerprint must contain exactly 4 characters")
    ~enc:Fun.id Jsont.string

let checked_at_jsont =
  Jsont.map ~kind:"account check time"
    ~dec:(fun at ->
      if Int64.compare at 0L >= 0 then at
      else decode_error "account checked_at must not be negative")
    ~enc:Fun.id Jsont.int64

(* The summary members every status but [missing] carries. [summary] projects
   them back out of whichever status arm the case belongs to. *)
let summary_mems summary map =
  map
  |> Jsont.Object.mem "source" Credential.Source.jsont ~enc:(fun status ->
      (summary status).source)
  |> Jsont.Object.mem "credential_kind" Credential.Kind.jsont
       ~enc:(fun status -> (summary status).kind)
  |> Jsont.Object.opt_mem "fingerprint" fingerprint_jsont ~enc:(fun status ->
      (summary status).fingerprint)

let missing_case =
  Jsont.Object.map ~kind:"missing account" Missing_status
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
  |> Jsont.Object.Case.map "missing" ~dec:Fun.id

let present_case =
  let summary = function
    | Present_status summary -> summary
    | Missing_status | Checked_status _ -> assert false
  in
  Jsont.Object.map ~kind:"present account" (fun source kind fingerprint ->
      Present_status { source; kind; fingerprint })
  |> summary_mems summary
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
  |> Jsont.Object.Case.map "present" ~dec:Fun.id

let checked_case =
  let summary = function
    | Checked_status (summary, _) -> summary
    | Missing_status | Present_status _ -> assert false
  in
  let facts = function
    | Checked_status (_, checked) -> checked
    | Missing_status | Present_status _ -> assert false
  in
  let dec source kind fingerprint at profile org problems models =
    Checked_status
      ( { source; kind; fingerprint },
        {
          at;
          profile;
          org;
          problems = normalize_problems "jsont" problems;
          models = Option.map (List.sort_uniq String.compare) models;
        } )
  in
  Jsont.Object.map ~kind:"checked account" dec
  |> summary_mems summary
  |> Jsont.Object.opt_mem "checked_at" checked_at_jsont ~enc:(fun status ->
      (facts status).at)
  |> Jsont.Object.opt_mem "profile" Profile.jsont ~enc:(fun status ->
      (facts status).profile)
  |> Jsont.Object.opt_mem "org" Org.jsont ~enc:(fun status ->
      (facts status).org)
  |> Jsont.Object.mem "problems"
       (Jsont.list Problem.jsont)
       ~dec_absent:[]
       ~enc:(fun status -> (facts status).problems)
       ~enc_omit:List.is_empty
  |> Jsont.Object.opt_mem "models"
       (Jsont.list Jsont.string)
       ~enc:(fun status -> (facts status).models)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
  |> Jsont.Object.Case.map "checked" ~dec:Fun.id

let jsont =
  let cases =
    List.map Jsont.Object.Case.make [ missing_case; present_case; checked_case ]
  in
  let enc_case = function
    | Missing_status as status -> Jsont.Object.Case.value missing_case status
    | Present_status _ as status -> Jsont.Object.Case.value present_case status
    | Checked_status _ as status -> Jsont.Object.Case.value checked_case status
  in
  Jsont.Object.map ~kind:"account" (fun provider status -> { provider; status })
  |> Jsont.Object.mem "provider" Mentat_llm.Provider.jsont ~enc:(fun t ->
      t.provider)
  |> Jsont.Object.case_mem "status" Jsont.string
       ~enc:(fun t -> t.status)
       ~enc_case cases
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let pp_timestamp ppf timestamp = Format.fprintf ppf "%Ld" timestamp

let pp_credential_summary ppf t =
  Format.fprintf ppf "@[<2>{source=%a; kind=%a; fingerprint=%a}@]"
    Credential.Source.pp t.source Credential.Kind.pp t.kind
    Format.(pp_print_option pp_print_string)
    t.fingerprint

let pp_problems ppf problems =
  Format.(
    pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
      Problem.pp)
    ppf problems

let pp_models ppf models =
  Format.(
    pp_print_option
      (pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
         pp_print_string))
    ppf models

let pp_checked ppf checked =
  Format.fprintf ppf
    "@[<2>{at=%a; profile=%a; org=%a; problems=[%a]; models=%a}@]"
    Format.(pp_print_option pp_timestamp)
    checked.at
    Format.(pp_print_option Profile.pp)
    checked.profile
    Format.(pp_print_option Org.pp)
    checked.org pp_problems checked.problems pp_models checked.models

let pp ppf t =
  match t.status with
  | Missing_status ->
      Format.fprintf ppf "@[<2>{provider=%a; state=missing}@]"
        Mentat_llm.Provider.pp t.provider
  | Present_status credential ->
      Format.fprintf ppf "@[<2>{provider=%a; state=present; credential=%a}@]"
        Mentat_llm.Provider.pp t.provider pp_credential_summary credential
  | Checked_status (credential, checked) ->
      Format.fprintf ppf
        "@[<2>{provider=%a; state=checked; credential=%a; checked=%a}@]"
        Mentat_llm.Provider.pp t.provider pp_credential_summary credential
        pp_checked checked

let account_equal = equal
let account_jsont = jsont

module Discovery = struct
  type nonrec t =
    | Known of t
    | Resolution_failed of {
        provider : Mentat_llm.Provider.t;
        error : Credential_error.t;
      }

  let provider = function
    | Known account -> provider account
    | Resolution_failed { provider; _ } -> provider

  let connected = function
    | Known account -> connected account
    | Resolution_failed _ -> false

  let equal a b =
    match (a, b) with
    | Known a, Known b -> account_equal a b
    | ( Resolution_failed { provider = a_provider; error = a_error },
        Resolution_failed { provider = b_provider; error = b_error } ) ->
        Mentat_llm.Provider.equal a_provider b_provider
        && Credential_error.equal a_error b_error
    | Known _, Resolution_failed _ | Resolution_failed _, Known _ -> false

  let jsont =
    let known =
      Jsont.Object.map ~kind:"known account discovery" (fun account ->
          Known account)
      |> Jsont.Object.mem "account" account_jsont ~enc:(function
        | Known account -> account
        | Resolution_failed _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "known" ~dec:Fun.id
    in
    let resolution_failed =
      Jsont.Object.map ~kind:"failed account discovery" (fun provider error ->
          Resolution_failed { provider; error })
      |> Jsont.Object.mem "provider" Mentat_llm.Provider.jsont ~enc:(function
        | Resolution_failed { provider; _ } -> provider
        | Known _ -> assert false)
      |> Jsont.Object.mem "error" Credential_error.jsont ~enc:(function
        | Resolution_failed { error; _ } -> error
        | Known _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "resolution_failed" ~dec:Fun.id
    in
    let cases = List.map Jsont.Object.Case.make [ known; resolution_failed ] in
    let enc_case = function
      | Known _ as discovery -> Jsont.Object.Case.value known discovery
      | Resolution_failed _ as discovery ->
          Jsont.Object.Case.value resolution_failed discovery
    in
    Jsont.Object.map ~kind:"account discovery" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Logout = struct
  type remote = Revoked | Unsupported | Failed of Problem.t
  type local = Removed | Superseded
  type revocation = Not_stored | Settled of { remote : remote; local : local }
  type t = { current : Discovery.t; revocation : revocation option }

  let equal_remote a b =
    match (a, b) with
    | Revoked, Revoked | Unsupported, Unsupported -> true
    | Failed a, Failed b -> Problem.equal a b
    | (Revoked | Unsupported | Failed _), _ -> false

  let equal_local a b =
    match (a, b) with
    | Removed, Removed | Superseded, Superseded -> true
    | (Removed | Superseded), _ -> false

  let equal_revocation a b =
    match (a, b) with
    | Not_stored, Not_stored -> true
    | ( Settled { remote = a_remote; local = a_local },
        Settled { remote = b_remote; local = b_local } ) ->
        equal_remote a_remote b_remote && equal_local a_local b_local
    | Not_stored, Settled _ | Settled _, Not_stored -> false

  let equal a b =
    Discovery.equal a.current b.current
    && equal_option equal_revocation a.revocation b.revocation

  let remote_jsont =
    let revoked =
      Jsont.Object.map ~kind:"revoked remote credential" Revoked
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "revoked" ~dec:Fun.id
    in
    let unsupported =
      Jsont.Object.map ~kind:"unsupported remote revocation" Unsupported
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "unsupported" ~dec:Fun.id
    in
    let failed =
      Jsont.Object.map ~kind:"failed remote revocation" (fun problem ->
          Failed problem)
      |> Jsont.Object.mem "problem" Problem.jsont ~enc:(function
        | Failed problem -> problem
        | Revoked | Unsupported -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "failed" ~dec:Fun.id
    in
    let cases =
      List.map Jsont.Object.Case.make [ revoked; unsupported; failed ]
    in
    let enc_case = function
      | Revoked as remote -> Jsont.Object.Case.value revoked remote
      | Unsupported as remote -> Jsont.Object.Case.value unsupported remote
      | Failed _ as remote -> Jsont.Object.Case.value failed remote
    in
    Jsont.Object.map ~kind:"remote revocation result" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let local_jsont =
    Jsont.enum ~kind:"local revocation result"
      [ ("removed", Removed); ("superseded", Superseded) ]

  let revocation_jsont =
    let not_stored =
      Jsont.Object.map ~kind:"unstored revocation" Not_stored
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "not_stored" ~dec:Fun.id
    in
    let settled =
      Jsont.Object.map ~kind:"settled revocation" (fun remote local ->
          Settled { remote; local })
      |> Jsont.Object.mem "remote" remote_jsont ~enc:(function
        | Settled { remote; _ } -> remote
        | Not_stored -> assert false)
      |> Jsont.Object.mem "local" local_jsont ~enc:(function
        | Settled { local; _ } -> local
        | Not_stored -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "settled" ~dec:Fun.id
    in
    let cases = List.map Jsont.Object.Case.make [ not_stored; settled ] in
    let enc_case = function
      | Not_stored as result -> Jsont.Object.Case.value not_stored result
      | Settled _ as result -> Jsont.Object.Case.value settled result
    in
    Jsont.Object.map ~kind:"account revocation result" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let jsont =
    Jsont.Object.map ~kind:"account logout result" (fun current revocation ->
        { current; revocation })
    |> Jsont.Object.mem "current" Discovery.jsont ~enc:(fun t -> t.current)
    |> Jsont.Object.opt_mem "revocation" revocation_jsont ~enc:(fun t ->
        t.revocation)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end
