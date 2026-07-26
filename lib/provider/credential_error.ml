(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t =
  | Unsupported_credential_kind of {
      source : Credential.Source.t;
      kind : Credential.Kind.t;
      accepted : Credential.Kind.t list;
    }
  | Invalid_credential_text of {
      source : Credential.Source.t;
      kind : Credential.Kind.t;
    }

let equal a b =
  match (a, b) with
  | Unsupported_credential_kind a, Unsupported_credential_kind b ->
      Credential.Source.equal a.source b.source
      && Credential.Kind.equal a.kind b.kind
      && List.equal Credential.Kind.equal a.accepted b.accepted
  | Invalid_credential_text a, Invalid_credential_text b ->
      Credential.Source.equal a.source b.source
      && Credential.Kind.equal a.kind b.kind
  | Unsupported_credential_kind _, Invalid_credential_text _
  | Invalid_credential_text _, Unsupported_credential_kind _ ->
      false

let message = function
  | Unsupported_credential_kind { source; kind; accepted } ->
      let accepted =
        match accepted with
        | [] -> "none"
        | kinds ->
            String.concat ", "
              (List.map
                 (fun kind -> Format.asprintf "%a" Credential.Kind.pp kind)
                 kinds)
      in
      Format.asprintf
        "credential from %a has kind %a, which the provider does not accept \
         (accepted: %s)"
        Credential.Source.pp source Credential.Kind.pp kind accepted
  | Invalid_credential_text { source; kind } ->
      Format.asprintf "credential from %a has kind %a with invalid text"
        Credential.Source.pp source Credential.Kind.pp kind

let pp ppf error = Format.pp_print_string ppf (message error)

let jsont =
  let unsupported =
    Jsont.Object.map ~kind:"unsupported credential kind"
      (fun source kind accepted ->
        Unsupported_credential_kind { source; kind; accepted })
    |> Jsont.Object.mem "source" Credential.Source.jsont ~enc:(function
      | Unsupported_credential_kind { source; _ } -> source
      | Invalid_credential_text _ -> assert false)
    |> Jsont.Object.mem "kind" Credential.Kind.jsont ~enc:(function
      | Unsupported_credential_kind { kind; _ } -> kind
      | Invalid_credential_text _ -> assert false)
    |> Jsont.Object.mem "accepted" (Jsont.list Credential.Kind.jsont)
         ~enc:(function
         | Unsupported_credential_kind { accepted; _ } -> accepted
         | Invalid_credential_text _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "unsupported_credential_kind" ~dec:Fun.id
  in
  let invalid_text =
    Jsont.Object.map ~kind:"invalid credential text" (fun source kind ->
        Invalid_credential_text { source; kind })
    |> Jsont.Object.mem "source" Credential.Source.jsont ~enc:(function
      | Invalid_credential_text { source; _ } -> source
      | Unsupported_credential_kind _ -> assert false)
    |> Jsont.Object.mem "kind" Credential.Kind.jsont ~enc:(function
      | Invalid_credential_text { kind; _ } -> kind
      | Unsupported_credential_kind _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "invalid_credential_text" ~dec:Fun.id
  in
  let cases = List.map Jsont.Object.Case.make [ unsupported; invalid_text ] in
  let enc_case = function
    | Unsupported_credential_kind _ as error ->
        Jsont.Object.Case.value unsupported error
    | Invalid_credential_text _ as error ->
        Jsont.Object.Case.value invalid_text error
  in
  Jsont.Object.map ~kind:"credential resolution error" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
