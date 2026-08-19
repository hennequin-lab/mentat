(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

let invalid fn message =
  invalid_arg' "Mentat_session.Provider_request" fn message

module Id =
  String_id.Make
    (struct
      let module_path = "Mentat_session.Provider_request.Id"
      let kind = "provider request id"
    end)
    ()

let derive ~turn ~request_digest =
  Id.of_string
    (String_id.derive_id ~domain:"mentat.session.provider.v1"
       [ Turn.Id.to_string turn; Mentat_digest.to_hex request_digest ])

module Started = struct
  type t = { turn : Turn.Id.t; request_digest : Mentat_digest.t }

  let make ~turn ~request_digest = { turn; request_digest }
  let turn t = t.turn
  let request_digest t = t.request_digest
  let id t = derive ~turn:t.turn ~request_digest:t.request_digest

  let equal a b =
    Turn.Id.equal a.turn b.turn
    && Mentat_digest.equal a.request_digest b.request_digest

  let pp ppf t =
    Format.fprintf ppf "@[<hov>{ id = %a; turn = %a; request = %a }@]" Id.pp
      (id t) Turn.Id.pp t.turn Mentat_digest.pp t.request_digest

  let jsont =
    Jsont.Object.map ~kind:"provider request claim" (fun turn request_digest ->
        make ~turn ~request_digest)
    |> Jsont.Object.mem "turn" Turn.Id.jsont ~enc:turn
    |> Jsont.Object.mem "request_digest" Mentat_digest.jsont ~enc:request_digest
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Settled = struct
  type outcome =
    | Responded of Mentat_llm.Response.t
    | Interrupted of { text : string; usage : Mentat_llm.Usage.t option }
    | Failed of Mentat_llm.Error.t
    | Ambiguous

  type t = { id : Id.t; outcome : outcome }

  let responded ~id response = { id; outcome = Responded response }

  let interrupted ~id ?usage ~text () =
    if String.is_empty (String.trim text) then
      invalid "Settled.interrupted" "text must contain visible prose";
    { id; outcome = Interrupted { text; usage } }

  let failed ~id error = { id; outcome = Failed error }
  let ambiguous ~id = { id; outcome = Ambiguous }
  let id t = t.id
  let outcome t = t.outcome

  let equal_outcome a b =
    match (a, b) with
    | Responded a, Responded b -> Mentat_llm.Response.equal a b
    | Interrupted a, Interrupted b ->
        String.equal a.text b.text
        && Option.equal Mentat_llm.Usage.equal a.usage b.usage
    | Failed a, Failed b -> Mentat_llm.Error.equal a b
    | Ambiguous, Ambiguous -> true
    | (Responded _ | Interrupted _ | Failed _ | Ambiguous), _ -> false

  let equal a b = Id.equal a.id b.id && equal_outcome a.outcome b.outcome

  let pp ppf t =
    match t.outcome with
    | Responded response ->
        Format.fprintf ppf "responded(%a, %S)" Id.pp t.id
          (Mentat_llm.Response.text response)
    | Interrupted { text; _ } ->
        Format.fprintf ppf "interrupted(%a, %S)" Id.pp t.id text
    | Failed error ->
        Format.fprintf ppf "failed(%a, %a)" Id.pp t.id Mentat_llm.Error.pp error
    | Ambiguous -> Format.fprintf ppf "ambiguous(%a)" Id.pp t.id

  let jsont =
    let responded_case =
      Jsont.Object.map ~kind:"provider responded" (fun id response ->
          responded ~id response)
      |> Jsont.Object.mem "id" Id.jsont ~enc:id
      |> Jsont.Object.mem "response" Mentat_llm.Response.jsont ~enc:(fun t ->
          match t.outcome with
          | Responded response -> response
          | Interrupted _ | Failed _ | Ambiguous -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "responded" ~dec:Fun.id
    in
    let interrupted_case =
      Jsont.Object.map ~kind:"provider interrupted" (fun id text usage ->
          decode_invalid_arg (fun () -> interrupted ~id ?usage ~text ()))
      |> Jsont.Object.mem "id" Id.jsont ~enc:id
      |> Jsont.Object.mem "text" Jsont.string ~enc:(fun t ->
          match t.outcome with
          | Interrupted { text; _ } -> text
          | Responded _ | Failed _ | Ambiguous -> assert false)
      |> Jsont.Object.opt_mem "usage" Mentat_llm.Usage.jsont ~enc:(fun t ->
          match t.outcome with
          | Interrupted { usage; _ } -> usage
          | Responded _ | Failed _ | Ambiguous -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "interrupted" ~dec:Fun.id
    in
    let failed_case =
      Jsont.Object.map ~kind:"provider failed" (fun id error ->
          failed ~id error)
      |> Jsont.Object.mem "id" Id.jsont ~enc:id
      |> Jsont.Object.mem "error" Mentat_llm.Error.jsont ~enc:(fun t ->
          match t.outcome with
          | Failed error -> error
          | Responded _ | Interrupted _ | Ambiguous -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "failed" ~dec:Fun.id
    in
    let ambiguous_case =
      Jsont.Object.map ~kind:"provider ambiguous" (fun id -> ambiguous ~id)
      |> Jsont.Object.mem "id" Id.jsont ~enc:id
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "ambiguous" ~dec:Fun.id
    in
    let cases =
      List.map Jsont.Object.Case.make
        [ responded_case; interrupted_case; failed_case; ambiguous_case ]
    in
    let enc_case t =
      match t.outcome with
      | Responded _ -> Jsont.Object.Case.value responded_case t
      | Interrupted _ -> Jsont.Object.Case.value interrupted_case t
      | Failed _ -> Jsont.Object.Case.value failed_case t
      | Ambiguous -> Jsont.Object.Case.value ambiguous_case t
    in
    Jsont.Object.map ~kind:"provider settlement" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end
