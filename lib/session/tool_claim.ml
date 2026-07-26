(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Id =
  String_id.Make
    (struct
      let module_path = "Mentat_session.Tool_claim.Id"
      let kind = "tool claim id"
    end)
    ()

let derive ~turn ~call_id ~stage =
  Id.of_string
    (String_id.derive_id ~domain:"mentat.session.tool.v1"
       [ Turn.Id.to_string turn; call_id; Mentat_tool.Stage.to_string stage ])

(* The erased result codec the session stores: an [Output.t Result.t]. *)
let result_jsont = Mentat_tool.Result.jsont Mentat_tool.Output.jsont
let result_equal = Mentat_tool.Result.equal Mentat_tool.Output.equal

module Started = struct
  type t = {
    turn : Turn.Id.t;
    stage : Mentat_tool.Stage.t;
    call : Mentat_llm.Tool.Call.t;
    input : Jsont.json;
    requests : Mentat_permission.Request.t list;
  }

  let make ~turn ~stage ~call ~input ~requests =
    { turn; stage; call; input; requests }

  let turn t = t.turn
  let stage t = t.stage
  let call t = t.call
  let input t = t.input
  let requests t = t.requests

  let id t =
    derive ~turn:t.turn ~call_id:(Mentat_llm.Tool.Call.id t.call) ~stage:t.stage

  let equal a b =
    Turn.Id.equal a.turn b.turn
    && Mentat_tool.Stage.equal a.stage b.stage
    && Mentat_llm.Tool.Call.equal a.call b.call
    && Jsont.Json.equal a.input b.input
    && List.equal Mentat_permission.Request.equal a.requests b.requests

  let pp ppf t =
    Format.fprintf ppf "@[<hov>{ id = %a; turn = %a; stage = %a; call = %s }@]"
      Id.pp (id t) Turn.Id.pp t.turn Mentat_tool.Stage.pp t.stage
      (Mentat_llm.Tool.Call.id t.call)

  let jsont =
    Jsont.Object.map ~kind:"started tool claim"
      (fun turn stage call input requests ->
        make ~turn ~stage ~call ~input ~requests)
    |> Jsont.Object.mem "turn" Turn.Id.jsont ~enc:turn
    |> Jsont.Object.mem "stage" Mentat_tool.Stage.jsont ~enc:stage
    |> Jsont.Object.mem "call" Mentat_llm.Tool.Call.jsont ~enc:call
    |> Jsont.Object.mem "input" Jsont.json ~enc:input
    |> Jsont.Object.mem "requests"
         (Jsont.list Mentat_permission.Request.jsont)
         ~enc:requests
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Settled = struct
  type outcome =
    | Prepared of Mentat_tool.Prepared.t
    | Returned of Mentat_tool.Output.t Mentat_tool.Result.t
    | Ambiguous

  type t = { id : Id.t; outcome : outcome }

  let prepared ~id payload = { id; outcome = Prepared payload }
  let returned ~id result = { id; outcome = Returned result }
  let ambiguous ~id = { id; outcome = Ambiguous }
  let id t = t.id
  let outcome t = t.outcome

  let equal_outcome a b =
    match (a, b) with
    | Prepared a, Prepared b -> Mentat_tool.Prepared.equal a b
    | Returned a, Returned b -> result_equal a b
    | Ambiguous, Ambiguous -> true
    | (Prepared _ | Returned _ | Ambiguous), _ -> false

  let equal a b = Id.equal a.id b.id && equal_outcome a.outcome b.outcome

  let pp ppf t =
    match t.outcome with
    | Prepared _ -> Format.fprintf ppf "prepared(%a)" Id.pp t.id
    | Returned _ -> Format.fprintf ppf "returned(%a)" Id.pp t.id
    | Ambiguous -> Format.fprintf ppf "ambiguous(%a)" Id.pp t.id

  let jsont =
    let prepared_case =
      Jsont.Object.map ~kind:"tool claim prepared" (fun id payload ->
          prepared ~id payload)
      |> Jsont.Object.mem "id" Id.jsont ~enc:id
      |> Jsont.Object.mem "payload" Mentat_tool.Prepared.jsont ~enc:(fun t ->
          match t.outcome with
          | Prepared payload -> payload
          | Returned _ | Ambiguous -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "prepared" ~dec:Fun.id
    in
    let returned_case =
      Jsont.Object.map ~kind:"tool claim returned" (fun id result ->
          returned ~id result)
      |> Jsont.Object.mem "id" Id.jsont ~enc:id
      |> Jsont.Object.mem "result" result_jsont ~enc:(fun t ->
          match t.outcome with
          | Returned result -> result
          | Prepared _ | Ambiguous -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "returned" ~dec:Fun.id
    in
    let ambiguous_case =
      Jsont.Object.map ~kind:"tool claim ambiguous" (fun id -> ambiguous ~id)
      |> Jsont.Object.mem "id" Id.jsont ~enc:id
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "ambiguous" ~dec:Fun.id
    in
    let cases =
      List.map Jsont.Object.Case.make
        [ prepared_case; returned_case; ambiguous_case ]
    in
    let enc_case t =
      match t.outcome with
      | Prepared _ -> Jsont.Object.Case.value prepared_case t
      | Returned _ -> Jsont.Object.Case.value returned_case t
      | Ambiguous -> Jsont.Object.Case.value ambiguous_case t
    in
    Jsont.Object.map ~kind:"tool claim settlement" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end
