(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Id =
  String_id.Make
    (struct
      let module_path = "Mentat_session.Decision.Id"
      let kind = "decision id"
    end)
    ()

let derive_id ~turn ~call_id ~stage =
  Id.of_string
    (String_id.derive_id ~domain:"mentat.session.decision.v1"
       [ Turn.Id.to_string turn; call_id; Mentat_tool.Stage.to_string stage ])

module Request = struct
  type t =
    | Permission of Mentat_permission.Policy.Review.t
    | Question of Question.t
    | Plan of Plan.t

  let tag = function
    | Permission _ -> "permission"
    | Question _ -> "question"
    | Plan _ -> "plan"

  let jsont =
    let permission_case =
      Jsont.Object.map ~kind:"permission request" (fun review ->
          Permission review)
      |> Jsont.Object.mem "review" Mentat_permission.Policy.Review.jsont
           ~enc:(function
           | Permission review -> review
           | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "permission" ~dec:Fun.id
    in
    let question_case =
      Jsont.Object.map ~kind:"question request" (fun q -> Question q)
      |> Jsont.Object.mem "prompt" Question.jsont ~enc:(function
        | Question q -> q
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "question" ~dec:Fun.id
    in
    let plan_case =
      Jsont.Object.map ~kind:"plan request" (fun p -> Plan p)
      |> Jsont.Object.mem "proposal" Plan.jsont ~enc:(function
        | Plan p -> p
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "plan" ~dec:Fun.id
    in
    let cases =
      List.map Jsont.Object.Case.make
        [ permission_case; question_case; plan_case ]
    in
    let enc_case req =
      match req with
      | Permission _ -> Jsont.Object.Case.value permission_case req
      | Question _ -> Jsont.Object.Case.value question_case req
      | Plan _ -> Jsont.Object.Case.value plan_case req
    in
    Jsont.Object.map ~kind:"decision request payload" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let equal a b =
    match (a, b) with
    | Permission a, Permission b -> Mentat_permission.Policy.Review.equal a b
    | Question a, Question b -> Question.equal a b
    | Plan a, Plan b -> Plan.equal a b
    | (Permission _ | Question _ | Plan _), _ -> false
end

module Requested = struct
  type t = {
    id : Id.t;
    turn : Turn.Id.t;
    call : Mentat_llm.Tool.Call.t;
    stage : Mentat_tool.Stage.t;
    request : Request.t;
  }

  let of_fields ~turn ~call ~stage request =
    let call_id = Mentat_llm.Tool.Call.id call in
    { id = derive_id ~turn ~call_id ~stage; turn; call; stage; request }

  let make ~turn ~call ~stage request = of_fields ~turn ~call ~stage request
  let id t = t.id
  let turn t = t.turn
  let call t = t.call
  let call_id t = Mentat_llm.Tool.Call.id t.call
  let name t = Mentat_llm.Tool.Call.name t.call
  let stage t = t.stage
  let request t = t.request
  let tag t = Request.tag t.request

  let pp ppf t =
    Format.fprintf ppf "@[<hov>decision(%a, %s, call=%s)@]" Id.pp t.id (tag t)
      (call_id t)

  let jsont =
    Jsont.Object.map ~kind:"decision request" (fun turn call stage request ->
        of_fields ~turn ~call ~stage request)
    |> Jsont.Object.mem "turn" Turn.Id.jsont ~enc:turn
    |> Jsont.Object.mem "call" Mentat_llm.Tool.Call.jsont ~enc:call
    |> Jsont.Object.mem "stage" Mentat_tool.Stage.jsont ~enc:stage
    |> Jsont.Object.mem "request" Request.jsont ~enc:request
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let equal a b =
    Id.equal a.id b.id
    && Turn.Id.equal a.turn b.turn
    && Mentat_llm.Tool.Call.equal a.call b.call
    && Mentat_tool.Stage.equal a.stage b.stage
    && Request.equal a.request b.request
end

module Answer = struct
  type t =
    | Permission of {
        answer : Mentat_permission.Answer.t;
        message : string option;
      }
    | Question of Question.Answer.t
    | Plan of Plan.Answer.t

  let tag = function
    | Permission _ -> "permission"
    | Question _ -> "question"
    | Plan _ -> "plan"

  let equal a b =
    match (a, b) with
    | Permission a, Permission b ->
        Mentat_permission.Answer.equal a.answer b.answer
        && Option.equal String.equal a.message b.message
    | Question a, Question b -> Question.Answer.equal a b
    | Plan a, Plan b -> Plan.Answer.equal a b
    | (Permission _ | Question _ | Plan _), _ -> false

  let pp ppf = function
    | Permission { answer; message = None } ->
        Format.fprintf ppf "permission(%a)" Mentat_permission.Answer.pp answer
    | Permission { answer; message = Some m } ->
        Format.fprintf ppf "permission(%a, %S)" Mentat_permission.Answer.pp
          answer m
    | Question a -> Format.fprintf ppf "question(%a)" Question.Answer.pp a
    | Plan a -> Format.fprintf ppf "plan(%a)" Plan.Answer.pp a

  let jsont =
    let case tag mem_name answer_jsont inj proj =
      Jsont.Object.map ~kind:(tag ^ " answer") (fun a -> inj a)
      |> Jsont.Object.mem mem_name answer_jsont ~enc:proj
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map tag ~dec:Fun.id
    in
    let permission_case =
      Jsont.Object.map ~kind:"permission answer" (fun answer message ->
          Permission { answer; message })
      |> Jsont.Object.mem "answer" Mentat_permission.Answer.jsont ~enc:(function
        | Permission { answer; _ } -> answer
        | _ -> assert false)
      |> Jsont.Object.opt_mem "message" Jsont.string ~enc:(function
        | Permission { message; _ } -> message
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "permission" ~dec:Fun.id
    in
    let question_case =
      case "question" "answer" Question.Answer.jsont
        (fun a -> Question a)
        (function Question a -> a | _ -> assert false)
    in
    let plan_case =
      case "plan" "answer" Plan.Answer.jsont
        (fun a -> Plan a)
        (function Plan a -> a | _ -> assert false)
    in
    let cases =
      List.map Jsont.Object.Case.make
        [ permission_case; question_case; plan_case ]
    in
    let enc_case = function
      | Permission _ as a -> Jsont.Object.Case.value permission_case a
      | Question _ as a -> Jsont.Object.Case.value question_case a
      | Plan _ as a -> Jsont.Object.Case.value plan_case a
    in
    Jsont.Object.map ~kind:"decision answer" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

type handling =
  | Proceed_permission
  | Proceed_plan of Plan.Approval.t
  | Consume of Mentat_llm.Tool.Result.t

module Resolved = struct
  type t = { id : Id.t; answered_by : Principal.t; answer : Answer.t }

  let make ~id ~answered_by ~answer = { id; answered_by; answer }
  let id t = t.id
  let answered_by t = t.answered_by
  let answer t = t.answer

  let equal a b =
    Id.equal a.id b.id
    && Principal.equal a.answered_by b.answered_by
    && Answer.equal a.answer b.answer

  let pp ppf t =
    Format.fprintf ppf "@[<hov>resolved(%a, %a)@]" Id.pp t.id Answer.pp t.answer

  let jsont =
    Jsont.Object.map ~kind:"decision resolution" (fun id answered_by answer ->
        make ~id ~answered_by ~answer)
    |> Jsont.Object.mem "id" Id.jsont ~enc:id
    |> Jsont.Object.mem "answered_by" Principal.jsont ~enc:answered_by
    |> Jsont.Object.mem "answer" Answer.jsont ~enc:answer
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Resolve_error = struct
  type t =
    | Tag_mismatch of { expected : string; got : string }
    | Call_mismatch of {
        expected : string;
        got : string;
        expected_name : string;
        got_name : string;
      }
    | Invalid_choice of { index : int; count : int }
    | Unattended_not_permitted

  let message = function
    | Tag_mismatch { expected; got } ->
        Printf.sprintf "answer %S does not match pending request %S" got
          expected
    | Call_mismatch { expected; got; expected_name; got_name } ->
        Printf.sprintf
          "resolution call %S/%S is not the exact blocked call %S/%S (input or \
           signature may differ)"
          got got_name expected expected_name
    | Invalid_choice { index; count } ->
        Printf.sprintf "choice %d is out of range for %d option(s)" index count
    | Unattended_not_permitted ->
        "an unattended-policy principal may only deny a permission"

  let pp ppf t = Format.pp_print_string ppf (message t)
end

let question_handling question answer ~call =
  match answer with
  | Question.Answer.Free text ->
      Ok (Consume (Mentat_llm.Tool.Result.text call text))
  | Question.Answer.Choice index -> (
      let choices = Option.value (Question.choices question) ~default:[] in
      match List.nth_opt choices index with
      | Some choice -> Ok (Consume (Mentat_llm.Tool.Result.text call choice))
      | None ->
          Error
            (Resolve_error.Invalid_choice { index; count = List.length choices })
      )

let handling requested answer ~call =
  match (Requested.request requested, answer) with
  | ( Request.Permission _,
      Answer.Permission { answer = Mentat_permission.Answer.Allow _; _ } ) ->
      Ok Proceed_permission
  | ( Request.Permission _,
      Answer.Permission { answer = Mentat_permission.Answer.Deny; message } ) ->
      (* Name the user's choice: a deny must read as a decision, never as an
         environment failure, or the model retries or escalates a refusal. Any
         reviewer guidance is appended as the reason, not substituted. *)
      let text =
        match message with
        | None -> "The user denied permission to run this command."
        | Some m ->
            "The user denied permission to run this command. They said: " ^ m
      in
      Ok (Consume (Mentat_llm.Tool.Result.text ~error:true call text))
  | Request.Question question, Answer.Question answer ->
      question_handling question answer ~call
  | Request.Plan _, Answer.Plan (Plan.Answer.Approve approval) ->
      Ok (Proceed_plan approval)
  | Request.Plan _, Answer.Plan Plan.Answer.Keep_planning ->
      Ok
        (Consume
           (Mentat_llm.Tool.Result.text call
              "The plan was not approved; continue planning."))
  | Request.Plan _, Answer.Plan (Plan.Answer.Revise feedback) ->
      Ok (Consume (Mentat_llm.Tool.Result.text call feedback))
  | request, answer ->
      Error
        (Resolve_error.Tag_mismatch
           { expected = Request.tag request; got = Answer.tag answer })

let validate_principal by answer =
  match by with
  | Principal.Local_user -> Ok ()
  | Principal.Unattended_policy -> (
      match answer with
      | Answer.Permission { answer = Mentat_permission.Answer.Deny; _ } -> Ok ()
      | Answer.Permission { answer = Mentat_permission.Answer.Allow _; _ }
      | Answer.Question _ | Answer.Plan _ ->
          Error Resolve_error.Unattended_not_permitted)

let resolve requested ~call ~by answer =
  if not (Mentat_llm.Tool.Call.equal call (Requested.call requested)) then
    Error
      (Resolve_error.Call_mismatch
         {
           expected = Requested.call_id requested;
           got = Mentat_llm.Tool.Call.id call;
           expected_name = Requested.name requested;
           got_name = Mentat_llm.Tool.Call.name call;
         })
  else
    match handling requested answer ~call with
    | Error _ as e -> e
    | Ok handling -> (
        match validate_principal by answer with
        | Error _ as e -> e
        | Ok () ->
            Ok
              ( Resolved.make ~id:(Requested.id requested) ~answered_by:by
                  ~answer,
                handling ))

let matches requested resolved =
  Id.equal (Requested.id requested) (Resolved.id resolved)
