(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

let invalid fn message = invalid_arg' "Mentat_session.Plan" fn message

module Error = struct
  type t = Empty_body | Empty_title

  let message = function
    | Empty_body -> "body must not be empty"
    | Empty_title -> "title must not be empty"

  let pp ppf t = Format.pp_print_string ppf (message t)
end

type t = { title : string option; body : string }

let make ?title ~body () =
  if String.is_empty body then Error Error.Empty_body
  else
    match title with
    | Some title when String.is_empty title -> Error Error.Empty_title
    | Some _ | None -> Ok { title; body }

let title t = t.title
let body t = t.body

let equal a b =
  Option.equal String.equal a.title b.title && String.equal a.body b.body

let pp ppf t =
  match t.title with
  | None -> Format.fprintf ppf "plan(%S)" t.body
  | Some title -> Format.fprintf ppf "plan(%S, %S)" title t.body

let jsont =
  Jsont.Object.map ~kind:"plan proposal" (fun title body ->
      match make ?title ~body () with
      | Ok plan -> plan
      | Error e -> decode_error (Error.message e))
  |> Jsont.Object.opt_mem "title" Jsont.string ~enc:title
  |> Jsont.Object.mem "body" Jsont.string ~enc:body
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let plan_title = title
let plan_body = body
let equal_plan = equal

module Approval = struct
  type plan = t

  type t = {
    body : plan;
    context : [ `Current | `Fresh ];
    feedback : string option;
  }

  let make ~body ~context ?feedback () =
    (match feedback with
    | Some feedback when String.is_empty feedback ->
        invalid "Answer.approve" "feedback must not be empty"
    | Some _ | None -> ());
    { body; context; feedback }

  let body (t : t) = t.body
  let context (t : t) = t.context
  let feedback (t : t) = t.feedback

  let to_model_text (t : t) =
    let heading =
      match plan_title t.body with
      | None -> "## Approved plan"
      | Some title -> "## Approved plan: " ^ title
    in
    let feedback =
      match t.feedback with
      | None -> ""
      | Some feedback -> "\n\n## Implementation feedback\n" ^ feedback
    in
    "The user approved this plan. Implement it now.\n\n" ^ heading ^ "\n"
    ^ plan_body t.body ^ feedback

  let equal (a : t) (b : t) =
    equal_plan a.body b.body && a.context = b.context
    && Option.equal String.equal a.feedback b.feedback

  let pp ppf (t : t) =
    let context =
      match t.context with `Current -> "current" | `Fresh -> "fresh"
    in
    Format.fprintf ppf "approval(%a, %s)" pp t.body context

  let context_jsont =
    Jsont.enum ~kind:"plan build context"
      [ ("current", `Current); ("fresh", `Fresh) ]

  let jsont =
    Jsont.Object.map ~kind:"plan approval" (fun body context feedback ->
        decode_invalid_arg (fun () -> make ~body ~context ?feedback ()))
    |> Jsont.Object.mem "body" jsont ~enc:body
    |> Jsont.Object.mem "context" context_jsont ~enc:context
    |> Jsont.Object.opt_mem "feedback" Jsont.string ~enc:feedback
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Answer = struct
  type plan = t
  type t = Keep_planning | Revise of string | Approve of Approval.t

  let keep_planning = Keep_planning

  let revise feedback =
    if String.is_empty feedback then
      invalid "Answer.revise" "feedback must not be empty";
    Revise feedback

  let approve ~body ~context ?feedback () =
    Approve (Approval.make ~body ~context ?feedback ())

  let equal a b =
    match (a, b) with
    | Keep_planning, Keep_planning -> true
    | Revise a, Revise b -> String.equal a b
    | Approve a, Approve b -> Approval.equal a b
    | (Keep_planning | Revise _ | Approve _), _ -> false

  let pp ppf = function
    | Keep_planning -> Format.pp_print_string ppf "keep-planning"
    | Revise feedback -> Format.fprintf ppf "revise(%S)" feedback
    | Approve approval -> Format.fprintf ppf "approve(%a)" Approval.pp approval

  let jsont =
    let keep_case =
      Jsont.Object.map ~kind:"keep planning" Keep_planning
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "keep_planning" ~dec:Fun.id
    in
    let revise_case =
      Jsont.Object.map ~kind:"revise plan" (fun feedback ->
          decode_invalid_arg (fun () -> revise feedback))
      |> Jsont.Object.mem "feedback" Jsont.string ~enc:(function
        | Revise feedback -> feedback
        | Keep_planning | Approve _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "revise" ~dec:Fun.id
    in
    let approve_case =
      Jsont.Object.map ~kind:"approve plan" (fun body context feedback ->
          decode_invalid_arg (fun () ->
              Approve (Approval.make ~body ~context ?feedback ())))
      |> Jsont.Object.mem "body" jsont ~enc:(function
        | Approve approval -> Approval.body approval
        | Keep_planning | Revise _ -> assert false)
      |> Jsont.Object.mem "context" Approval.context_jsont ~enc:(function
        | Approve approval -> Approval.context approval
        | Keep_planning | Revise _ -> assert false)
      |> Jsont.Object.opt_mem "feedback" Jsont.string ~enc:(function
        | Approve approval -> Approval.feedback approval
        | Keep_planning | Revise _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "approve" ~dec:Fun.id
    in
    let cases =
      List.map Jsont.Object.Case.make [ keep_case; revise_case; approve_case ]
    in
    let enc_case = function
      | Keep_planning as answer -> Jsont.Object.Case.value keep_case answer
      | Revise _ as answer -> Jsont.Object.Case.value revise_case answer
      | Approve _ as answer -> Jsont.Object.Case.value approve_case answer
    in
    Jsont.Object.map ~kind:"plan answer" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end
