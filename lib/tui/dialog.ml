(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Decision = Mentat_session.Decision

type form =
  | Permission of Permission_dialog.t
  | Plan of Plan_dialog.t
  | Question of Question_dialog.t

type t = { requested : Decision.Requested.t; form : form }
type outcome = Stay | Resolve of Decision.Answer.t | Flash of string

type msg =
  | Permission_msg of Permission_dialog.msg
  | Plan_msg of Plan_dialog.msg
  | Question_msg of Question_dialog.msg

let make requested =
  let form =
    match Decision.Requested.request requested with
    | Decision.Request.Permission review ->
        Permission (Permission_dialog.make review)
    | Decision.Request.Plan plan -> Plan (Plan_dialog.make plan)
    | Decision.Request.Question question ->
        Question (Question_dialog.make question)
  in
  { requested; form }

let requested t = t.requested

let permission_outcome t dialog = function
  | Permission_dialog.Stay -> ({ t with form = Permission dialog }, Stay)
  | Permission_dialog.Answer { answer; message } ->
      ( { t with form = Permission dialog },
        Resolve (Decision.Answer.Permission { answer; message }) )

let plan_outcome t dialog = function
  | Plan_dialog.Stay -> ({ t with form = Plan dialog }, Stay)
  | Plan_dialog.Answer answer ->
      ({ t with form = Plan dialog }, Resolve (Decision.Answer.Plan answer))
  | Plan_dialog.Flash message -> ({ t with form = Plan dialog }, Flash message)

let question_outcome t dialog = function
  | Question_dialog.Stay -> ({ t with form = Question dialog }, Stay)
  | Question_dialog.Answer answer ->
      ( { t with form = Question dialog },
        Resolve (Decision.Answer.Question answer) )
  | Question_dialog.Flash message ->
      ({ t with form = Question dialog }, Flash message)

let key event t =
  match t.form with
  | Permission dialog ->
      let dialog, outcome = Permission_dialog.key event dialog in
      permission_outcome t dialog outcome
  | Plan dialog ->
      let dialog, outcome = Plan_dialog.key event dialog in
      plan_outcome t dialog outcome
  | Question dialog ->
      let dialog, outcome = Question_dialog.key event dialog in
      question_outcome t dialog outcome

let update message t =
  match (message, t.form) with
  | Permission_msg message, Permission dialog ->
      let dialog, outcome = Permission_dialog.update message dialog in
      permission_outcome t dialog outcome
  | Plan_msg message, Plan dialog ->
      let dialog, outcome = Plan_dialog.update message dialog in
      plan_outcome t dialog outcome
  | Question_msg message, Question dialog ->
      let dialog, outcome = Question_dialog.update message dialog in
      question_outcome t dialog outcome
  | Permission_msg _, (Plan _ | Question _)
  | Plan_msg _, (Permission _ | Question _)
  | Question_msg _, (Permission _ | Plan _) ->
      (t, Stay)

let editing t =
  match t.form with
  | Permission dialog -> Permission_dialog.editing dialog
  | Plan dialog -> Plan_dialog.editing dialog
  | Question dialog -> Question_dialog.editing dialog

let view ~palette (t : t) : msg Mosaic.t =
  match t.form with
  | Permission dialog ->
      Permission_dialog.view ~palette dialog
      |> Mosaic.map (fun message -> Permission_msg message)
  | Plan dialog ->
      Plan_dialog.view ~palette dialog
      |> Mosaic.map (fun message -> Plan_msg message)
  | Question dialog ->
      Question_dialog.view ~palette dialog
      |> Mosaic.map (fun message -> Question_msg message)
