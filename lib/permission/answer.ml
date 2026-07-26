(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

let invalid fn message = invalid_arg' "Mentat_permission.Answer" fn message

type lifetime = Conversation | User

type allowance =
  | Once
  | Exact_for_conversation
  | Family of { lifetime : lifetime; rules : Policy.Rule.t list }

type t = Allow of allowance | Deny

let once = Allow Once
let exact_for_conversation = Allow Exact_for_conversation
let deny = Deny

(* The [Family] invariant is enforced here so a private [allowance] cannot carry
   an empty, duplicate, or non-allow rule set. *)
let check_family rules =
  if List.is_empty rules then invalid "family" "rules must not be empty";
  if
    List.exists (fun rule -> Policy.Rule.action rule <> Policy.Rule.Allow) rules
  then invalid "family" "rules must all allow";
  let rec unique = function
    | [] -> true
    | rule :: rules ->
        (not (List.exists (Policy.Rule.equal rule) rules)) && unique rules
  in
  if not (unique rules) then
    invalid "family" "rules must not contain duplicates"

let family ~lifetime ~rules =
  check_family rules;
  Allow (Family { lifetime; rules })

let equal_allowance a b =
  match (a, b) with
  | Once, Once | Exact_for_conversation, Exact_for_conversation -> true
  | Family a, Family b ->
      a.lifetime = b.lifetime && List.equal Policy.Rule.equal a.rules b.rules
  | (Once | Exact_for_conversation | Family _), _ -> false

let equal a b =
  match (a, b) with
  | Allow a, Allow b -> equal_allowance a b
  | Deny, Deny -> true
  | (Allow _ | Deny), _ -> false

let pp ppf t =
  let text =
    match t with
    | Allow Once -> "allow-once"
    | Allow Exact_for_conversation -> "allow-exact-for-conversation"
    | Allow (Family { lifetime = Conversation; _ }) ->
        "allow-family-for-conversation"
    | Allow (Family { lifetime = User; _ }) -> "allow-family-for-user"
    | Deny -> "deny"
  in
  Format.pp_print_string ppf text

type answer_kind = Allow_once | Allow_exact | Allow_family | Deny_answer

let answer_jsont =
  Jsont.enum ~kind:"permission answer"
    [
      ("allow-once", Allow_once);
      ("allow-exact-for-conversation", Allow_exact);
      ("allow-family", Allow_family);
      ("deny", Deny_answer);
    ]

let lifetime_jsont =
  Jsont.enum ~kind:"permission family lifetime"
    [ ("conversation", Conversation); ("user", User) ]

let answer_kind = function
  | Allow Once -> Allow_once
  | Allow Exact_for_conversation -> Allow_exact
  | Allow (Family _) -> Allow_family
  | Deny -> Deny_answer

let lifetime_of = function
  | Allow (Family { lifetime; _ }) -> Some lifetime
  | Allow (Once | Exact_for_conversation) | Deny -> None

let rules_of = function
  | Allow (Family { rules; _ }) -> Some rules
  | Allow (Once | Exact_for_conversation) | Deny -> None

let jsont =
  let of_json version answer lifetime rules =
    if version <> 1 then
      decode_error
        ("unknown permission answer version: " ^ string_of_int version);
    decode_invalid_arg (fun () ->
        match (answer, lifetime, rules) with
        | Allow_once, None, None -> once
        | Allow_exact, None, None -> exact_for_conversation
        | Allow_family, Some lifetime, Some rules -> family ~lifetime ~rules
        | Deny_answer, None, None -> deny
        | Allow_family, _, _ ->
            invalid "jsont"
              "family permission answer requires lifetime and rules"
        | (Allow_once | Allow_exact | Deny_answer), _, _ ->
            invalid "jsont"
              "permission answer carries fields for another answer kind")
  in
  Jsont.Object.map ~kind:"permission answer" of_json
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun _ -> 1)
  |> Jsont.Object.mem "answer" answer_jsont ~enc:answer_kind
  |> Jsont.Object.opt_mem "lifetime" lifetime_jsont ~enc:lifetime_of
  |> Jsont.Object.opt_mem "rules" Jsont.(list Policy.Rule.jsont) ~enc:rules_of
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
