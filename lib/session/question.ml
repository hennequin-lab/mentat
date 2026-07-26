(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

let invalid fn message = invalid_arg' "Mentat_session.Question" fn message

module Error = struct
  type t = Empty_prompt | Empty_choices | Empty_choice

  let message = function
    | Empty_prompt -> "prompt must not be empty"
    | Empty_choices -> "choices must not be empty"
    | Empty_choice -> "choices must not contain an empty choice"

  let pp ppf t = Format.pp_print_string ppf (message t)
end

type t = { prompt : string; choices : string list option }

let check_choices = function
  | None -> Ok ()
  | Some [] -> Error Error.Empty_choices
  | Some choices ->
      if List.exists String.is_empty choices then Error Error.Empty_choice
      else Ok ()

let make ~prompt ?choices () =
  if String.is_empty prompt then Error Error.Empty_prompt
  else
    match check_choices choices with
    | Error _ as e -> e
    | Ok () -> Ok { prompt; choices }

let prompt t = t.prompt
let choices t = t.choices

module Answer = struct
  type t = Free of string | Choice of int

  let free s =
    if String.is_empty s then invalid "Answer.free" "answer must not be empty";
    Free s

  let choice i =
    if i < 0 then invalid "Answer.choice" "choice index must not be negative";
    Choice i

  let equal a b =
    match (a, b) with
    | Free a, Free b -> String.equal a b
    | Choice a, Choice b -> Int.equal a b
    | (Free _ | Choice _), _ -> false

  let pp ppf = function
    | Free s -> Format.fprintf ppf "free(%S)" s
    | Choice i -> Format.fprintf ppf "choice(%d)" i

  let jsont =
    let free_case =
      Jsont.Object.map ~kind:"free answer" (fun s ->
          decode_invalid_arg (fun () -> free s))
      |> Jsont.Object.mem "text" Jsont.string ~enc:(function
        | Free s -> s
        | Choice _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "free" ~dec:Fun.id
    in
    let choice_case =
      Jsont.Object.map ~kind:"choice answer" (fun i ->
          decode_invalid_arg (fun () -> choice i))
      |> Jsont.Object.mem "index" Jsont.int ~enc:(function
        | Choice i -> i
        | Free _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "choice" ~dec:Fun.id
    in
    let cases = List.map Jsont.Object.Case.make [ free_case; choice_case ] in
    let enc_case = function
      | Free _ as answer -> Jsont.Object.Case.value free_case answer
      | Choice _ as answer -> Jsont.Object.Case.value choice_case answer
    in
    Jsont.Object.map ~kind:"question answer" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

let equal a b =
  String.equal a.prompt b.prompt
  && Option.equal (List.equal String.equal) a.choices b.choices

let pp ppf t =
  match t.choices with
  | None -> Format.fprintf ppf "question(%S)" t.prompt
  | Some choices ->
      Format.fprintf ppf "question(%S, [%d choice(s)])" t.prompt
        (List.length choices)

let jsont =
  Jsont.Object.map ~kind:"reviewer question" (fun prompt choices ->
      match make ~prompt ?choices () with
      | Ok question -> question
      | Error e -> decode_error (Error.message e))
  |> Jsont.Object.mem "prompt" Jsont.string ~enc:prompt
  |> Jsont.Object.opt_mem "choices" (Jsont.list Jsont.string) ~enc:choices
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
