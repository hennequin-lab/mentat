(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = Missing | Text of Mentat_digest.Content_ref.t

let equal a b =
  match (a, b) with
  | Missing, Missing -> true
  | Text a, Text b -> Mentat_digest.Content_ref.equal a b
  | (Missing | Text _), _ -> false

let pp ppf = function
  | Missing -> Format.pp_print_string ppf "missing"
  | Text identity ->
      Format.fprintf ppf "text(%a)" Mentat_digest.Content_ref.pp identity

let jsont =
  let missing_case =
    Jsont.Object.map ~kind:"missing image" Missing
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "missing" ~dec:Fun.id
  in
  let text_case =
    Jsont.Object.map ~kind:"text image" (fun identity -> Text identity)
    |> Jsont.Object.mem "identity" Mentat_digest.Content_ref.jsont
         ~enc:(function
         | Text identity -> identity
         | Missing -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "text" ~dec:Fun.id
  in
  let cases = List.map Jsont.Object.Case.make [ missing_case; text_case ] in
  let enc_case = function
    | Missing -> Jsont.Object.Case.value missing_case Missing
    | Text _ as image -> Jsont.Object.Case.value text_case image
  in
  Jsont.Object.map ~kind:"image" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
