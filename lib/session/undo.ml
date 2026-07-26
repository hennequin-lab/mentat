(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

let invalid fn message = invalid_arg' "Mentat_session.Undo" fn message

module Update = struct
  type t = Armed of { anchor : Turn.Id.t; revert : string option } | Released

  let armed ~anchor ?revert () =
    (match revert with
    | Some revert when String.is_empty revert ->
        invalid "armed" "revert must not be empty"
    | Some _ | None -> ());
    Armed { anchor; revert }

  let released = Released
  let anchor = function Armed { anchor; _ } -> Some anchor | Released -> None
  let revert = function Armed { revert; _ } -> revert | Released -> None

  let equal a b =
    match (a, b) with
    | Armed a, Armed b ->
        Turn.Id.equal a.anchor b.anchor
        && Option.equal String.equal a.revert b.revert
    | Released, Released -> true
    | (Armed _ | Released), _ -> false

  let pp ppf = function
    | Armed { anchor; revert } ->
        Format.fprintf ppf "armed(anchor=%a, revert=%a)" Turn.Id.pp anchor
          (Format.pp_print_option Format.pp_print_string)
          revert
    | Released -> Format.pp_print_string ppf "released"

  let jsont =
    let armed_case =
      Jsont.Object.map ~kind:"undo armed" (fun anchor revert ->
          decode_invalid_arg (fun () -> armed ~anchor ?revert ()))
      |> Jsont.Object.mem "anchor" Turn.Id.jsont ~enc:(function
        | Armed { anchor; _ } -> anchor
        | Released -> assert false)
      |> Jsont.Object.opt_mem "revert" Jsont.string ~enc:(function
        | Armed { revert; _ } -> revert
        | Released -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "armed" ~dec:Fun.id
    in
    let released_case =
      Jsont.Object.map ~kind:"undo released" Released
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "released" ~dec:Fun.id
    in
    let cases = List.map Jsont.Object.Case.make [ armed_case; released_case ] in
    let enc_case = function
      | Armed _ as update -> Jsont.Object.Case.value armed_case update
      | Released as update -> Jsont.Object.Case.value released_case update
    in
    Jsont.Object.map ~kind:"undo update" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end
