(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t =
  | Committed of { position : Position.t; fact : Fact.t }
  | Progress of Progress.t

let equal a b =
  match (a, b) with
  | Committed a, Committed b ->
      Position.equal a.position b.position && Fact.equal a.fact b.fact
  | Progress a, Progress b -> Progress.equal a b
  | (Committed _ | Progress _), _ -> false

let pp ppf = function
  | Committed { position; fact } ->
      Format.fprintf ppf "committed(%a, %a)" Position.pp position Fact.pp fact
  | Progress progress -> Format.fprintf ppf "progress(%a)" Progress.pp progress

let jsont =
  let committed_case =
    Jsont.Object.map ~kind:"committed update" (fun position fact ->
        Committed { position; fact })
    |> Jsont.Object.mem "position" Position.jsont ~enc:(function
      | Committed { position; _ } -> position
      | _ -> assert false)
    |> Jsont.Object.mem "fact" Fact.jsont ~enc:(function
      | Committed { fact; _ } -> fact
      | _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "committed" ~dec:Fun.id
  in
  let progress_case =
    Jsont.Object.map ~kind:"progress update" (fun progress -> Progress progress)
    |> Jsont.Object.mem "progress" Progress.jsont ~enc:(function
      | Progress progress -> progress
      | _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "progress" ~dec:Fun.id
  in
  let cases =
    List.map Jsont.Object.Case.make [ committed_case; progress_case ]
  in
  let enc_case = function
    | Committed _ as u -> Jsont.Object.Case.value committed_case u
    | Progress _ as u -> Jsont.Object.Case.value progress_case u
  in
  Jsont.Object.map ~kind:"feed update" (fun v u ->
      Wire.check v;
      u)
  |> Wire.mem
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.finish
