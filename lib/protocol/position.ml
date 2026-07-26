(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

type t = { session : Mentat_session.Id.t; seq : int }

let make ~session ~seq =
  if seq < 0 then
    invalid_arg' "Mentat_protocol.Position" "make"
      "sequence must be non-negative";
  { session; seq }

let session t = t.session
let seq t = t.seq

let equal a b =
  Mentat_session.Id.equal a.session b.session && Int.equal a.seq b.seq

let pp ppf t = Format.fprintf ppf "%a#%d" Mentat_session.Id.pp t.session t.seq

let jsont =
  Jsont.Object.map ~kind:"feed position" (fun session seq ->
      decode_invalid_arg (fun () -> make ~session ~seq))
  |> Jsont.Object.mem "session" Mentat_session.Id.jsont ~enc:session
  |> Jsont.Object.mem "seq" Jsont.int ~enc:seq
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

module Invalid = struct
  type nonrec t =
    | Foreign_session of {
        expected : Mentat_session.Id.t;
        actual : Mentat_session.Id.t;
      }
    | Not_in_feed of t

  let equal a b =
    match (a, b) with
    | ( Foreign_session { expected = ea; actual = aa },
        Foreign_session { expected = eb; actual = ab } ) ->
        Mentat_session.Id.equal ea eb && Mentat_session.Id.equal aa ab
    | Not_in_feed a, Not_in_feed b -> equal a b
    | (Foreign_session _ | Not_in_feed _), _ -> false

  let message = function
    | Foreign_session { expected; actual } ->
        Format.asprintf "position is for session %a, not session %a"
          Mentat_session.Id.pp actual Mentat_session.Id.pp expected
    | Not_in_feed position ->
        Format.asprintf "position %a names no committed fact of its feed" pp
          position

  let pp ppf t = Format.pp_print_string ppf (message t)

  let jsont =
    let foreign_case =
      Jsont.Object.map ~kind:"foreign-session position" (fun expected actual ->
          Foreign_session { expected; actual })
      |> Jsont.Object.mem "expected" Mentat_session.Id.jsont ~enc:(function
        | Foreign_session { expected; _ } -> expected
        | _ -> assert false)
      |> Jsont.Object.mem "actual" Mentat_session.Id.jsont ~enc:(function
        | Foreign_session { actual; _ } -> actual
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "foreign_session" ~dec:Fun.id
    in
    let not_in_feed_case =
      Jsont.Object.map ~kind:"not-in-feed position" (fun position ->
          Not_in_feed position)
      |> Jsont.Object.mem "position" jsont ~enc:(function
        | Not_in_feed position -> position
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "not_in_feed" ~dec:Fun.id
    in
    let cases =
      List.map Jsont.Object.Case.make [ foreign_case; not_in_feed_case ]
    in
    let enc_case = function
      | Foreign_session _ as e -> Jsont.Object.Case.value foreign_case e
      | Not_in_feed _ as e -> Jsont.Object.Case.value not_in_feed_case e
    in
    Jsont.Object.map ~kind:"invalid position" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.finish
end
