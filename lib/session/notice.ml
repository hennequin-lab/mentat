(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

let invalid fn message = invalid_arg' "Mentat_session.Notice" fn message

module Severity = struct
  type t = Info | Warning | Error

  let equal a b =
    match (a, b) with
    | Info, Info | Warning, Warning | Error, Error -> true
    | (Info | Warning | Error), _ -> false

  let to_string = function
    | Info -> "info"
    | Warning -> "warning"
    | Error -> "error"

  let pp ppf severity = Format.pp_print_string ppf (to_string severity)

  let jsont =
    Jsont.enum ~kind:"Mentat_session.Notice.Severity"
      [ ("info", Info); ("warning", Warning); ("error", Error) ]
end

type t = {
  source : string;
  severity : Severity.t;
  title : string;
  body : string option;
}

let check_non_empty fn what s =
  if String.length s = 0 then invalid fn (what ^ " is empty")

let make ~source ~severity ~title ?body () =
  check_non_empty "make" "source" source;
  check_non_empty "make" "title" title;
  Option.iter (check_non_empty "make" "body") body;
  { source; severity; title; body }

let source t = t.source
let severity t = t.severity
let title t = t.title
let body t = t.body

let equal a b =
  String.equal a.source b.source
  && Severity.equal a.severity b.severity
  && String.equal a.title b.title
  && Option.equal String.equal a.body b.body

let pp_body ppf = function
  | None -> Format.pp_print_string ppf "None"
  | Some body -> Format.fprintf ppf "%S" body

let pp ppf t =
  Format.fprintf ppf
    "@[<2>{ source = %S;@ severity = %a;@ title = %S;@ body = %a }@]" t.source
    Severity.pp t.severity t.title pp_body t.body

let jsont =
  Jsont.Object.map ~kind:"Mentat_session.Notice"
    (fun source severity title body ->
      decode_invalid_arg (fun () -> make ~source ~severity ~title ?body ()))
  |> Jsont.Object.mem "source" Jsont.string ~enc:(fun t -> t.source)
  |> Jsont.Object.mem "severity" Severity.jsont ~enc:(fun t -> t.severity)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun t -> t.title)
  |> Jsont.Object.opt_mem "body" Jsont.string ~enc:(fun t -> t.body)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
