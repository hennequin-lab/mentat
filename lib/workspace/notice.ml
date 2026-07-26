(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

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
end

type t = {
  source : string;
  severity : Severity.t;
  title : string;
  body : string option;
  key : string;
}

let check_non_empty what s =
  if String.length s = 0 then
    invalid_arg ("Mentat_workspace.Notice.make: " ^ what ^ " is empty")

let make ~source ~severity ~title ?body ~key () =
  check_non_empty "source" source;
  check_non_empty "title" title;
  check_non_empty "key" key;
  Option.iter (check_non_empty "body") body;
  { source; severity; title; body; key }

let source t = t.source
let severity t = t.severity
let title t = t.title
let body t = t.body
let key t = t.key

let equal a b =
  String.equal a.source b.source
  && Severity.equal a.severity b.severity
  && String.equal a.title b.title
  && Option.equal String.equal a.body b.body
  && String.equal a.key b.key

let pp_body ppf = function
  | None -> Format.pp_print_string ppf "None"
  | Some body -> Format.fprintf ppf "%S" body

let pp ppf t =
  Format.fprintf ppf
    "@[<2>{ source = %S;@ severity = %a;@ title = %S;@ body = %a;@ key = %S }@]"
    t.source Severity.pp t.severity t.title pp_body t.body t.key

let severity_jsont =
  Jsont.enum ~kind:"Mentat_workspace.Notice.Severity"
    [
      ("info", Severity.Info);
      ("warning", Severity.Warning);
      ("error", Severity.Error);
    ]

let jsont =
  Jsont.Object.map ~kind:"Mentat_workspace.Notice"
    (fun source severity title body key ->
      try make ~source ~severity ~title ?body ~key ()
      with Invalid_argument error -> Jsont.Error.msg Jsont.Meta.none error)
  |> Jsont.Object.mem "source" Jsont.string ~enc:(fun t -> t.source)
  |> Jsont.Object.mem "severity" severity_jsont ~enc:(fun t -> t.severity)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun t -> t.title)
  |> Jsont.Object.opt_mem "body" Jsont.string ~enc:(fun t -> t.body)
  |> Jsont.Object.mem "key" Jsont.string ~enc:(fun t -> t.key)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
