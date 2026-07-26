(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let source_label label =
  let valid_char = function
    | 'a' .. 'z' | '0' .. '9' | '-' -> true
    | _ -> false
  in
  (not (String.is_empty label))
  && String.for_all valid_char label
  && (not (String.starts_with ~prefix:"-" label))
  && (not (String.ends_with ~suffix:"-" label))
  && not (String.contains label '\000')

module Severity = struct
  type t = Error | Warning | Information | Hint

  let rank (severity : t) =
    match severity with
    | Error -> 0
    | Warning -> 1
    | Information -> 2
    | Hint -> 3

  let compare a b = Int.compare (rank a) (rank b)
  let equal a b = compare a b = 0

  let pp ppf (severity : t) =
    match severity with
    | Error -> Format.pp_print_string ppf "error"
    | Warning -> Format.pp_print_string ppf "warning"
    | Information -> Format.pp_print_string ppf "information"
    | Hint -> Format.pp_print_string ppf "hint"
end

module Source = struct
  type t = Dune | Merlin | Compiler | Ocamlformat | Odoc | Other of string

  let to_string = function
    | Dune -> "dune"
    | Merlin -> "merlin"
    | Compiler -> "compiler"
    | Ocamlformat -> "ocamlformat"
    | Odoc -> "odoc"
    | Other label -> label

  let builtins = [ Dune; Merlin; Compiler; Ocamlformat; Odoc ]
  let dune = Dune
  let merlin = Merlin
  let compiler = Compiler
  let ocamlformat = Ocamlformat
  let odoc = Odoc

  let other label =
    if not (source_label label) then
      Import.invalid_arg' "Mentat_ocaml.Diagnostic.Source" "other"
        "label must be lowercase ASCII words separated by hyphens";
    if
      List.exists (fun source -> String.equal label (to_string source)) builtins
    then
      Import.invalid_arg' "Mentat_ocaml.Diagnostic.Source" "other"
        "label must not collide with a built-in source";
    Other label

  let rank = function
    | Dune -> 0
    | Merlin -> 1
    | Compiler -> 2
    | Ocamlformat -> 3
    | Odoc -> 4
    | Other _ -> 5

  let compare a b =
    match Int.compare (rank a) (rank b) with
    | 0 -> String.compare (to_string a) (to_string b)
    | order -> order

  let equal a b = compare a b = 0
  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

module Tag = struct
  type t = Unnecessary | Deprecated

  let rank = function Unnecessary -> 0 | Deprecated -> 1
  let compare a b = Int.compare (rank a) (rank b)
  let equal a b = compare a b = 0

  let pp ppf = function
    | Unnecessary -> Format.pp_print_string ppf "unnecessary"
    | Deprecated -> Format.pp_print_string ppf "deprecated"
end

module Related = struct
  type t = { message : string; location : Location.t option }

  let make ?location message =
    Import.require_non_empty "Mentat_ocaml.Diagnostic.Related" "make" "message"
      message;
    { message; location }

  let message t = t.message
  let location t = t.location

  let compare a b =
    match Option.compare Location.compare a.location b.location with
    | 0 -> String.compare a.message b.message
    | order -> order

  let pp ppf t =
    match t.location with
    | None -> Format.pp_print_string ppf t.message
    | Some location ->
        Format.fprintf ppf "%a: %s" Location.pp location t.message
end

type t = {
  message : string;
  source : Source.t;
  severity : Severity.t;
  location : Location.t option;
  code : string option;
  tags : Tag.t list;
  related : Related.t list;
}

let duplicate_tag tags =
  let rec loop seen = function
    | [] -> false
    | tag :: rest -> List.exists (Tag.equal tag) seen || loop (tag :: seen) rest
  in
  loop [] tags

let make ?location ?code ?(tags = []) ?(related = []) ~source ~severity message
    =
  Import.require_non_empty "Mentat_ocaml.Diagnostic" "make" "message" message;
  Option.iter
    (Import.require_non_empty "Mentat_ocaml.Diagnostic" "make" "code")
    code;
  if duplicate_tag tags then
    Import.invalid_arg' "Mentat_ocaml.Diagnostic" "make"
      "tags must not contain duplicates";
  { message; source; severity; location; code; tags; related }

let message t = t.message
let source t = t.source
let severity t = t.severity
let location t = t.location
let code t = t.code
let tags t = t.tags
let related t = t.related

let compare a b =
  match Option.compare Location.compare a.location b.location with
  | 0 -> (
      match Source.compare a.source b.source with
      | 0 -> (
          match Severity.compare a.severity b.severity with
          | 0 -> (
              match Option.compare String.compare a.code b.code with
              | 0 -> (
                  match String.compare a.message b.message with
                  | 0 -> (
                      match List.compare Tag.compare a.tags b.tags with
                      | 0 -> List.compare Related.compare a.related b.related
                      | order -> order)
                  | order -> order)
              | order -> order)
          | order -> order)
      | order -> order)
  | order -> order

let equal a b = compare a b = 0

let pp ppf t =
  match t.location with
  | None ->
      Format.fprintf ppf "%a[%a]: %s" Source.pp t.source Severity.pp t.severity
        t.message
  | Some location ->
      Format.fprintf ppf "%a: %a[%a]: %s" Location.pp location Source.pp
        t.source Severity.pp t.severity t.message
