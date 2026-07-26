(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let module_name_label label =
  let valid_rest = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false
  in
  let rec loop i =
    if i = String.length label then true
    else valid_rest label.[i] && loop (i + 1)
  in
  String.length label > 0
  && match label.[0] with 'A' .. 'Z' -> loop 1 | _ -> false

type t = string

let make name =
  if not (module_name_label name) then
    Import.invalid_arg' "Mentat_ocaml.Module_name" "make"
      "name must be an OCaml module name";
  name

let to_string t = t
let compare = String.compare
let pp ppf t = Format.pp_print_string ppf t
