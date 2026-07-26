(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = { line : int; column : int }

let make ~line ~column =
  if line < 1 then
    Import.invalid_arg' "Mentat_ocaml.Position" "make" "line must be >= 1";
  if column < 0 then
    Import.invalid_arg' "Mentat_ocaml.Position" "make" "column must be >= 0";
  { line; column }

let line t = t.line
let column t = t.column

let compare a b =
  match Int.compare a.line b.line with
  | 0 -> Int.compare a.column b.column
  | order -> order

let equal a b = Int.equal a.line b.line && Int.equal a.column b.column
let pp ppf t = Format.fprintf ppf "%d:%d" t.line t.column
