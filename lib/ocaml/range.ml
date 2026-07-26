(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = { start : Position.t; end_ : Position.t }

let make ~start ~end_ =
  if Position.compare end_ start < 0 then
    Import.invalid_arg' "Mentat_ocaml.Range" "make"
      "end_ must not be before start";
  { start; end_ }

let point position = make ~start:position ~end_:position
let start t = t.start
let end_ t = t.end_

let contains ~outer t =
  Position.compare outer.start t.start <= 0
  && Position.compare t.end_ outer.end_ <= 0

let compare a b =
  match Position.compare a.start b.start with
  | 0 -> Position.compare a.end_ b.end_
  | order -> order

let equal a b = Position.equal a.start b.start && Position.equal a.end_ b.end_
let pp ppf t = Format.fprintf ppf "%a-%a" Position.pp t.start Position.pp t.end_
