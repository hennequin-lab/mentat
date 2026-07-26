(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = { path : Mentat_workspace.Path.t; range : Range.t }

let make ~path ~range = { path; range }
let path t = t.path
let range t = t.range
let start t = Range.start t.range
let end_ t = Range.end_ t.range

let compare a b =
  match Mentat_workspace.Path.compare a.path b.path with
  | 0 -> Range.compare a.range b.range
  | order -> order

let pp ppf t =
  Format.fprintf ppf "%s:%a"
    (Mentat_workspace.Path.display t.path)
    Range.pp t.range
