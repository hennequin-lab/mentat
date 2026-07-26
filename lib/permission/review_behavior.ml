(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = Enforce | Bypass

let equal a b =
  match (a, b) with
  | Enforce, Enforce | Bypass, Bypass -> true
  | (Enforce | Bypass), _ -> false

let to_string = function Enforce -> "enforce" | Bypass -> "bypass"
let pp ppf t = Format.pp_print_string ppf (to_string t)

let jsont =
  Jsont.enum ~kind:"permission review behavior"
    [ ("enforce", Enforce); ("bypass", Bypass) ]
