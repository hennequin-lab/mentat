(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = Block | Deny

let all = [ Block; Deny ]

let equal a b =
  match (a, b) with
  | Block, Block | Deny, Deny -> true
  | (Block | Deny), _ -> false

let to_string = function Block -> "block" | Deny -> "deny"

let of_string = function
  | "block" -> Some Block
  | "deny" -> Some Deny
  | _ -> None

let pp ppf t = Format.pp_print_string ppf (to_string t)
