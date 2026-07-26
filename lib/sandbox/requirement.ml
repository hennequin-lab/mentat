(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = Off | Enforced_or_external | Enforced

let all = [ Off; Enforced_or_external; Enforced ]

let to_string = function
  | Off -> "off"
  | Enforced_or_external -> "enforced-or-external"
  | Enforced -> "enforced"

let of_string = function
  | "off" -> Some Off
  | "enforced-or-external" -> Some Enforced_or_external
  | "enforced" -> Some Enforced
  | _ -> None

let equal a b =
  match (a, b) with
  | Off, Off | Enforced_or_external, Enforced_or_external | Enforced, Enforced
    ->
      true
  | (Off | Enforced_or_external | Enforced), _ -> false

let pp ppf t = Format.pp_print_string ppf (to_string t)

module Rejection = struct
  type t = Unenforceable of Error.t | External_not_enforced

  let message = function
    | Unenforceable error ->
        "sandbox unavailable: the requested confinement could not be enforced: "
        ^ Error.message error
    | External_not_enforced ->
        "sandbox unavailable: a declared external sandbox does not satisfy \
         sandbox.require=enforced"

  let pp ppf t = Format.pp_print_string ppf (message t)
end
