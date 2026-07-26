(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = Local_user | Unattended_policy

let local_user = Local_user
let unattended_policy = Unattended_policy
let equal a b = a = b

let pp ppf = function
  | Local_user -> Format.pp_print_string ppf "local_user"
  | Unattended_policy -> Format.pp_print_string ppf "unattended_policy"

let jsont =
  Jsont.enum ~kind:"session principal"
    [ ("local_user", Local_user); ("unattended_policy", Unattended_policy) ]
