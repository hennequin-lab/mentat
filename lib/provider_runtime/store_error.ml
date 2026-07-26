(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = Io of string | Decode of string | Locked of string

let message = function
  | Io message -> message
  | Decode message -> message
  | Locked message -> message

let pp ppf t = Format.pp_print_string ppf (message t)
