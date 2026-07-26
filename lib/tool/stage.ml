(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = Direct | Prepare | Run

let to_string = function
  | Direct -> "direct"
  | Prepare -> "prepare"
  | Run -> "run"

let equal a b =
  match (a, b) with
  | Direct, Direct | Prepare, Prepare | Run, Run -> true
  | (Direct | Prepare | Run), _ -> false

let pp ppf t = Format.pp_print_string ppf (to_string t)

let jsont =
  Jsont.enum ~kind:"tool stage"
    [ ("direct", Direct); ("prepare", Prepare); ("run", Run) ]
