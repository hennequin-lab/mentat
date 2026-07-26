(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t =
  | Conflicting_root of { existing : Root.t; duplicate : Root.t }
  | Root_not_in_workspace of Root.Key.t

let message = function
  | Conflicting_root { existing; duplicate } ->
      Format.asprintf "conflicting workspace roots: %S -> %a and %S -> %a"
        (Root.Key.to_string (Root.key existing))
        Root.pp existing
        (Root.Key.to_string (Root.key duplicate))
        Root.pp duplicate
  | Root_not_in_workspace key ->
      "current directory root " ^ Root.Key.to_string key
      ^ " is not in workspace"

let equal a b =
  match (a, b) with
  | ( Conflicting_root { existing = ae; duplicate = ad },
      Conflicting_root { existing = be; duplicate = bd } ) ->
      Root.equal ae be && Root.equal ad bd
  | Root_not_in_workspace a, Root_not_in_workspace b -> Root.Key.equal a b
  | (Conflicting_root _ | Root_not_in_workspace _), _ -> false

let pp ppf error = Format.pp_print_string ppf (message error)
