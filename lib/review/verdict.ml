(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = Pending | Approved of { feature : Mentat_digest.Content_ref.t }
type freshness = [ `Pending | `Approved | `Stale ]

let freshness t ~feature =
  match t with
  | Pending -> `Pending
  | Approved { feature = approved } ->
      if Mentat_digest.Content_ref.equal approved feature then `Approved
      else `Stale

let equal a b =
  match (a, b) with
  | Pending, Pending -> true
  | Approved a, Approved b ->
      Mentat_digest.Content_ref.equal a.feature b.feature
  | (Pending | Approved _), _ -> false

let pp ppf = function
  | Pending -> Format.pp_print_string ppf "pending"
  | Approved { feature } ->
      Format.fprintf ppf "approved %a" Mentat_digest.Content_ref.pp feature
