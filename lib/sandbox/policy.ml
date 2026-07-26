(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Network = struct
  type t = Restricted | Enabled

  let all = [ Restricted; Enabled ]

  let of_string = function
    | "restricted" -> Some Restricted
    | "enabled" -> Some Enabled
    | _ -> None

  let to_string = function Restricted -> "restricted" | Enabled -> "enabled"

  let equal a b =
    match (a, b) with
    | Restricted, Restricted | Enabled, Enabled -> true
    | (Restricted | Enabled), _ -> false

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

type reads = All | Only of Lpath.Abs.t list

type t = {
  scratch : Lpath.Abs.t;
  reads : reads;
  writable_roots : Lpath.Abs.t list;
  protected_paths : Lpath.Abs.t list;
  denied_paths : Lpath.Abs.t list;
  network : Network.t;
}

let canonical_paths paths = List.sort_uniq Lpath.Abs.compare paths

let under roots path =
  List.exists (fun root -> Lpath.Abs.is_within ~root path) roots

let normalize_roots roots =
  let roots = canonical_paths roots in
  List.filter
    (fun root ->
      not
        (List.exists
           (fun candidate -> Lpath.Abs.is_strictly_within ~root:candidate root)
           roots))
    roots

let make ~scratch ~reads ~writable_roots ~protected_paths ~denied_paths ~network
    =
  let writable_roots = normalize_roots writable_roots in
  let reads =
    match reads with
    | All -> All
    | Only roots -> Only (normalize_roots ((scratch :: writable_roots) @ roots))
  in
  let protected_paths =
    protected_paths |> List.filter (under writable_roots) |> canonical_paths
  in
  let denied_paths = normalize_roots denied_paths in
  { scratch; reads; writable_roots; protected_paths; denied_paths; network }

let scratch t = t.scratch
let reads t = t.reads
let writable_roots t = t.writable_roots
let protected_paths t = t.protected_paths
let denied_paths t = t.denied_paths
let network t = t.network

let reads_equal a b =
  match (a, b) with
  | All, All -> true
  | Only a, Only b -> List.equal Lpath.Abs.equal a b
  | (All | Only _), _ -> false

let equal a b =
  Lpath.Abs.equal a.scratch b.scratch
  && reads_equal a.reads b.reads
  && List.equal Lpath.Abs.equal a.writable_roots b.writable_roots
  && List.equal Lpath.Abs.equal a.protected_paths b.protected_paths
  && List.equal Lpath.Abs.equal a.denied_paths b.denied_paths
  && Network.equal a.network b.network

let pp_paths ppf paths =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
    Lpath.Abs.pp ppf paths

let pp_reads ppf = function
  | All -> Format.pp_print_string ppf "all"
  | Only roots -> Format.fprintf ppf "only [%a]" pp_paths roots

let pp ppf t =
  Format.fprintf ppf
    "@[<v>confined@,\
     scratch: %a@,\
     reads: %a@,\
     writable: %a@,\
     protected: %a@,\
     denied: %a@,\
     network: %a@]"
    Lpath.Abs.pp t.scratch pp_reads t.reads pp_paths t.writable_roots pp_paths
    t.protected_paths pp_paths t.denied_paths Network.pp t.network
