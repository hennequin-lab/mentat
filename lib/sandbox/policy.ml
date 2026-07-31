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

(* The confinement semantics — normalization, resolution order, the widening
   refusals — live in theories/sandbox/Confinement.v; the kernel library is
   the OCaml extracted from it. Access and the reads default re-export the
   kernel's types, so only paths convert at the boundary: a path crosses as
   its component list — total in both directions, since the stored spelling
   is canonical — and the kernel's equal-depth tie-break instantiates to
   [Lpath.Abs.compare]'s byte order. *)

module Access = struct
  type t = Mentat_sandbox_kernel.Confinement.access = Read | Write | Deny

  let rank = function Read -> 0 | Write -> 1 | Deny -> 2
  let compare a b = Int.compare (rank a) (rank b)
  let equal a b = rank a = rank b
  let to_string = function Read -> "read" | Write -> "write" | Deny -> "deny"
  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

type reads_default = Mentat_sandbox_kernel.Confinement.reads_default =
  | All
  | Denied

type t = {
  entries : (Lpath.Abs.t * Access.t) list;
  reads_default : reads_default;
  network : Network.t;
}

let render segs = "/" ^ String.concat "/" segs
let path_leb a b = String.compare (render a) (render b) <= 0
let to_kernel (path, access) = (Lpath.Abs.components path, access)
let of_kernel (segs, access) = (Lpath.Abs.of_string_exn (render segs), access)

let normalize entries =
  List.map of_kernel
    (Mentat_sandbox_kernel.Confinement.normalize String.equal path_leb
       (List.map to_kernel entries))

let make ~entries ~reads_default ~network =
  { entries = normalize entries; reads_default; network }

let entries t = t.entries
let reads_default t = t.reads_default
let network t = t.network

let paths_with access t =
  List.filter_map
    (fun (path, entry) -> if Access.equal entry access then Some path else None)
    t.entries

let writable_roots t = paths_with Access.Write t
let denied_paths t = paths_with Access.Deny t

let readable_roots t =
  List.filter_map
    (fun (path, entry) ->
      match entry with
      | Access.Read | Access.Write -> Some path
      | Access.Deny -> None)
    t.entries

let grant t entries =
  match
    Mentat_sandbox_kernel.Confinement.grant String.equal path_leb
      (List.map to_kernel t.entries)
      (List.map to_kernel entries)
  with
  | Mentat_sandbox_kernel.Confinement.Granted granted ->
      Ok { t with entries = List.map of_kernel granted }
  | Mentat_sandbox_kernel.Confinement.Refused (path, defeated_by) ->
      Error
        ( Lpath.Abs.of_string_exn (render path),
          Lpath.Abs.of_string_exn (render defeated_by) )

let floor t =
  make
    ~entries:
      (List.map of_kernel
         (Mentat_sandbox_kernel.Confinement.floor
            (List.map to_kernel t.entries)))
    ~reads_default:All ~network:Network.Enabled

let reads_default_equal a b =
  match (a, b) with
  | All, All | Denied, Denied -> true
  | (All | Denied), _ -> false

let equal a b =
  reads_default_equal a.reads_default b.reads_default
  && Network.equal a.network b.network
  && List.equal
       (fun (pa, aa) (pb, ab) -> Lpath.Abs.equal pa pb && Access.equal aa ab)
       a.entries b.entries

let pp_reads_default ppf = function
  | All -> Format.pp_print_string ppf "all"
  | Denied -> Format.pp_print_string ppf "denied"

let pp ppf t =
  Format.fprintf ppf "@[<v>confined@,reads-default: %a@,network: %a@,%a@]"
    pp_reads_default t.reads_default Network.pp t.network
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.pp_print_cut ppf ())
       (fun ppf (path, access) ->
         Format.fprintf ppf "%a %a" Access.pp access Lpath.Abs.pp path))
    t.entries
