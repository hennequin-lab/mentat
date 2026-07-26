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

module Access = struct
  type t = Read | Write | Deny

  (* Ordered by how much each takes away, so the strongest wins a tie between
     two clauses naming the same path. *)
  let rank = function Read -> 0 | Write -> 1 | Deny -> 2
  let compare a b = Int.compare (rank a) (rank b)
  let equal a b = rank a = rank b
  let to_string = function Read -> "read" | Write -> "write" | Deny -> "deny"
  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

type reads_default = All | Denied

type t = {
  entries : (Lpath.Abs.t * Access.t) list;
  reads_default : reads_default;
  network : Network.t;
}

let depth path =
  Lpath.Abs.to_string path
  |> String.fold_left (fun n c -> if Char.equal c '/' then n + 1 else n) 0

(* Emission order is resolution order read backwards. Both backends let the
   last clause touching a path win — SBPL by rule order, bubblewrap by mount
   order — so sorting shallowest-first makes the most specific clause the one
   that decides, and neither generator has to know that rule. Two clauses on one
   path collapse to the stronger, which is what makes a carveout an ordinary
   entry rather than an exception threaded through the rule that contains it. *)
let normalize entries =
  let table = Hashtbl.create 16 in
  List.iter
    (fun (path, access) ->
      let key = Lpath.Abs.to_string path in
      match Hashtbl.find_opt table key with
      | Some (_, existing) when Access.compare existing access >= 0 -> ()
      | Some _ | None -> Hashtbl.replace table key (path, access))
    entries;
  Hashtbl.fold (fun _ entry acc -> entry :: acc) table []
  |> List.sort (fun (a, _) (b, _) ->
      match Int.compare (depth a) (depth b) with
      | 0 -> Lpath.Abs.compare a b
      | order -> order)

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
