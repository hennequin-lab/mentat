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

  (* The precedence a tie between two clauses on one path is settled by: a
     write outranks a read, and a denial outranks both. Not an ordering by
     permissiveness — a write is the {e most} permissive of the three and still
     outranks a read — but by which clause the posture meant, which is the
     reference implementation's rule. *)
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

(* Specificity is the component count, not the separator count: the two agree
   everywhere except at the root, which is spelled ["/"] and so carries a
   separator without naming a component. Counting separators would tie the root
   with every single-component path — the one tie where the answer matters,
   since the root contains them all. *)
let depth path =
  if Lpath.Abs.is_root path then 0
  else
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

(* A grant only ever widens, and normalization already keeps the stronger of two
   clauses on one path — so a grant beneath an existing clause resolves by the
   ordinary law and needs no special case. A denied path is the exception: it
   would win that tie silently, and a grant answered by doing nothing is worse
   than one that is refused, so the containment is checked here and reported.
   Only the direction that would be defeated is refused. A grant *containing* a
   denied path is admitted and the deeper denial keeps winning inside it, which
   is how the deny set survives a grant made over a whole tree. *)
let grant t entries =
  (* A clause that grants less than one enclosing it was lowered on purpose —
     [.git] inside the workspace, the dune store inside its cache. Those are
     what a grant must not raise, and the deny set is only the loudest of them:
     admitting a write over a carveout hands back exactly the path some earlier
     ruling took away, and [normalize] would collapse the two clauses to the
     stronger without a word.

     Only a carveout, though. A read root is not a carveout, and a grant beneath
     one is the ordinary case this exists to serve — [/usr] being unwritable is
     the default reach, not a decision about [/usr/local/x]. So the test is
     relative: refuse where the covering clause is weaker than the grant *and*
     something shallower is stronger than it. *)
  let carveouts =
    List.filter
      (fun (path, access) ->
        List.exists
          (fun (enclosing, enclosing_access) ->
            (not (Lpath.Abs.equal enclosing path))
            && Lpath.Abs.is_within ~root:enclosing path
            && Access.compare enclosing_access access > 0)
          t.entries)
      t.entries
  in
  let defeated (path, access) =
    List.find_opt
      (fun (root, root_access) ->
        Lpath.Abs.is_within ~root path && Access.compare root_access access < 0)
      carveouts
    |> Option.map (fun (root, _) -> (path, root))
  in
  let denied (path, _) =
    List.find_opt (fun root -> Lpath.Abs.is_within ~root path) (denied_paths t)
    |> Option.map (fun root -> (path, root))
  in
  match
    List.find_map
      (fun entry ->
        match denied entry with
        | Some _ as defeat -> defeat
        | None -> defeated entry)
      entries
  with
  | Some defeat -> Error defeat
  | None -> Ok { t with entries = normalize (t.entries @ entries) }

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
