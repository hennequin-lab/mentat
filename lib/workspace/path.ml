(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = { root_key : Root.Key.t; rel : Lpath.Rel.t }

let make ~root_key rel = { root_key; rel }
let root_key t = t.root_key
let root_of t = make ~root_key:(root_key t) Lpath.Rel.root
let rel t = t.rel
let is_root t = Lpath.Rel.is_root t.rel
let basename t = Lpath.Rel.basename t.rel
let parent t = Option.map (fun rel -> { t with rel }) (Lpath.Rel.parent t.rel)

let add_component t component =
  Result.map
    (fun rel -> { t with rel })
    (Lpath.Rel.add_component t.rel component)

let append t suffix = { t with rel = Lpath.Rel.append t.rel suffix }

let relativize ~root t =
  if Root.Key.equal root.root_key t.root_key then
    Lpath.Rel.relativize ~root:root.rel t.rel
  else None

let is_within ~root t = Option.is_some (relativize ~root t)
let display t = Lpath.Rel.to_string t.rel

let equal a b =
  Root.Key.equal a.root_key b.root_key && Lpath.Rel.equal a.rel b.rel

let compare a b =
  match Root.Key.compare a.root_key b.root_key with
  | 0 -> Lpath.Rel.compare a.rel b.rel
  | order -> order

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)

let pp ppf t =
  Format.fprintf ppf "@[<1>{ root_key = %a;@ rel = %a }@]" Root.Key.pp
    t.root_key Lpath.Rel.pp t.rel

let rel_jsont =
  Jsont.map ~kind:"relative path"
    ~dec:(fun s ->
      match Lpath.Rel.of_string s with
      | Ok rel -> rel
      | Error error ->
          Jsont.Error.msg Jsont.Meta.none (Lpath.Error.message error))
    ~enc:Lpath.Rel.to_string Jsont.string

let jsont =
  Jsont.Object.map ~kind:"Mentat_workspace.Path" (fun root_key rel ->
      { root_key; rel })
  |> Jsont.Object.mem "root" Root.Key.jsont ~enc:(fun t -> t.root_key)
  |> Jsont.Object.mem "rel" rel_jsont ~enc:(fun t -> t.rel)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
