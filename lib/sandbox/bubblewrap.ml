(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* One mount per clause, shallowest first. Bubblewrap applies mounts in order
   and the last one on a path wins, so emission order is the resolution law:
   a [Read] clause beneath a [Write] one simply lands after it. Nothing here
   knows what a carveout is.

   Every bind is strict — on a missing source [bwrap] aborts the spawn rather
   than silently dropping the clause and running unprotected. The effect twin
   materializes every path Mentat owns, so a bound clause always exists at seal
   time; a path deleted between sealing and launch is a loud stale-policy
   refusal from the sealed obligations. *)
let clause (path, access) =
  let p = Lpath.Abs.to_string path in
  match access with
  | Policy.Access.Read -> [ "--ro-bind"; p; p ]
  | Policy.Access.Write -> [ "--bind"; p; p ]
  (* The empty mount only; its seal is deferred to {!seal_denials}. Sealing here
     would freeze the tree before any deeper clause could be mounted inside it,
     and bubblewrap creates each mount point as it goes — so a grant beneath a
     denial, which the resolution law says must win, instead aborts the spawn
     with [Can't mkdir: Read-only file system] before the command runs. *)
  | Policy.Access.Deny -> [ "--perms"; "000"; "--tmpfs"; p ]

let filesystem_args policy =
  let root =
    match Policy.reads_default policy with
    | Policy.All -> [ "--ro-bind"; "/"; "/" ]
    | Policy.Denied -> [ "--tmpfs"; "/" ]
  in
  root @ [ "--dev"; "/dev" ] @ List.concat_map clause (Policy.entries policy)

(* An [Only] read scope builds its root as a [--tmpfs], which is writable. A
   write to a path no bind covers therefore lands in that ephemeral mount and
   the command exits 0 — the data is gone at teardown and nothing told the
   caller. Sealing the root read-only turns those writes into [EROFS] without
   touching the binds beneath it, which keep the access their own mount carries.
   [All] needs no seal: its root is already a read-only bind.

   The seal must be the last argument. Bubblewrap applies operations in order
   and creates each mount point inside the new root as it goes, so [--proc] and
   [--dev] fail with [EROFS] if the root is sealed before they run. *)
let seal_root policy =
  match Policy.reads_default policy with
  | Policy.All -> []
  | Policy.Denied -> [ "--remount-ro"; "/" ]

(* Every denial's tmpfs is sealed here, after the last clause has been mounted,
   for the same reason the root is: a mount point cannot be created inside a
   tree that is already read-only. Deferring only the seal — not the [--perms
   000] or the [--tmpfs] — keeps a denial with nothing beneath it exactly as it
   was, and lets one with a grant beneath it resolve the way the law says and
   the way seatbelt already did. The order among the seals is free: each names
   its own mount, and a seal never has to create anything. *)
let seal_denials policy =
  List.concat_map
    (fun path -> [ "--remount-ro"; Lpath.Abs.to_string path ])
    (Policy.denied_paths policy)

let arguments policy =
  let namespace =
    [ "--new-session"; "--die-with-parent"; "--unshare-user"; "--unshare-pid" ]
  in
  let network =
    match Policy.network policy with
    | Policy.Network.Restricted -> [ "--unshare-net" ]
    | Policy.Network.Enabled -> []
  in
  namespace @ filesystem_args policy @ network @ [ "--proc"; "/proc" ]
  @ seal_denials policy @ seal_root policy
