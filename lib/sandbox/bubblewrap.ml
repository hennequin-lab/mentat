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

(* A clause naming the root is hoisted into the root position rather than
   emitted with the others. [--bind] is recursive, so a root clause landing after
   [--dev] re-binds the host's root over the devtmpfs bwrap just made: [/dev]
   becomes the host mount and [/dev/null] stops working, which fails every
   command rather than confining it. Hoisting is the fix and reordering [--dev]
   is not — moving [--dev] would change the argv of every existing policy and
   trip resume on every suspended session. *)
let filesystem_args policy =
  let root_clause, rest =
    List.partition
      (fun (path, _) -> Lpath.Abs.is_root path)
      (Policy.entries policy)
  in
  (* The root mount follows the root clause's own access, exactly as the
     resolution law resolves the root: a [Write] clause binds the host root
     read-write, a [Read] clause binds it read-only, a [Deny] clause makes it a
     sealed empty tmpfs. None of these consult the reads default — that decides
     only the root no clause names, [All] reading the host root and [Denied]
     hiding it. A [Read] or [Deny] clause that once fell through to the default
     is the two configurations that made bubblewrap disagree with both the
     resolution law and seatbelt; deciding on the clause alone closes them. *)
  let root =
    match root_clause with
    | (_, Policy.Access.Write) :: _ -> [ "--bind"; "/"; "/" ]
    | (_, Policy.Access.Read) :: _ -> [ "--ro-bind"; "/"; "/" ]
    | (_, Policy.Access.Deny) :: _ -> [ "--tmpfs"; "/" ]
    | [] -> (
        match Policy.reads_default policy with
        | Policy.All -> [ "--ro-bind"; "/"; "/" ]
        | Policy.Denied -> [ "--tmpfs"; "/" ])
  in
  (* [--proc] is hoisted here with [--dev], ahead of the clause mounts, so a
     clause naming [/proc] wins over the procfs mount the way the resolution
     law says it must: the last mount on a path wins, and a hoisted [--proc]
     lands before it rather than after. Both still precede the seals, which
     bwrap cannot run under a sealed root. *)
  root @ [ "--dev"; "/dev"; "--proc"; "/proc" ] @ List.concat_map clause rest

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
  namespace @ filesystem_args policy @ network @ seal_denials policy
  @ seal_root policy
