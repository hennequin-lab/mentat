(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let path path = Lpath.Abs.to_string path

(* Every bind uses strict [--ro-bind]/[--bind]: on a missing source [bwrap]
   aborts the spawn. That is deliberate. Protected carveouts (e.g. [.git]) that
   may not exist under a writable root — a fresh checkout, or a cache root added
   by config — are existence-filtered by the effect twin before they reach the
   policy, so a bound carveout always exists at seal time. The only refused case
   is a carveout deleted between sealing and launch, which the sealed sandbox's
   obligations turn into a loud stale-policy refusal. The [--ro-bind-try]
   variant would instead silently drop the read-only overlay and run the command
   unprotected — fail-open, the wrong direction for a path bound to prevent
   writes. *)
let bind_readable root = [ "--ro-bind"; path root; path root ]
let bind_protected root = [ "--ro-bind"; path root; path root ]
let bind_writable root = [ "--bind"; path root; path root ]

let filesystem_args policy =
  let read_args =
    match Policy.reads policy with
    | Policy.All -> [ "--ro-bind"; "/"; "/"; "--dev"; "/dev" ]
    | Policy.Only roots ->
        [ "--tmpfs"; "/"; "--dev"; "/dev" ]
        @ List.concat_map bind_readable roots
  in
  let roots = Policy.scratch policy :: Policy.writable_roots policy in
  let carveouts = Policy.protected_paths policy in
  read_args
  @ List.concat_map bind_writable roots
  @ List.concat_map bind_protected carveouts

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
  match Policy.reads policy with
  | Policy.All -> []
  | Policy.Only _ -> [ "--remount-ro"; "/" ]

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
  @ seal_root policy
