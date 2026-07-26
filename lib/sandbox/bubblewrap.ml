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
