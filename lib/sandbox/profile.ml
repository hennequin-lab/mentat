(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = { prefix : string list; chdir : bool; digest : Mentat_digest.t }

let domain = "mentat.sandbox.profile.v1"

let seatbelt policy =
  let sbpl, params = Seatbelt.sbpl policy in
  let prefix =
    (Backend.executable Backend.Seatbelt :: [ "-p"; sbpl ])
    @ List.map (fun (key, value) -> "-D" ^ key ^ "=" ^ value) params
    @ [ "--" ]
  in
  let parts = sbpl :: List.map (fun (key, value) -> key ^ "=" ^ value) params in
  { prefix; chdir = false; digest = Mentat_digest.derive ~domain parts }

let bubblewrap policy =
  let prefix =
    Backend.executable Backend.Bubblewrap :: Bubblewrap.arguments policy
  in
  { prefix; chdir = true; digest = Mentat_digest.derive ~domain prefix }

let prepare backend policy =
  match backend with
  | Backend.Seatbelt -> seatbelt policy
  | Backend.Bubblewrap -> bubblewrap policy

let digest t = t.digest

let wrap { prefix; chdir; _ } ~cwd argv =
  let prefix =
    if chdir then prefix @ [ "--chdir"; Lpath.Abs.to_string cwd; "--" ]
    else prefix
  in
  prefix @ argv

(* The per-run scratch path is minted fresh every process, so the real profile
   digest differs on every resume. [identity_digest] regenerates the profile
   with the policy's scratch replaced by this fixed sentinel throughout, making
   the digest invariant under scratch regeneration while still changing on any
   durable confinement change. The sentinel is a synthetic absolute path that
   does not interact with real workspace roots.

   Invariance assumes the per-run scratch is disjoint from the policy's root
   lattice, which the real resolver guarantees (scratch is a fresh [temp_dir]).
   A scratch that were a lexical ancestor of a confined root would collapse that
   root under normalization differently from the sentinel, yielding a differing
   identity — fail-closed (resume re-approves), never a collision. *)
let sentinel_scratch =
  Lpath.Abs.of_string_exn "/mentat-sandbox/identity-scratch"

let scratch_normalized policy =
  let real_scratch = Policy.scratch policy in
  let reads =
    match Policy.reads policy with
    | Policy.All -> Policy.All
    | Policy.Only roots ->
        Policy.Only
          (List.filter
             (fun root -> not (Lpath.Abs.equal root real_scratch))
             roots)
  in
  Policy.make ~scratch:sentinel_scratch ~reads
    ~writable_roots:(Policy.writable_roots policy)
    ~protected_paths:(Policy.protected_paths policy)
    ~network:(Policy.network policy)

let identity_digest backend policy =
  digest (prepare backend (scratch_normalized policy))
