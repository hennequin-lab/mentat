(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type refusal = Filesystem | Network

type t = {
  reads_scoped : bool;
  reads_attributable : bool;
  escalatable : bool;
  refusal : refusal option;
}

let reads_scoped t = t.reads_scoped
let reads_attributable t = t.reads_attributable
let escalatable t = t.escalatable
let refusal t = t.refusal

(* Both lists are lenient lowercase affix scans over the child's own bytes: a
   hint about the likely cause, never proof. The network list keeps the specific
   "...while establishing" wording so a network refusal does not also match the
   filesystem list, and a matching network signature wins — it is the more
   specific diagnosis. *)
let network_signatures =
  [
    "could not resolve host";
    "couldn't resolve host";
    "could not resolve";
    "name or service not known";
    "temporary failure in name resolution";
    "couldn't connect to server";
    "connection refused";
    "network is unreachable";
    "no route to host";
    "operation not permitted while establishing";
  ]

(* A filesystem refusal surfaces as the bare OS wording, which differs by
   backend: seatbelt denies with [EPERM], bubblewrap with [EROFS] against a
   read-only mount. *)
let filesystem_signatures =
  [ "operation not permitted"; "permission denied"; "read-only file system" ]

let matches signatures transcript =
  List.exists (fun affix -> String.includes ~affix transcript) signatures

(* A denied *read* is not observable after the fact on every backend. Under
   bubblewrap a scoped read scope builds its root as a tmpfs, so a path no bind
   covers is simply absent and the child reports ENOENT — indistinguishable from
   a file that was never there. Seatbelt denies the open outright and says so.
   Nothing can recover the difference from the transcript, so the fact is
   carried rather than guessed at. *)
let reads_attributable_on ~backend ~reads_scoped =
  match (backend : Mentat_sandbox.Backend.t) with
  | Mentat_sandbox.Backend.Bubblewrap -> not reads_scoped
  | Mentat_sandbox.Backend.Seatbelt -> true

let observe ~sandbox ~evidence ~transcript =
  match (evidence : Mentat_sandbox.Evidence.t) with
  | Mentat_sandbox.Evidence.Not_requested | Mentat_sandbox.Evidence.Refused _
  | Mentat_sandbox.Evidence.Declared_external ->
      None
  | Mentat_sandbox.Evidence.Enforced { backend; _ } ->
      let reads_scoped =
        match Mentat_sandbox.policy sandbox with
        | None -> false
        | Some policy -> (
            match Mentat_sandbox.Policy.reads_default policy with
            | Mentat_sandbox.Policy.Denied -> true
            | Mentat_sandbox.Policy.All -> false)
      in
      let transcript = String.lowercase_ascii transcript in
      let network_restricted =
        match Mentat_sandbox.policy sandbox with
        | None -> false
        | Some policy -> (
            match Mentat_sandbox.Policy.network policy with
            | Mentat_sandbox.Policy.Network.Restricted -> true
            | Mentat_sandbox.Policy.Network.Enabled -> false)
      in
      (* A network-shaped failure is a refusal only where the posture refuses
         the network. Where it does not, the host really was unreachable, and
         the answer is no refusal at all rather than a filesystem one — the
         filesystem list would otherwise claim "operation not permitted while
         establishing" as its own. *)
      let refusal =
        if matches network_signatures transcript then
          if network_restricted then Some Network else None
        else if matches filesystem_signatures transcript then Some Filesystem
        else None
      in
      Some
        {
          reads_scoped;
          reads_attributable = reads_attributable_on ~backend ~reads_scoped;
          escalatable =
            (match Mentat_sandbox.escalation sandbox with
            | Mentat_sandbox.Available -> true
            | Mentat_sandbox.Denied _ | Mentat_sandbox.Ignored -> false);
          refusal;
        }
