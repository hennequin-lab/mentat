(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let confinement ~escalation (policy : Mentat_sandbox.Policy.t) :
    Mentat_permission.Access.Command.Confinement.t =
  let read =
    match Mentat_sandbox.Policy.reads_default policy with
    | Mentat_sandbox.Policy.Denied ->
        Mentat_permission.Access.Command.Confinement.Project
    | Mentat_sandbox.Policy.All ->
        Mentat_permission.Access.Command.Confinement.All
  in
  (* The posture, not the shape of a list. A no-mutation route is still granted
     somewhere to put a temporary file, so an inhabited writable list no longer
     means the workspace is writable — the same inference that made the
     escalation stance wrong. [escalation] is the sealed posture's own answer:
     it is [Denied] exactly when the route promised no mutation. *)
  let write =
    match (escalation : Mentat_sandbox.escalation) with
    | Mentat_sandbox.Denied _ ->
        Mentat_permission.Access.Command.Confinement.Read_only
    | Mentat_sandbox.Available | Mentat_sandbox.Ignored ->
        Mentat_permission.Access.Command.Confinement.Workspace
  in
  let network =
    match Mentat_sandbox.Policy.network policy with
    | Mentat_sandbox.Policy.Network.Restricted ->
        Mentat_permission.Access.Command.Confinement.Restricted
    | Mentat_sandbox.Policy.Network.Enabled ->
        Mentat_permission.Access.Command.Confinement.Enabled
  in
  { Mentat_permission.Access.Command.Confinement.read; write; network }

(* [Refused], and [Enforced] with no sealed policy, are contract-impossible past
   the [check] startup gate: they refuse the whole run before any command. Reaching here degrades to [Direct] — no confinement claim — rather
   than fabricating one. *)
let execution_of ws (evidence : Mentat_sandbox.Evidence.t) =
  match evidence with
  | Mentat_sandbox.Evidence.Enforced _ -> (
      match Mentat_workspace_io.policy ws with
      | Some policy ->
          Mentat_permission.Access.Command.Enforced
            (confinement ~escalation:(Mentat_workspace_io.escalation ws) policy)
      | None -> Mentat_permission.Access.Command.Direct)
  | Mentat_sandbox.Evidence.Declared_external ->
      Mentat_permission.Access.Command.External
  | Mentat_sandbox.Evidence.Not_requested | Mentat_sandbox.Evidence.Refused _ ->
      Mentat_permission.Access.Command.Direct

let confined ws = execution_of ws (Mentat_workspace_io.evidence ws)
let escalated = Mentat_permission.Access.Command.Direct

let custom_access execution =
  Mentat_permission.Access.custom
    ~subject:(Mentat_permission.Access.Command.execution_to_string execution)
    "command.confinement"

(* One renderer for every spawn site. The boundary states what guarded a
   command; this turns that into the sentence its audience — the model — can act
   on, and the advice is only ever a move the route actually admits.

   Three things it must not do. It must not name [sandbox.readable_roots] on an
   unscoped route, where the resolver discards that field. It must not advise
   [escalate=true] where the route will refuse the escalation, which is every
   site but the foreground shell. And it must not claim a refused read on a
   backend that cannot tell one from a missing file. *)
let denial_note observation =
  match observation with
  | None -> ""
  | Some observation -> (
      let recovery =
        if Mentat_workspace_io.Confinement.escalatable observation then
          "retry the exact command with escalate=true only if the access is \
           genuinely needed"
        else
          "this tool cannot be escalated; run the equivalent command through \
           shell with escalate=true if the access is genuinely needed"
      in
      let standing =
        if Mentat_workspace_io.Confinement.reads_scoped observation then
          "sandbox.readable_roots (reads) or sandbox.writable_roots (writes)"
        else "sandbox.writable_roots"
      in
      match Mentat_workspace_io.Confinement.refusal observation with
      | None -> ""
      | Some Mentat_workspace_io.Confinement.Network ->
          "\n\n\
           This command ran inside a sandbox with network access restricted, \
           and its output looks like a blocked network request. This is a \
           policy restriction, not a transient error: " ^ recovery ^ "."
      | Some Mentat_workspace_io.Confinement.Filesystem ->
          let what =
            if Mentat_workspace_io.Confinement.reads_attributable observation
            then
              "its output looks like a refused read or write. This is a policy \
               restriction, not a transient error"
            else
              "its output looks like a refused write. A denied read is not \
               distinguishable from a missing file under this backend, so if a \
               file you expected is absent it may be outside the read scope \
               rather than gone"
          in
          "\n\n\
           This command ran inside a sandbox that confines filesystem access, \
           and " ^ what ^ ": " ^ recovery
          ^ ", or ask the user to add the path to " ^ standing
          ^ " for a standing grant.")
