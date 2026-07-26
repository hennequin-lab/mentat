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
   on, and the advice is only ever a move that is actually available.

   Availability has two halves, and conflating them is how this went wrong
   before. The {e posture} decides whether any widening exists: a route that
   promised no mutation refuses both a grant and an escalation, so neither may
   be suggested. The {e tool} decides who can ask: only the foreground shell
   carries the parameters, so every other site must be told to go through it
   rather than handed a parameter it does not have.

   Where a widening is available, the narrow one leads. A grant names the path
   and keeps the rest of the sandbox; an escalation drops all of it to write one
   file. Steering to the total move first is how a lock file ends up costing the
   whole posture.

   Two further things it must not do: name [sandbox.readable_roots] on an
   unscoped route, where the resolver discards that field, and claim a refused
   read on a backend that cannot tell one from a missing file. *)
let denial_note ~widening observation =
  match observation with
  | None -> ""
  | Some observation -> (
      let escalatable =
        Mentat_workspace_io.Confinement.escalatable observation
      in
      let through_shell =
        match widening with
        | `Here -> ""
        | `Through_shell ->
            "this tool takes no sandbox parameters, so run the equivalent \
             command through shell: "
      in
      (* The two refusals do not share a remedy. A grant admits a path, which
         can do nothing about a socket, so steering a blocked connection at
         [grant_write] spends a turn on a parameter that cannot help. Only the
         total move reaches the network. *)
      let filesystem_recovery =
        if not escalatable then
          "this sandbox promises no mutation, so neither a grant nor an \
           escalation is available; the access has to come from configuration"
        else
          through_shell
          ^ "retry with grant_write naming the directory that contains the \
             path the output reports, which widens the sandbox to that \
             directory alone for one command, or escalate=true if the access \
             is genuinely broader"
      in
      let network_recovery =
        if not escalatable then
          "this sandbox promises no mutation, so no escalation is available; \
           the access has to come from configuration"
        else
          through_shell
          ^ "retry the exact command with escalate=true only if the access is \
             genuinely needed — a grant admits a path and cannot open the \
             network"
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
           and its output looks like a blocked request. If it is one, retrying \
           unchanged will not help: " ^ network_recovery
          ^ ". To lift the restriction for the session instead, ask the user \
             to set sandbox.network to enabled."
      | Some Mentat_workspace_io.Confinement.Filesystem ->
          let what =
            if Mentat_workspace_io.Confinement.reads_attributable observation
            then
              "its output looks like a refused read or write — a resemblance, \
               not a reading of the policy, so check it against what the \
               command was doing before acting on it. If it is one"
            else
              "its output looks like a refused write. A denied read is not \
               distinguishable from a missing file under this backend, so if a \
               file you expected is absent it may be outside the read scope \
               rather than gone"
          in
          "\n\n\
           This command ran inside a sandbox that confines filesystem access, \
           and " ^ what ^ ": " ^ filesystem_recovery
          ^ ". For a standing grant instead of a one-command one, ask the user \
             to add the path to " ^ standing ^ ".")
