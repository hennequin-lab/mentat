(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let confinement (policy : Mentat_sandbox.Policy.t) :
    Mentat_permission.Access.Command.Confinement.t =
  let read =
    match Mentat_sandbox.Policy.reads policy with
    | Mentat_sandbox.Policy.Only _ ->
        Mentat_permission.Access.Command.Confinement.Project
    | Mentat_sandbox.Policy.All ->
        Mentat_permission.Access.Command.Confinement.All
  in
  let write =
    match Mentat_sandbox.Policy.writable_roots policy with
    | [] -> Mentat_permission.Access.Command.Confinement.Read_only
    | _ :: _ -> Mentat_permission.Access.Command.Confinement.Workspace
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
          Mentat_permission.Access.Command.Enforced (confinement policy)
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
