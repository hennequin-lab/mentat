(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = {
  catalog : Mentat_agent_step.Catalog.t;
  workspace : Ports.workspace;
  policy : Mentat_permission.Policy.t;
  prelude : Mentat_llm.Request.Prelude.t;
}

let make ~catalog ~workspace ~policy ~prelude =
  { catalog; workspace; policy; prelude }

type selector =
  configured:Config.t ->
  model:Mentat_llm.Model.t ->
  sealed_declarations:Mentat_llm.Tool.t list option ->
  Mentat_session.Contract.Mode.t ->
  t

type factory =
  background:Eio.Switch.t ->
  selector * (unit -> Mentat_protocol.Process.View.t list)

type delegated_factory =
  role:Mentat_session.Delegation.Role.t option ->
  background:Eio.Switch.t ->
  selector * (unit -> Mentat_protocol.Process.View.t list)
