(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Error = Error
module Policy = Policy
module Backend = Backend
module Requirement = Requirement
module Evidence = Evidence
module Identity = Identity

type t = Seal.t
type escalation = Seal.escalation = Available | Denied of Error.t | Ignored

module Obligation = Seal.Obligation

let confined = Seal.confined
let grant = Seal.grant
let direct = Seal.direct
let external_ = Seal.external_
let policy = Seal.policy
let evidence = Seal.evidence
let escalated_evidence = Seal.escalated_evidence
let escalation = Seal.escalation
let identity = Seal.identity
let obligations = Seal.obligations
let lower_argv = Seal.lower_argv
let lower_escalated_argv = Seal.lower_escalated_argv

let admits (requirement : Requirement.t) sandbox =
  match (requirement, evidence sandbox) with
  | Requirement.Off, _ -> Ok ()
  | _, Evidence.Enforced _ -> Ok ()
  | _, Evidence.Not_requested -> Ok ()
  | Requirement.Enforced_or_external, Evidence.Declared_external -> Ok ()
  | Requirement.Enforced, Evidence.Declared_external ->
      Error Requirement.Rejection.External_not_enforced
  | ( (Requirement.Enforced_or_external | Requirement.Enforced),
      Evidence.Refused error ) ->
      Error (Requirement.Rejection.Unenforceable error)
