(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The command-confinement projection off the workspace capability.

    A command-spawning tool ([shell], and the OCaml family) surfaces the route
    its child will run through as an inert
    {!Mentat_permission.Access.Command.execution} fact on its permission
    request. This module projects that fact from the capability's {e sealed}
    posture — {!Mentat_workspace_io.policy} and {!Mentat_workspace_io.evidence}
    — the one owner of the confinement fact. The projection is pure over the
    immutable sealed value: a family reads it {e once} at construction and
    closes the result into its [permissions]. A tool never holds a
    {!Mentat_sandbox.t}. *)

val confined :
  Mentat_workspace_io.t -> Mentat_permission.Access.Command.execution
(** [confined ws] is the execution route of an ordinary (non-escalated) command
    run through [ws]: [Enforced confinement] under an enforcing sealed policy,
    [External] on the declared-external route, [Direct] on the unconfined route.

    The projection is total. A refused sealed posture, and an enforcing posture
    with no sealed policy, are contract-impossible past the startup resolution
    gate — they refuse the whole run before any command — so [confined] needs no
    failure case. Should such a value reach it, it degrades to [Direct], making
    no confinement claim rather than fabricating one. *)

val escalated : Mentat_permission.Access.Command.execution
(** [escalated] is [Direct]. An approved escalation escapes the enforcing
    profile; the command outcome separately records the route that actually ran.
*)

val custom_access :
  Mentat_permission.Access.Command.execution -> Mentat_permission.Access.t
(** [custom_access execution] is the family-wide inert custom fact named
    ["command.confinement"]. Its subject is
    {!Mentat_permission.Access.Command.execution_to_string}[ execution], so the
    stable permission identity distinguishes enforced, external, and direct
    routes without exposing a fixed implementation argv as model-authored
    command text. OCaml intelligence tools use this fact alongside their read
    scope. *)

val denial_note :
  widening:[ `Here | `Through_shell ] ->
  Mentat_workspace_io.Confinement.t option ->
  string
(** [denial_note ~widening observation] is the advisory to append to a failed
    command's message, or [""] when nothing guarded the command or nothing in
    its output looks like the guard refusing something.

    [widening] is whether the calling tool carries the sandbox parameters
    itself. Only the foreground shell does, so every other site passes
    [`Through_shell] and is told to route the command there rather than being
    handed a parameter it does not accept. It is separate from whether a
    widening exists at all, which is the sealed posture's answer and is read
    from [observation] — a route that promised no mutation is offered neither.

    Every spawn site renders through here, so the advice is always a move that
    exists: the narrow grant is offered before the total escalation, an unscoped
    route never names a read field the resolver discards, and a backend that
    cannot distinguish a denied read from a missing file says so rather than
    asserting a refusal. *)
