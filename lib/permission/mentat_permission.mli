(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Pure permission modelling for agent operations.

    This library owns the {e consent} question of the agent authority model:
    given a trusted description of an operation, {e may it proceed?} Keep three
    complementary questions apart; permission answers only the middle one:
    - {b trust} — may this workspace be activated at all? Runtime activation
      state, not modelled here.
    - {b permission} — is this described operation authorized? {e This library.}
    - {b sandbox} — once it runs, what confines it? Independent enforcement, not
      modelled here.

    An allowed request means the pure policy accepted the described accesses; it
    never asserts that a process was confined. Permission and sandbox are
    complementary foundations: consent bounds {e whether} an operation is
    authorized, confinement bounds {e what} it can then do.

    {b The four nouns compose left to right.} {!Access.t} is a trusted, inert
    fact describing one operation (a filesystem op, a command, a network reach,
    a custom subject); {!Request.t} groups the accesses of one action into a
    single review unit; {!Policy.t} decides a request — allow, deny, or send to
    review; {!Answer.t} records the authority a reviewer selects over a request.
    [Match]/[Rule]/[Grants]/[Review]/[Denial]/[Decision] are {!Policy}'s
    sub-vocabulary, not further top-level concepts. Beside the nouns,
    {!Review_behavior} is run-posture vocabulary: what a host does with review
    outcomes ({e enforce} or {e bypass}); it changes no pure evaluation.

    {b Typical flow.} Construct trusted {!Access.t} facts in host or tool code
    from already-decoded operation input; group them with {!Request.of_accesses}
    or {!Request.make}; evaluate with {!Policy.decide}; and, when review is
    needed, record the reviewer's choice as an {!Answer.t}. Stable permission
    identity is carried by {!Access.t}; display text, prompt ids, turns, tool
    calls, model-visible results, and resolution provenance belong to the
    resolving session, not this library. {!Unattended} names the narrower
    headless fallback for a permission review when no reviewer is present.

    {b Pure and dependency-light.} It performs no operation, confines no
    process, spawns nothing, touches no filesystem, resolves no path against
    disk, normalizes no name, prompts nobody, persists nothing, and writes no
    audit record — those are host jobs. {!Access.stable_text} is canonical
    authority text; {!Policy.Rule.id} owns stable compact rule identity through
    the pure {!Mentat_digest} foundation. The library otherwise imports only
    [jsont], {!Lpath}, and {!Mentat_workspace}. *)

module Access = Access
(** Permission-relevant operation facts. *)

module Request = Request
(** Permission requests over access facts. *)

module Policy = Policy
(** Permission policy: matchers, rules, evaluation, grants, and reviews. *)

module Answer = Answer
(** The authority a reviewer selects when resolving a request. *)

module Review_behavior = Review_behavior
(** Per-run handling of review outcomes. *)

module Unattended = Unattended
(** Headless handling of permission reviews. *)
