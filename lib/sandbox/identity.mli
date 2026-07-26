(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The durable identity of a sealed sandbox's confinement.

    A small, secret-free fingerprint of {e what a sealed sandbox confines}: its
    evidence class, its backend, and a {b scratch-invariant} confinement digest.
    It holds no profile text and cannot spawn: it is a value to persist, reload,
    and compare. [Session.Contract] seals it as the turn's sandbox identity.

    {b Confinement identity, not execution identity.} The child environment is
    excluded structurally — this library never sees one. The
    {b per-run scratch path is also excluded}: it is minted fresh every process,
    so a digest that included it would differ on every resume and refuse every
    pending invocation. The digest is computed from the {b scratch-normalized}
    profile: the profile regenerated with the policy's scratch field replaced by
    a fixed sentinel. It is therefore invariant under scratch regeneration.

    {b The profile text is the digest input, deliberately — not the confinement
       fields directly.} Hashing the reads, writable roots, protected paths, and
    network fields directly would be simpler, but it would miss a change that
    alters {e effective} confinement while every input stays equal: a Mentat
    binary upgrade whose profile generator changes (a new SBPL clause, a changed
    bind structure). Under the profile-digest form that change flips the digest,
    so resume refuses and re-approves under the new lowering — fail-closed, the
    D3 property that a changed confinement never runs under an old approval.
    Hashing the inputs directly would let such an upgrade run a pending
    invocation looser than approved. The honest cost is that a cosmetic
    generator change (whitespace, comment order) trips resume too; that false
    positive is safe (one re-approval), fires only across a binary upgrade
    mid-session, and is bounded by the golden-profile tests that pin the
    canonical text.

    {b Reload-and-revalidate.} A suspended contract persists an identity via
    {!jsont}; no live sandbox is stored. On resume the executable re-resolves
    and re-seals the sandbox, projects a fresh identity, and compares digests:
    equal means the confinement is unchanged and the pending invocation may
    proceed; different means it changed and may have widened — do not run under
    the old approval; reconcile or refuse.

    {b Comparison data, not authority.} {!jsont} is bidirectional, and a
    bidirectional codec is a constructor: it decodes any well-shaped
    class/backend/profile triple, so an identity can be written that no seal
    produced. That is harmless because an identity {e authorizes} nothing — it
    is the comparison datum resume checks against a freshly sealed sandbox, and
    a fabricated identity can only make that check refuse.

    {b Constructing comparison values.} A live identity is ordinarily projected
    from a sealed sandbox by {!Mentat_sandbox.identity}. The constructors below
    are also supported for tests, persisted-contract tooling, and other code
    that needs comparison data without a live seal. They do not seal a sandbox,
    mint {!Evidence}, or authorize execution. *)

type t
(** The type for confinement identities. Two identities are {!equal} iff their
    {!digest}s are equal. *)

val not_requested : t
(** [not_requested] is the identity of an intentionally unconfined direct seal.
    It is comparison data only; constructing it does not select an execution
    route. *)

val declared_external : t
(** [declared_external] is the identity of a declared-external seal. It is
    comparison data only; constructing it does not declare an execution
    boundary. *)

val refused : t
(** [refused] is the identity of a confined seal whose backend was unavailable.
    All refused seals share one identity: no confinement was established.
    Constructing it does not refuse or attempt a command. *)

val enforced : backend:Backend.t -> profile:Mentat_digest.t -> t
(** [enforced ~backend ~profile] is the identity of an enforced confined seal.
    [profile] is the scratch-normalized profile digest used for resume
    comparison, not the run's real profile digest. Constructing this value does
    not establish confinement or mint enforcement evidence. *)

val digest : t -> Mentat_digest.t
(** [digest t] is {e the} sandbox digest sealed into the turn contract:
    [Mentat_digest.derive ~domain:"mentat.sandbox.identity.v1"] over the
    evidence class, the backend id (enforced only), and the scratch-normalized
    profile digest in hexadecimal (enforced only). Stable across processes and
    reloadable; never derived from a live value. No absolute host path appears
    in plaintext — paths are carried as the profile hash. This is {b not}
    {!Evidence}'s profile digest, which keeps the real scratch for the run-start
    audit. *)

val equal : t -> t -> bool
(** [equal a b] is digest equality — what the resume reload check compares.

    {b Law (scratch-invariant).} Two seals of the same policy differing only in
    their per-run scratch path have equal identity. The digest changes on any
    durable confinement change — a widened writable root, a flipped network, an
    added read root, a changed protected path, a changed backend — and on a
    generator change that alters the profile text.

    Invariance assumes the per-run scratch is disjoint from the policy's root
    lattice, which the resolver guarantees (scratch is a fresh temporary
    directory). A scratch that were a lexical ancestor of a confined root yields
    a differing identity — fail-closed, never a collision. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats the class and backend for diagnostics. It is not storage
    syntax; persist with {!jsont}. *)

val jsont : t Jsont.t
(** [jsont] is the durable codec [Session.Contract.jsont] composes. Round-trip
    preserves {!digest}. Decode rejects non-canonical states — an enforced
    identity without a backend or profile, or a non-enforced class carrying one
    — so a reloaded identity is always {e well-shaped}. Decode does not (and
    cannot) prove a seal produced the value: identity is comparison data, not
    authority. *)
