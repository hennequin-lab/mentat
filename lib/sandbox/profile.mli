(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** A policy lowered by one backend: the internal enforcement profile.

    Internal to the library — no consumer outside it holds a profile. [Seal]
    builds a profile from the policy it seals, so the enforcing prefix a command
    runs under and the digest its {!Evidence} reports are generated together and
    cannot describe different policies. Generation is pure and deterministic:
    equal policies produce byte-equal profiles and equal digests. *)

type t
(** A lowered profile: the enforcing argv prefix, whether a working-directory
    argument is inserted at wrap time, and the digest of the generated profile
    text. *)

val prepare : Backend.t -> Policy.t -> t
(** [prepare backend policy] lowers [policy] with [backend]'s pure generator
    ({!Seatbelt.sbpl} or {!Bubblewrap.arguments}), assembling the enforcing
    prefix behind {!Backend.executable} and digesting the canonical profile text
    with [Mentat_digest.derive ~domain:"mentat.sandbox.profile.v1"]. Total. *)

val digest : t -> Mentat_digest.t
(** [digest t] is the derived digest of the generated profile text. {!Evidence}
    reports it. *)

val wrap : t -> cwd:Lpath.Abs.t -> string list -> string list
(** [wrap t ~cwd argv] is the enforcing argv around [argv]: [t]'s prefix,
    optionally a [--chdir cwd --] segment (Bubblewrap), then [argv]'s tokens
    verbatim. A prefix cannot rewrite, reorder, or drop the command it wraps.
    Pure prefix application; [argv] is [Seal]-validated non-empty. *)

val identity_digest : Backend.t -> Policy.t -> Mentat_digest.t
(** [identity_digest backend policy] is the profile digest {!Identity} seals for
    resume comparison: the digest of the profile [policy] lowers to on
    [backend]. It changes on any confinement change and on a generator change
    that alters the profile text. It once normalized a per-run scratch path out
    of the profile first; nothing in a policy is per-run any more, so it is now
    the same function as {!digest} applied to a freshly prepared profile, kept
    separate because what the two answer for differs — {!Identity} compares
    across processes, {!Evidence} audits one run. Total. *)
