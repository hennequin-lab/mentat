(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** macOS Seatbelt profile generation — pure, library-internal.

    Lowers a {!Policy.t} to a [sandbox-exec] invocation: an SBPL profile string
    plus ordered [-D] path parameters. Generation is deterministic, so equal
    policies produce byte-equal output and equal profile digests, and it is
    golden-testable on any platform — a macOS SBPL snapshot verifies on Linux
    CI, asserted through {!Mentat_sandbox.lower_argv} (the SBPL document is the
    argv token after ["-p"]). Availability probing and the launch are the effect
    twin's ([mentat.workspace_io]); the enforcing executable is
    {!Backend.executable}.

    The base profile is ported from the Codex reference agent's Seatbelt policy,
    itself derived from Chrome's macOS sandbox policy, and is production-proven
    against real build tools. [Policy.All] permits host-wide reads and
    executable mappings; [Policy.Only roots] admits both operations only beneath
    the roots. Writes are denied except under writable roots and the private
    scratch, with concrete protected paths carved out; network is denied unless
    the policy enables it.

    {b No path byte enters the SBPL text}: every resolved path reaches
    [sandbox-exec] as a [-D KEY=VALUE] parameter referenced by name from the
    profile, so SBPL escaping is structurally moot. Hosts canonicalize writable
    and protected paths (for example [/tmp] to [/private/tmp]) before building
    the policy; see {!Policy}. *)

val sbpl : Policy.t -> string * (string * string) list
(** [sbpl policy] is the SBPL profile text and the ordered [-D] parameters
    [(key, absolute path)] it references. Total, pure, and deterministic —
    {b the golden surface}. Every path is a [-D] parameter; no path byte enters
    the SBPL text. *)
