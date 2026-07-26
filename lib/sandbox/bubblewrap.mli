(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Linux Bubblewrap argv generation — pure, library-internal.

    Lowers a {!Policy.t} to the [bwrap] arguments that wrap a command: namespace
    flags, filesystem binds, the network flag, and [--proc /proc]. Generation is
    deterministic, so equal policies produce byte-equal output and equal profile
    digests, and it is golden-testable on any platform, asserted through
    {!Mentat_sandbox.lower_argv} (the enforcing prefix is the argv verbatim).
    The namespace-availability probe, platform detection, and the launch are the
    effect twin's ([mentat.workspace_io]); the enforcing executable is
    {!Backend.executable}. *)

val arguments : Policy.t -> string list
(** [arguments policy] is the [bwrap] argument list following the executable —
    {b the golden surface}. Total, pure, and deterministic.

    Protected carveouts are bound with strict [--ro-bind]: an absent source
    aborts the spawn, which is fail-closed by design. The effect twin
    existence-filters carveouts at resolution, so an absent protected path never
    reaches the policy; a mid-flight deletion is caught by the sealed sandbox's
    obligations as a loud {!Error.Stale_policy} rather than silently skipped
    (which [--ro-bind-try] would do, failing open). *)
