(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Resolution-time root derivation.

    The effect half of sandbox resolution that turns the logical workspace and
    the ambient environment into policy inputs: canonicalized workspace roots,
    validated configured roots, platform/executable/toolchain/git-worktree read
    roots, the existence-filtered protected set, the derived child [PATH], and
    the origin-labeled display facts behind
    {!Mentat_workspace_io.describe_roots}.

    Every spelling is canonicalized exactly once, here; broad roots — [/], the
    account home, a workspace ancestor — are rejected at admission. The module
    performs resolution-time filesystem observation only ([stat], [realpath],
    reading git worktree metadata); it opens no capability and launches nothing.
*)

type derived = {
  workspace_roots : (Mentat_workspace.Root.t * Lpath.Abs.t) list;
      (** Each admitted logical root with its canonical directory, primary
          first, in admission order. These are the roots the resolver opens. *)
  writable : Lpath.Abs.t list;
      (** The primary root and the validated configured writable roots. *)
  platform_writable : Lpath.Abs.t list;
      (** Shared scratch space, and the platform infrastructure directories that
          must accept writes for system tooling to function: [/tmp] on both
          platforms, plus on macOS the per-user Darwin temp and cache buckets
          Apple's developer-tool shims cache under. The scratch may nest inside
          them, so they are exempt from the scratch-disjointness guard; they
          join the sandbox policy's writable roots and are never described or
          protected. *)
  toolchain_writable : Lpath.Abs.t list;
      (** Toolchain state a build must write, outside the workspace — dune's
          cache, which holds the revision-store lock a pinned-source build takes
          unconditionally. Separate from {!platform_writable} because it is
          persistent user state rather than scratch space: a route that promises
          no mutation is still granted somewhere to put a temporary file, and is
          not granted this. *)
  readable : Lpath.Abs.t list;
      (** The scoped read roots (workspace, configured, platform, executable,
          toolchain, git-worktree); [[]] when not scoped. *)
  protected : Lpath.Abs.t list;
      (** The existence-filtered protected paths: existing protected metadata
          under the primary root, the read-only workspace roots, the linked
          git-worktree directories, and the carveouts of each [Read_write]
          carried directory. Only existing entries appear, so a carveout naming
          a directory that does not exist yet protects nothing — the strict
          read-only bind a carveout lowers to cannot name a missing source. *)
  denied : Lpath.Abs.t list;
      (** Mentat's own user directories — the config, data and state homes —
          which no confined command may read or write on any route, in any mode.
          Materialized at resolution under the guard in {!run}, so the set is
          independent of machine state and a planted symlink cannot relocate the
          exclusion; entries whose parent Mentat does not own are dropped rather
          than created. Unlike {!protected}, these are not filtered against the
          writable roots — a denial is meaningful wherever it lies, and all of
          these lie outside. *)
  path : string;
      (** The derived child [PATH] value. Scoped, it is rebuilt from the
          toolchain bin directory followed by the admitted executable roots,
          deduplicated; unscoped, it prepends the toolchain bin directory to the
          ambient [PATH]. *)
  describe : (string * Lpath.Abs.t) list;
      (** Origin-labeled read-root facts in display order, before the
          policy-membership filter the facade applies. *)
}

val run :
  scoped:bool ->
  lookup:(string -> string option) ->
  logical:Mentat_workspace.t ->
  configured_reads:string list ->
  configured_writes:string list ->
  mentat_dirs:Lpath.Abs.t list ->
  (derived, Resolve_error.t) result
(** [run ~scoped ~lookup ~logical ~configured_reads ~configured_writes
     ~mentat_dirs] derives every policy input for one resolution.

    [mentat_dirs] are Mentat's own user directories, which become {!denied}. A
    denied path that {e contains} a writable root is refused
    ({!Resolve_error.Denied_overlaps_writable}): denials lower last, so it would
    mask the root itself and leave the agent unable to tell an emptied workspace
    from a deleted one. A denial nested {e inside} a writable root is admitted
    and enforced — that is a store kept inside the workspace, and masking just
    that subtree is the point. [scoped] is [true] iff the route is confined with
    project-scoped reads; unscoped derivation still canonicalizes the workspace
    roots and validates the configured writable roots. [lookup] reads the
    ambient environment. *)

val canonical : Lpath.Abs.t -> Lpath.Abs.t
(** [canonical path] is [path] resolved through [realpath] where it exists (so
    the described path equals the enforced path, for example [/tmp] to
    [/private/tmp] on macOS); a path that cannot be resolved keeps its lexical
    spelling. *)

val is_linux : unit -> bool
(** [is_linux ()] is [true] iff the host is Linux. *)

val is_executable_file : string -> bool
(** [is_executable_file path] is [true] iff [path] (symlinks followed) is a
    regular file this process may execute ([X_OK]). A directory or a
    non-executable file is [false], so neither can shadow a real executable in a
    [PATH] search or pass an availability probe. *)
