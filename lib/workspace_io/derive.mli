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
      (** Platform infrastructure directories that must accept writes for system
          tooling to function — on macOS the per-user Darwin temp and cache
          buckets that Apple's developer-tool shims cache under. The scratch may
          nest inside them, so they are exempt from the scratch-disjointness
          guard; they join the sandbox policy's writable roots and are never
          described or protected. Empty on Linux. *)
  readable : Lpath.Abs.t list;
      (** The scoped read roots (workspace, configured, platform, executable,
          toolchain, git-worktree); [[]] when not scoped. *)
  protected : Lpath.Abs.t list;
      (** The existence-filtered protected paths: existing protected metadata
          under the primary root, the read-only workspace roots, and the linked
          git-worktree directories. Only existing entries appear. *)
  path : string;
      (** The derived child [PATH] value. Scoped, it is rebuilt from the
          toolchain bin directory followed by the admitted executable roots,
          deduplicated; unscoped, it prepends the toolchain bin directory to the
          ambient [PATH]. *)
  carried_dirs : (string * Lpath.Abs.t) list;
      (** Real user-level toolchain directories to carry onto the child
          environment as [(variable, canonical directory)]: opam's root
          ([OPAMROOT], else [$HOME/.opam]) and dune's XDG base directories
          ([XDG_CACHE_HOME], [XDG_CONFIG_HOME], [XDG_STATE_HOME],
          [XDG_DATA_HOME], else their [$HOME]-derived defaults). Each is the
          ambient value when the launcher set one, otherwise the standard
          default, and only when it exists as a directory. The child environment
          exports them so opam still resolves its root and dune still finds its
          shared cache after [HOME] is redirected to the scratch — without which
          a dune package build rebuilds its whole closure — and a scoped route
          admits each as a read root. Recovered independently of [scoped], since
          the exports are needed on every route. *)
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
  (derived, Resolve_error.t) result
(** [run ~scoped ~lookup ~logical ~configured_reads ~configured_writes] derives
    every policy input for one resolution. [scoped] is [true] iff the route is
    confined with project-scoped reads; unscoped derivation still canonicalizes
    the workspace roots and validates the configured writable roots. [lookup]
    reads the ambient environment. *)

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
