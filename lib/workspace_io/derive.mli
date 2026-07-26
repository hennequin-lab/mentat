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

(** What a carried variable's directory may be used for. The child environment
    binds the variable to the directory itself in every case — that is what the
    tool resolves against — so these describe the {e admitted scope}, not the
    exported value. *)
type access =
  | Read  (** the whole directory is a read root; the tool never writes it *)
  | Read_subpaths of string list
      (** only these directory-relative subpaths are read roots, for a base
          directory shared with tools whose state is none of Mentat's business
      *)
  | Read_write of string list
      (** the whole directory is a writable root, less these directory-relative
          carveouts, for state a write would leak outside the sandbox *)

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
  path : string;
      (** The derived child [PATH] value. Scoped, it is rebuilt from the
          toolchain bin directory followed by the admitted executable roots,
          deduplicated; unscoped, it prepends the toolchain bin directory to the
          ambient [PATH]. *)
  carried_dirs : (string * Lpath.Abs.t * access) list;
      (** Real user-level toolchain directories to carry onto the child
          environment as [(variable, canonical directory, access)]: opam's root
          ([OPAMROOT], else [$HOME/.opam]) and the two XDG base directories dune
          resolves ([XDG_CACHE_HOME] and [XDG_CONFIG_HOME], else their
          [$HOME]-derived defaults). Each is the ambient value when the launcher
          set one, otherwise the standard default, and only when it exists as a
          directory. No other XDG variable is carried: nothing in the toolchain
          resolves them, and the state and data homes hold Mentat's own session
          store and logs. The child environment exports them so opam still
          resolves its root and dune still finds its shared cache after [HOME]
          is redirected to the scratch — without which a dune package build
          rebuilds its whole closure — and a scoped route admits each as a read
          root. Recovered independently of [scoped], since the exports are
          needed on every route.

          {b Law (access is decided, never defaulted).} [access] is part of the
          entry, so a variable cannot be carried without deciding what the tool
          it points at may do there. Both directions of that decision are real:
          carrying a directory a tool must write while withholding the write
          turns a cold-cache slowdown into a hard failure, and carrying one
          writable that the tool never writes hands the workspace-write posture
          state it has no reason to reach. An entry named [Read_write] is
          admitted as a writable root by every route that grants writes at all,
          less its carveouts, which join {!protected}. Project the views with
          {!carried_bindings} and {!carried_writable} rather than rebuilding
          either list. A broad resolution ([/] or the account home) drops a
          [Read] entry with a warning and fails a [Read_write] one closed. *)
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

val carried_bindings : derived -> (string * Lpath.Abs.t) list
(** [carried_bindings derived] is [derived.carried_dirs] as the
    [(variable, directory)] exports the child environment binds, in carry order.
*)

val carried_writable : derived -> Lpath.Abs.t list
(** [carried_writable derived] are the carried directories admitted
    {!Read_write}, in carry order — the roots a write-granting route must add to
    its policy so every directory it names on the child environment is usable
    for what it names it for. *)

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
