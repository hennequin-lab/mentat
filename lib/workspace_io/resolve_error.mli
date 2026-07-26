(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Workspace capability resolution errors.

    Returned by {!Mentat_workspace_io.resolve}, the one constructor of the
    workspace capability. Resolution fails closed: any error here means no
    capability exists, so nothing later can launch or write through a partially
    resolved workspace. Roots opened and scratch minted before the failure are
    released with the resolve switch. *)

(** Why a configured root spelling could not be admitted. *)
type root_reason =
  | Does_not_exist  (** no filesystem entry exists at the spelling *)
  | Not_a_directory
      (** an entry exists but is not the directory its role requires (a
          directory root, a [PATH] or toolchain segment, or a path prefix) *)
  | Not_a_directory_or_file
      (** an entry exists but is neither a directory nor a regular file, for a
          role that admits both (a read root, the [.git] entry) — sockets,
          FIFOs, and device nodes are refused *)
  | Not_accessible
      (** the spelling could not be admitted for another reason: a malformed or
          relative spelling, a failed [~] expansion, an unreadable parent, or a
          canonicalization failure *)

type t =
  | Invalid_root of { spelling : string; reason : root_reason }
      (** A configured or derived root spelling ([sandbox.readable_roots],
          [sandbox.writable_roots], a [PATH] or toolchain-variable segment, a
          git-worktree metadata target, or the scratch base) could not be
          admitted. *)
  | Broad_root of { field : string; spelling : string }
      (** A root is too broad to admit: [/], the account home, an ancestor of a
          workspace root — or, for [sandbox.writable_roots], a spelling that
          would grant writes over an admitted read-only root; for [TMPDIR], a
          scratch base that would place the writable per-run scratch inside an
          admitted root. [field] names the surface that supplied it: a
          configuration field, [PATH], [TMPDIR], a toolchain variable, the
          platform set, or the project gitdir. *)
  | Denied_overlaps_writable of { denied : string; writable : string }
      (** A path Mentat denies to every confined command overlaps the write
          lattice. Denials are lowered last, so the overlap would not narrow the
          sandbox — it would mask the writable root with an empty mount the
          agent cannot distinguish from a wiped directory. Refused at
          resolution, loudly, instead. *)
  | Missing_root of Mentat_workspace.Root.t
      (** An admitted logical workspace root does not currently name an existing
          directory, so it cannot be opened. *)
  | Io of { operation : string; spelling : string; cause : Eio.Exn.err }
      (** A filesystem effect of resolution itself failed — creating the scratch
          directory or opening an admitted root. [operation] names the effect,
          [spelling] the path it was applied to. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] for diagnostics. The output is not a stable matching
    surface; branch on the constructor. *)
