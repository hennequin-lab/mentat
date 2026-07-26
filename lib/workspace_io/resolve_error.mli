(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Workspace capability resolution errors.

    Returned by {!Mentat_workspace_io.resolve}, the one constructor of the
    workspace capability. Resolution fails closed: any error here means no
    capability exists, so nothing later can launch or write through a partially
    resolved workspace. Roots opened before the failure are released with the
    resolve switch. *)

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
          [sandbox.writable_roots], a [PATH] or toolchain-variable segment, or a
          git-worktree metadata target) could not be admitted. *)
  | Broad_root of { field : string; spelling : string }
      (** A root is too broad to admit: [/], the account home, an ancestor of a
          workspace root — or, for [sandbox.writable_roots], a spelling that
          would grant writes over an admitted read-only root. [field] names the
          surface that supplied it: a configuration field, [PATH], a toolchain
          variable, the platform set, or the project gitdir. A broad temp-dir
          variable is dropped from the write set rather than refused, so it
          never reaches here. *)
  | Denied_overlaps_grant of { denied : string; granted : string }
      (** A path Mentat denies to every confined command {e contains} a root the
          policy would grant. Denials are lowered last, so the overlap does not
          narrow the sandbox: it masks the granted root with an empty mount the
          agent cannot distinguish from a wiped directory, and under bubblewrap
          it is worse than that — the denial freezes the tree read-only before
          the nested grant can be mounted inside it, so the sandbox fails to
          start and every command in the session dies at setup. Refused at
          resolution, loudly, instead.

          Only this direction. A denial nested {e inside} a granted root is the
          case worth keeping: it masks that subtree and leaves the rest of the
          root intact, which is what a store kept in the workspace needs. *)
  | Missing_root of Mentat_workspace.Root.t
      (** An admitted logical workspace root does not currently name an existing
          directory, so it cannot be opened. *)
  | Io of { operation : string; spelling : string; cause : Eio.Exn.err }
      (** A filesystem effect of resolution itself failed — opening an admitted
          root, or creating one of the directories Mentat owns. [operation]
          names the effect, [spelling] the path it was applied to. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] for diagnostics. The output is not a stable matching
    surface; branch on the constructor. *)
