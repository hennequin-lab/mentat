(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Rendering a workspace path as the address string a tool response carries.

    A tool reports a path to the model as a short address: relative to the
    workspace current directory when the path lies within it, and canonical
    absolute otherwise. These helpers share that rendering so every tool's
    addresses read alike. *)

val absolute_exn : Mentat_workspace_io.t -> Mentat_workspace.Path.t -> string
(** [absolute_exn workspace_io path] is [path]'s canonical absolute address
    string for a caller that has already proven [path] resolvable. Raises
    [Assert_failure] if it does not. *)

val provider :
  Mentat_workspace_io.t -> Mentat_workspace.Path.t -> (string, string) result
(** [provider workspace_io path] is [path] rendered relative to the workspace's
    current directory when it lies within — ["."] at the root — and its
    canonical absolute address otherwise, or the resolve error message. *)

val display : Mentat_workspace_io.t -> Mentat_workspace.Path.t -> string
(** [display workspace_io path] is {!provider}'s address for a caller that has
    already proven both the workspace current directory and [path] resolvable.
    Raises [Assert_failure] if either is not: every rendered path was resolved
    through the same capability. *)

val display_relative :
  Mentat_workspace_io.t ->
  cwd:Mentat_workspace.Path.t ->
  Mentat_workspace.Path.t ->
  string
(** [display_relative workspace_io ~cwd path] is [path] rendered relative to an
    already-resolved [cwd] — ["."] at the root — and its {!absolute_exn}
    otherwise. It is {!display} reusing a [cwd] the caller resolved once. *)
