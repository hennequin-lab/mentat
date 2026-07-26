(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Helpers shared by the merlin-backed OCaml tools: adapting a {!Merlin} error
    to a tool failure, resolving and reading a source file, and mapping an
    absolute path back into the workspace. Kept beside {!Merlin} rather than in
    a lower library so the merlin runner stays free of the tool-result and
    workspace vocabularies. *)

val merlin_failure : Merlin.error -> 'a Mentat_tool.Result.t
(** [merlin_failure error] is the failed tool result for a merlin [error]:
    {!Merlin.Unavailable} is [`Unavailable], {!Merlin.Timed_out} is
    [`Timed_out], every other error is [`Failed], each carrying
    {!Merlin.error_message}. *)

val workspace_path_of_absolute :
  Mentat_workspace_io.t ->
  source_root:Mentat_workspace.Path.t ->
  cwd:Lpath.Abs.t ->
  Lpath.Abs.t ->
  Mentat_workspace.Path.t option
(** [workspace_path_of_absolute workspace_io ~source_root ~cwd absolute] is the
    workspace path for [absolute] when it belongs to the workspace — resolving
    the path through [workspace_io], else relativizing it against [cwd] under
    [source_root]'s root — and [None] when [absolute] escapes the workspace. *)

val load_source :
  Mentat_workspace_io.t ->
  Mentat_workspace.Path.t ->
  max_bytes:int ->
  (string, 'a Mentat_tool.Result.t) result
(** [load_source workspace_io path ~max_bytes] reads an already-resolved [path]
    when it is a regular file of at most [max_bytes] bytes. It is [Ok source] or
    a failed tool result: [`Invalid_input] when [path] is not a regular file,
    and {!Fs_error.failed} for a stat or read fault. {!resolve_source} is this
    read behind a {!Mentat_workspace_io.resolve_path}. *)

val resolve_source :
  Mentat_workspace_io.t ->
  path:string ->
  max_bytes:int ->
  (Mentat_workspace.Path.t * string, 'a Mentat_tool.Result.t) result
(** [resolve_source workspace_io ~path ~max_bytes] resolves [path] and, when it
    is a regular file of at most [max_bytes] bytes, reads it whole. It is
    [Ok (resolved, source)] with the resolved workspace path and its contents,
    or a failed tool result: [`Invalid_input] when [path] does not resolve or is
    not a regular file, and {!Fs_error.failed} for a stat or read fault. A file
    over [max_bytes] is such a read fault, not a truncated prefix; each merlin
    tool sets its own [max_bytes] ceiling. *)
