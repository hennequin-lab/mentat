(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Workspace-contained filesystem observations for instruction and skill
    discovery.

    This is the impure companion to {!Context} and {!Skills}: the small set of
    filesystem reads those loaders need, with the one security property they
    must not get wrong — a followed symbolic link may not resolve outside a
    containment root. It performs no path parsing or workspace binding; callers
    pass already-normalized absolute paths and own the containment root.

    Containment is decided by resolving the target with the platform [realpath]
    and testing lexical {!Lpath.Abs.is_within} against the resolved root, so a
    symbolic link that escapes the root is rejected even when its lexical path
    stayed inside it. *)

type kind =
  | Regular
  | Directory
  | Other
      (** A filesystem entry kind. {!Other} covers every non-regular,
          non-directory target, including a symbolic link observed without
          following it. *)

type observation =
  | Missing  (** The path, or an intermediate component, does not exist. *)
  | Found of kind
      (** The resolved target lies within the root and has this kind. *)
  | Escapes  (** The resolved target lies outside the root. *)
  | Failed of string
      (** The observation itself failed; the string is a diagnostic message. *)

val observe : fs:_ Eio.Path.t -> root:Lpath.Abs.t -> Lpath.Abs.t -> observation
(** [observe ~fs ~root path] follows symbolic links to [path]'s target and
    requires the resolved target to lie within [root].

    A missing target — including one whose intermediate component is not a
    directory — is {!Missing}. A target whose resolved real path is not within
    [root] is {!Escapes}, even when the lexical [path] was inside it. Otherwise
    the target is {!Found} with the kind observed after following links. A
    [realpath] or stat failure is {!Failed}. *)

val read_limit : int
(** The read ceiling suitable for {!read_file}: a memory guard above any
    legitimate instruction, skill, or resource file, distinct from the far
    smaller display budgets a caller applies to what it keeps. *)

val read_file :
  fs:_ Eio.Path.t -> max_bytes:int -> Lpath.Abs.t -> (string, string) result
(** [read_file ~fs ~max_bytes path] is the contents of [path] when it is at most
    [max_bytes] bytes, following symbolic links, or [Error message] on any
    filesystem failure — including a [path] whose size exceeds [max_bytes],
    which is never loaded whole. The bound is mandatory so a project-owned file
    cannot exhaust memory. Containment is the caller's responsibility: read only
    paths a prior {!observe} placed in-root. *)

val read_dir_names :
  fs:_ Eio.Path.t -> Lpath.Abs.t -> (string list, string) result
(** [read_dir_names ~fs dir] is the entry names of [dir] in filesystem order, or
    [Error message] on failure. *)

val entry_kind : fs:_ Eio.Path.t -> Lpath.Abs.t -> kind option
(** [entry_kind ~fs path] is [path]'s kind observed {e without} following a
    final symbolic link — a symbolic link reads as {!Other} — or [None] when
    [path] is missing or cannot be observed. The nested-scan audit uses it: it
    classifies entries without ever following a directory symlink, so it needs
    no containment check. *)

val is_file_or_directory : fs:_ Eio.Path.t -> Lpath.Abs.t -> bool
(** [is_file_or_directory ~fs path] is whether [path] exists as a regular file
    or a directory, following symbolic links. Used for workspace root-marker
    detection. *)
