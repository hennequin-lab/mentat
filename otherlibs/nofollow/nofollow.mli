(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Symlink-refusing filesystem primitives over directory descriptors.

    The [openat]-family syscalls for walking a tree one component at a time
    without ever following a symbolic link: every step is relative to an
    already-open directory descriptor and passes [O_NOFOLLOW], so no symbolic
    link on the path can redirect an operation outside the subtree the caller is
    walking. This every-component-[O_NOFOLLOW] discipline is the reason the
    library exists — a guarantee neither a path string nor a portable filesystem
    API can express, and the basis for confining an operation to an anchored
    root.

    These are primitives, not a resolver or a sandbox: each call acts on a
    single path component beneath an open directory, and the caller sequences
    the walk and owns the policy.

    All functions raise [Unix.Unix_error]; a refused symlink surfaces as
    [ELOOP]. Returned descriptors are close-on-exec; callers own closing them.
*)

val open_dir : string -> Unix.file_descr
(** [open_dir path] opens the directory at the absolute [path]
    ([O_RDONLY|O_DIRECTORY]), following symlinks — used once to anchor a root,
    never on a later traversal. *)

val openat_dir : Unix.file_descr -> string -> Unix.file_descr
(** [openat_dir dir name] opens the directory entry [name] beneath [dir],
    refusing a symlink ([ELOOP]) and a non-directory ([ENOTDIR]). *)

val openat_read : Unix.file_descr -> string -> Unix.file_descr
(** [openat_read dir name] opens [name] beneath [dir] read-only, refusing a
    final symlink ([ELOOP]). The open is non-blocking so a FIFO cannot stall it;
    callers must check the descriptor's kind before reading. *)

type stat = {
  kind : [ `Regular | `Directory | `Symlink | `Other ];
  perm : int;  (** the entry's permission bits ([st_mode land 0o7777]) *)
  size : int64;
}
(** No-follow entry metadata, as much of it as the write path needs. *)

val fstatat : Unix.file_descr -> string -> stat
(** [fstatat dir name] is the metadata of the entry [name] beneath [dir], never
    following a final symlink ([AT_SYMLINK_NOFOLLOW]) — the disambiguator for
    platforms whose [O_DIRECTORY|O_NOFOLLOW] open reports a symlink as
    [ENOTDIR]. *)

val openat_create : Unix.file_descr -> string -> int -> Unix.file_descr
(** [openat_create dir name perm] exclusively creates and opens [name] beneath
    [dir] for writing ([O_CREAT|O_EXCL|O_NOFOLLOW]); [perm] is masked by the
    process umask. An existing entry is [EEXIST]. *)

val mkdirat : Unix.file_descr -> string -> int -> unit
(** [mkdirat dir name perm] creates the directory [name] beneath [dir]; an
    existing entry — including a symlink — is [EEXIST]. *)

val unlinkat : Unix.file_descr -> string -> dir:bool -> unit
(** [unlinkat dir name ~dir:remove_dir] removes the entry [name] beneath [dir] —
    the entry itself, never a symlink's referent. [~dir:true] removes an empty
    directory ([AT_REMOVEDIR]). *)

val renameat : Unix.file_descr -> string -> string -> unit
(** [renameat dir old_name new_name] atomically renames [old_name] over
    [new_name] within [dir]. Neither final component is followed: a symlink at
    [new_name] is replaced as an entry, never written through. *)

val linkat : Unix.file_descr -> string -> string -> unit
(** [linkat dir old_name new_name] publishes [old_name] at [new_name] within
    [dir] without replacing an existing entry ([EEXIST]) — the atomic no-replace
    publication a creation commits with. *)
