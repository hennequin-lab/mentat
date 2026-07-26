(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Shared filesystem-failure payloads.

    Every store domain reports filesystem and lock failures with the same
    payload — which primitive failed, on which path, with the structured OS
    cause — while keeping {e what was damaged} in its own error vocabulary.
    Control flow matches {!op} (and {!cause} where a specific errno decides
    recovery); {!message} renders, and callers never parse errno prose out of
    the rendered string. *)

(** The type for failed filesystem or lock primitives. *)
type op =
  | Open  (** Opening a file or directory. *)
  | Read  (** Reading file bytes or directory entries. *)
  | Write  (** Writing, creating, truncating, or removing an entry. *)
  | Sync  (** Flushing file data or a directory entry to stable storage. *)
  | Rename  (** Atomically replacing a target. *)
  | Lock  (** Acquiring or releasing an advisory lock. *)

(** The type for the structured cause of a failure. *)
type cause =
  | Unix_error of Unix.error
      (** The errno of a direct syscall (write, fsync, lockf, unlink) — retained
          structurally so recovery can match the specific error, not a rendered
          prose string. *)
  | Message of string
      (** A rendered non-errno failure: an {!Eio} backend error raised by a
          capability operation, or any other exception. The structured errno is
          not recovered from the Eio wrapper; the rendering carries its
          diagnostic. *)

type t = { op : op; path : string; cause : cause }
(** The type for a failed filesystem or lock operation: which primitive, on
    which path, with the structured OS cause. [path] locates the failure for
    diagnostics only — the store-relative path of the logical target — never a
    stable matching surface. {!cause} carries the errno where a syscall raised
    it. *)

val op_label : op -> string
(** [op_label op] is [op]'s lowercase verb ("open", "read", …), the same word
    {!message} uses. *)

val message : t -> string
(** [message t] is a human-readable diagnostic for [t]: the path, the failed
    primitive, and the rendered cause. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats {!message} output. *)
