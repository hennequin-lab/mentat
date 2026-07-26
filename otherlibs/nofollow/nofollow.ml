(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

external open_dir : string -> Unix.file_descr = "caml_nofollow_open_dir"

external openat_dir : Unix.file_descr -> string -> Unix.file_descr
  = "caml_nofollow_openat_dir"

external openat_read : Unix.file_descr -> string -> Unix.file_descr
  = "caml_nofollow_openat_read"

external openat_create : Unix.file_descr -> string -> int -> Unix.file_descr
  = "caml_nofollow_openat_create"

type stat = {
  kind : [ `Regular | `Directory | `Symlink | `Other ];
  perm : int;
  size : int64;
}

external fstatat_raw : Unix.file_descr -> string -> int * int * int64
  = "caml_nofollow_fstatat"

let fstatat dir name =
  let kind, perm, size = fstatat_raw dir name in
  let kind =
    match kind with
    | 0 -> `Regular
    | 1 -> `Directory
    | 2 -> `Symlink
    | _ -> `Other
  in
  { kind; perm; size }

external mkdirat : Unix.file_descr -> string -> int -> unit
  = "caml_nofollow_mkdirat"

external unlinkat : Unix.file_descr -> string -> dir:bool -> unit
  = "caml_nofollow_unlinkat"

external renameat : Unix.file_descr -> string -> string -> unit
  = "caml_nofollow_renameat"

external linkat : Unix.file_descr -> string -> string -> unit
  = "caml_nofollow_linkat"
