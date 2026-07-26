(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The one private child environment.

    Constructed exactly once, at resolution, and served byte-for-byte to every
    launch on every route: ambient secrets and agent sockets never appear
    because inheritance is allow-listed, and the per-run scratch directory
    doubles as [HOME] and the temp-dir family. Construction is total — ambient
    values that cannot be represented (a NUL byte, a malformed path segment) are
    dropped, never fatal.

    [PWD] is deliberately absent here and written by the launch instead: it
    names the directory a single command starts in, which the resolution does
    not know, and the external backends assign it themselves once they enter
    the sandbox. *)

type t = {
  bindings : string array;
      (** ["NAME=value"] bindings sorted by name, the exact array every spawn
          receives. *)
  path_dirs : string list;
      (** The normalized [PATH] directories, in search order — the same
          directories the child's [PATH] names, used by the launch boundary to
          resolve an implicit program. *)
}

val make :
  path:string -> scratch:Lpath.Abs.t -> lookup:(string -> string option) -> t
(** [make ~path ~scratch ~lookup] builds the environment.

    [path] is the resolver-derived [PATH] value; its segments are normalized
    (absolute, deduplicated, malformed segments dropped). [scratch] becomes
    [TEMP], [TMP], and [TMPDIR]. [HOME] is {e inherited}, not rewritten: the
    resolver derives the toolchain's [$HOME]-relative roots from the same value
    the child reads, so no directory can be named to a tool without the grant
    that makes it usable. [lookup] reads the ambient environment for the
    allow-listed names: [HOME], locale variables verbatim, and the OCaml
    toolchain variables with their path values normalized. Fixed pager, color,
    and terminal bindings complete the set. *)
