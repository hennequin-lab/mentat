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
    dropped, never fatal. *)

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
  path:string ->
  scratch:Lpath.Abs.t ->
  carried_dirs:(string * Lpath.Abs.t) list ->
  lookup:(string -> string option) ->
  t
(** [make ~path ~scratch ~carried_dirs ~lookup] builds the environment.

    [path] is the resolver-derived [PATH] value; its segments are normalized
    (absolute, deduplicated, malformed segments dropped). [scratch] becomes
    [HOME], [TEMP], [TMP], and [TMPDIR]. [carried_dirs] are real user-level
    toolchain directories exported as [(variable, value)] — opam's [OPAMROOT]
    and dune's [XDG_*] base directories — so those tools still resolve their
    root and shared cache once [HOME] is redirected to the scratch; the resolver
    recovers them already existence-checked. [lookup] reads the ambient
    environment for the allow-listed names: locale variables verbatim and the
    OCaml toolchain variables with their path values normalized. Fixed pager,
    color, and terminal bindings complete the set. *)
