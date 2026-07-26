(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Workspace path resolution errors.

    A resolution error is the failure to place an external address inside the
    workspace or bind a workspace path to the current root set. It is returned
    by {!Mentat_workspace.resolve_string}, {!Mentat_workspace.import_abs}, and
    {!Mentat_workspace.to_abs}, and is distinct from the configuration failures
    reported by {!Mentat_workspace.Error}.

    These errors are pure address-model failures. They do not report filesystem
    observation failures such as missing files, permission errors,
    non-directories, invalid UTF-8, symlink escapes, or size limits; those
    belong to the host observation layer. *)

type t =
  | Outside_workspace of Lpath.Abs.t
      (** [Outside_workspace path] means absolute [path] is not lexically under
          any workspace root. *)
  | Invalid_input of Lpath.Error.t
      (** [Invalid_input error] means raw path syntax failed before workspace
          resolution could produce a workspace path. *)
  | Unknown_root of Root.Key.t
      (** [Unknown_root key] means a path names a root key that the current
          workspace does not admit. Decoding a {!Path.t} does not establish
          admission; callers may recover by selecting a workspace containing
          [key]. *)

val message : t -> string
(** [message error] is a human-readable diagnostic for [error].

    [message] is for display, not programmatic matching. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same resolution error. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf error] formats [error] for diagnostics. *)
