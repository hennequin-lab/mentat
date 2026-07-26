(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Workspace file observation errors.

    Returned by the {!Mentat_workspace_io.File} operations. Observation is
    interpreted beneath the opened root capabilities, so containment failures
    are decided by the platform backend's confined resolution, never by a
    separate check-then-open path. *)

type t =
  | Unknown_root of Mentat_workspace.Root.Key.t
      (** The path could not be bound: its root key is not admitted by the
          resolved workspace. *)
  | Not_found of Mentat_workspace.Path.t
      (** No entry exists at the path (including a missing intermediate
          directory). *)
  | Escapes_workspace of Mentat_workspace.Path.t
      (** Resolution of the path — including a followed symlink — escaped the
          opened root it is bound to. Containment is per bound root: a symlink
          whose target lies within a {e different} admitted root still escapes.
          The refusal is made by the confined resolution of the platform
          backend, at its honesty tier. *)
  | Too_large of {
      path : Mentat_workspace.Path.t;
      size : int64;
      max_bytes : int;
    }
      (** The regular file at [path] is [size] bytes, over the [max_bytes] bound
          {!Mentat_workspace_io.File.load} was given. Observation reads are
          bounded so a large file cannot exhaust memory. *)
  | Io of { path : Mentat_workspace.Path.t; cause : Eio.Exn.err }
      (** Any other filesystem failure, including a genuine permission refusal
          ([EACCES]/[EPERM]) inside the root. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] for diagnostics. The output is not a stable matching
    surface; branch on the constructor. *)
