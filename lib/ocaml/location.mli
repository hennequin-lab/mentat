(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** File locations pairing a workspace path with a source {!Range.t}. *)

type t
(** A workspace file location. *)

val make : path:Mentat_workspace.Path.t -> range:Range.t -> t
(** [make ~path ~range] is the location of [range] within [path]. *)

val path : t -> Mentat_workspace.Path.t
(** [path t] is [t]'s workspace file path. *)

val range : t -> Range.t
(** [range t] is [t]'s source range. *)

val start : t -> Position.t
(** [start t] is [Range.start (range t)]. *)

val end_ : t -> Position.t
(** [end_ t] is [Range.end_ (range t)]. *)

val compare : t -> t -> int
(** [compare a b] orders locations by path, then by range. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] as ["path:range"], using the path's display form. *)
