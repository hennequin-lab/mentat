(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Structured errors returned by the [dune describe] normaliser. *)

type source =
  | Workspace_describe
  | Tests_describe
      (** The describe surface whose output failed to normalize. *)

type t =
  | Parse_error of { source : source; offset : int option; message : string }
  | Path_error of { path : string; message : string }
  | Duplicate_library_uid of string
  | Unknown_library_uid of string
      (** The type for recoverable normalisation errors.

          - [Parse_error] reports undecodable Dune output.
          - [Path_error] reports a path that cannot be resolved into the Mentat
            workspace.
          - [Duplicate_library_uid] and [Unknown_library_uid] report a library
            uid that Dune emitted twice, or referenced without declaring. *)

val message : t -> string
(** [message e] is a human-readable diagnostic for [e]. It is not stable enough
    for programmatic matching. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf e] formats {!message} [e]. *)
