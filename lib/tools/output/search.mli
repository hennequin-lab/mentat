(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Compact result counts for search tools.

    Matched paths, source lines, and previews remain solely in
    {!Mentat_tool.Output.text}. *)

(** The type for search-result quantities. Counts are non-negative. *)
type t = private
  | Files of { total : int }  (** A file-result count. *)
  | Matches of { total : int; files : int }
      (** Match and distinct-file counts. *)
  | Matching_lines of { total : int }  (** A matching-line count. *)

val files : total:int -> t
(** [files ~total] is a result containing [total] files.

    Raises [Invalid_argument] if [total] is negative. *)

val matches : total:int -> files:int -> t
(** [matches ~total ~files] is a result containing [total] matches across
    [files] files.

    Raises [Invalid_argument] if either count is negative or [files > total]. *)

val matching_lines : total:int -> t
(** [matching_lines ~total] is a result containing [total] matching lines.

    Raises [Invalid_argument] if [total] is negative. *)

val jsont : t Jsont.t
(** [jsont] maps search counts to their closed version-1 JSON shape. *)
