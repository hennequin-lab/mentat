(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Compact result quantities for [read_file].

    File contents and directory entries remain solely in
    {!Mentat_tool.Output.text}. *)

(** The type for a file read, directory listing, returned image, or matching
    conditional read. Counts are non-negative. *)
type t = private
  | File of { lines : int }  (** A returned file slice. *)
  | Listing of { entries : int }  (** A returned directory page. *)
  | Image
      (** A returned image: the read yielded model-visible media, not lines, so
          it carries no count. *)
  | Unchanged  (** A matching conditional read. *)

val file : lines:int -> t
(** [file ~lines] is a file result containing [lines] logical lines.

    Raises [Invalid_argument] if [lines] is negative. *)

val listing : entries:int -> t
(** [listing ~entries] is a directory result containing [entries] entries.

    Raises [Invalid_argument] if [entries] is negative. *)

val image : t
(** [image] is a read that returned an image as model-visible media. *)

val unchanged : t
(** [unchanged] is a matching conditional read. *)

val jsont : t Jsont.t
(** [jsont] maps read facts to their closed version-1 JSON shape. *)
