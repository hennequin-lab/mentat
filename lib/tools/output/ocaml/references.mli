(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Compact result counts for [ocaml_find_references].

    Reference locations remain solely in {!Mentat_tool.Output.text}. *)

type t
(** The type for eligible deduplicated reference and file counts. *)

val make : references:int -> files:int -> t
(** [make ~references ~files] is a reference result.

    Raises [Invalid_argument] if either count is negative or [files] exceeds
    [references]. *)

val references : t -> int
(** [references t] is the number of eligible deduplicated references. *)

val files : t -> int
(** [files t] is the number of files containing those references. *)

val jsont : t Jsont.t
(** [jsont] maps reference facts to their closed version-1 JSON shape. *)
