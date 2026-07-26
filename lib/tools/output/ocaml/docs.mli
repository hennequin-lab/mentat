(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Compact declaration counts for [ocaml_docs].

    Signatures, documentation, source addresses, provenance, library metadata,
    and continuations remain solely in {!Mentat_tool.Output.text}. *)

type t
(** The type for returned-page declaration counts. Exceptions count as values,
    class types count as types, and classes and module types count as modules.
    All counts are non-negative. *)

val make : values:int -> types:int -> modules:int -> t
(** [make ~values ~types ~modules] is an OCaml documentation result containing
    the given declaration counts.

    Raises [Invalid_argument] if any count is negative. *)

val values : t -> int
(** [values t] is the number of returned values and exceptions. *)

val types : t -> int
(** [types t] is the number of returned types and class types. *)

val modules : t -> int
(** [modules t] is the number of returned modules, module types, and classes. *)

val jsont : t Jsont.t
(** [jsont] maps declaration counts to their closed version-1 JSON shape. *)
