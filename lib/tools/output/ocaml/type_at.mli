(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Compact primary result for [ocaml_type_at].

    Only the bounded first type-expression line is included as structured
    semantics. Remaining frames and documentation stay solely in
    {!Mentat_tool.Output.text}. *)

type t
(** The type for a non-empty first type expression and the number of additional
    frames. *)

val max_head_bytes : int
(** [max_head_bytes] is the 512-byte bound on the structured primary result. *)

val make : head:string -> more:int -> t
(** [make ~head ~more] is a type-query result.

    Raises [Invalid_argument] if [head] is empty, invalid UTF-8, contains a line
    break, exceeds {!max_head_bytes}, or [more] is negative. *)

val head : t -> string
(** [head t] is the bounded first type-expression line. *)

val more : t -> int
(** [more t] is the number of additional type frames. *)

val jsont : t Jsont.t
(** [jsont] maps type-query facts to their closed version-1 JSON shape. *)
