(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Compact result location for [ocaml_find_definitions].

    Lookup misses are tool failures, so this type contains no absent arm. *)

type t
(** The type for a provider-facing path and one-based source line. *)

val make : path:string -> line:int -> t
(** [make ~path ~line] is a definition location.

    Raises [Invalid_argument] if [path] is empty or [line < 1]. *)

val path : t -> string
(** [path t] is the provider-facing definition path. *)

val line : t -> int
(** [line t] is the one-based definition line. *)

val jsont : t Jsont.t
(** [jsont] maps definition facts to their closed version-1 JSON shape. *)
