(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Compact project-structure counts for [ocaml_dune_describe].

    Component identities, paths, dependency edges, compilation units, test
    details, and build-context detail are omitted from durable JSON and
    represented only in {!Mentat_tool.Output.text}. *)

type t
(** The type for normalized Dune component and test counts. Both counts are
    non-negative. *)

val make : components:int -> tests:int -> t
(** [make ~components ~tests] is a project description containing [components]
    normalized components and [tests] described tests.

    Raises [Invalid_argument] if either count is negative. *)

val components : t -> int
(** [components t] is the total normalized component count, including local and
    external components. *)

val tests : t -> int
(** [tests t] is the described Dune test count. *)

val jsont : t Jsont.t
(** [jsont] maps project counts to their closed version-1 JSON shape. *)
