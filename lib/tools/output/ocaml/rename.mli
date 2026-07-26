(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Compact result facts for [ocaml_rename].

    Names, source positions, target paths, occurrence spans, content identities,
    diffs, and mutation evidence remain outside durable semantic output. They
    are represented in authoritative text, the private prepared plan, or
    mutation facts according to their owner. *)

(** Whether the rename was applied or only previewed. *)
type disposition =
  | Applied  (** The prepared rename was committed. *)
  | Previewed  (** The rename was validated without modifying files. *)

type t
(** The type for one rename result. Counts are positive and the number of files
    cannot exceed the number of occurrences. *)

val make : disposition:disposition -> occurrences:int -> files:int -> t
(** [make ~disposition ~occurrences ~files] is a compact rename result.

    Raises [Invalid_argument] if either count is not positive or [files] exceeds
    [occurrences]. *)

val disposition : t -> disposition
(** [disposition t] is whether the rename was applied or previewed. *)

val occurrences : t -> int
(** [occurrences t] is the number of deduplicated identifier occurrences. *)

val files : t -> int
(** [files t] is the number of files containing those occurrences. *)

val jsont : t Jsont.t
(** [jsont] maps rename facts to their closed version-1 JSON shape. *)
