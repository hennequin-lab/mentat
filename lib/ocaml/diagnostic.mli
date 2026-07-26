(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Diagnostics reported by OCaml tools and the build system.

    A {!type:t} carries a message, a {!Source.t}, and a {!Severity.t}, and may
    add a {!Location.t}, a code, {!Tag.t} markers, and {!Related.t} information.
*)

module Severity : sig
  (** Diagnostic severity levels, ordered from [Error] (most urgent) to [Hint]
      (least urgent). *)

  type t =
    | Error  (** An error. *)
    | Warning  (** A warning. *)
    | Information  (** Informational feedback. *)
    | Hint  (** A hint or suggestion. *)

  val compare : t -> t -> int
  (** [compare a b] orders severities from most to least urgent: [Error] <
      [Warning] < [Information] < [Hint]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same severity. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] as its lowercase name, one of ["error"],
      ["warning"], ["information"], or ["hint"]. *)
end

module Source : sig
  (** Producers of diagnostics. *)

  type t = private
    | Dune
    | Merlin
    | Compiler
    | Ocamlformat
    | Odoc
    | Other of string
        (** The producer of a diagnostic. [Other label] is for integrations that
            are not part of the core vocabulary.

            [label] must be non-empty, must not collide with a built-in source,
            and must use lowercase ASCII words separated by hyphens. Construct
            other sources with {!other}; direct construction is intentionally
            unavailable so those invariants cannot be bypassed. *)

  val dune : t
  (** [dune] is the source for diagnostics produced by Dune. *)

  val merlin : t
  (** [merlin] is the source for diagnostics produced by Merlin. *)

  val compiler : t
  (** [compiler] is the source for diagnostics produced by the OCaml compiler.
  *)

  val ocamlformat : t
  (** [ocamlformat] is the source for diagnostics produced by ocamlformat. *)

  val odoc : t
  (** [odoc] is the source for diagnostics produced by odoc. *)

  val other : string -> t
  (** [other label] is [Other label].

      Raises [Invalid_argument] if [label] is empty, malformed, or collides with
      a built-in source such as ["dune"] or ["merlin"]. *)

  val to_string : t -> string
  (** [to_string t] is [t]'s label: the lowercase producer name for a built-in
      source, or [label] for [Other label]. *)

  val compare : t -> t -> int
  (** [compare a b] orders sources with the built-in producers first in a fixed
      order ([Dune], [Merlin], [Compiler], [Ocamlformat], [Odoc]) and [Other]
      sources last, ties broken by label. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same source. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] as its label (see {!to_string}). *)
end

module Tag : sig
  (** Diagnostic tags describing how a span should be treated. *)

  type t =
    | Unnecessary  (** Marks unused or unreachable code. *)
    | Deprecated  (** Marks use of a deprecated construct. *)

  val compare : t -> t -> int
  (** [compare a b] orders tags with [Unnecessary] before [Deprecated]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same tag. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] as ["unnecessary"] or ["deprecated"]. *)
end

module Related : sig
  (** Secondary locations and messages attached to a diagnostic. *)

  type t
  (** The type for related diagnostic information: a message and an optional
      {!Location.t}. *)

  val make : ?location:Location.t -> string -> t
  (** [make ?location message] is related diagnostic information.

      Raises [Invalid_argument] if [message] is empty. *)

  val message : t -> string
  (** [message t] is [t]'s message. *)

  val location : t -> Location.t option
  (** [location t] is [t]'s location, or [None] when the producer gave no
      precise location. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] as ["location: message"], or as just the message
      when [t] has no location. *)
end

type t
(** A compiler, build, or OCaml tooling diagnostic.

    Diagnostics may be attached to a location, or to a source with only a
    message when the producer did not report a precise file range. *)

val make :
  ?location:Location.t ->
  ?code:string ->
  ?tags:Tag.t list ->
  ?related:Related.t list ->
  source:Source.t ->
  severity:Severity.t ->
  string ->
  t
(** [make ... message] is a diagnostic.

    Raises [Invalid_argument] if [message] is empty, [code] is empty when
    present, or [tags] contains duplicates. *)

val message : t -> string
(** [message t] is [t]'s human-readable message. *)

val source : t -> Source.t
(** [source t] is the producer of [t]. *)

val severity : t -> Severity.t
(** [severity t] is [t]'s severity. *)

val location : t -> Location.t option
(** [location t] is [t]'s location, or [None] when the producer reported no
    precise file range. *)

val code : t -> string option
(** [code t] is [t]'s diagnostic code, or [None] when the producer reported
    none. *)

val tags : t -> Tag.t list
(** [tags t] is [t]'s tags. *)

val related : t -> Related.t list
(** [related t] is [t]'s related information. *)

val compare : t -> t -> int
(** [compare a b] is a total order on diagnostics. It compares by location, then
    source, severity, code, message, tags, and related information. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are equal in all fields. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] for users as ["source[severity]: message"], prefixed
    with ["location: "] when [t] has a location. *)
