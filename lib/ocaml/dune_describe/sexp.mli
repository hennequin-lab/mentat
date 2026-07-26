(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The s-expression surface of [dune describe] output.

    Dune's one-shot describe commands print plain s-expressions, not the
    length-prefixed canonical form the RPC protocol speaks. This parser is
    therefore deliberately local to {!Describe} and carries no [csexp]
    dependency: it is the reason the normaliser links no Dune RPC library.

    The representation is transparent because {!Describe} pattern matches over
    it directly; there are no invariants to protect. *)

type t =
  | Atom of string
  | List of t list  (** The type for parsed s-expressions. *)

val parse : source:Error.source -> string -> (t, Error.t) result
(** [parse ~source input] is the single s-expression [input] denotes.

    [source] names the describe command in the {!Error.Parse_error} raised for
    an unterminated string or list, an unexpected [')'], a missing expression,
    or trailing input after the first complete expression. *)

val atom : t -> string option
(** [atom t] is [Some value] iff [t] is an {!Atom}. *)

val list : t -> t list option
(** [list t] is [Some values] iff [t] is a {!List}. *)

val to_string : t -> string
(** [to_string t] is [t]'s atom, or ["<list>"] when [t] is a list.

    This is a diagnostic rendering used to name an unexpected value in a parse
    error; it is deliberately lossy and never round-trips. *)
