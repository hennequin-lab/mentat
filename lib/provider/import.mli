(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** House helpers — the [lib/*/import.mli] decode pair plus the one map
    combinator [Jsont] does not provide.

    Not part of the public {!Mentat_provider} surface. {!decode_invalid_arg}
    turns a raising smart constructor into a {!Jsont} decoder that re-validates
    its invariants: a decoded value is exactly as checked as a constructed one.

    Everything else this library's codecs need — typed members, unknown-member
    rejection, tag dispatch — is a {!Jsont.Object} combinator. Only
    {!strict_string_map} has no counterpart there: [Jsont] resolves a repeated
    member name last-wins, and the account store must not silently drop one of
    two entries claiming the same provider or credential name. *)

val decode_error : string -> 'a
(** [decode_error msg] raises the {!Jsont} decoding error carrying [msg] with no
    source location. *)

val decode_invalid_arg : (unit -> 'a) -> 'a
(** [decode_invalid_arg f] is [f ()], turning an [Invalid_argument] raised by a
    smart constructor into a {!decode_error} so decoding re-validates every
    construction invariant. *)

val strict_string_map : kind:string -> 'a Jsont.t -> (string * 'a) list Jsont.t
(** [strict_string_map ~kind codec] maps a JSON object of uniform values to its
    members, each value typed by [codec]. Decoding orders the result by member
    name and errors on a repeated name; encoding emits the members in the order
    given. [kind] names the decoded entity in diagnostics.

    This is {!Jsont.Object.as_string_map} over an association list, with the
    repeated-name error that map cannot report: it collects into a map, so a
    repeat overwrites rather than fails. *)
