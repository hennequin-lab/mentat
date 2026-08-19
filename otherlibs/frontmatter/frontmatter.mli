(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Markdown frontmatter.

    Frontmatter is a YAML header fenced by [---] lines at the very start of a
    markdown document. {!parse} splits a document into header fields and the
    body; accessors answer string-field questions without exposing YAML values.

    The header must be empty or a YAML mapping. Mapping values may use the full
    YAML language so documents written for other agents parse rather than error;
    Mentat itself only reads YAML string nodes. A non-string value under a key
    is visible in {!keys} but reads as [None] from {!string}; callers
    distinguish absent from present-but-not-a-string with {!field}.

    Parsing splits bytes and never normalizes the body: the body is the exact
    byte suffix following the closing fence line.

    This is the single parser shared by the build-time builtin-skill validator
    ([prompts/gen]) and the runtime instruction and skill loaders, so a document
    that passes the build passes at runtime and vice versa. *)

(** {1:errors Errors} *)

module Error : sig
  (** Frontmatter parse errors. *)

  type t =
    | Unterminated  (** An opening fence without a closing [---] line. *)
    | Invalid_yaml of string
        (** The fenced text is not valid YAML; carries the parser message. *)
    | Not_a_mapping
        (** The fenced YAML is valid but its top level is not a mapping. *)

  val message : t -> string
  (** [message error] is a one-line human-readable description of [error]. The
      wording is for diagnostics and is not a stable programmatic interface. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf error] formats [error] for diagnostics. *)
end

(** {1:parsing Parsing} *)

type t
(** The type for parsed documents: header fields and the exact body bytes. *)

val parse : string -> (t, Error.t) result
(** [parse doc] splits [doc] into frontmatter fields and body.

    A document whose first line is exactly [---] opens a fence, and the header
    ends at the next line that is exactly [---]. A trailing carriage return is
    accepted on either fence line to support CRLF input. Leading spaces,
    trailing spaces, indentation, or additional dashes do not form a fence.

    The text between the fences is parsed as YAML and must be a mapping or
    empty. Invalid YAML errors with [Invalid_yaml message]; valid non-mapping
    YAML errors with [Not_a_mapping]. A document that does not start with an
    exact opening fence has no fields and is all body.

    An unquoted colon inside a plain value is invalid YAML yet common in
    headers written for lenient parsers. A header that fails to parse is
    retried once with every unindented plain [key: value] line whose value
    embeds a colon read as if the value were single-quoted, so
    [description: Build for AWS: ECS] reads as the full string; a header that
    still fails reports its original error.

    The body is the byte suffix following the closing fence line and its
    newline, if present, unchanged: no trimming, no line-ending normalization.
*)

(** {1:accessors Accessors} *)

val body : t -> string
(** [body t] is the exact body bytes of the parsed document. *)

val keys : t -> string list
(** [keys t] is the header's mapping keys in document order, duplicates
    preserved. *)

val string : string -> t -> string option
(** [string key t] is the YAML string node bound to [key]'s first occurrence.

    Plain unquoted YAML strings and quoted YAML strings are returned as
    [Some value]. YAML-resolved booleans, numbers, nulls, lists, mappings, and
    aliases return [None], as does an absent [key]. Use {!field} to tell an
    absent [key] apart from a present non-string value. *)

val field : string -> t -> [ `Absent | `Not_string | `String of string ]
(** [field key t] classifies [key]'s first mapping entry into three arms:
    [`Absent] when no entry binds [key]; [`String value] when its value is a
    plain or quoted YAML string; and [`Not_string] when [key] is bound to a
    value that is not a string — a boolean, number, null, list, mapping, or
    alias. {!string} maps both the [`Absent] and [`Not_string] arms to [None]
    and so cannot tell an unbound [key] from one bound to a non-string value;
    [field] keeps them apart, letting a caller reject a present-but-non-string
    value rather than silently read it as absent.

    Duplicate keys are not reported here: a later entry for [key] is ignored,
    and duplication is observable only through {!keys}. *)
