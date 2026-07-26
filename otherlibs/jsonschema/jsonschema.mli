(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** A validated JSON Schema subset and the conformance check against it.

    A {!t} is a JSON Schema restricted to the supported subset and the
    conformance check of an instance against it. It is constructed only through
    {!of_json}, which rejects any schema outside the subset, so a [t] is always
    something {!validate} can enforce completely — a value that {!validate}
    accepts conforms to the whole schema, never a silently-dropped constraint.

    The supported keywords, with arbitrary nesting, are [type] (["object"],
    ["array"], ["string"], ["number"], ["integer"], ["boolean"], ["null"], or an
    array of these), [properties], [required], [additionalProperties] (a
    boolean), [items] (a single schema), [enum], and [const]. The
    pure-annotation keywords [$schema], [$id], [title], [description],
    [$comment], [default], [examples], [readOnly], [writeOnly], and [deprecated]
    are accepted and ignored: they carry no validation semantics, so ignoring
    them drops no conformance check. Every other keyword — [$ref]/[$defs],
    [oneOf]/[anyOf]/[allOf]/[not], [patternProperties], [pattern], [format],
    numeric and length bounds — is out of subset and rejected by {!of_json}.

    The module is pure: no I/O, no network, and both {!of_json} and {!validate}
    are total over their inputs. *)

type t
(** The type for a JSON Schema restricted to the supported subset. Constructed
    only through {!of_json}. *)

(** {1:errors Load-time errors} *)

module Error : sig
  (** Why a schema is not a supported subset schema. *)

  type t =
    | Unsupported_keyword of { keyword : string; path : string }
        (** A keyword outside the supported subset. [path] is the JSON Pointer
            location of the schema object carrying it ([""] at the root). *)
    | Malformed of { path : string; expected : string }
        (** The schema is not well-formed for a supported keyword — [required]
            not an array of strings, [type] naming an unknown type,
            [additionalProperties] not a boolean, a non-object subschema.
            [expected] names what was required at [path]. *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic for [e]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an error for diagnostics. *)
end

val of_json : Jsont.json -> (t, Error.t) result
(** [of_json j] is the schema [j] denotes, or an {!Error.t} if [j] uses an
    unsupported keyword or is malformed. The check is a subset membership test:
    it validates the {e schema}, not any instance. Pure and total. *)

(** {1:violations Conformance} *)

module Violation : sig
  (** One conformance failure of an instance against a schema. *)

  type t = {
    path : string;
        (** The JSON Pointer location in the instance ([""] is the whole value).
        *)
    message : string;  (** Human-readable detail. *)
  }

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a violation as [path: message]. *)
end

val validate : t -> Jsont.json -> (unit, Violation.t list) result
(** [validate t v] is [Ok ()] iff [v] conforms to [t]; otherwise the ordered
    non-empty list of {!Violation.t}s, in instance-traversal order. Pure and
    total. *)
