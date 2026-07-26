(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Durable paths within a workspace root identity.

    A path pairs a stable {!Root.Key.t} with a normalized {!Lpath.Rel.t}.
    Because a relative path cannot begin with [..], the value proves lexical
    containment within the root identity it names. It does not carry the root's
    current absolute directory or prove that the key is admitted by a particular
    workspace.

    This is the common path representation for tools, permissions, edits,
    mutation records, and persistence. Decode paths with {!jsont}; bind them to
    the current filesystem only through {!Mentat_workspace.to_abs}. *)

type t
(** The type for durable workspace paths. The representation is abstract so
    callers depend on the root-key-relative contract rather than its storage
    layout. *)

(** {1:constructors Constructors} *)

val make : root_key:Root.Key.t -> Lpath.Rel.t -> t
(** [make ~root_key rel] addresses [rel] within [root_key]. Both arguments are
    already validated typed syntax; the root's current absolute directory is
    deliberately not part of the value. *)

val root_of : t -> t
(** [root_of path] is the root path of [path]'s root: {!make} applied to
    [path]'s {!root_key} and the empty relative path {!Lpath.Rel.root}, so
    [is_root (root_of path)] always holds. In a multi-root workspace
    [equal (root_of a) (root_of b)] holds exactly when [a] and [b] carry the
    same {!root_key} — [root_of] collapses each path onto the base of the root
    that contains it. *)

(** {1:queries Queries} *)

val root_key : t -> Root.Key.t
(** [root_key path] is the stable identity of the root containing [path]. *)

val rel : t -> Lpath.Rel.t
(** [rel path] is [path]'s normalized root-relative component. *)

val is_root : t -> bool
(** [is_root path] is [true] iff [path] addresses its root. *)

val basename : t -> string option
(** [basename path] is [Some name] when [path] is not a root path, and [None]
    otherwise. *)

val parent : t -> t option
(** [parent path] is [Some parent] with the final component removed when [path]
    is not a root path, and [None] otherwise. *)

(** {1:composing Composing} *)

val add_component : t -> string -> (t, Lpath.Error.t) result
(** [add_component path component] appends [component] below [path]. It is
    [Error (Malformed_component component)] if [component] is empty, [.], [..],
    contains a path separator or NUL, or uses absolute-path syntax. *)

val append : t -> Lpath.Rel.t -> t
(** [append path suffix] appends [suffix] while preserving [path]'s root key. *)

(** {1:containment Containment} *)

val relativize : root:t -> t -> Lpath.Rel.t option
(** [relativize ~root path] is [Some suffix] iff [path] is [root] or lies below
    it and both carry the same root key. In the success case,
    [append root suffix] equals [path]; equal paths return
    [Some Lpath.Rel.root]. *)

val is_within : root:t -> t -> bool
(** [is_within ~root path] is [true] iff {!relativize} returns [Some _]. *)

(** {1:formatting Formatting} *)

val display : t -> string
(** [display path] is its root-relative text. Multi-root disambiguation belongs
    to the caller. The result is display text, not stable serialization. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf path] formats the root key and relative path for diagnostics. *)

(** {1:comparison Comparison and collections} *)

val equal : t -> t -> bool
(** [equal a b] compares root key and relative path. *)

val compare : t -> t -> int
(** [compare a b] is a total order compatible with {!equal}. *)

module Set : Set.S with type elt = t
(** Sets of workspace paths ordered by {!compare}. *)

module Map : Map.S with type key = t
(** Maps keyed by workspace paths ordered by {!compare}. *)

(** {1:codec Codec} *)

val jsont : t Jsont.t
(** [jsont] encodes the strict JSON object with string members ["root"] and
    ["rel"]. Decoding rejects missing or unknown members, a root key violating
    {!Root.Key}'s text invariant, and malformed, absolute, or escaping relative
    paths. Decoding does not prove that the root key is admitted by a live
    workspace. *)
