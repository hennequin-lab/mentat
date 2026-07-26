(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Lexical workspace boundaries and their durable identities.

    A root pairs a stable {!Key.t} with a normalized logical absolute directory.
    It does not inspect the filesystem, resolve symlinks, prove that the
    directory exists, or grant access. Writable versus read-only is an admission
    property assigned by {!Mentat_workspace.make}, not a property of the root
    itself.

    Root identity is byte-lexical and case-sensitive. Two roots are equal when
    both key and directory are equal; {!same_key} compares durable identity
    alone. Filesystem canonicalization belongs to the host layer. *)

module Key : sig
  (** Stable workspace-root identities.

      Durable addresses store keys rather than absolute directories so the same
      identity can be rebound after workspace relocation. Keys are non-empty
      valid UTF-8 text without ASCII control characters. Equality and ordering
      are byte-lexical and case-sensitive; no Unicode normalization or naming
      scheme is imposed. *)

  type t
  (** The type for JSON-safe workspace-root keys. *)

  (** The type for key validation failures. *)
  type error =
    | Empty  (** An empty key was supplied. *)
    | Invalid_utf_8  (** The supplied bytes are not valid UTF-8. *)
    | Control_character of { byte_index : int; byte : int }
        (** An ASCII control byte was found. [byte_index] is its zero-based byte
            index and [byte] is its value in [0; 255]. *)

  val of_string : string -> (t, error) result
  (** [of_string s] is [Ok key] preserving [s] when it is non-empty valid UTF-8
      without bytes [0x00; 0x1F] or [0x7F], and [Error error] describing the
      first violated invariant otherwise. *)

  val of_string_exn : string -> t
  (** [of_string_exn s] is [s] as a key.

      Raises [Invalid_argument] if [s] violates the key invariant. Use this
      constructor for trusted literals and fixtures; use {!of_string} at input
      boundaries. *)

  val to_string : t -> string
  (** [to_string key] is the original stable key string. *)

  val message : error -> string
  (** [message error] is a human-readable diagnostic for [error]. The text is
      for display, not programmatic matching. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same key. *)

  val compare : t -> t -> int
  (** [compare a b] orders keys by their bytes. The order is compatible with
      {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf key] formats {!to_string}. *)

  val pp_error : Format.formatter -> error -> unit
  (** [pp_error ppf error] formats [error] for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] encodes a key as a JSON string. Decoding revalidates every key
      invariant. *)
end

type t
(** The type for workspace roots. A value pairs a stable key with a normalized
    logical absolute directory; it is a lexical boundary, not a filesystem
    capability. *)

(** {1:constructors Constructors} *)

val make : key:Key.t -> Lpath.Abs.t -> t
(** [make ~key dir] binds stable identity [key] to logical directory [dir]. No
    filesystem access is performed. A host that later admits the same [key] at
    another directory rebinds paths carrying that identity. *)

val of_dir : Lpath.Abs.t -> t
(** [of_dir dir] is a location-bound root whose key is derived injectively from
    the normalized bytes of [dir]. Printable ASCII bytes except [%] are kept;
    all other bytes and [%] are encoded as uppercase [%HH].

    The derived key is JSON-safe but changes when [dir] changes. Use {!make}
    with a host-owned key when paths must survive relocation. *)

(** {1:queries Queries} *)

val dir : t -> Lpath.Abs.t
(** [dir root] is [root]'s logical absolute directory. *)

val key : t -> Key.t
(** [key root] is [root]'s durable identity. It is suitable for
    equality-sensitive, persistent, and identity-qualified diagnostic uses, but
    it is not the root's current filesystem location. *)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have equal keys and directories. *)

val same_key : t -> t -> bool
(** [same_key a b] is [true] iff [a] and [b] have the same durable identity,
    regardless of their logical directories. Workspace construction uses this
    predicate to detect conflicting admissions. *)

val compare : t -> t -> int
(** [compare a b] orders roots by key and then logical directory. The order is
    compatible with {!equal}. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf root] formats [root]'s logical directory. *)
