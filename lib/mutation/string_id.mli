(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Shared implementation of the string-backed identifier modules — keep
    byte-identical across lib/*/string_id copies.

    Every identifier in this library is an opaque, non-empty string. This
    functor factors that discipline: {!Make} produces a fresh abstract id type
    with the standard constructor, conversion, comparison, and codec surface.
    Each instantiation is generative, so distinct id modules never unify. *)

module type S = sig
  type t
  (** The type for a stable identifier. Invariant: its textual form is
      non-empty. *)

  val of_string : string -> t
  (** [of_string s] is [s] as an id. Raises [Invalid_argument] if [s] is empty.
  *)

  val to_string : t -> string
  (** [to_string id] is [id]'s stable string representation. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same id. *)

  val compare : t -> t -> int
  (** [compare a b] orders ids by their stable string representations,
      compatibly with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an id for diagnostics. The output is not stable storage
      syntax. *)

  val jsont : t Jsont.t
  (** [jsont] maps ids to JSON strings, validating the non-empty invariant of
      {!of_string} on decode. *)
end

module Make
    (_ : sig
      val module_path : string
      (** Fully qualified module path used in [Invalid_argument] messages, e.g.
          ["<Facade>.<Kind>.Id"]. *)

      val kind : string
      (** JSON codec kind, e.g. ["<kind> id"]. *)
    end)
    () : S

val derive_id : domain:string -> string list -> string
(** [derive_id ~domain parts] is the content-derived identifier for [parts]
    under the versioned [domain] — the full lowercase hex SHA-256, so a crash
    re-drive recomputes the same id from the same stable inputs. *)
