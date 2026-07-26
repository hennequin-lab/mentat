(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Session identifiers.

    A session id names one durable session document. Root ids are minted by the
    host and carry no host, pid, or path; a child session's id is minted by its
    parent and stored in the {!Delegation} edge that created it. This module
    validates only the local identifier shape; store uniqueness and path mapping
    belong to the session store. *)

type t
(** The type for stable session identifiers.

    Invariant: an identifier's stable textual form is non-empty. *)

val of_string : string -> t
(** [of_string s] is [s] as a session id.

    Raises [Invalid_argument] if [s] is empty. *)

val to_string : t -> string
(** [to_string id] is [id]'s stable string representation. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same session id. *)

val compare : t -> t -> int
(** [compare a b] orders ids by their stable string representations, compatibly
    with {!equal}. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats an id for diagnostics. The output is not stable storage syntax.
*)

val jsont : t Jsont.t
(** [jsont] maps session ids to JSON strings, validating the non-empty invariant
    of {!of_string} on decode. *)
