(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Credential resolution errors.

    These errors describe why a selected credential candidate cannot satisfy a
    provider declaration. They contain credential provenance and kind, never
    credential material. {!Declaration.resolve_credential} is their effect-free
    producer; account discovery carries them without erasing healthy provider
    routes. *)

type t =
  | Unsupported_credential_kind of {
      source : Credential.Source.t;
          (** Provenance of the rejected higher-precedence candidate. *)
      kind : Credential.Kind.t;  (** Kind supplied by that candidate. *)
      accepted : Credential.Kind.t list;
          (** Kinds accepted by the provider declaration, in declaration order.
          *)
    }
      (** A higher-precedence candidate has a kind the provider does not accept.
          Resolution errors rather than falling through to a lower-precedence
          candidate. *)
  | Invalid_credential_text of {
      source : Credential.Source.t;
          (** Provenance of the rejected textual candidate. *)
      kind : Credential.Kind.t;  (** Kind the candidate was declared to carry. *)
    }
      (** A selected textual candidate is not valid UTF-8. The error carries no
          credential material. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] describe the same rejection. *)

val message : t -> string
(** [message e] is a human-readable diagnostic. It is not stable syntax; use
    {!jsont} for storage or transport. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf e] formats {!message}. *)

val jsont : t Jsont.t
(** [jsont] maps errors to strict tagged JSON objects. Decoding rejects unknown
    tags and members, and enforces the invariants of {!Credential.Source.jsont}.
*)
