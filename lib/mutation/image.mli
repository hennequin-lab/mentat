(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Durable file images.

    An image is the recorded state of one path at one moment, narrowed to the
    two cases mutation evidence can carry: a successful [Mentat_edit] transition
    never carries a non-text side, so there is no unsupported arm. *)

(** The type for durable file images. *)
type t =
  | Missing  (** No file existed at the path. *)
  | Text of Mentat_digest.Content_ref.t
      (** A regular UTF-8 text file; the bytes live in the store's blob store
          keyed by the content reference. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same image. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats an image for diagnostics. *)

val jsont : t Jsont.t
(** [jsont] maps images to JSON objects by a per-arm tag, rejecting unknown tags
    and members. *)
