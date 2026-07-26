(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Failure of the credential file machinery around [auth.json]. *)

type t =
  | Io of string  (** A read, write, rename, or mkdir of the store failed. *)
  | Decode of string
      (** [auth.json] failed its [Credential.Store.jsont] decode: the file is
          corrupt, an obsolete format, or not the credential store. The message
          is a generic diagnostic authored here (path plus a fixed reason),
          never the raw decoder output over the file's bytes, so {!message}
          carries no credential material by this library's own construction. *)
  | Locked of string
      (** The store or a credential lock could not be acquired — held by another
          process, or a permission failure on the lock file. *)

val message : t -> string
(** [message t] is a human-readable diagnostic. It carries no credential
    material. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats {!message}. *)
