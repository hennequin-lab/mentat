(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The decode-side bridge from a smart constructor's [Invalid_argument] to a
    [Jsont] decode error, shared by every tool codec that validates a value it
    has just decoded. *)

val decode_invalid_arg : (unit -> 'a) -> 'a
(** [decode_invalid_arg f] runs [f], turning an [Invalid_argument] it raises
    into a [Jsont] decode error — so an input smart constructor's invariant
    becomes a decode failure rather than an exception that escapes the codec. *)
