(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** House helpers — keep byte-identical across lib/*/import.mli copies. *)

val invalid_arg' : string -> string -> string -> 'a
(** [invalid_arg' module_path fn message] raises [Invalid_argument] with a
    ["module_path.fn: message"] diagnostic. *)

val decode_error : string -> 'a
(** [decode_error message] raises a {!Jsont} decode error with [message]. *)

val decode_invalid_arg : (unit -> 'a) -> 'a
(** [decode_invalid_arg f] runs [f], turning an [Invalid_argument] raised by a
    smart constructor into a {!Jsont} decode error so JSON decoding reports the
    same local-shape failures as direct construction. *)
