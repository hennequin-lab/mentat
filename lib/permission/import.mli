(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Library-private construction and decoding helpers shared by every module.

    Not part of the public {!Mentat_permission} surface. The two decode helpers
    turn a raising smart constructor into a {!Jsont} decoder that re-validates
    its invariants: a decoded value is exactly as checked as a constructed one.
*)

val invalid_arg' : string -> string -> string -> 'a
(** [invalid_arg' m fn msg] raises [Invalid_argument] with message
    ["m.fn: msg"], the qualified constructor location and reason. *)

val decode_error : string -> 'a
(** [decode_error msg] raises the {!Jsont} decoding error carrying [msg] with no
    source location. *)

val decode_invalid_arg : (unit -> 'a) -> 'a
(** [decode_invalid_arg f] is [f ()], turning an [Invalid_argument] raised by a
    smart constructor into a {!decode_error} so decoding re-validates every
    construction invariant. *)
