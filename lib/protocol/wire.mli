(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The shared wire-envelope version. *)

val version : int
(** [version] is the protocol envelope version: [1]. *)

val check : int -> unit
(** [check v] raises a {!Jsont} decode error naming [v] unless [v] is
    {!version}. *)

val mem : ('o, int -> 'a) Jsont.Object.map -> ('o, 'a) Jsont.Object.map
(** [mem map] adds the required integer ["v"] member to [map], encoding
    {!version}. Pair it with a decode function that calls {!check}. *)
