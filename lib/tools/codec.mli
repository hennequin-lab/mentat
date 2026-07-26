(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Small JSON-building and decode-error helpers shared by every tool's input
    codec and {!Mentat_tool.Output.json} projection. *)

val decode_error : string -> 'a
(** [decode_error message] raises the [Jsont] decode error [message], for use
    inside a codec's decoding function. *)

val strict_object : kind:string -> 'a Jsont.t -> 'a Jsont.t
(** [strict_object ~kind codec] preserves the object mapping [codec] while
    rejecting repeated member names before [codec] decodes them. [codec] should
    itself reject unknown members and non-object JSON. *)

val obj : (string * Jsont.json) list -> Jsont.json
(** [obj fields] is a JSON object with [fields] in order. *)
