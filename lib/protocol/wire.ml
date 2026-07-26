(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* The protocol envelope version: every serialized wire value
   carries a top-level [v]; decoders accept [v = 1] and reject anything else
   loudly, naming what they saw. *)

let version = 1

let check v =
  if not (Int.equal v version) then
    Import.decode_error
      (Printf.sprintf "unsupported protocol version %d (expected %d)" v version)

let mem map = Jsont.Object.mem "v" Jsont.int ~enc:(fun _ -> version) map
