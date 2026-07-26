(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* House helpers — the codec-free subset of lib/*/import.ml. This library
   carries no jsont dependency, so the decode helpers the other copies define
   have nothing to raise here. *)

let invalid_arg' m fn msg = invalid_arg (m ^ "." ^ fn ^ ": " ^ msg)

let require_non_empty m fn field = function
  | "" -> invalid_arg' m fn (field ^ " must not be empty")
  | _ -> ()
