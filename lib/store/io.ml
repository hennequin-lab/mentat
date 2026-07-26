(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type op = Open | Read | Write | Sync | Rename | Lock
type cause = Unix_error of Unix.error | Message of string
type t = { op : op; path : string; cause : cause }

let op_label = function
  | Open -> "open"
  | Read -> "read"
  | Write -> "write"
  | Sync -> "sync"
  | Rename -> "rename"
  | Lock -> "lock"

let cause_label = function
  | Unix_error code -> Unix.error_message code
  | Message message -> message

let message t =
  t.path ^ ": " ^ op_label t.op ^ " failed: " ^ cause_label t.cause

let pp ppf t = Format.pp_print_string ppf (message t)
