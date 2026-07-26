(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = Seatbelt | Bubblewrap

let id = function
  | Seatbelt -> "macos-seatbelt"
  | Bubblewrap -> "linux-bubblewrap"

let of_id = function
  | "macos-seatbelt" -> Some Seatbelt
  | "linux-bubblewrap" -> Some Bubblewrap
  | _ -> None

let executable = function
  | Seatbelt -> "/usr/bin/sandbox-exec"
  | Bubblewrap -> "/usr/bin/bwrap"

let all = [ Seatbelt; Bubblewrap ]

let equal a b =
  match (a, b) with
  | Seatbelt, Seatbelt | Bubblewrap, Bubblewrap -> true
  | (Seatbelt | Bubblewrap), _ -> false
