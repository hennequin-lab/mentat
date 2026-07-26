(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Raw terminal input bytes shared by the in-process and PTY test drivers. *)

let enter = "\r"
let linefeed = "\n"
let ctrl_c = "\003"
let ctrl_o = "\015"
let ctrl_r = "\018"
let ctrl_w = "\023"
let escape = "\027"
let up = "\027[A"
let down = "\027[B"
let left = "\027[D"
let right = "\027[C"
let page_up = "\027[5~"
let page_down = "\027[6~"
let backspace = "\127"
let tab = "\t"
let shift_tab = "\027[Z"
let bracketed_paste text = "\027[200~" ^ text ^ "\027[201~"
