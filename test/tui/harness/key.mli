(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Raw terminal input byte strings for the TUI test harness.

    Values are complete chunks suitable for both the in-process input driver and
    a real PTY. Escape sequences use the seven-bit form beginning with the bytes
    [0x1B 0x5B]. *)

val enter : string
(** [enter] is the single carriage-return byte [0x0D]. *)

val linefeed : string
(** [linefeed] is the single line-feed byte [0x0A]. *)

val ctrl_c : string
(** [ctrl_c] is the single Control-C byte [0x03]. *)

val ctrl_o : string
(** [ctrl_o] is the single Control-O byte [0x0F]. *)

val ctrl_r : string
(** [ctrl_r] is the single Control-R byte [0x12]. *)

val ctrl_w : string
(** [ctrl_w] is the single Control-W byte [0x17]. *)

val escape : string
(** [escape] is the single Escape byte [0x1B]. *)

val up : string
(** [up] contains the bytes [0x1B 0x5B 0x41]. *)

val down : string
(** [down] contains the bytes [0x1B 0x5B 0x42]. *)

val left : string
(** [left] contains the bytes [0x1B 0x5B 0x44]. *)

val right : string
(** [right] contains the bytes [0x1B 0x5B 0x43]. *)

val page_up : string
(** [page_up] contains the bytes [0x1B 0x5B 0x35 0x7E]. *)

val page_down : string
(** [page_down] contains the bytes [0x1B 0x5B 0x36 0x7E]. *)

val backspace : string
(** [backspace] is the single Delete byte [0x7F] commonly sent by the Backspace
    key. It is not the Backspace control byte [0x08]. *)

val tab : string
(** [tab] is the single horizontal-tab byte [0x09]. *)

val shift_tab : string
(** [shift_tab] contains the bytes [0x1B 0x5B 0x5A]. *)

val bracketed_paste : string -> string
(** [bracketed_paste text] is the start marker [0x1B 0x5B 0x32 0x30 0x30 0x7E],
    followed by the bytes of [text] unchanged, followed by the end marker
    [0x1B 0x5B 0x32 0x30 0x31 0x7E]. *)
