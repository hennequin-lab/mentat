(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Shared text classification and UTF-8 helpers for model-visible tool text.

    These operate on already-decoded strings: they detect likely-binary content,
    strip a trailing carriage return, find the longest valid UTF-8 prefix within
    a byte budget, repair arbitrary bytes to UTF-8, and count logical lines.
    Tools share them so binary detection, line-ending handling, and
    model-visible UTF-8 repair stay identical across the suite. *)

val looks_binary : string -> bool
(** [looks_binary text] is [true] iff [text] contains a NUL byte or its control
    bytes exceed one tenth of its length — the cheap heuristic the file tools
    use to refuse binary content. *)

val strip_trailing_cr : string -> string
(** [strip_trailing_cr text] drops a single trailing ['\r'] from [text]. *)

val valid_utf8_prefix : string -> int -> string
(** [valid_utf8_prefix text max_bytes] is the longest prefix of [text] that is
    at most [max_bytes] bytes and is valid UTF-8. *)

val utf8_lossy : string -> string
(** [utf8_lossy text] is [text] when [text] is valid UTF-8. Otherwise it
    replaces each malformed UTF-8 sequence with [U+FFFD], preserving every valid
    sequence and all surrounding ASCII bytes. *)

val strip_ansi : string -> string
(** [strip_ansi text] removes ECMA-48 control-sequence-introducer escapes and
    operating-system-command escapes from [text]. OSC accepts either BEL or the
    string-terminator sequence ESC followed by a backslash. An incomplete CSI or
    OSC at the end is removed. Other bytes, including other escape forms, are
    preserved. Repair arbitrary bytes with {!utf8_lossy} before calling this
    function. *)

val scrub_controls : string -> string
(** [scrub_controls text] replaces control bytes in [text] for safe display: a
    tab or newline is kept, a carriage return becomes a newline, and every other
    C0 control byte or DEL (U+007F) becomes a space. All other bytes are
    unchanged. Repair arbitrary bytes with {!utf8_lossy} before calling this
    function. *)

val bounded_diagnostic : max_bytes:int -> string -> string
(** [bounded_diagnostic ~max_bytes text] repairs [text] to UTF-8 with
    {!utf8_lossy}, strips ANSI escapes with {!strip_ansi}, and trims surrounding
    whitespace, then clips the result to the longest valid-UTF-8 prefix of at
    most [max_bytes] bytes. It is the shared bound a tool applies to a captured
    diagnostic string before it reaches model-visible text. *)

val logical_line_count : string -> int
(** [logical_line_count text] is the number of logical lines in [text]: the
    newline count, plus one when [text] is non-empty and does not end in ['\n'].
*)
