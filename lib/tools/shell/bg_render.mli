(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Shared rendering for the background shell tools ({!Shell_output} and
    {!Shell_kill}): status wording, tail capping, UTF-8/ANSI/filter rendering of
    a captured stream, and the two shared result-text fragments. *)

module Session = Mentat_workspace_io.Command.Session

val status_line : Session.status -> string
(** [status_line status] is the human status: ["running"], ["exited N"],
    ["signaled N"] (the child exited on a signal from elsewhere), or
    ["terminated"] (we killed it). *)

val status_keyword : Session.status -> string
(** [status_keyword status] is the compact keyword for durable JSON:
    ["running"], ["exited"], ["signaled"], or ["terminated"]. *)

val cap_tail : max_bytes:int -> string -> string * int
(** [cap_tail ~max_bytes raw] keeps the last [max_bytes] bytes of [raw] — the
    most recent, log reading order — and returns them with the count of older
    bytes dropped ([0] when [raw] already fits). *)

val render_stream : ?filter:Re.re -> max_bytes:int -> string -> string * int
(** [render_stream ?filter ~max_bytes raw] caps [raw] to its [max_bytes] tail,
    repairs invalid UTF-8 to [U+FFFD], strips ANSI escapes, and — when [filter]
    is given — keeps only matching rendered lines. Returns the rendered text and
    the byte count the tail cap dropped. A multibyte UTF-8 sequence split across
    the cap renders as two [U+FFFD], an accepted cosmetic cost of byte-exact
    cursors. *)

val dropped_note : int -> string
(** [dropped_note n] is a leading
    ["... n bytes rolled off before this read ...\n"] note when [n > 0], and
    empty otherwise. *)

val not_found_message : string -> string
(** [not_found_message handle] is the model-visible explanation for an unknown
    handle — never started in this session, or minted by a prior engine that did
    not survive a restart. *)
