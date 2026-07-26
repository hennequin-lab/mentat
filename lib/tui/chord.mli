(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Key-chord algebra: presses, parsing, printing, and conflict detection.

    A press is a set of modifiers and a key; a chord is a sequence of one or two
    presses matched in order. This leaf module carries the press/chord algebra
    that {!Command} projects onto the keymap. It holds no registry data and
    reads no application state. *)

(** {1:presses Presses} *)

type press = {
  ctrl : bool;
  alt : bool;
  shift : bool;
  super : bool;
  key : Matrix.Input.Key.t;
}
(** One key press: a modifier set matched exactly against a decoded event
    ([ctrl], [alt], [shift], [super]) and a key. Meta, hyper, and the lock
    toggles are ignored, since a terminal's Alt already sets meta. *)

val press_equal : press -> press -> bool
(** [press_equal a b] is [true] iff [a] and [b] have the same modifiers and key.
*)

val press_matches : press -> Matrix.Input.Key.event -> bool
(** [press_matches p event] is [true] iff [event]'s key and its
    ctrl/alt/shift/super modifiers equal [p]'s. *)

(** {1:chords Chords} *)

type t
(** A chord of one or two presses matched in order. Two presses model gestures
    like [Ctrl+X Ctrl+E]. *)

val of_string : string -> (t, string) result
(** [of_string s] parses a chord written as space-separated presses, each a
    [+]-joined list of modifiers ([ctrl], [shift], [alt], [cmd]/[super])
    followed by a key token (a single printable character or a named key such as
    [escape], [tab], [pageup], [pagedown], [enter], [space], the arrows,
    [f1]..[f35]). Case-insensitive. [Error msg] names the offending token; the
    empty chord and chords longer than two presses are rejected. *)

val to_string : t -> string
(** [to_string c] is the canonical spelling {!of_string} accepts back. *)

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit

val presses : t -> press list
(** [presses c] is [c]'s one or two presses, in order. *)

val conflict : t -> t -> bool
(** [conflict a b] is [true] iff one chord is a prefix of the other: an equal
    pair, or a single-press chord shadowing a two-press chord's first press
    (which would fire before the chord could complete). [ctrl+x] conflicts with
    [ctrl+x ctrl+e]; [ctrl+x ctrl+e] does not conflict with [ctrl+x ctrl+r]. *)
