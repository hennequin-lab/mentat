(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Cursor navigation and row rendering for numbered decision choices.

    Decision dialogs construct a cursor with {!make}, update it with {!up},
    {!down}, or {!jump}, and use {!row} to keep the visible cursor, option
    number, and optional checkbox aligned. *)

(** {1:navigation Navigation} *)

type t
(** The type for a cursor over a non-empty, fixed-size option list. The selected
    index is always within the list. *)

val make : count:int -> t
(** [make ~count] is a cursor over [count] options with the first option
    selected.

    Raises [Invalid_argument] if [count <= 0]. *)

val selected : t -> int
(** [selected cursor] is the zero-based index of its selected option. *)

val up : t -> t
(** [up cursor] selects the preceding option, wrapping from the first option to
    the last. *)

val down : t -> t
(** [down cursor] selects the following option, wrapping from the last option to
    the first. *)

val jump : int -> t -> t
(** [jump number cursor] selects the one-based option [number]. It is [cursor]
    unchanged when [number] is outside the option list. *)

val wrap : count:int -> int -> int -> int
(** [wrap ~count index delta] steps [index] by [delta] positions within a list
    of [count] rows, wrapping past either end round to the other. It is the raw
    modular arithmetic {!up} and {!down} apply, exposed for a caller that tracks
    a bare selection index rather than a {!t}.

    Raises [Division_by_zero] if [count] is zero, so a caller guards the empty
    list itself. *)

(** {1:rows Rows} *)

(** The checkbox displayed between a row's selection cursor and number. *)
type checkbox =
  | No_box  (** No checkbox column. *)
  | Checked  (** A checked box, ["[x] "]. *)
  | Unchecked  (** An unchecked box, ["[ ] "]. *)

val row :
  palette:Theme.Palette.t ->
  selected:bool ->
  ?checkbox:checkbox ->
  number:int ->
  label:'msg Mosaic.t ->
  unit ->
  'msg Mosaic.t
(** [row ~selected ?checkbox ~number ~label ()] is a non-shrinking horizontal
    option row containing, from left to right:

    - {!Theme.cursor} when [selected] and two spaces otherwise;
    - ["[x] "] or ["[ ] "] when [checkbox] is {!Checked} or {!Unchecked};
    - the one-based [number] followed by [". "];
    - the caller-styled [label].

    The cursor and number use {!Theme.accent} when selected and {!Theme.muted}
    otherwise. A checked box follows the same selection style; an unchecked box
    is always muted. [checkbox] defaults to {!No_box}. *)
