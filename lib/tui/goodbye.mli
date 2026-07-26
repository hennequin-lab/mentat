(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Restored-terminal farewell frames.

    The executable writes a rendered frame to stdout after the alternate screen
    has been restored, so the farewell remains visible in the user's normal
    terminal history. *)

val render :
  palette:Theme.Palette.t ->
  color:bool ->
  session:Mentat_session.Id.t option ->
  string
(** [render ~color ~session] is the two-row brand lockup followed, when
    [session] is [Some id], by a muted [mentat resume ID] continuation hint.
    [None] renders the lockup alone for an exit before a session exists.

    [color] applies {!Theme.accent} to each lockup row and {!Theme.muted} to the
    continuation hint. [false] emits no ANSI control sequences. The result has
    one leading blank row, one trailing blank row, and, when a hint is present,
    one blank row between the lockup and the hint; it is ready to write verbatim
    to stdout. *)
