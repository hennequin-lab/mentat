(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Brand lockups for the home stage and transcript.

    {!home} renders the shell's current snapshot, whose model and effort follow
    the last started turn. {!record} renders the immutable snapshot captured
    when that transcript was opened. Layout is intrinsic: Mosaic truncates text
    at grapheme boundaries, shrinks rows to the space offered by their parent,
    and wraps the transcript's lockup and facts when they no longer fit side by
    side. *)

val home :
  palette:Theme.Palette.t -> Snapshot.t -> rows:string list -> 'msg Mosaic.t
(** [home snapshot ~rows] is the centered home-stage brand: the two animated
    lockup [rows] in {!Theme.accent}, followed after one row of vertical gap by
    [snapshot.version], {!Theme.separator}, and {!Snapshot.model_line}. The cwd
    is omitted because the home footer owns it. Text truncates within the width
    offered by the stage.

    Raises [Invalid_argument] if [rows] does not contain exactly two rows. *)

val record : palette:Theme.Palette.t -> Snapshot.t -> 'msg Mosaic.t
(** [record snapshot] is the immutable opening record at the head of a
    transcript. The static {!Theme.lockup} and a two-row facts column share a
    row while their intrinsic sizes fit and wrap naturally otherwise. The facts
    are [snapshot.version] joined to {!Snapshot.model_line} by
    {!Theme.separator} and the home-relative cwd. The launch sandbox posture is
    an ambient status carried by the footer, not an identity fact of this
    record.

    The record has one cell of padding on every side. Non-wrapping text shrinks
    and truncates within the width offered by the transcript. *)
