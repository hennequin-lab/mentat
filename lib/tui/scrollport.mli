(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Sticky transcript viewports.

    A scrollport is the TUI's single wrapper around {!Mosaic.scroll_box}. It
    follows appended transcript content while parked at the bottom, never takes
    keyboard focus, consumes wheel events only when the viewport moves, and
    shows no scrollbar. Position is communicated by the transcript itself. *)

val view :
  ?key:string ->
  ?reveal:Mosaic.Scroll_box.reveal ->
  ?scroll_by:Mosaic.Scroll_box.scroll_by ->
  ?reset_sticky:string ->
  ?on_scroll:(x:int -> y:int -> 'msg option) ->
  ?on_scroll_by_applied:(key:string -> 'msg option) ->
  ?on_reset_sticky_applied:(key:string -> 'msg option) ->
  'msg Mosaic.t list ->
  'msg Mosaic.t
(** [view children] is a vertically scrolling viewport over [children]. It grows
    and shrinks to fill the height granted by its parent and initially shows the
    bottom of overflowing content.

    Appending content keeps the viewport at the bottom until a manual scroll or
    reveal moves it away. Returning to the bottom re-engages following.

    - [key] is the Mosaic identity of the viewport. It defaults to
      ["transcript.scrollport"]. Distinct simultaneously mounted transcripts
      must use distinct keys.
    - [reveal] is a one-shot request for a content coordinate. Its request key
      must change to reapply a coordinate that has already been revealed;
      coordinates outside the content are clamped by Mosaic.
    - [scroll_by] is a keyed one-shot relative request. Viewport-unit requests
      use this scrollport's measured allocation, so callers never predict its
      height from terminal geometry.
    - [reset_sticky] is a keyed one-shot request that clears a manual scroll
      latch and resumes following the bottom edge. A stable key cannot override
      a later manual scroll.
    - [on_scroll] receives the settled horizontal and vertical offsets whenever
      either changes.
    - [on_scroll_by_applied] receives the key after a relative request is
      consumed, including when clamping leaves the position unchanged. It lets
      callers retire declarative requests before a later remount.
    - [on_reset_sticky_applied] receives the key after a sticky reset is
      consumed, including when the viewport is already at the bottom. It lets
      callers retire the request without replaying it on a later render.

    The viewport has no horizontal scrolling, scrollbar, or keyboard focus. A
    wheel event that changes its vertical offset stops there, so an ancestor
    wheel handler does not apply the same event again. Events at a scroll
    boundary, over content that does not overflow, or along the disabled
    horizontal axis bubble to ancestors. *)
