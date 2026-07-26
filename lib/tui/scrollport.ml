(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic

let view ?(key = "transcript.scrollport") ?reveal ?scroll_by ?reset_sticky
    ?on_scroll ?on_scroll_by_applied ?on_reset_sticky_applied children =
  scroll_box ~key ~scroll_y:true ~sticky_scroll:true
    ~sticky_start:`Bottom
      (* Transcript position is felt through its content, never shown with a
       scrollbar. Scrolling remains enabled when the bar is hidden. *)
    ~show_scrollbars:false ~focusable:false ?reveal ?scroll_by ?reset_sticky
    ?on_scroll ?on_scroll_by_applied ?on_reset_sticky_applied ~flex_grow:1.
    ~flex_shrink:1.
    ~size:{ width = pct 100; height = px 0 }
    children
