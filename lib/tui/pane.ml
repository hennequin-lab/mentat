(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic

let zero_size = { width = px 0; height = px 0 }
let fill_region = { width = pct 100; height = px 0 }
let hidden_overflow = { x = Overflow.Hidden; y = Overflow.Hidden }

let transcript ~width ~min_width ~tail left =
  box ~key:"pane.transcript" ~flex_direction:Flex_direction.Column ~flex_grow:1.
    ~flex_shrink:1.
    ~min_size:{ zero_size with width = min_width }
    ~size:{ width; height = pct 100 }
    (left :: tail)

let wide ~palette ~left ~tail ~activity =
  (* A zero main-axis flex basis (a [flex: 1 1 0] column) is what keeps the
     activity column static at a fixed viewport width. With an [auto] basis the
     transcript claims its content's max-content width, and because both columns
     shrink, a long unbreakable transcript line drags the bounded activity
     column below its own width. A zero basis makes the transcript grow purely
     into the width the activity column leaves, so transcript content can never
     move or narrow the pane; its 80-cell minimum still floors the column. *)
  let transcript = transcript ~width:(px 0) ~min_width:(px 80) ~tail left in
  let children =
    match activity with
    | [] -> [ transcript ]
    | _ :: _ ->
        let activity =
          box ~key:"pane.activity.wide" ~flex_direction:Flex_direction.Column
            ~box_sizing:Box_sizing.Border_box ~overflow:hidden_overflow
            ~border:true ~border_sides:[ `Left ]
            ~border_color:(Theme.Palette.rule palette)
            ~padding:(padding_lrtb 1 0 0 0) ~flex_shrink:1.
            ~min_size:{ width = px 30; height = px 0 }
            ~max_size:{ width = px 45; height = auto }
            ~size:{ width = pct 33; height = pct 100 }
            activity
        in
        [ transcript; activity ]
  in
  box ~key:"pane.frame" ~flex_direction:Flex_direction.Row ~flex_grow:1.
    ~flex_shrink:1. ~min_size:zero_size ~size:fill_region children

let narrow ~left ~tail ~activity =
  box ~key:"pane.frame" ~flex_direction:Flex_direction.Column ~flex_grow:1.
    ~flex_shrink:1. ~min_size:zero_size ~size:fill_region
    (transcript ~width:auto ~min_width:(px 0) ~tail left :: activity)

let frame ?(tail = []) ~palette ~left ~wide_activity ~narrow_activity () =
  viewport_switch ~at_least_width:110
    ~wide:(wide ~palette ~left ~tail ~activity:wide_activity)
    ~narrow:(narrow ~left ~tail ~activity:narrow_activity)
