(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic

type 'msg t = {
  label : string;
  facts : string list;
  content : 'msg Mosaic.t list;
}

let section ~label ?(facts = []) content = { label; facts; content }
let zero_size = { width = px 0; height = px 0 }
let fill_width = { width = pct 100; height = auto }
let section_gap = { width = px 0; height = px 1 }

let header ~palette section =
  let facts =
    match section.facts with
    | [] -> []
    | facts ->
        [
          text
            ~style:(Theme.Palette.faint_style palette)
            ~wrap:`None ~truncate:true ~flex_grow:1. ~flex_shrink:100.
            ~min_size:zero_size
            (Theme.separator ^ String.concat Theme.separator facts);
        ]
  in
  box ~flex_direction:Flex_direction.Row ~flex_shrink:0. ~min_size:zero_size
    ~size:{ width = pct 100; height = px 1 }
    (text
       ~style:(Theme.Palette.muted_style palette)
       ~wrap:`None ~truncate:true ~flex_shrink:1.
       ~min_size:{ width = px 1; height = px 0 }
       section.label
    :: facts)

let block ~palette section =
  box ~flex_direction:Flex_direction.Column ~flex_shrink:0. ~min_size:zero_size
    ~size:fill_width
    (header ~palette section :: section.content)

let view ~palette sections =
  let sections =
    List.filter
      (fun section -> match section.content with [] -> false | _ :: _ -> true)
      sections
  in
  match sections with
  | [] -> []
  | _ ->
      let content =
        box ~flex_direction:Flex_direction.Column ~gap:section_gap
          ~flex_shrink:0. ~min_size:zero_size ~size:fill_width
          (List.map (block ~palette) sections)
      in
      [
        scroll_box ~key:"pane.sections" ~scroll_x:false ~scroll_y:true
          ~focusable:false ~show_scrollbars:true ~flex_grow:1. ~flex_shrink:1.
          ~min_size:zero_size
          ~size:{ width = pct 100; height = px 0 }
          [ content ];
      ]
