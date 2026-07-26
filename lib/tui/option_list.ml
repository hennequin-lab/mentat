(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
open Prims

type t = { selected : int; count : int }

let make ~count =
  if count <= 0 then invalid_arg "Option_list.make: count must be positive";
  { selected = 0; count }

let selected t = t.selected
let up t = { t with selected = (t.selected - 1 + t.count) mod t.count }
let down t = { t with selected = (t.selected + 1) mod t.count }
let wrap ~count index delta = (((index + delta) mod count) + count) mod count

let jump number t =
  if number >= 1 && number <= t.count then { t with selected = number - 1 }
  else t

type checkbox = No_box | Checked | Unchecked

let row ~palette ~selected ?(checkbox = No_box) ~number ~label () =
  let selection_style =
    if selected then Theme.Palette.accent_style palette
    else Theme.Palette.muted_style palette
  in
  let cursor = if selected then Theme.cursor else "  " in
  let checkbox_segments =
    match checkbox with
    | No_box -> []
    | Checked -> [ seg selection_style "[x] " ]
    | Unchecked -> [ seg (Theme.Palette.muted_style palette) "[ ] " ]
  in
  box ~flex_direction:Flex_direction.Row ~flex_shrink:0.
    ((seg selection_style cursor :: checkbox_segments)
    @ [ seg selection_style (string_of_int number ^ ". "); label ])
