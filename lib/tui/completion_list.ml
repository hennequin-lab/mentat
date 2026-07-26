(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic

type segment = { run : string; run_style : Ansi.Style.t option }
type row = { primary : segment; detail : segment option; hint : segment option }

let segment ?style text = { run = text; run_style = style }
let row ?detail ?hint primary = { primary; detail; hint }
let maximum_size = { width = pct 100; height = px 5 }
let minimum_size = { width = px 0; height = px 0 }
let natural_size = { width = pct 100; height = auto }
let cell segment = Table.cell ?style:segment.run_style segment.run

let has_details rows =
  List.exists
    (fun row -> Option.is_some row.detail || Option.is_some row.hint)
    rows

let columns ~details =
  let cursor = Table.column ~width:`Auto ~overflow:`Crop "" in
  if details then
    [
      cursor;
      Table.column ~width:`Auto ~overflow:`Crop "";
      Table.column ~width:(`Flex 1.) ~overflow:`Ellipsis "";
      Table.column ~width:`Auto ~overflow:`Crop "";
    ]
  else [ cursor; Table.column ~width:(`Flex 1.) ~overflow:`Ellipsis "" ]

let table_row ~palette ~details ~selected index row =
  let cursor =
    Table.cell
      ~style:(Theme.Palette.accent_style palette)
      (if index = selected then "❯" else "")
  in
  if details then
    [|
      cursor;
      cell row.primary;
      Option.fold ~none:(Table.cell "") ~some:cell row.detail;
      Option.fold ~none:(Table.cell "") ~some:cell row.hint;
    |]
  else [| cursor; cell row.primary |]

let view ~palette ~selected ~on_select ~on_activate rows =
  let selected =
    match rows with [] -> 0 | _ -> max 0 (min selected (List.length rows - 1))
  in
  let details = has_details rows in
  let table_rows =
    List.mapi
      (fun index row -> table_row ~palette ~details ~selected index row)
      rows
  in
  table ~key:"completion-list" ~columns:(columns ~details) ~rows:table_rows
    ~selected_row:selected ~border:false ~show_header:false
    ~show_column_separator:false ~show_row_separator:false ~cell_padding:0
    ~background:Ansi.Color.default ~text_color:Ansi.Color.default
    ~selected_background:(Theme.Palette.selection_bg palette)
    ~selected_text_color:(Theme.Palette.selection_fg palette)
    ~focused_selected_background:(Theme.Palette.selection_bg palette)
    ~focused_selected_text_color:(Theme.Palette.selection_fg palette)
    ~wrap_selection:true ~focusable:true ~autofocus:false ~flex_shrink:1.
    ~min_size:minimum_size ~size:natural_size ~max_size:maximum_size
    ~on_change:(fun index -> Some (on_select index))
    ~on_activate:(fun index -> Some (on_activate index))
    ()

let state_row style text_value =
  text ~style ~wrap:`None ~truncate:true ~flex_shrink:1. ~min_size:minimum_size
    ~size:natural_size text_value

let note ~palette text_value =
  state_row (Theme.Palette.muted_style palette) ("  " ^ text_value)

let error ~palette text_value =
  state_row (Theme.Palette.error_style palette) (Theme.problem ^ text_value)
