(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic

type row =
  | Main
  | Thread of {
      glyph : string;
      style : Ansi.Style.t;
      kind : string;
      task : string;
      facts : string list;
      depth : int;
    }

type placement = Below_transcript | Agents_pane

type msg =
  | Select_index of int
  | Activate_index of int
  | Hover_index of int option

let main_glyph = "◯"
let enter_hint = "enter to open"
let glance_rows = 4
let browser_rows = 10
let zero_size = { width = px 0; height = px 0 }
let fill_width = { width = pct 100; height = auto }

(* The selection marker is the accent cursor. The table's inter-column gap
   supplies the separation that {!Theme.cursor}'s trailing space gives inline
   callers, so the marker is carried without it; every row still reserves the
   one column the cursor occupies, so the glyph column never shifts between the
   unfocused glance and the focused browser. *)
let cursor_mark = String.trim Theme.cursor
let cursor selected = if selected then cursor_mark else ""

let open_hint ~placement ~selected ~can_open ~hint =
  match placement with
  | Agents_pane when selected && can_open -> hint
  | Below_transcript | Agents_pane -> ""

(* A child delegated by another delegated child nests one level per delegation
   hop. [main] and its direct children stay flush at depth 0; a nested child
   marks its task with a branch connector, indented a further two columns for
   each level below the first, so the switcher reads as a tree without a
   dedicated indent column stealing width from every flush row. *)
let tree_prefix depth =
  if depth <= 0 then "" else String.make (2 * (depth - 1)) ' ' ^ "└ "

let content_cells ~placement ~style ~kind ~task =
  match placement with
  | Below_transcript -> [ Table.cell ~style kind; Table.cell ~style task ]
  | Agents_pane -> [ Table.cell ~style task ]

(* The trailing column carries the row's status facts, except that a selected,
   openable row shows the activation hint in their place. The agents pane is too
   narrow to seat the task label, its facts, and the hint at once; a focused
   row's glyph already states its status, so ceding the facts to the hint keeps
   the label readable and still names the one action [Enter] performs. The
   inline {!Below_transcript} placement never yields a hint, so it always shows
   the facts. *)
let trailing_cell ~palette ~placement ~selected ~can_open ~hint ~facts =
  match open_hint ~placement ~selected ~can_open ~hint with
  | "" ->
      Table.cell
        ~style:(Theme.Palette.muted_style palette)
        (String.concat Theme.separator facts)
  | hint -> Table.cell ~style:(Theme.Palette.faint_style palette) hint

let row_cells ~palette ~placement ~selected ~hovered ~can_open ~hint = function
  | Main ->
      let style =
        if selected || hovered then Theme.Palette.accent_style palette
        else Theme.Palette.muted_style palette
      in
      let kind, task =
        match placement with
        | Below_transcript -> ("main", "")
        | Agents_pane -> ("", "main")
      in
      Array.of_list
        ([
           Table.cell
             ~style:(Theme.Palette.accent_style palette)
             (cursor selected);
           Table.cell ~style main_glyph;
         ]
        @ content_cells ~placement ~style ~kind ~task
        @ [
            trailing_cell ~palette ~placement ~selected ~can_open ~hint
              ~facts:[];
          ])
  | Thread { glyph; style; kind; task; facts; depth } ->
      let body_style =
        if selected || hovered then Theme.Palette.accent_style palette
        else Theme.Palette.muted_style palette
      in
      let task = tree_prefix depth ^ task in
      Array.of_list
        ([
           Table.cell
             ~style:(Theme.Palette.accent_style palette)
             (cursor selected);
           Table.cell ~style glyph;
         ]
        @ content_cells ~placement ~style:body_style ~kind ~task
        @ [ trailing_cell ~palette ~placement ~selected ~can_open ~hint ~facts ]
        )

(* Every row reserves the same two leading columns for the cursor and glyph, so
   [main]'s glyph aligns with each thread's; nesting rides the task cell instead.
   The task claims the flexible middle with a readable floor so a long trailing
   fact or hint can never collapse the label to a sliver. A single trailing
   column carries the facts, or the hint on the selected row. *)
let columns placement =
  [
    Table.column ~width:`Auto ~overflow:`Crop ~min_width:1 "";
    Table.column ~width:`Auto ~overflow:`Crop "";
  ]
  @ (match placement with
    | Below_transcript ->
        [ Table.column ~width:`Auto ~overflow:`Ellipsis ~max_width:10 "" ]
    | Agents_pane -> [])
  @ [
      Table.column ~width:(`Flex 1.) ~overflow:`Ellipsis ~min_width:12 "";
      Table.column ~width:`Auto ~overflow:`Ellipsis "";
    ]

let overflow_row ~palette hidden =
  box ~key:"threads.strip.overflow" ~flex_direction:Flex_direction.Row
    ~justify_content:Justify.End ~flex_shrink:0. ~min_size:zero_size
    ~size:{ width = pct 100; height = px 1 }
    [
      text
        ~style:(Theme.Palette.faint_style palette)
        ~wrap:`None
        (Printf.sprintf "↓ %d more" hidden);
    ]

let view ~palette ~placement ?(can_open = true) ?(activate_hint = enter_hint)
    ?(hovered = None) ~rows ~selected () =
  match rows with
  | [] -> []
  | _ ->
      let count = List.length rows in
      let selected_index =
        match selected with
        | None -> 0
        | Some index -> max 0 (min (count - 1) index)
      in
      let selected = Option.map (Fun.const selected_index) selected in
      let focused = Option.is_some selected in
      let max_rows = if focused then browser_rows else glance_rows in
      let table_rows =
        List.mapi
          (fun index row ->
            row_cells ~palette ~placement ~selected:(selected = Some index)
              ~hovered:(hovered = Some index) ~can_open ~hint:activate_hint row)
          rows
      in
      let rows_table =
        table ~key:"threads.strip.table" ~columns:(columns placement)
          ~rows:table_rows ~selected_row:selected_index ~border:false
          ~show_header:false ~show_column_separator:false
          ~show_row_separator:false ~cell_padding:0
          ~text_color:Ansi.Color.default ~background:Ansi.Color.default
          ~selected_text_color:(Theme.Palette.selection_fg palette)
          ~focused_selected_text_color:(Theme.Palette.selection_fg palette)
          ~selected_background:(Theme.Palette.selection_bg palette)
          ~focused_selected_background:(Theme.Palette.selection_bg palette)
          ~wrap_selection:true ~selection_visible:focused
          ~show_scroll_indicator:focused ~activate_on_click:true
          ~wheel_navigation:false ~flex_grow:1. ~flex_shrink:1.
          ~min_size:zero_size
          ~size:{ width = px 0; height = auto }
          ~max_size:{ width = pct 100; height = px max_rows }
          ~on_change:(fun index -> Some (Select_index index))
          ~on_activate:(fun index -> Some (Activate_index index))
          ~on_hover:(fun index -> Some (Hover_index index))
          ()
      in
      (* The unfocused glance is a static top-of-list overview, so a muted
         "↓ N more" names the rows the cap hides beneath it. The focused browser
         scrolls to keep the selected row visible and shows the table's own
         indicator instead, which stays honest as the viewport moves. *)
      let overflow =
        if (not focused) && count > max_rows then
          [ overflow_row ~palette (count - max_rows) ]
        else []
      in
      (* The table grows across a [Row] parent to claim the strip's width, so it
         keeps that parent; the overflow line stacks beneath both in an outer
         [Column]. *)
      let strip =
        box ~key:"threads.strip.rows" ~flex_direction:Flex_direction.Row
          ~flex_shrink:1. ~min_size:zero_size ~size:fill_width [ rows_table ]
      in
      [
        box ~key:"threads.strip" ~flex_direction:Flex_direction.Column
          ~flex_shrink:1. ~min_size:zero_size ~size:fill_width
          (strip :: overflow);
      ]
