(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
module Preset = Theme.Preset

(* The panel browses a pre-resolved catalog of themes — the built-in presets
   merged with the user's theme files, resolved once by the executable. It holds
   no palette itself: moving the cursor emits a {!Preview} the shell applies to
   the whole TUI, cancelling emits {!Close} for the shell to restore the palette
   it saved when the panel opened, and confirming emits {!Commit} with the chosen
   name for the shell to persist. The panel performs no I/O. *)

type t = {
  presets : Preset.t list;
  input : string;
  selected : int; (* An index into the filtered rows, not the full catalog. *)
  current : string;
}

type msg = Key of Panel.key | Select_index of int | Activate_index of int

type event =
  | Stay
  | Preview of Theme.Palette.t
  | Commit of { name : string }
  | Close

let drop_last_grapheme text =
  let last = ref None in
  Matrix.Text.iter_graphemes
    (fun ~offset ~len ->
      if offset + len <= String.length text then last := Some offset)
    text;
  match !last with None -> text | Some offset -> String.sub text 0 offset

let matches ~input (preset : Preset.t) =
  let needle = String.trim input |> String.lowercase_ascii in
  String.is_empty needle
  || String.includes ~affix:needle (String.lowercase_ascii preset.Preset.name)

let visible_rows t = List.filter (matches ~input:t.input) t.presets

let index_of_name name presets =
  let rec loop i = function
    | [] -> 0
    | (preset : Preset.t) :: rest ->
        if String.equal preset.Preset.name name then i else loop (i + 1) rest
  in
  loop 0 presets

let make ~presets ~current =
  { presets; input = ""; selected = index_of_name current presets; current }

let selected_preset t = List.nth_opt (visible_rows t) t.selected

let clamp t =
  let last = List.length (visible_rows t) - 1 in
  { t with selected = (if last < 0 then 0 else max 0 (min last t.selected)) }

(* Every cursor move live-previews the row's palette, so the whole TUI recolours
   under the list. A filtered-away or empty list previews nothing. *)
let preview_of t =
  match selected_preset t with
  | Some preset -> Preview preset.Preset.palette
  | None -> Stay

let select_index index t =
  let t = clamp { t with selected = index } in
  (t, preview_of t)

let append text t =
  let text = Prims.normalize_inline text in
  let t = clamp { t with input = t.input ^ text; selected = 0 } in
  (t, preview_of t)

let paste = append

let backspace t =
  let t = clamp { t with input = drop_last_grapheme t.input; selected = 0 } in
  (t, preview_of t)

let commit t =
  match selected_preset t with
  | Some preset -> (t, Commit { name = preset.Preset.name })
  | None -> (t, Stay)

let key event =
  match Panel.classify event with
  | Panel.Action Panel.Other -> None
  | classified -> Some (Key classified)

let update message t =
  match message with
  | Select_index index -> select_index index t
  | Activate_index index -> commit (clamp { t with selected = index })
  | Key (Panel.Action Panel.Escape) -> (t, Close)
  | Key (Panel.Action Panel.Enter) -> commit t
  | Key (Panel.Action Panel.Backspace) -> backspace t
  | Key (Panel.Printable text) -> append text t
  | Key (Panel.Digit digit) -> append (string_of_int digit) t
  | Key
      (Panel.Action
         ( Panel.Tab | Panel.Up | Panel.Down | Panel.Left | Panel.Right
         | Panel.Ctrl_d | Panel.Other )) ->
      (t, Stay)

(* View. *)

let minimum_size = { width = px 0; height = px 0 }
let fill_remaining = { width = pct 100; height = px 0 }

let appearance_label = function
  | Theme.Palette.Dark -> "dark"
  | Theme.Palette.Light -> "light"

(* The picker: a selection cursor, the theme name, its dark/light tag, and the
   current theme's trailing ✓. Selection is marker and accent only — never a
   fill — routed through {!Theme.Palette.selection_fg}/[_bg], matching the model
   and sessions pickers. *)
let table ~palette t rows =
  let selected =
    if List.is_empty rows then 0 else min t.selected (List.length rows - 1)
  in
  let columns =
    [
      Table.column ~width:`Auto ~overflow:`Crop ~min_width:1 "";
      Table.column ~width:(`Flex 1.) ~overflow:`Ellipsis ~min_width:1 "theme";
      Table.column ~width:`Auto ~overflow:`Crop ~min_width:1 "appearance";
      Table.column ~width:`Auto ~alignment:`Right ~overflow:`Crop "";
    ]
  in
  let table_rows =
    List.mapi
      (fun index (preset : Preset.t) ->
        let is_current = String.equal preset.Preset.name t.current in
        [|
          Table.cell
            ~style:(Theme.Palette.accent_style palette)
            (if index = selected then "❯" else "");
          Table.cell (Prims.normalize_inline preset.Preset.name);
          Table.cell
            ~style:(Theme.Palette.muted_style palette)
            (appearance_label (Theme.Palette.appearance preset.Preset.palette));
          Table.cell
            ~style:(Theme.Palette.success_style palette)
            (if is_current then "✓" else "");
        |])
      rows
  in
  table ~key:"theme.catalog.table" ~columns ~rows:table_rows
    ~selected_row:selected ~border:false ~show_header:false
    ~show_column_separator:false ~show_row_separator:false ~cell_padding:1
    ~background:Ansi.Color.default ~text_color:Ansi.Color.default
    ~selected_background:(Theme.Palette.selection_bg palette)
    ~selected_text_color:(Theme.Palette.selection_fg palette)
    ~focused_selected_background:(Theme.Palette.selection_bg palette)
    ~focused_selected_text_color:(Theme.Palette.selection_fg palette)
    ~wrap_selection:true ~show_scroll_indicator:true ~focusable:true
    ~autofocus:true ~flex_grow:1. ~flex_shrink:1. ~min_size:minimum_size
    ~size:fill_remaining ~activate_on_click:true
    ~on_change:(fun index -> Some (Select_index index))
    ~on_activate:(fun index -> Some (Activate_index index))
    ()

let empty_content ~palette =
  scroll_box ~key:"theme.catalog.empty" ~scroll_x:false ~scroll_y:true
    ~focusable:false ~flex_grow:1. ~flex_shrink:1. ~min_size:minimum_size
    ~size:{ width = pct 100; height = pct 100 }
    [
      text
        ~style:(Theme.Palette.muted_style palette)
        ~wrap:`Word ~selectable:false ~flex_shrink:0.
        ~size:{ width = pct 100; height = auto }
        "No theme matches the filter.";
    ]

let content ~palette t =
  let rows = visible_rows t in
  let summary =
    if String.is_empty t.input then
      Printf.sprintf "%d themes" (List.length t.presets)
    else
      Printf.sprintf "%d of %d themes" (List.length rows)
        (List.length t.presets)
  in
  box ~key:"theme.content" ~flex_direction:Flex_direction.Column ~flex_grow:1.
    ~flex_shrink:1. ~min_size:minimum_size
    ~size:{ width = pct 100; height = px 0 }
    ~gap:{ width = px 0; height = px 1 }
    [
      text
        ~style:(Theme.Palette.muted_style palette)
        ~wrap:`None ~truncate:true ~flex_shrink:0. summary;
      (if List.is_empty rows then empty_content ~palette
       else table ~palette t rows);
    ]

let hints t =
  (if List.is_empty (visible_rows t) then [] else [ "↑↓ preview" ])
  @ [ "↵ apply"; "type filter"; "esc cancel" ]

let view ~palette ~frame t =
  Panel.view ~palette ~frame ~name:"theme"
    ~filter:(Prims.normalize_inline t.input)
    ~hint:(hints t) ~content:(content ~palette t)
