(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
module Plan = Mentat_session.Plan

type t = {
  plan : Plan.t;
  title : string;
  body : string;
  nav : Option_list.t;
  expanded : bool;
  revision : Inline_input.t option;
  flash : string option;
}

type outcome = Stay | Answer of Plan.Answer.t | Flash of string

type msg =
  | Select_choice of int
  | Activate_choice of int
  | Input_msg of Inline_input.msg

type line_breaks = Preserve | Flatten

let replacement = Uchar.rep

let add_line_break ~line_breaks buffer =
  Buffer.add_char buffer
    (match line_breaks with Preserve -> '\n' | Flatten -> ' ')

let add_uchar ~line_breaks buffer uchar =
  let code = Uchar.to_int uchar in
  if code = 0x09 then Buffer.add_char buffer ' '
  else if code = 0x0a || code = 0x0d || code = 0x2028 || code = 0x2029 then
    add_line_break ~line_breaks buffer
  else if code < 0x20 || (code >= 0x7f && code <= 0x9f) then
    Buffer.add_utf_8_uchar buffer replacement
  else Buffer.add_utf_8_uchar buffer uchar

let normalize ~line_breaks value =
  let buffer = Buffer.create (String.length value) in
  let rec loop offset =
    if offset < String.length value then
      match value.[offset] with
      | '\r' ->
          let next = offset + 1 in
          let next =
            if next < String.length value && Char.equal value.[next] '\n' then
              next + 1
            else next
          in
          add_line_break ~line_breaks buffer;
          loop next
      | '\n' ->
          add_line_break ~line_breaks buffer;
          loop (offset + 1)
      | _ ->
          let decoded = String.get_utf_8_uchar value offset in
          let length = max 1 (Uchar.utf_decode_length decoded) in
          let uchar =
            if Uchar.utf_decode_is_valid decoded then
              Uchar.utf_decode_uchar decoded
            else replacement
          in
          add_uchar ~line_breaks buffer uchar;
          loop (offset + length)
  in
  loop 0;
  Buffer.contents buffer

let normalize_lines = normalize ~line_breaks:Preserve
let normalize_one_line = normalize ~line_breaks:Flatten

let is_white_space uchar =
  match Uchar.to_int uchar with
  | 0x09 | 0x0a | 0x0b | 0x0c | 0x0d | 0x20 | 0x85 | 0xa0 | 0x1680 | 0x2028
  | 0x2029 | 0x202f | 0x205f | 0x3000 ->
      true
  | code -> code >= 0x2000 && code <= 0x200a

let without_white_space grapheme =
  let buffer = Buffer.create (String.length grapheme) in
  let rec loop offset =
    if offset < String.length grapheme then begin
      let decoded = String.get_utf_8_uchar grapheme offset in
      let uchar = Uchar.utf_decode_uchar decoded in
      if not (is_white_space uchar) then Buffer.add_utf_8_uchar buffer uchar;
      loop (offset + Uchar.utf_decode_length decoded)
    end
  in
  loop 0;
  Buffer.contents buffer

let has_visible_text value =
  let visible = ref false in
  Matrix.Text.iter_graphemes
    (fun ~offset ~len ->
      if not !visible then begin
        let grapheme = String.sub value offset len |> without_white_space in
        Matrix.Text.iter_grapheme_info ~width_method:`Unicode ~tab_width:2
          (fun ~offset:_ ~len:_ ~width -> if width > 0 then visible := true)
          grapheme
      end)
    value;
  !visible

let visible_or ~fallback value =
  if has_visible_text value then value else fallback

let choices =
  [
    (`Approve_current, "approve and continue current context");
    (`Approve_fresh, "approve and start fresh");
    (`Revise, "revise \226\128\148 tell the model what to change");
    (`Keep_planning, "keep planning");
  ]

let make plan =
  let title =
    match Plan.title plan with
    | None -> "[untitled plan]"
    | Some title ->
        title |> normalize_one_line |> visible_or ~fallback:"[blank plan title]"
  in
  let body =
    Plan.body plan |> normalize_lines
    |> visible_or ~fallback:"[blank plan body]"
  in
  {
    plan;
    title;
    body;
    nav = Option_list.make ~count:(List.length choices);
    expanded = false;
    revision = None;
    flash = None;
  }

let ctrl_o (event : Matrix.Input.Key.event) =
  let modifier = event.Matrix.Input.Key.modifier in
  modifier.Matrix.Input.Modifier.ctrl
  && (not modifier.Matrix.Input.Modifier.alt)
  && (not modifier.Matrix.Input.Modifier.super)
  &&
  match event.Matrix.Input.Key.key with
  | Matrix.Input.Key.Char uchar ->
      Uchar.equal uchar (Uchar.of_char 'o')
      || Uchar.equal uchar (Uchar.of_char 'O')
  | _ -> false

let open_revision t =
  ({ t with revision = Some Inline_input.empty; flash = None }, Stay)

let confirm t =
  match List.nth_opt choices (Option_list.selected t.nav) with
  | Some (`Approve_current, _) ->
      (t, Answer (Plan.Answer.approve ~body:t.plan ~context:`Current ()))
  | Some (`Approve_fresh, _) ->
      (t, Answer (Plan.Answer.approve ~body:t.plan ~context:`Fresh ()))
  | Some (`Revise, _) -> open_revision t
  | Some (`Keep_planning, _) -> (t, Answer Plan.Answer.keep_planning)
  | None -> (t, Stay)

let input_outcome t = function
  | Inline_input.Stay input ->
      let flash =
        if has_visible_text (Inline_input.value input) then None else t.flash
      in
      ({ t with revision = Some input; flash }, Stay)
  | Inline_input.Cancel -> ({ t with revision = None; flash = None }, Stay)
  | Inline_input.Submit feedback when not (has_visible_text feedback) ->
      let message = "describe what should change" in
      ({ t with flash = Some message }, Flash message)
  | Inline_input.Submit feedback -> (t, Answer (Plan.Answer.revise feedback))

let edit_key event input t = input_outcome t (Inline_input.key event input)

let key event t =
  let t = { t with flash = None } in
  match t.revision with
  | Some input -> edit_key event input t
  | None -> (
      if ctrl_o event then ({ t with expanded = not t.expanded }, Stay)
      else
        match Panel.classify event with
        | Panel.Digit digit ->
            ({ t with nav = Option_list.jump digit t.nav }, Stay)
        | Panel.Printable _ -> (t, Stay)
        | Panel.Action Panel.Up -> ({ t with nav = Option_list.up t.nav }, Stay)
        | Panel.Action Panel.Down ->
            ({ t with nav = Option_list.down t.nav }, Stay)
        | Panel.Action Panel.Enter -> confirm t
        | Panel.Action Panel.Escape -> (t, Answer Plan.Answer.keep_planning)
        | Panel.Action
            ( Panel.Tab | Panel.Left | Panel.Right | Panel.Backspace
            | Panel.Ctrl_d | Panel.Other ) ->
            (t, Stay))

let editing t = Option.is_some t.revision

let select_choice index t =
  let count = List.length choices in
  let index = max 0 (min (count - 1) index) in
  { t with nav = Option_list.jump (index + 1) t.nav }

let update message t =
  match message with
  | Select_choice index -> (select_choice index t, Stay)
  | Activate_choice index ->
      let t = select_choice index t in
      confirm t
  | Input_msg message -> (
      match t.revision with
      | None -> (t, Stay)
      | Some input -> input_outcome t (Inline_input.update message input))

let zero_size = { width = px 0; height = px 0 }
let full_width = { width = pct 100; height = auto }
let fill_remaining = { width = pct 100; height = px 0 }
let indent = padding_lrtb 2 2 0 0

let option_label ~selected index label =
  let cursor = if selected then Theme.cursor else "  " in
  cursor ^ string_of_int (index + 1) ^ ". " ^ label

let options_view ~palette t =
  let selected = Option_list.selected t.nav in
  let rows =
    List.mapi
      (fun index (_, label) ->
        [|
          Table.cell
            (option_label ~selected:(Int.equal selected index) index label);
        |])
      choices
  in
  let interactive = Option.is_none t.revision in
  let on_change =
    if interactive then Some (fun index -> Some (Select_choice index)) else None
  in
  let on_activate =
    if interactive then Some (fun index -> Some (Activate_choice index))
    else None
  in
  table ~key:"plan.choices"
    ~columns:
      [ Table.column ~width:(`Flex 1.) ~overflow:`Ellipsis ~min_width:1 "" ]
    ~rows
    ~selected_row:(Option_list.selected t.nav)
    ~border:false ~show_header:false ~show_column_separator:false
    ~show_row_separator:false ~cell_padding:0
    ~text_color:(Theme.Palette.muted palette)
    ~background:Ansi.Color.default
    ~selected_text_color:(Theme.Palette.selection_fg palette)
    ~selected_background:(Theme.Palette.selection_bg palette)
    ~focused_selected_text_color:(Theme.Palette.selection_fg palette)
    ~focused_selected_background:(Theme.Palette.selection_bg palette)
    ~wrap_selection:true ~focusable:interactive ~autofocus:interactive
    ~flex_shrink:1. ~min_size:zero_size ~size:full_width ?on_change ?on_activate
    ()

let body_view t =
  let max_height = if t.expanded then pct 100 else px 8 in
  scroll_box ~key:"plan.body" ~scroll_x:false ~scroll_y:true ~focusable:false
    ~flex_grow:(if t.expanded then 1. else 0.)
    ~flex_shrink:1. ~min_size:zero_size
    ~max_size:{ width = pct 100; height = max_height }
    ~size:full_width
    [ box ~padding:indent ~size:full_width [ text ~wrap:`Word t.body ] ]

let title_view ~palette t =
  box ~padding:indent ~size:full_width
    [ text ~style:(Theme.Palette.muted_style palette) ~wrap:`Word t.title ]

let issue ~palette = function
  | None -> []
  | Some message ->
      [
        box ~padding:indent ~flex_shrink:0. ~size:full_width
          [
            text
              ~style:(Theme.Palette.warning_style palette)
              ~wrap:`Word ~truncate:false message;
          ];
      ]

let hint t =
  match t.revision with
  | Some _ -> [ "type feedback"; "↵ revise"; "esc cancel" ]
  | None ->
      let disclosure = if t.expanded then "^O less" else "^O more" in
      [ "1-4 select"; "↵ choose"; disclosure; "esc keep" ]

let view ~palette (t : t) : msg Mosaic.t =
  let gap_height = if Option.is_some t.flash then px 0 else px 1 in
  let decisions =
    match t.revision with
    | None -> [ options_view ~palette t ]
    | Some input ->
        [
          Inline_input.view ~palette input
          |> Mosaic.map (fun message -> Input_msg message);
        ]
  in
  let content =
    box ~key:"plan.content" ~flex_direction:Flex_direction.Column ~flex_grow:1.
      ~flex_shrink:1. ~min_size:zero_size ~size:fill_remaining
      ~gap:{ width = px 0; height = gap_height }
      ([ title_view ~palette t; body_view t ]
      @ issue ~palette t.flash @ decisions)
  in
  Panel.view ~palette
    ~frame:(Theme.Palette.mode_plan palette)
    ~name:"plan" ~filter:"" ~hint:(hint t) ~content
