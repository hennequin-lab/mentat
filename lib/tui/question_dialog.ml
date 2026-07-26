(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
module Question = Mentat_session.Question

type t = {
  prompt : string;
  choices : string list;
  nav : Option_list.t;
  input : Inline_input.t option;
  flash : string option;
}

type outcome = Stay | Answer of Question.Answer.t | Flash of string

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

let normalize ~line_breaks text =
  let buffer = Buffer.create (String.length text) in
  let rec loop offset =
    if offset < String.length text then
      match text.[offset] with
      | '\r' ->
          let next = offset + 1 in
          let next =
            if next < String.length text && Char.equal text.[next] '\n' then
              next + 1
            else next
          in
          add_line_break ~line_breaks buffer;
          loop next
      | '\n' ->
          add_line_break ~line_breaks buffer;
          loop (offset + 1)
      | _ ->
          let decoded = String.get_utf_8_uchar text offset in
          let uchar = Uchar.utf_decode_uchar decoded in
          add_uchar ~line_breaks buffer uchar;
          loop (offset + Uchar.utf_decode_length decoded)
  in
  loop 0;
  Buffer.contents buffer

let normalize_prompt = normalize ~line_breaks:Preserve
let normalize_one_line = normalize ~line_breaks:Flatten

let occupied_columns text =
  Matrix.Text.measure ~width_method:`Unicode ~tab_width:2 text

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

let has_visible_text text =
  let visible = ref false in
  (try
     Matrix.Text.iter_graphemes
       (fun ~offset ~len ->
         let grapheme = String.sub text offset len in
         if occupied_columns (without_white_space grapheme) > 0 then begin
           visible := true;
           raise Exit
         end)
       text
   with Exit -> ());
  !visible

let visible_or ~fallback text = if has_visible_text text then text else fallback

let make question =
  let prompt =
    Question.prompt question |> normalize_prompt
    |> visible_or ~fallback:"[blank prompt]"
  in
  let choices =
    Question.choices question |> Option.value ~default:[]
    |> List.map normalize_one_line
    |> List.map (visible_or ~fallback:"[blank choice]")
  in
  {
    prompt;
    choices;
    nav = Option_list.make ~count:(List.length choices + 1);
    input = None;
    flash = None;
  }

let choice_count t = List.length t.choices
let custom_index t = choice_count t

let open_editor t =
  ({ t with input = Some Inline_input.empty; flash = None }, Stay)

let confirm t index =
  if index = custom_index t then open_editor t
  else (t, Answer (Question.Answer.choice index))

let input_outcome t = function
  | Inline_input.Stay input ->
      ({ t with input = Some input; flash = None }, Stay)
  | Inline_input.Cancel -> ({ t with input = None; flash = None }, Stay)
  | Inline_input.Submit text when not (has_visible_text text) ->
      let message = "type an answer" in
      ({ t with flash = Some message }, Flash message)
  | Inline_input.Submit text -> (t, Answer (Question.Answer.free text))

let edit_key event input t = input_outcome t (Inline_input.key event input)

let key event t =
  let t = { t with flash = None } in
  match t.input with
  | Some input -> edit_key event input t
  | None -> (
      match Panel.classify event with
      | Panel.Digit digit
        when digit >= 1 && digit <= 9 && digit <= choice_count t + 1 ->
          ({ t with nav = Option_list.jump digit t.nav }, Stay)
      | Panel.Digit _ | Panel.Printable _ -> (t, Stay)
      | Panel.Action Panel.Up -> ({ t with nav = Option_list.up t.nav }, Stay)
      | Panel.Action Panel.Down ->
          ({ t with nav = Option_list.down t.nav }, Stay)
      | Panel.Action Panel.Enter -> confirm t (Option_list.selected t.nav)
      | Panel.Action Panel.Escape -> open_editor t
      | Panel.Action
          ( Panel.Tab | Panel.Left | Panel.Right | Panel.Backspace
          | Panel.Ctrl_d | Panel.Other ) ->
          (t, Stay))

let editing t = Option.is_some t.input

let select_choice index t =
  let count = choice_count t + 1 in
  let index = max 0 (min (count - 1) index) in
  { t with nav = Option_list.jump (index + 1) t.nav }

let update message t =
  match message with
  | Select_choice index -> (select_choice index t, Stay)
  | Activate_choice index ->
      let t = select_choice index t in
      confirm t (Option_list.selected t.nav)
  | Input_msg message -> (
      match t.input with
      | None -> (t, Stay)
      | Some input -> input_outcome t (Inline_input.update message input))

let zero_size = { width = px 0; height = px 0 }
let full_width = { width = pct 100; height = auto }
let fill_remaining = { width = pct 100; height = px 0 }

let choice_label ~selected index choice =
  (if selected then "❯ " else "  ") ^ string_of_int (index + 1) ^ ". " ^ choice

let choice_table ~palette t =
  let interactive = Option.is_none t.input in
  let selected = Option_list.selected t.nav in
  let label index choice =
    choice_label ~selected:(interactive && index = selected) index choice
  in
  let rows =
    List.mapi
      (fun index choice -> [| Table.cell (label index choice) |])
      t.choices
    @ [
        [|
          Table.cell
            (label (custom_index t)
               (Theme.own_answer ^ " type your own answer"));
        |];
      ]
  in
  let on_change =
    if interactive then Some (fun index -> Some (Select_choice index)) else None
  in
  let on_activate =
    if interactive then Some (fun index -> Some (Activate_choice index))
    else None
  in
  table ~key:"question.choices"
    ~columns:
      [ Table.column ~width:(`Flex 1.) ~overflow:`Ellipsis ~min_width:1 "" ]
    ~rows ~selected_row:selected ~border:false ~show_header:false
    ~show_column_separator:false ~show_row_separator:false ~cell_padding:0
    ~text_color:(Theme.Palette.muted palette)
    ~background:Ansi.Color.default
    ~selected_text_color:(Theme.Palette.selection_fg palette)
    ~selected_background:(Theme.Palette.selection_bg palette)
    ~focused_selected_text_color:(Theme.Palette.selection_fg palette)
    ~focused_selected_background:(Theme.Palette.selection_bg palette)
    ~wrap_selection:true ~focusable:interactive ~autofocus:interactive
    ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size
    ~max_size:{ width = pct 100; height = px 9 }
    ~size:fill_remaining ?on_change ?on_activate ()

let prompt t =
  scroll_box ~key:"question.prompt" ~scroll_x:false ~scroll_y:true
    ~focusable:false ~flex_shrink:1. ~min_size:zero_size
    ~max_size:{ width = pct 100; height = pct 35 }
    ~size:full_width
    [
      box ~padding:(padding_lrtb 2 2 0 0) ~size:full_width
        [ text ~wrap:`Word ~truncate:false t.prompt ];
    ]

let issue ~palette = function
  | None -> []
  | Some message ->
      [
        box ~padding:(padding_lrtb 2 2 0 0) ~size:full_width
          [
            text
              ~style:(Theme.Palette.warning_style palette)
              ~wrap:`Word ~truncate:false message;
          ];
      ]

let hint t =
  if Option.is_some t.input then
    [ "type answer"; "enter submit"; "esc cancel"; "paste works" ]
  else if choice_count t = 0 then [ "enter type answer"; "esc type answer" ]
  else [ "1-9 jump"; "↑↓ move"; "enter answer"; "esc custom" ]

let view ~palette (t : t) : msg Mosaic.t =
  let interaction =
    match t.input with
    | None -> [ choice_table ~palette t ]
    | Some input ->
        [
          Inline_input.view ~palette input
          |> Mosaic.map (fun message -> Input_msg message);
        ]
  in
  let content =
    box ~key:"question.content" ~flex_direction:Flex_direction.Column
      ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size ~size:fill_remaining
      ~gap:{ width = px 0; height = px 1 }
      ([ prompt t ] @ interaction @ issue ~palette t.flash)
  in
  Panel.view ~palette
    ~frame:(Theme.Palette.accent palette)
    ~name:"question" ~filter:"" ~hint:(hint t) ~content
