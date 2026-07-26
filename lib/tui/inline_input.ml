(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic

type t = { text : string }
type msg = Edited of string | Submitted
type outcome = Stay of t | Submit of string | Cancel

let empty = { text = "" }
let value (t : t) = t.text
let replacement = Uchar.rep

let normalize text =
  let buffer = Buffer.create (String.length text) in
  let rec loop offset =
    if offset < String.length text then begin
      let decoded = String.get_utf_8_uchar text offset in
      let length = max 1 (Uchar.utf_decode_length decoded) in
      let uchar =
        if Uchar.utf_decode_is_valid decoded then Uchar.utf_decode_uchar decoded
        else replacement
      in
      let code = Uchar.to_int uchar in
      if
        code = 0x09 || code = 0x0A || code = 0x0D || code = 0x2028
        || code = 0x2029
      then Buffer.add_char buffer ' '
      else if code < 0x20 || (code >= 0x7F && code <= 0x9F) then
        Buffer.add_utf_8_uchar buffer replacement
      else Buffer.add_utf_8_uchar buffer uchar;
      loop (offset + length)
    end
  in
  loop 0;
  Buffer.contents buffer

let of_text text = { text = normalize text }

let key event t =
  match Panel.classify event with
  | Panel.Action Panel.Escape -> Cancel
  | Panel.Printable _ | Panel.Digit _
  | Panel.Action
      ( Panel.Tab | Panel.Up | Panel.Down | Panel.Left | Panel.Right
      | Panel.Backspace | Panel.Enter | Panel.Ctrl_d | Panel.Other ) ->
      Stay t

let update message (t : t) =
  match message with
  | Edited text -> Stay { text = normalize text }
  | Submitted -> Submit (String.trim t.text)

let zero_size = { width = px 0; height = px 0 }
let hidden_overflow = { x = Overflow.Hidden; y = Overflow.Hidden }

let inset () =
  box ~flex_shrink:200. ~min_size:zero_size
    ~size:{ width = px 2; height = auto }
    []

let view ~palette (t : t) =
  box ~key:"inline-input" ~box_sizing:Box_sizing.Border_box
    ~overflow:hidden_overflow ~flex_direction:Flex_direction.Row
    ~align_items:Align.Center ~flex_shrink:0. ~min_size:zero_size ~border:true
    ~border_style:Border.single ~border_sides:[ `Top; `Bottom ]
    ~border_color:(Theme.Palette.rule palette)
    ~fill:false
    ~size:{ width = pct 100; height = auto }
    [
      inset ();
      text
        ~style:(Theme.Palette.accent_style palette)
        ~wrap:`None ~flex_shrink:0. Theme.cursor;
      Mosaic.input ~key:"inline-input.editor" ~id:"mentat-tui-inline-input"
        ~autofocus:true ~value:t.text ~max_length:max_int
        ~text_color:(Theme.Palette.accent palette)
        ~background_color:Ansi.Color.default
        ~focused_text_color:(Theme.Palette.accent palette)
        ~focused_background_color:Ansi.Color.default
        ~cursor_color:(Theme.Palette.accent palette)
        ~cursor_style:`Line
        ~on_input:(fun text -> Some (Edited text))
        ~on_submit:(Fun.const (Some Submitted))
        ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size
        ~size:{ width = auto; height = px 1 }
        ();
      inset ();
    ]
