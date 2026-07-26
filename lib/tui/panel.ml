(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
open Prims

let zero_size = { width = px 0; height = px 0 }
let fill_remaining = { width = pct 100; height = px 0 }
let hidden_overflow = { x = Overflow.Hidden; y = Overflow.Hidden }

let panel_boundary =
  let decoded = String.get_utf_8_uchar Theme.panel_boundary 0 in
  if
    (not (Uchar.utf_decode_is_valid decoded))
    || Uchar.utf_decode_length decoded <> String.length Theme.panel_boundary
  then invalid_arg "Theme.panel_boundary must be one Unicode scalar";
  Uchar.utf_decode_uchar decoded

let panel_border = Border.modify ~horizontal:panel_boundary Border.empty

let boundary ~frame =
  (* The boundary is chrome in the same column as the other pinned rows. Its
     border box therefore participates in flex layout instead of insetting the
     flexible panel and displacing the terminal-edge hint. *)
  box ~key:"panel.boundary" ~box_sizing:Box_sizing.Border_box
    ~overflow:hidden_overflow ~flex_shrink:0. ~border:true
    ~border_style:panel_border ~border_sides:[ `Top ] ~border_color:frame
    ~fill:false
    ~size:{ width = pct 100; height = px 1 }
    []

type action =
  | Enter
  | Escape
  | Tab
  | Left
  | Right
  | Up
  | Down
  | Backspace
  | Ctrl_d
  | Other

type key = Printable of string | Digit of int | Action of action

let utf8_of_uchar u =
  let buffer = Buffer.create 4 in
  Buffer.add_utf_8_uchar buffer u;
  Buffer.contents buffer

let classify (event : Matrix.Input.Key.event) =
  let open Matrix.Input in
  let modifier = event.Key.modifier in
  let chord =
    modifier.Modifier.ctrl || modifier.Modifier.alt || modifier.Modifier.super
  in
  match event.Key.key with
  | Key.Char uchar
    when modifier.Modifier.ctrl && Uchar.equal uchar (Uchar.of_char 'd') ->
      Action Ctrl_d
  (* Lists expose the same previous/next action for arrow and Emacs chords, so
     callers cannot accidentally implement different movement semantics. *)
  | Key.Char uchar
    when modifier.Modifier.ctrl && Uchar.equal uchar (Uchar.of_char 'p') ->
      Action Up
  | Key.Char uchar
    when modifier.Modifier.ctrl && Uchar.equal uchar (Uchar.of_char 'n') ->
      Action Down
  | Key.Char uchar when not chord ->
      let code = Uchar.to_int uchar in
      if code >= 0x30 && code <= 0x39 then Digit (code - 0x30)
      else if code >= 0x20 && code <> 0x7f then Printable (utf8_of_uchar uchar)
      else Action Other
  | Key.Char _ -> Action Other
  | Key.Enter -> Action Enter
  | Key.Escape -> Action Escape
  | Key.Tab -> Action Tab
  | Key.Left -> Action Left
  | Key.Right -> Action Right
  | Key.Up -> Action Up
  | Key.Down -> Action Down
  | Key.Backspace | Key.Delete -> Action Backspace
  (* Panel lists are short: page keys take one step instead of implying a page
     size that the shared classifier cannot know. *)
  | Key.Page_up -> Action Up
  | Key.Page_down -> Action Down
  | _ -> Action Other

let chip_row ~palette ~frame ~name ~filter =
  (* The chip row is the panel's only filter readout. *)
  let filter_echo =
    if String.equal filter "" then []
    else
      [
        text
          ~style:(Theme.Palette.faint_style palette)
          ~wrap:`None ~truncate:true ~flex_grow:1. ~flex_shrink:1.
          ~min_size:zero_size ("  " ^ filter);
      ]
  in
  box ~key:"panel.chip" ~flex_direction:Flex_direction.Row ~flex_shrink:0.
    ~overflow:hidden_overflow ~padding:(padding_lrtb 2 2 0 0)
    ~size:{ width = pct 100; height = px 1 }
    (Theme.chip ~palette ~color:frame name :: filter_echo)

let hint_row ~palette hints =
  (* This row occupies the footer's place when a panel owns the keyboard. *)
  let pieces =
    List.concat_map
      (fun (index, hint) ->
        let hint =
          text
            ~style:(Theme.Palette.faint_style palette)
            ~wrap:`None ~truncate:true ~flex_shrink:1. ~min_size:zero_size hint
        in
        if index = 0 then [ hint ]
        else [ seg (Theme.Palette.muted_style palette) Theme.separator; hint ])
      (List.mapi (fun index hint -> (index, hint)) hints)
  in
  box ~key:"panel.hint" ~flex_direction:Flex_direction.Row ~flex_shrink:0.
    ~overflow:hidden_overflow ~padding:(padding_lrtb 2 2 0 0)
    ~size:{ width = pct 100; height = px 1 }
    pieces

let blank_row = box ~flex_shrink:0. ~size:{ width = pct 100; height = px 1 } []

let view ~palette ~frame ~name ~filter ~hint ~content =
  let body =
    box ~key:"panel.body" ~flex_direction:Flex_direction.Column ~flex_grow:1.
      ~flex_shrink:1. ~min_size:zero_size ~size:fill_remaining
      ~overflow:hidden_overflow [ content ]
  in
  box ~key:"panel" ~flex_direction:Flex_direction.Column
    ~overflow:hidden_overflow ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size
    ~size:fill_remaining
    [
      boundary ~frame;
      chip_row ~palette ~frame ~name ~filter;
      blank_row;
      body;
      blank_row;
      hint_row ~palette hint;
    ]
