(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
open Prims

type action =
  | Enter
  | Escape
  | Tab
  | Up
  | Down
  | Left
  | Right
  | Backspace
  | Page_up
  | Page_down
  | Other

type key = Char of string | Action of action

let zero_size = { width = px 0; height = px 0 }
let fill_width = { width = pct 100; height = auto }
let fill_remaining = { width = pct 100; height = px 0 }
let hidden_overflow = { x = Overflow.Hidden; y = Overflow.Hidden }

let utf8_of_uchar uchar =
  let buffer = Buffer.create 4 in
  Buffer.add_utf_8_uchar buffer uchar;
  Buffer.contents buffer

let classify (event : Matrix.Input.Key.event) =
  let open Matrix.Input in
  let modifier = event.Key.modifier in
  let chord =
    modifier.Modifier.ctrl || modifier.Modifier.alt || modifier.Modifier.super
  in
  match event.Key.key with
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
      if code >= 0x20 && code <> 0x7f then Char (utf8_of_uchar uchar)
      else Action Other
  | Key.Char _ -> Action Other
  | Key.Enter -> Action Enter
  | Key.Escape -> Action Escape
  | Key.Tab -> Action Tab
  | Key.Up -> Action Up
  | Key.Down -> Action Down
  | Key.Left -> Action Left
  | Key.Right -> Action Right
  | Key.Backspace | Key.Delete -> Action Backspace
  | Key.Page_up -> Action Page_up
  | Key.Page_down -> Action Page_down
  | _ -> Action Other

type filter = { query : string; matches : int }

let flex_rule ~frame =
  box ~key:"screen.rule.fill" ~box_sizing:Box_sizing.Border_box
    ~overflow:hidden_overflow ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size
    ~border:true ~border_style:Border.single ~border_sides:[ `Top ]
    ~border_color:frame ~fill:false
    ~size:{ width = auto; height = px 1 }
    []

let rule_row ~palette ~frame ~name ~fact =
  let fact =
    if String.equal fact "" then []
    else
      [
        text
          ~style:(Theme.Palette.muted_style palette)
          ~wrap:`None ~truncate:true ~flex_shrink:1. ~min_size:zero_size
          (" " ^ fact);
      ]
  in
  box ~key:"screen.rule" ~flex_direction:Flex_direction.Row
    ~overflow:hidden_overflow ~size:fill_width
    (Theme.chip ~palette ~color:frame name :: flex_rule ~frame :: fact)

let filter_row ~palette filter =
  let count =
    match filter.matches with
    | 0 -> "  no matches"
    | 1 -> "  1 match"
    | matches -> Printf.sprintf "  %d matches" matches
  in
  box ~key:"screen.filter" ~flex_direction:Flex_direction.Row
    ~overflow:hidden_overflow ~padding:(padding_lrtb 2 2 0 0) ~size:fill_width
    [
      seg (Theme.Palette.atom_style palette) "/";
      text ~style:Ansi.Style.default ~wrap:`None ~truncate:true ~flex_shrink:1.
        ~min_size:zero_size filter.query;
      text
        ~style:(Theme.Palette.faint_style palette)
        ~wrap:`None ~truncate:true ~flex_shrink:1. ~min_size:zero_size count;
    ]

let hint_row ~palette hints =
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
  box ~key:"screen.hint" ~flex_direction:Flex_direction.Row
    ~overflow:hidden_overflow ~padding:(padding_lrtb 2 2 0 0) ~size:fill_width
    pieces

let blank_row = box ~size:{ width = pct 100; height = px 1 } []

let view ~palette ~frame ~name ~fact ~filter ~hint ~content =
  let filter_rows =
    match filter with
    | Some filter -> [ filter_row ~palette filter; blank_row ]
    | None -> [ blank_row ]
  in
  let body =
    box ~key:"screen.body" ~flex_direction:Flex_direction.Column ~flex_grow:1.
      ~flex_shrink:1. ~min_size:zero_size ~overflow:hidden_overflow
      ~size:fill_remaining [ content ]
  in
  box ~key:"screen" ~flex_direction:Flex_direction.Column ~flex_grow:1.
    ~flex_shrink:1. ~min_size:zero_size ~overflow:hidden_overflow
    ~size:fill_remaining
    ((rule_row ~palette ~frame ~name ~fact :: filter_rows)
    @ [ body; blank_row; hint_row ~palette hint ])
