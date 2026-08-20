(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
open Prims

type fact =
  | Fact of string
  | Change of { added : int; removed : int }
  | Errors of int

type level = Info | Warning | Error

type t =
  | Event of string
  | Echo of { command : string; result : string option }
  | Interrupt
  | Failure of { message : string; next_step : string; count : int }
  | Seam of string
  | Data of { source : string; facts : fact list; atom : string option }
  | Workspace of {
      level : level;
      source : string;
      title : string;
      body : string option;
    }

let zero_size = { width = px 0; height = px 0 }
let hidden_overflow = { x = Overflow.Hidden; y = Overflow.Hidden }

let one_row children =
  box ~flex_direction:Flex_direction.Row ~flex_shrink:1. ~min_size:zero_size
    ~overflow:hidden_overflow
    ~size:{ width = pct 100; height = px 1 }
    children

let shrinking_text style value =
  text ~style ~wrap:`None ~truncate:true ~flex_shrink:1. ~min_size:zero_size
    value

(* An indent-two muted line. Wrapped lines fall back to column zero: notices are
   normally one line, and the indent is not a prose gutter. *)
let event ~palette message =
  text ~style:(Theme.Palette.muted_style palette) ~wrap:`Word ("  " ^ message)

let echo ~palette ~command ~result =
  let head =
    shrinking_text (Theme.Palette.muted_style palette) (Theme.cursor ^ command)
  in
  match result with
  | None -> head
  | Some result ->
      box ~flex_direction:Flex_direction.Column
        ~size:{ width = pct 100; height = auto }
        [
          head;
          text
            ~style:(Theme.Palette.muted_style palette)
            ~wrap:`Word ("  " ^ result);
        ]

let interrupt ~palette =
  one_row
    [
      seg (Theme.Palette.muted_style palette) (Theme.interrupted ^ " ");
      shrinking_text
        (Theme.Palette.muted_style palette)
        "Interrupted \u{2014} tell mentat what to do differently.";
    ]

(* Failures are the one notice class carrying prose, so the message hangs at
   column two like any other block: a fixed failure-glyph gutter and a wrapping
   body. The collapse count stays muted beside the message because it is a
   fact. *)
let failure ~palette ~message ~next_step ~count =
  let head =
    box ~flex_direction:Flex_direction.Row
      ~size:{ width = pct 100; height = auto }
      [
        seg (Theme.Palette.error_style palette) (Theme.failed ^ " ");
        box ~flex_direction:Flex_direction.Column ~flex_grow:1. ~flex_shrink:1.
          [
            box ~flex_direction:Flex_direction.Row
              ~size:{ width = pct 100; height = auto }
              (text
                 ~style:(Theme.Palette.error_style palette)
                 ~wrap:`Word ~flex_shrink:1. message
              ::
              (if count > 1 then
                 [
                   seg
                     (Theme.Palette.muted_style palette)
                     (Printf.sprintf " \u{00D7} %d" count);
                 ]
               else []));
          ];
      ]
  in
  box ~flex_direction:Flex_direction.Column
    ~size:{ width = pct 100; height = auto }
    [
      head;
      text
        ~style:(Theme.Palette.muted_style palette)
        ~wrap:`Word ("  " ^ next_step);
    ]

(* A top-only border box is an intrinsic horizontal rule. Both sides grow by
   the same factor, so Mosaic centers the label and owns all narrow-layout
   shrinkage without a terminal-column projection in this widget. One cell is
   each side's floor: a label wide enough to squeeze the rules to zero would
   otherwise erase the boundary the seam exists to draw. *)
let rule_side ~palette () =
  box ~box_sizing:Box_sizing.Border_box ~overflow:hidden_overflow ~flex_grow:1.
    ~flex_shrink:1.
    ~min_size:{ width = px 1; height = px 0 }
    ~border:true ~border_style:Border.single
    ~border_sides:[ `Top ]
    ~border_color:(Theme.Palette.rule palette)
    ~fill:false
    ~size:{ width = auto; height = px 1 }
    []

let seam ~palette label =
  let label =
    shrinking_text (Theme.Palette.muted_style palette) ("  " ^ label ^ "  ")
  in
  box ~flex_direction:Flex_direction.Row ~overflow:hidden_overflow
    ~padding:(padding_lrtb 1 1 0 0)
    ~size:{ width = pct 100; height = px 1 }
    [ rule_side ~palette (); label; rule_side ~palette () ]

(* The change pair is the single success/error moment in a notice, matched to
   the home brief: unbolded so it reads as a fact, not an outcome banner. *)
let fact_segs ~palette = function
  | Fact value -> [ shrinking_text (Theme.Palette.muted_style palette) value ]
  | Change { added; removed } ->
      [
        shrinking_text
          (Ansi.Style.make ~fg:(Theme.Palette.success palette) ())
          (Printf.sprintf "+%d" added);
        seg (Theme.Palette.muted_style palette) " ";
        shrinking_text
          (Ansi.Style.make ~fg:(Theme.Palette.error palette) ())
          (Printf.sprintf "\u{2212}%d" removed);
      ]
  | Errors count ->
      [
        shrinking_text (Theme.Palette.error_style palette) (string_of_int count);
        shrinking_text
          (Theme.Palette.muted_style palette)
          (if count = 1 then " error" else " errors");
      ]

let data ~palette ~source ~facts ~atom =
  let separator = seg (Theme.Palette.muted_style palette) Theme.separator in
  let head =
    [
      seg (Theme.Palette.muted_style palette) (Theme.watcher ^ " ");
      shrinking_text (Theme.Palette.muted_style palette) source;
    ]
  in
  let facts =
    List.concat_map (fun fact -> separator :: fact_segs ~palette fact) facts
  in
  let atom =
    match atom with
    | None -> []
    | Some atom ->
        [ separator; shrinking_text (Theme.Palette.atom_style palette) atom ]
  in
  one_row (head @ facts @ atom)

(* A workspace observation hangs like a failure: a level-colored watcher glyph
   and head (source and title), then the whole body wrapping at column two. The
   head carries the color so the level reads at a glance; the body stays muted so
   a long file list or diagnostic does not shout. *)
let workspace ~palette ~(level : level) ~source ~title ~body =
  let head_style =
    match level with
    | Error -> Theme.Palette.error_style palette
    | Warning -> Theme.Palette.warning_style palette
    | Info -> Theme.Palette.muted_style palette
  in
  let glyph =
    match level with Error -> Theme.failed | Warning | Info -> Theme.watcher
  in
  let head =
    box ~flex_direction:Flex_direction.Row
      ~size:{ width = pct 100; height = auto }
      [
        seg head_style (glyph ^ " ");
        text ~style:head_style ~wrap:`Word ~flex_shrink:1.
          (source ^ " \u{2014} " ^ title);
      ]
  in
  match body with
  | None -> head
  | Some body ->
      (* The hang is structural padding, not leading spaces: a body carries
         embedded newlines (one file per line) and wraps, and every one of
         those lines must land at column two, not just the first. *)
      box ~flex_direction:Flex_direction.Column
        ~size:{ width = pct 100; height = auto }
        [
          head;
          box ~padding:(padding_lrtb 2 0 0 0)
            ~size:{ width = pct 100; height = auto }
            [ text ~style:(Theme.Palette.muted_style palette) ~wrap:`Word body ];
        ]

let view ~palette = function
  | Event message -> event ~palette message
  | Echo { command; result } -> echo ~palette ~command ~result
  | Interrupt -> interrupt ~palette
  | Failure { message; next_step; count } ->
      failure ~palette ~message ~next_step ~count
  | Seam label -> seam ~palette label
  | Data { source; facts; atom } -> data ~palette ~source ~facts ~atom
  | Workspace { level; source; title; body } ->
      workspace ~palette ~level ~source ~title ~body
