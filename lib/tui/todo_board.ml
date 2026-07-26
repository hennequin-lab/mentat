(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
module Task = Mentat_session.Task

let running_glyph = "◼"
let pending_glyph = "◻"
let done_glyph = "☒"
let zero_size = { width = px 0; height = px 0 }
let fill_width = { width = pct 100; height = auto }
let space value = text ~wrap:`None ~flex_shrink:0. value

type projection = {
  running : string list;
  pending : string list;
  completed : string list;
  total : int;
}

let project board =
  let add projection (item : Task.Item.t) =
    match item.Task.Item.status with
    | Task.Status.In_progress ->
        {
          projection with
          running = item.Task.Item.content :: projection.running;
        }
    | Task.Status.Pending ->
        {
          projection with
          pending = item.Task.Item.content :: projection.pending;
        }
    | Task.Status.Completed | Task.Status.Cancelled ->
        {
          projection with
          completed = item.Task.Item.content :: projection.completed;
        }
  in
  let items = Task.Board.items board in
  let projection =
    List.fold_left add
      { running = []; pending = []; completed = []; total = List.length items }
      items
  in
  {
    projection with
    running = List.rev projection.running;
    pending = List.rev projection.pending;
    completed = List.rev projection.completed;
  }

let text_row style value =
  box ~flex_direction:Flex_direction.Row ~align_items:Align.Flex_start
    ~flex_shrink:0. ~min_size:zero_size ~size:fill_width
    [
      space "  ";
      text ~style ~wrap:`Word ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size
        value;
    ]

let item_row style glyph content =
  box ~flex_direction:Flex_direction.Row ~align_items:Align.Flex_start
    ~flex_shrink:0. ~min_size:zero_size ~size:fill_width
    [
      space "  ";
      text ~style ~wrap:`None ~flex_shrink:0. glyph;
      space " ";
      text ~style ~wrap:`Word ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size
        content;
    ]

let count_header_row ~palette projection =
  text_row
    (Theme.Palette.muted_style palette)
    (Printf.sprintf "%s %d tasks%s%d done%s%d running" pending_glyph
       projection.total Theme.separator
       (List.length projection.completed)
       Theme.separator
       (List.length projection.running))

let done_digest ~palette count =
  text_row
    (Theme.Palette.muted_style palette)
    (Printf.sprintf "… %d done" count)

let view ?(count_header = true) ~palette board =
  let projection = project board in
  if projection.total = 0 then []
  else
    let header =
      if count_header then [ count_header_row ~palette projection ] else []
    in
    let running =
      List.map
        (item_row (Theme.Palette.running_style palette) running_glyph)
        projection.running
    in
    let pending =
      List.map (item_row Ansi.Style.default pending_glyph) projection.pending
    in
    let open_work = projection.running <> [] || projection.pending <> [] in
    let completed =
      match projection.completed with
      | [] -> []
      | items when open_work -> [ done_digest ~palette (List.length items) ]
      | items ->
          List.map
            (item_row (Theme.Palette.muted_style palette) done_glyph)
            items
    in
    [
      box ~key:"todo.board" ~flex_direction:Flex_direction.Column
        ~flex_shrink:1. ~min_size:zero_size ~size:fill_width
        (header @ running @ pending @ completed);
    ]

let strip_border = Border.modify ~horizontal:(Uchar.of_int 0x2508) Border.empty

let strip_rule ~palette =
  box ~key:"todo.rule" ~box_sizing:Box_sizing.Border_box ~flex_shrink:0.
    ~border:true ~border_style:strip_border ~border_sides:[ `Top ]
    ~border_color:(Theme.Palette.rule palette)
    ~fill:false
    ~size:{ width = pct 100; height = px 1 }
    []
