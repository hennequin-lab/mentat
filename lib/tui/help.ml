(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic

type entry = { keys : string; action : string }
type section = { title : string; entries : entry list }

let sections =
  [
    {
      title = "composer";
      entries =
        [
          { keys = "/"; action = "commands" };
          { keys = "@"; action = "file paths" };
          { keys = "!"; action = "shell mode" };
          { keys = "?"; action = "this help" };
        ];
    };
    {
      title = "history";
      entries =
        [
          { keys = "shift+enter"; action = "newline" };
          { keys = "↑ ↓"; action = "prompt history" };
          { keys = "ctrl+r"; action = "search history" };
          { keys = "esc esc"; action = "interrupt turn" };
        ];
    };
    {
      title = "controls";
      entries =
        [
          { keys = "tab"; action = "focus agents" };
          { keys = "ctrl+o"; action = "verbose reasoning" };
          { keys = "shift+tab"; action = "switch build/plan mode" };
          { keys = "pageup pagedown"; action = "scroll" };
          { keys = "ctrl+c ctrl+c"; action = "quit" };
        ];
    };
  ]

let cell ?grid_column ~style value =
  text ?grid_column ~style ~wrap:`None ~truncate:true
    ~min_size:{ width = px 0; height = auto }
    value

let section ~palette { title; entries } =
  let rows =
    List.concat_map
      (fun entry ->
        [
          cell ~style:(Theme.Palette.faint_style palette) entry.keys;
          cell ~style:(Theme.Palette.muted_style palette) entry.action;
        ])
      entries
  in
  box ~display:Display.Grid
    ~grid_template_columns:[ Grid.max_content; Grid.max_content ]
    ~gap:(gap_xy 2 0) ~align_content:Justify.Start
    ~justify_content:Justify.Start ~flex_shrink:1.
    ~min_size:{ width = px 0; height = auto }
    (cell ~grid_column:(Grid.line_range 1 3)
       ~style:(Theme.Palette.muted_style palette)
       title
    :: rows)

let view ~palette () =
  let inset =
    box ~flex_shrink:100.
      ~size:{ width = px 2; height = auto }
      ~min_size:{ width = px 0; height = auto }
      []
  in
  let sheet =
    box ~flex_direction:Flex_direction.Row ~flex_wrap:Flex_wrap.Wrap
      ~gap:(gap_xy 2 0) ~flex_grow:1. ~flex_shrink:1.
      ~min_size:{ width = px 0; height = auto }
      (List.map (section ~palette) sections)
  in
  box ~flex_direction:Flex_direction.Row
    ~size:{ width = pct 100; height = auto }
    [ inset; sheet ]
