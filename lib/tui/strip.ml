(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic

let verbose_glyph = "◎"
let queued_glyph = "↥"
let zero_size = { width = px 0; height = px 0 }
let hidden_overflow = { x = Overflow.Hidden; y = Overflow.Hidden }

(* A status row is fixed chrome: one full-width line with the same two-column
   inset as other transient notices. *)
let row children =
  box ~flex_direction:Flex_direction.Row ~flex_shrink:0. ~min_size:zero_size
    ~overflow:hidden_overflow ~padding:(padding_lrtb 2 0 0 0)
    ~size:{ width = pct 100; height = px 1 }
    children

let intrinsic style value = text ~style ~wrap:`None ~flex_shrink:0. value

let first_line prompt =
  match String.index_opt prompt '\n' with
  | Some index -> String.sub prompt 0 index
  | None -> prompt

let queued_row ~palette prompt =
  row
    [
      intrinsic
        (Theme.Palette.muted_style palette)
        (queued_glyph ^ " queued" ^ Theme.separator ^ "\"");
      text
        ~style:(Theme.Palette.muted_style palette)
        ~wrap:`None ~truncate:true ~flex_shrink:1. ~min_size:zero_size
        (first_line prompt);
      intrinsic (Theme.Palette.muted_style palette) "\"";
      intrinsic (Theme.Palette.faint_style palette) " (↑ edits)";
    ]

let verbose_row ~palette =
  row
    [
      intrinsic
        (Theme.Palette.warning_style palette)
        (verbose_glyph ^ " verbose");
      intrinsic (Theme.Palette.faint_style palette) " ctrl+o closes";
    ]

let view ~palette ~verbose ~queued =
  (if verbose then [ verbose_row ~palette ] else [])
  @ List.map (queued_row ~palette) queued
