(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic

(* Mosaic compares a Markdown block's style resolver and a fence's syntax by
   physical equality when deciding whether to re-render or re-highlight. A
   palette-derived value is memoized on the palette's physical identity: the one
   value in use yields the same physical closure every frame, and a runtime
   theme swap gives it a new identity so the derivation reruns exactly once. A
   fresh closure per frame would re-render and re-parse every settled block on
   otherwise unchanged frames. *)
let per_palette make =
  let cache = ref None in
  fun palette ->
    match !cache with
    | Some (previous, value) when previous == palette -> value
    | _ ->
        let value = make palette in
        cache := Some (palette, value);
        value

let seg style s = text ~style ~wrap:`None ~flex_shrink:0. s

let group_digits value =
  let digits = string_of_int (abs value) in
  let length = String.length digits in
  let buffer = Buffer.create (length + (length / 3) + 1) in
  if value < 0 then Buffer.add_char buffer '-';
  String.iteri
    (fun index char ->
      if index > 0 && (length - index) mod 3 = 0 then Buffer.add_char buffer ',';
      Buffer.add_char buffer char)
    digits;
  Buffer.contents buffer

let compact_count n =
  if n >= 1_000_000 then Printf.sprintf "%.1fM" (float n /. 1_000_000.)
  else if n >= 100_000 then
    Printf.sprintf "%dK" (int_of_float (Float.round (float n /. 1000.)))
  else if n >= 1000 then Printf.sprintf "%.1fK" (float n /. 1000.)
  else string_of_int n

let normalize_inline value =
  let replacement = Uchar.rep in
  let buffer = Buffer.create (String.length value) in
  let rec loop offset =
    if offset < String.length value then begin
      let decoded = String.get_utf_8_uchar value offset in
      let length = max 1 (Uchar.utf_decode_length decoded) in
      if Uchar.utf_decode_is_valid decoded then begin
        let uchar = Uchar.utf_decode_uchar decoded in
        let code = Uchar.to_int uchar in
        if
          code = 0x09 || code = 0x0A || code = 0x0D || code = 0x2028
          || code = 0x2029
        then Buffer.add_char buffer ' '
        else if code < 0x20 || (code >= 0x7F && code <= 0x9F) then
          Buffer.add_utf_8_uchar buffer replacement
        else Buffer.add_utf_8_uchar buffer uchar
      end
      else Buffer.add_utf_8_uchar buffer replacement;
      loop (offset + length)
    end
  in
  loop 0;
  Buffer.contents buffer
