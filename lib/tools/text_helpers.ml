(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let is_text_control = function
  | '\t' | '\n' | '\r' | '\x0c' -> true
  | _ -> false

let is_control_byte c = Char.Ascii.is_control c && not (is_text_control c)

let looks_binary text =
  if String.contains text '\x00' then true
  else
    let controls = ref 0 in
    String.iter (fun c -> if is_control_byte c then incr controls) text;
    !controls * 10 > String.length text

let strip_trailing_cr text =
  if String.ends_with ~suffix:"\r" text then String.drop_last 1 text else text

let rec valid_utf8_prefix_from text index =
  if index <= 0 then ""
  else
    let prefix = String.sub text 0 index in
    if String.is_valid_utf_8 prefix then prefix
    else valid_utf8_prefix_from text (index - 1)

let valid_utf8_prefix text max_bytes =
  valid_utf8_prefix_from text (min (String.length text) max_bytes)

let utf8_lossy text =
  if String.is_valid_utf_8 text then text
  else
    let replacement = Uchar.of_int 0xFFFD in
    let buffer = Buffer.create (String.length text) in
    let rec loop index =
      if index >= String.length text then Buffer.contents buffer
      else
        let decoded = String.get_utf_8_uchar text index in
        let length = max 1 (Uchar.utf_decode_length decoded) in
        let uchar =
          if Uchar.utf_decode_is_valid decoded then
            Uchar.utf_decode_uchar decoded
          else replacement
        in
        Buffer.add_utf_8_uchar buffer uchar;
        loop (index + length)
    in
    loop 0

let strip_ansi text =
  if not (String.contains text '\x1b') then text
  else
    let length = String.length text in
    let buffer = Buffer.create length in
    let rec skip_csi index =
      if index >= length then index
      else
        let byte = Char.code text.[index] in
        if byte >= 0x40 && byte <= 0x7E then index + 1 else skip_csi (index + 1)
    in
    let rec skip_osc index =
      if index >= length then index
      else if Char.equal text.[index] '\x07' then index + 1
      else if
        Char.equal text.[index] '\x1b'
        && index + 1 < length
        && Char.equal text.[index + 1] '\\'
      then index + 2
      else skip_osc (index + 1)
    in
    let rec loop index =
      if index >= length then Buffer.contents buffer
      else if
        Char.equal text.[index] '\x1b'
        && index + 1 < length
        && Char.equal text.[index + 1] '['
      then loop (skip_csi (index + 2))
      else if
        Char.equal text.[index] '\x1b'
        && index + 1 < length
        && Char.equal text.[index + 1] ']'
      then loop (skip_osc (index + 2))
      else begin
        Buffer.add_char buffer text.[index];
        loop (index + 1)
      end
    in
    loop 0

let scrub_controls text =
  String.map
    (fun byte ->
      match byte with
      | '\t' | '\n' -> byte
      | '\r' -> '\n'
      | _ when Char.code byte < 0x20 || Char.code byte = 0x7f -> ' '
      | _ -> byte)
    text

let bounded_diagnostic ~max_bytes text =
  let text = text |> utf8_lossy |> strip_ansi |> String.trim in
  if String.length text <= max_bytes then text
  else valid_utf8_prefix text max_bytes

let logical_line_count text =
  if String.is_empty text then 0
  else
    let newlines = ref 0 in
    String.iter (fun c -> if Char.equal c '\n' then incr newlines) text;
    if Char.equal text.[String.length text - 1] '\n' then !newlines
    else !newlines + 1
