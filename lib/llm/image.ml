(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Format = struct
  type t = Png | Jpeg | Gif | Webp

  let media_type = function
    | Png -> "image/png"
    | Jpeg -> "image/jpeg"
    | Gif -> "image/gif"
    | Webp -> "image/webp"

  let of_media_type s =
    match String.lowercase_ascii (String.trim s) with
    | "image/png" -> Some Png
    | "image/jpeg" | "image/jpg" -> Some Jpeg
    | "image/gif" -> Some Gif
    | "image/webp" -> Some Webp
    | _ -> None

  let equal a b =
    match (a, b) with
    | Png, Png | Jpeg, Jpeg | Gif, Gif | Webp, Webp -> true
    | (Png | Jpeg | Gif | Webp), _ -> false

  let pp ppf t = Format.pp_print_string ppf (media_type t)
end

type dimensions = { width : int; height : int }

(* Magic-number prefixes, shortest sufficient to disambiguate the four
   formats. The PNG signature includes CR/LF/EOF bytes that reject a
   text-transferred file. *)

let png_signature = "\x89PNG\r\n\x1a\n"
let is_png s = String.starts_with ~prefix:png_signature s
let is_jpeg s = String.starts_with ~prefix:"\xff\xd8\xff" s

let is_gif s =
  String.length s >= 6
  && String.starts_with ~prefix:"GIF8" s
  && (s.[4] = '7' || s.[4] = '9')
  && s.[5] = 'a'

let is_webp s =
  String.length s >= 12
  && String.starts_with ~prefix:"RIFF" s
  && String.sub s 8 4 = "WEBP"

let sniff s =
  if is_png s then Some Format.Png
  else if is_jpeg s then Some Format.Jpeg
  else if is_gif s then Some Format.Gif
  else if is_webp s then Some Format.Webp
  else None

(* Unsigned byte reads. Every caller bounds-checks the read window against
   [String.length] before reading, so [String.get] stays in range. *)

let u8 s i = Char.code s.[i]
let u16be s i = (u8 s i lsl 8) lor u8 s (i + 1)
let u16le s i = (u8 s (i + 1) lsl 8) lor u8 s i
let u24le s i = (u8 s (i + 2) lsl 16) lor (u8 s (i + 1) lsl 8) lor u8 s i

let u32be s i =
  (u8 s i lsl 24)
  lor (u8 s (i + 1) lsl 16)
  lor (u8 s (i + 2) lsl 8)
  lor u8 s (i + 3)

(* PNG: an 8-byte signature, then the mandatory IHDR chunk whose data begins at
   offset 16 with width then height as big-endian 32-bit integers. *)
let png_dimensions s =
  if String.length s < 24 then None
  else Some { width = u32be s 16; height = u32be s 20 }

(* JPEG: after the SOI marker, walk segments to the first Start-Of-Frame marker
   (SOFn: 0xC0-0xCF, excluding the DHT/JPG/DAC markers 0xC4/0xC8/0xCC) and read
   its height and width. Standalone markers (SOI, EOI, RSTn, TEM) carry no
   length; all others carry a 2-byte segment length. *)
let jpeg_dimensions s =
  let n = String.length s in
  let is_sof marker =
    marker >= 0xC0 && marker <= 0xCF && marker <> 0xC4 && marker <> 0xC8
    && marker <> 0xCC
  in
  let standalone marker =
    marker = 0xD8 || marker = 0xD9 || marker = 0x01
    || (marker >= 0xD0 && marker <= 0xD7)
  in
  let rec scan i =
    if i + 1 >= n then None
    else if u8 s i <> 0xFF then None
    else
      let marker = u8 s (i + 1) in
      if marker = 0xFF then scan (i + 1) (* fill byte *)
      else if standalone marker then scan (i + 2)
      else if i + 3 >= n then None
      else
        let length = u16be s (i + 2) in
        if is_sof marker then
          if i + 8 >= n then None
          else Some { height = u16be s (i + 5); width = u16be s (i + 7) }
        else scan (i + 2 + length)
  in
  scan 2

(* GIF: the logical screen descriptor follows the 6-byte header, with width then
   height as little-endian 16-bit integers. *)
let gif_dimensions s =
  if String.length s < 10 then None
  else Some { width = u16le s 6; height = u16le s 8 }

(* WebP: a RIFF container whose first chunk fourcc at offset 12 selects the
   bitstream. VP8 (lossy) packs 14-bit width/height little-endian; VP8L
   (lossless) bit-packs 14-bit width-1/height-1 across four bytes; VP8X
   (extended) stores 24-bit canvas width-1/height-1. *)
let webp_dimensions s =
  let n = String.length s in
  if n < 16 then None
  else
    match String.sub s 12 4 with
    | "VP8 " ->
        if n < 30 then None
        else
          Some
            { width = u16le s 26 land 0x3FFF; height = u16le s 28 land 0x3FFF }
    | "VP8L" ->
        if n < 25 then None
        else
          let b0 = u8 s 21 and b1 = u8 s 22 and b2 = u8 s 23 and b3 = u8 s 24 in
          let width = 1 + (((b1 land 0x3F) lsl 8) lor b0) in
          let height =
            1
            + (((b3 land 0x0F) lsl 10) lor (b2 lsl 2) lor ((b1 land 0xC0) lsr 6))
          in
          Some { width; height }
    | "VP8X" ->
        if n < 30 then None
        else Some { width = 1 + u24le s 24; height = 1 + u24le s 27 }
    | _ -> None

let dimensions s =
  match sniff s with
  | Some Format.Png -> png_dimensions s
  | Some Format.Jpeg -> jpeg_dimensions s
  | Some Format.Gif -> gif_dimensions s
  | Some Format.Webp -> webp_dimensions s
  | None -> None
