(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Key = struct
  type t = string

  type error =
    | Empty
    | Invalid_utf_8
    | Control_character of { byte_index : int; byte : int }

  let message = function
    | Empty -> "root key must not be empty"
    | Invalid_utf_8 -> "root key must be valid UTF-8"
    | Control_character { byte_index; byte } ->
        Printf.sprintf
          "root key must not contain ASCII control characters (byte 0x%02X at \
           index %d)"
          byte byte_index

  let control_character s =
    let rec loop index =
      if index = String.length s then None
      else
        let byte = Char.code s.[index] in
        if byte < 0x20 || byte = 0x7F then Some (index, byte)
        else loop (index + 1)
    in
    loop 0

  let of_string s =
    if String.is_empty s then Error Empty
    else if not (String.is_valid_utf_8 s) then Error Invalid_utf_8
    else
      match control_character s with
      | Some (byte_index, byte) ->
          Error (Control_character { byte_index; byte })
      | None -> Ok s

  let of_string_exn s =
    match of_string s with
    | Ok t -> t
    | Error error ->
        invalid_arg ("Mentat_workspace.Root.Key.of_string_exn: " ^ message error)

  let to_string t = t
  let equal = String.equal
  let compare = String.compare
  let pp = Format.pp_print_string
  let pp_error ppf error = Format.pp_print_string ppf (message error)

  let jsont =
    Jsont.map ~kind:"Mentat_workspace.Root.Key"
      ~dec:(fun s ->
        match of_string s with
        | Ok t -> t
        | Error error -> Jsont.Error.msg Jsont.Meta.none (message error))
      ~enc:to_string Jsont.string
end

type t = { key : Key.t; dir : Lpath.Abs.t }

let make ~key dir = { key; dir }
let hex_digit n = "0123456789ABCDEF".[n]

let location_key dir =
  let raw = Lpath.Abs.to_string dir in
  let encoded = Buffer.create (String.length raw) in
  String.iter
    (fun char ->
      let byte = Char.code char in
      if byte >= 0x21 && byte <= 0x7E && byte <> Char.code '%' then
        Buffer.add_char encoded char
      else (
        Buffer.add_char encoded '%';
        Buffer.add_char encoded (hex_digit (byte lsr 4));
        Buffer.add_char encoded (hex_digit (byte land 0x0F))))
    raw;
  Key.of_string_exn (Buffer.contents encoded)

let of_dir dir = make ~key:(location_key dir) dir
let dir t = t.dir
let key t = t.key
let same_key a b = Key.equal a.key b.key
let equal a b = same_key a b && Lpath.Abs.equal a.dir b.dir

let compare a b =
  match Key.compare a.key b.key with
  | 0 -> Lpath.Abs.compare a.dir b.dir
  | order -> order

let pp ppf t = Lpath.Abs.pp ppf t.dir
