(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let algorithm = "sha256"
let digest_size = 32
let hex_size = 64

module Error = struct
  type t = Not_hex | Malformed_content_ref

  let message = function
    | Not_hex -> "digest must be exactly 64 lowercase hexadecimal characters"
    | Malformed_content_ref ->
        "content reference must be sha256:<64 lowercase hex>:<length>"

  let pp ppf error = Format.pp_print_string ppf (message error)
end

type t = string

external string : string -> t = "caml_mentat_digest_sha256_string"
external ctx_size : unit -> int = "caml_mentat_digest_sha256_ctx_size"
external fold_init : bytes -> unit = "caml_mentat_digest_sha256_fold_init"

external fold_add : bytes -> string -> unit
  = "caml_mentat_digest_sha256_fold_add"

external fold_digest : bytes -> t = "caml_mentat_digest_sha256_fold_digest"

let to_raw_string digest = digest
let is_lower_hex c = Char.Ascii.is_digit c || ('a' <= c && c <= 'f')

(* Digest values are produced by the C stub as exactly [digest_size] raw
   SHA-256 bytes, which makes the unsafe fixed-size loops valid. *)
let to_hex digest =
  let bytes = Bytes.create hex_size in
  for i = 0 to digest_size - 1 do
    let byte = Char.code (String.unsafe_get digest i) in
    let j = i * 2 in
    Bytes.unsafe_set bytes j (Char.Ascii.lower_hex_digit_of_int (byte lsr 4));
    Bytes.unsafe_set bytes (j + 1)
      (Char.Ascii.lower_hex_digit_of_int (byte land 0x0f))
  done;
  Bytes.unsafe_to_string bytes

let of_hex s =
  if String.length s <> hex_size || not (String.for_all is_lower_hex s) then
    Error Error.Not_hex
  else begin
    (* [s] is validated as [hex_size] lowercase hex characters, so every
       [hex_digit_to_int] is total and each byte value stays in [0x00;0xff]. *)
    let bytes = Bytes.create digest_size in
    for i = 0 to digest_size - 1 do
      let j = i * 2 in
      let hi = Char.Ascii.hex_digit_to_int (String.unsafe_get s j) in
      let lo = Char.Ascii.hex_digit_to_int (String.unsafe_get s (j + 1)) in
      Bytes.unsafe_set bytes i (Char.chr ((hi lsl 4) lor lo))
    done;
    Ok (Bytes.unsafe_to_string bytes)
  end

let jsont =
  Jsont.map ~kind:"Mentat_digest"
    ~dec:(fun s ->
      match of_hex s with
      | Ok t -> t
      | Error error -> Jsont.Error.msg Jsont.Meta.none (Error.message error))
    ~enc:to_hex Jsont.string

let equal = String.equal
let compare = String.compare
let pp ppf t = Format.pp_print_string ppf (to_hex t)

let frame buffer text =
  Buffer.add_string buffer (string_of_int (String.length text));
  Buffer.add_char buffer ':';
  Buffer.add_string buffer text

let derive ~domain parts =
  if String.is_empty domain then
    invalid_arg "Mentat_digest.derive: empty domain";
  let buffer = Buffer.create 128 in
  frame buffer domain;
  List.iter (frame buffer) parts;
  string (Buffer.contents buffer)

let key ~length ~domain parts =
  if length < 0 || length > hex_size then
    invalid_arg "Mentat_digest.key: length must be between 0 and 64";
  if String.is_empty domain then invalid_arg "Mentat_digest.key: empty domain";
  String.take_first length (to_hex (derive ~domain parts))

module Fold = struct
  (* The incremental C context lives in a [Bytes.t] the stub sizes; [finalized]
     forbids use past the single [digest]. The digest type [fold_digest] returns
     is the enclosing {!Mentat_digest.t}, captured before [type t] shadows it. *)
  type t = { ctx : Bytes.t; mutable finalized : bool }

  let create () =
    let ctx = Bytes.create (ctx_size ()) in
    fold_init ctx;
    { ctx; finalized = false }

  let add t chunk =
    if t.finalized then
      invalid_arg "Mentat_digest.Fold.add: context already finalized";
    fold_add t.ctx chunk

  let digest t =
    if t.finalized then
      invalid_arg "Mentat_digest.Fold.digest: context already finalized";
    t.finalized <- true;
    fold_digest t.ctx
end

module Content_ref = struct
  (* Capture the enclosing digest type before [type t] below shadows it. *)
  type digest = t
  type t = { digest : digest; length : int }

  let tag_size = String.length algorithm
  let digest_start = tag_size + 1
  let digest_end = digest_start + hex_size
  let length_start = digest_end + 1

  let of_contents contents =
    { digest = string contents; length = String.length contents }

  let digest r = r.digest
  let length r = r.length

  let matches r s =
    Int.equal (String.length s) r.length && equal r.digest (string s)

  let verify r s =
    let actual = of_contents s in
    if Int.equal actual.length r.length && equal actual.digest r.digest then
      Ok ()
    else Error actual

  let to_token r =
    algorithm ^ ":" ^ to_hex r.digest ^ ":" ^ string_of_int r.length

  (* Strict decimal byte length: no leading zero (except ["0"]), digits only,
     and no overflow past [max_int]. *)
  let parse_length s =
    let len = String.length s in
    if len = 0 || (len > 1 && Char.equal s.[0] '0') then None
    else
      let rec loop acc i =
        if i = len then Some acc
        else
          let c = s.[i] in
          if not (Char.Ascii.is_digit c) then None
          else
            let digit = Char.Ascii.digit_to_int c in
            if acc > (max_int - digit) / 10 then None
            else loop ((acc * 10) + digit) (i + 1)
      in
      loop 0 0

  let of_token s =
    let len = String.length s in
    if
      len <= length_start
      || (not (Char.equal s.[tag_size] ':'))
      || (not (Char.equal s.[digest_end] ':'))
      || not (String.starts_with ~prefix:algorithm s)
    then Error Error.Malformed_content_ref
    else
      match of_hex (String.sub s digest_start hex_size) with
      | Error _ -> Error Error.Malformed_content_ref
      | Ok digest -> (
          match
            parse_length (String.sub s length_start (len - length_start))
          with
          | None -> Error Error.Malformed_content_ref
          | Some length -> Ok { digest; length })

  let jsont =
    Jsont.map ~kind:"Mentat_digest.Content_ref"
      ~dec:(fun s ->
        match of_token s with
        | Ok r -> r
        | Error error -> Jsont.Error.msg Jsont.Meta.none (Error.message error))
      ~enc:to_token Jsont.string

  let equal a b = Int.equal a.length b.length && equal a.digest b.digest

  let compare a b =
    match Int.compare a.length b.length with
    | 0 -> compare a.digest b.digest
    | c -> c

  let pp ppf r = Format.pp_print_string ppf (to_token r)
end
