(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Rejection = struct
  type t =
    | Unsupported_format
    | Too_large of { bytes : int; cap : int }
    | Too_many_pixels of { width : int; height : int; cap : int }
    | Too_many_images of { count : int; cap : int }

  let pp ppf = function
    | Unsupported_format ->
        Format.pp_print_string ppf
          "unsupported image format (expected png, jpeg, gif, or webp)"
    | Too_large { bytes; cap } ->
        Format.fprintf ppf "image is too large (%d bytes, cap %d bytes)" bytes
          cap
    | Too_many_pixels { width; height; cap } ->
        Format.fprintf ppf "image is too large (%dx%d, cap %d px per side)"
          width height cap
    | Too_many_images { count; cap } ->
        Format.fprintf ppf "too many images (%d, cap %d)" count cap
end

type source =
  | Path of Mentat_workspace.Path.t
  | Bytes of { media_type : string; bytes : string }

type caps = { max_bytes : int; max_dimension : int; max_count : int }

let check caps bytes =
  match Mentat_llm.Image.sniff bytes with
  | None -> Error Rejection.Unsupported_format
  | Some format -> (
      let byte_size = String.length bytes in
      if byte_size > caps.max_bytes then
        Error (Rejection.Too_large { bytes = byte_size; cap = caps.max_bytes })
      else
        match Mentat_llm.Image.dimensions bytes with
        | Some { Mentat_llm.Image.width; height }
          when width > caps.max_dimension || height > caps.max_dimension ->
            Error
              (Rejection.Too_many_pixels
                 { width; height; cap = caps.max_dimension })
        | _ -> Ok format)

let check_count caps ~count =
  if count > caps.max_count then
    Error (Rejection.Too_many_images { count; cap = caps.max_count })
  else Ok ()

module Error = struct
  type t =
    | Rejected of Rejection.t
    | Not_found
    | Not_an_image
    | Unavailable of Mentat_diagnostic.t

  let pp ppf = function
    | Rejected rejection -> Rejection.pp ppf rejection
    | Not_found -> Format.pp_print_string ppf "image file not found"
    | Not_an_image ->
        Format.pp_print_string ppf "the file is not a recognized image"
    | Unavailable diagnostic ->
        Format.pp_print_string ppf (Mentat_diagnostic.to_string diagnostic)
end
