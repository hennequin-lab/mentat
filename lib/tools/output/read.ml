(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid message = invalid_arg ("Mentat_tools_output.Read: " ^ message)

type t =
  | File of { lines : int }
  | Listing of { entries : int }
  | Image
  | Unchanged

let file ~lines =
  if lines < 0 then invalid "lines must be non-negative";
  File { lines }

let listing ~entries =
  if entries < 0 then invalid "entries must be non-negative";
  Listing { entries }

let image = Image
let unchanged = Unchanged

let shape = function
  | File _ -> `File
  | Listing _ -> `Listing
  | Image -> `Image
  | Unchanged -> `Unchanged

let count = function
  | File { lines } -> Some lines
  | Listing { entries } -> Some entries
  | Image | Unchanged -> None

let shape_jsont =
  Jsont.enum ~kind:"read output shape"
    [
      ("file", `File);
      ("listing", `Listing);
      ("image", `Image);
      ("unchanged", `Unchanged);
    ]

let jsont =
  let make version shape count =
    Mentat_tool.Codec.decode_invalid_arg (fun () ->
        if version <> 1 then invalid "unsupported version";
        match (shape, count) with
        | `File, Some lines -> file ~lines
        | `Listing, Some entries -> listing ~entries
        | `Image, None -> image
        | `Unchanged, None -> unchanged
        | (`File | `Listing), None -> invalid "missing result count"
        | (`Image | `Unchanged), Some _ ->
            invalid "image and unchanged reads have no result count")
  in
  Jsont.Object.map ~kind:"read output" make
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun _ -> 1)
  |> Jsont.Object.mem "shape" shape_jsont ~enc:shape
  |> Jsont.Object.opt_mem "count" Jsont.int ~enc:count
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
