(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid message = invalid_arg ("Mentat_tools_output.Search: " ^ message)

let non_negative name value =
  if value < 0 then invalid (name ^ " must be non-negative")

type t =
  | Files of { total : int }
  | Matches of { total : int; files : int }
  | Matching_lines of { total : int }

let files ~total =
  non_negative "total" total;
  Files { total }

let matches ~total ~files =
  non_negative "total" total;
  non_negative "files" files;
  if files > total then invalid "files must not exceed matches";
  Matches { total; files }

let matching_lines ~total =
  non_negative "total" total;
  Matching_lines { total }

let shape = function
  | Files _ -> `Files
  | Matches _ -> `Matches
  | Matching_lines _ -> `Matching_lines

let total = function
  | Files { total } | Matches { total; _ } | Matching_lines { total } -> total

let file_count = function Matches { files; _ } -> Some files | _ -> None

let shape_jsont =
  Jsont.enum ~kind:"search output shape"
    [
      ("files", `Files);
      ("matches", `Matches);
      ("matching_lines", `Matching_lines);
    ]

let jsont =
  let make version shape total file_count =
    Mentat_tool.Codec.decode_invalid_arg (fun () ->
        if version <> 1 then invalid "unsupported version";
        match (shape, file_count) with
        | `Files, None -> files ~total
        | `Matches, Some count -> matches ~total ~files:count
        | `Matching_lines, None -> matching_lines ~total
        | (`Files | `Matching_lines), Some _ ->
            invalid "only matches have a file count"
        | `Matches, None -> invalid "matches require a file count")
  in
  Jsont.Object.map ~kind:"search output" make
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun _ -> 1)
  |> Jsont.Object.mem "shape" shape_jsont ~enc:shape
  |> Jsont.Object.mem "total" Jsont.int ~enc:total
  |> Jsont.Object.opt_mem "files" Jsont.int ~enc:file_count
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
