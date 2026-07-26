(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid message = invalid_arg ("Mentat_tools_output.Write: " ^ message)

type t = Wrote of { lines : int } | Unchanged

let wrote ~lines =
  if lines < 0 then invalid "lines must be non-negative";
  Wrote { lines }

let unchanged = Unchanged
let shape = function Wrote _ -> `Wrote | Unchanged -> `Unchanged
let lines = function Wrote { lines } -> Some lines | Unchanged -> None

let shape_jsont =
  Jsont.enum ~kind:"write output shape"
    [ ("wrote", `Wrote); ("unchanged", `Unchanged) ]

let jsont =
  let make version shape lines =
    Mentat_tool.Codec.decode_invalid_arg (fun () ->
        if version <> 1 then invalid "unsupported version";
        match (shape, lines) with
        | `Wrote, Some lines -> wrote ~lines
        | `Unchanged, None -> unchanged
        | `Wrote, None -> invalid "missing written line count"
        | `Unchanged, Some _ -> invalid "unchanged has no line count")
  in
  Jsont.Object.map ~kind:"write output" make
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun _ -> 1)
  |> Jsont.Object.mem "shape" shape_jsont ~enc:shape
  |> Jsont.Object.opt_mem "lines" Jsont.int ~enc:lines
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
