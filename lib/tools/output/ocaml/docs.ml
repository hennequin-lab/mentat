(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid message =
  invalid_arg ("Mentat_tools_output.Ocaml.Docs.make: " ^ message)

type t = { values : int; types : int; modules : int }

let make ~values ~types ~modules =
  if values < 0 then invalid "values must be non-negative";
  if types < 0 then invalid "types must be non-negative";
  if modules < 0 then invalid "modules must be non-negative";
  { values; types; modules }

let values t = t.values
let types t = t.types
let modules t = t.modules

let jsont =
  let make version values types modules =
    Mentat_tool.Codec.decode_invalid_arg (fun () ->
        if version <> 1 then invalid "unsupported version";
        make ~values ~types ~modules)
  in
  Jsont.Object.map ~kind:"OCaml documentation output" make
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun _ -> 1)
  |> Jsont.Object.mem "values" Jsont.int ~enc:values
  |> Jsont.Object.mem "types" Jsont.int ~enc:types
  |> Jsont.Object.mem "modules" Jsont.int ~enc:modules
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
