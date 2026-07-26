(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid message =
  invalid_arg ("Mentat_tools_output.Ocaml.Definition: " ^ message)

type t = { path : string; line : int }

let make ~path ~line =
  if String.is_empty path then invalid "path must not be empty";
  if line < 1 then invalid "line must be at least 1";
  { path; line }

let path t = t.path
let line t = t.line

let jsont =
  let make version path line =
    Mentat_tool.Codec.decode_invalid_arg (fun () ->
        if version <> 1 then invalid "unsupported version";
        make ~path ~line)
  in
  Jsont.Object.map ~kind:"ocaml definition output" make
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun _ -> 1)
  |> Jsont.Object.mem "path" Jsont.string ~enc:path
  |> Jsont.Object.mem "line" Jsont.int ~enc:line
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
