(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid message =
  invalid_arg ("Mentat_tools_output.Ocaml.References.make: " ^ message)

type t = { references : int; files : int }

let make ~references ~files =
  if references < 0 then invalid "references must be non-negative";
  if files < 0 then invalid "files must be non-negative";
  if files > references then invalid "files must not exceed references";
  { references; files }

let references t = t.references
let files t = t.files

let jsont =
  let make version references files =
    Mentat_tool.Codec.decode_invalid_arg (fun () ->
        if version <> 1 then invalid "unsupported version";
        make ~references ~files)
  in
  Jsont.Object.map ~kind:"ocaml references output" make
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun _ -> 1)
  |> Jsont.Object.mem "references" Jsont.int ~enc:references
  |> Jsont.Object.mem "files" Jsont.int ~enc:files
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
