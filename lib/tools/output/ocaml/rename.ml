(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid message =
  invalid_arg ("Mentat_tools_output.Ocaml.Rename.make: " ^ message)

type disposition = Applied | Previewed
type t = { disposition : disposition; occurrences : int; files : int }

let make ~disposition ~occurrences ~files =
  if occurrences < 1 then invalid "occurrences must be positive";
  if files < 1 then invalid "files must be positive";
  if files > occurrences then invalid "files must not exceed occurrences";
  { disposition; occurrences; files }

let disposition t = t.disposition
let occurrences t = t.occurrences
let files t = t.files

let disposition_jsont =
  Jsont.enum ~kind:"OCaml rename disposition"
    [ ("applied", Applied); ("previewed", Previewed) ]

let jsont =
  let make version disposition occurrences files =
    Mentat_tool.Codec.decode_invalid_arg (fun () ->
        if version <> 1 then invalid "unsupported version";
        make ~disposition ~occurrences ~files)
  in
  Jsont.Object.map ~kind:"OCaml rename output" make
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun _ -> 1)
  |> Jsont.Object.mem "disposition" disposition_jsont ~enc:disposition
  |> Jsont.Object.mem "occurrences" Jsont.int ~enc:occurrences
  |> Jsont.Object.mem "files" Jsont.int ~enc:files
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
