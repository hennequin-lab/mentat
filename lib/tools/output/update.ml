(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid message = invalid_arg ("Mentat_tools_output.Update.make: " ^ message)

let non_negative name = function
  | Some value when value < 0 -> invalid (name ^ " must be non-negative")
  | Some _ | None -> ()

type disposition = Applied | Previewed

type t = {
  disposition : disposition;
  files : int;
  additions : int option;
  deletions : int option;
  skipped : int;
}

let make ~disposition ~files ~additions ~deletions ~skipped =
  if files < 0 then invalid "files must be non-negative";
  if skipped < 0 then invalid "skipped must be non-negative";
  non_negative "additions" additions;
  non_negative "deletions" deletions;
  { disposition; files; additions; deletions; skipped }

let disposition t = t.disposition
let files t = t.files
let additions t = t.additions
let deletions t = t.deletions
let skipped t = t.skipped

let disposition_jsont =
  Jsont.enum ~kind:"update disposition"
    [ ("applied", Applied); ("previewed", Previewed) ]

let jsont =
  let make version disposition files additions deletions skipped =
    Mentat_tool.Codec.decode_invalid_arg (fun () ->
        if version <> 1 then invalid "unsupported version";
        make ~disposition ~files ~additions ~deletions ~skipped)
  in
  Jsont.Object.map ~kind:"update output" make
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun _ -> 1)
  |> Jsont.Object.mem "disposition" disposition_jsont ~enc:disposition
  |> Jsont.Object.mem "files" Jsont.int ~enc:files
  |> Jsont.Object.opt_mem "additions" Jsont.int ~enc:additions
  |> Jsont.Object.opt_mem "deletions" Jsont.int ~enc:deletions
  |> Jsont.Object.mem "skipped" Jsont.int ~enc:skipped
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
