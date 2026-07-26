(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid message =
  invalid_arg ("Mentat_tools_output.Ocaml.Type_at.make: " ^ message)

type t = { head : string; more : int }

let max_head_bytes = 512

let make ~head ~more =
  if String.is_empty head then invalid "head must not be empty";
  if String.length head > max_head_bytes then
    invalid "head exceeds the 512-byte bound";
  if not (String.is_valid_utf_8 head) then invalid "head must be valid UTF-8";
  if String.contains head '\n' || String.contains head '\r' then
    invalid "head must be one line";
  if more < 0 then invalid "more must be non-negative";
  { head; more }

let head t = t.head
let more t = t.more

let jsont =
  let make version head more =
    Mentat_tool.Codec.decode_invalid_arg (fun () ->
        if version <> 1 then invalid "unsupported version";
        make ~head ~more)
  in
  Jsont.Object.map ~kind:"ocaml_type_at output" make
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun _ -> 1)
  |> Jsont.Object.mem "head" Jsont.string ~enc:head
  |> Jsont.Object.mem "more" Jsont.int ~enc:more
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
