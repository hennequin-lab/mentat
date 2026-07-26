(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid fn message =
  invalid_arg ("Mentat_tool.Output." ^ fn ^ ": " ^ message)

let reject_empty fn field value =
  if String.is_empty value then invalid fn (field ^ " must not be empty")

type t = {
  text : string;
  json : Jsont.json option;
  media : Mentat_llm.Content.t list;
  truncated : bool;
}

type 'a encoder = 'a -> t

let make ~text ?json ?(media = []) ?(truncated = false) () =
  reject_empty "make" "text" text;
  { text; json; media; truncated }

let text t = t.text
let json t = t.json
let media t = t.media
let truncated t = t.truncated

let equal a b =
  String.equal a.text b.text
  && Option.equal Jsont.Json.equal a.json b.json
  && List.equal Mentat_llm.Content.equal a.media b.media
  && Bool.equal a.truncated b.truncated

let jsont =
  let make text json media truncated =
    Codec.decode_invalid_arg (fun () -> make ~text ?json ~media ~truncated ())
  in
  Jsont.Object.map ~kind:"tool output" make
  |> Jsont.Object.mem "text" Jsont.string ~enc:text
  |> Jsont.Object.opt_mem "json" Jsont.json ~enc:json
  (* The [media] member is what raises a document to version 2: an old reader
     refuses at the session version gate rather than erroring on it. Absent when
     empty, so a text-only output stays byte-identical to a pre-media output. *)
  |> Jsont.Object.mem "media"
       (Jsont.list Mentat_llm.Content.jsont)
       ~enc:media ~dec_absent:[] ~enc_omit:List.is_empty
  |> Jsont.Object.mem "truncated" Jsont.bool ~enc:truncated
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
