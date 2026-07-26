(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid message =
  invalid_arg ("Mentat_tools_output.Web.Fetch.make: " ^ message)

let max_safe_integer =
  Float.min 9_007_199_254_740_991. (float_of_int Int.max_int)

let exact_integer =
  let decode = function
    | Jsont.Number (value, _)
      when Float.is_integer value && Float.abs value <= max_safe_integer ->
        int_of_float value
    | Jsont.Number _ | Jsont.Null _ | Jsont.Bool _ | Jsont.String _
    | Jsont.Array _ | Jsont.Object _ ->
        Jsont.Error.msg Jsont.Meta.none
          "expected an integer in JSON's safe integer range"
  in
  Jsont.map ~kind:"integer" ~dec:decode
    ~enc:(fun value -> Jsont.Json.int value)
    Jsont.json

type disposition = Fetched | Redirected | Http_error
type t = { disposition : disposition; status : int; bytes : int }

let make ~disposition ~status ~bytes =
  if status < 100 || status > 599 then
    invalid "status must be between 100 and 599";
  (match disposition with
  | Fetched when status < 200 || status > 299 ->
      invalid "fetched status must be between 200 and 299"
  | Redirected when status < 300 || status > 399 ->
      invalid "redirected status must be between 300 and 399"
  | Redirected when bytes <> 0 -> invalid "redirected bytes must be zero"
  | Http_error when status >= 200 && status <= 299 ->
      invalid "http_error status must not be between 200 and 299"
  | Fetched | Redirected | Http_error -> ());
  if bytes < 0 then invalid "bytes must be non-negative";
  if float_of_int bytes > max_safe_integer then
    invalid "bytes must be in JSON's safe integer range";
  { disposition; status; bytes }

let disposition t = t.disposition
let status t = t.status
let bytes t = t.bytes

let disposition_jsont =
  Jsont.enum ~kind:"web fetch disposition"
    [
      ("fetched", Fetched);
      ("redirected", Redirected);
      ("http_error", Http_error);
    ]

let jsont =
  let make version disposition status bytes =
    Mentat_tool.Codec.decode_invalid_arg (fun () ->
        if version <> 1 then invalid "unsupported version";
        make ~disposition ~status ~bytes)
  in
  Jsont.Object.map ~kind:"web fetch output" make
  |> Jsont.Object.mem "version" exact_integer ~enc:(fun _ -> 1)
  |> Jsont.Object.mem "disposition" disposition_jsont ~enc:disposition
  |> Jsont.Object.mem "status" exact_integer ~enc:status
  |> Jsont.Object.mem "bytes" exact_integer ~enc:bytes
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
