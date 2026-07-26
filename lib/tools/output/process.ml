(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid message =
  invalid_arg ("Mentat_tools_output.Process.make: " ^ message)

type stream = Stdout | Stderr

type termination =
  | Exited of int
  | Signaled of int
  | Timed_out
  | Stopped
  | Output_limit of { stream : stream; limit : int }
  | Supervision_failed

type t = { termination : termination; duration_ms : int }

let make ~termination ~duration_ms =
  if duration_ms < 0 then invalid "duration_ms must be non-negative";
  (match termination with
  | Exited code when code < 0 -> invalid "exit code must be non-negative"
  | Output_limit { limit; _ } when limit < 0 ->
      invalid "output limit must be non-negative"
  | Output_limit _ | Exited _ | Signaled _ | Timed_out | Stopped
  | Supervision_failed ->
      ());
  { termination; duration_ms }

let termination t = t.termination
let duration_ms t = t.duration_ms

let stream_jsont =
  Jsont.enum ~kind:"process stream" [ ("stdout", Stdout); ("stderr", Stderr) ]

let termination_to_parts = function
  | Exited code -> (`Exited, Some code, None, None)
  | Signaled signal -> (`Signaled, Some signal, None, None)
  | Timed_out -> (`Timed_out, None, None, None)
  | Stopped -> (`Stopped, None, None, None)
  | Output_limit { stream; limit } ->
      (`Output_limit, None, Some stream, Some limit)
  | Supervision_failed -> (`Supervision_failed, None, None, None)

let termination_jsont =
  let kind =
    Jsont.enum ~kind:"process termination"
      [
        ("exited", `Exited);
        ("signaled", `Signaled);
        ("timed_out", `Timed_out);
        ("stopped", `Stopped);
        ("output_limit", `Output_limit);
        ("supervision_failed", `Supervision_failed);
      ]
  in
  let make kind code stream limit =
    match (kind, code, stream, limit) with
    | `Exited, Some code, None, None -> Exited code
    | `Signaled, Some signal, None, None -> Signaled signal
    | `Timed_out, None, None, None -> Timed_out
    | `Stopped, None, None, None -> Stopped
    | `Output_limit, None, Some stream, Some limit ->
        Output_limit { stream; limit }
    | `Supervision_failed, None, None, None -> Supervision_failed
    | _ -> Jsont.Error.msg Jsont.Meta.none "inconsistent process termination"
  in
  Jsont.Object.map ~kind:"process termination" make
  |> Jsont.Object.mem "kind" kind ~enc:(fun t ->
      let kind, _, _, _ = termination_to_parts t in
      kind)
  |> Jsont.Object.opt_mem "code" Jsont.int ~enc:(fun t ->
      let _, code, _, _ = termination_to_parts t in
      code)
  |> Jsont.Object.opt_mem "stream" stream_jsont ~enc:(fun t ->
      let _, _, stream, _ = termination_to_parts t in
      stream)
  |> Jsont.Object.opt_mem "limit" Jsont.int ~enc:(fun t ->
      let _, _, _, limit = termination_to_parts t in
      limit)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let jsont =
  let make version termination duration_ms =
    Mentat_tool.Codec.decode_invalid_arg (fun () ->
        if version <> 1 then invalid "unsupported version";
        make ~termination ~duration_ms)
  in
  Jsont.Object.map ~kind:"process output" make
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun _ -> 1)
  |> Jsont.Object.mem "termination" termination_jsont ~enc:termination
  |> Jsont.Object.mem "duration_ms" Jsont.int ~enc:duration_ms
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
