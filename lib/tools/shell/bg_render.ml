(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Session = Mentat_workspace_io.Command.Session

let status_line = function
  | Session.Running -> "running"
  | Session.Exited (`Exited code) -> "exited " ^ string_of_int code
  | Session.Exited (`Signaled signal) -> "signaled " ^ string_of_int signal
  | Session.Terminated -> "terminated"

let status_keyword = function
  | Session.Running -> "running"
  | Session.Exited (`Exited _) -> "exited"
  | Session.Exited (`Signaled _) -> "signaled"
  | Session.Terminated -> "terminated"

let cap_tail ~max_bytes raw =
  let len = String.length raw in
  if len <= max_bytes then (raw, 0)
  else (String.sub raw (len - max_bytes) max_bytes, len - max_bytes)

(* Byte-exact cursor accounting: cap the raw tail, then repair UTF-8, strip
   ANSI, and (optionally) keep only matching lines — the render pass never
   touches the raw byte counts. *)
let render_stream ?filter ~max_bytes raw =
  let capped, capped_bytes = cap_tail ~max_bytes raw in
  let repaired = Text_helpers.utf8_lossy capped |> Text_helpers.strip_ansi in
  let rendered =
    match filter with
    | None -> repaired
    | Some re ->
        String.split_on_char '\n' repaired
        |> List.filter (Re.execp re)
        |> String.concat "\n"
  in
  (rendered, capped_bytes)

let dropped_note dropped =
  if dropped > 0 then
    Printf.sprintf "... %d bytes rolled off before this read ...\n" dropped
  else ""

let not_found_message handle =
  Printf.sprintf
    "no background process %s in this session (it was never started here, or \
     did not survive a restart)"
    handle
