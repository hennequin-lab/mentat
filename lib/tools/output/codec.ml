(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let encode jsont ~text ?(media = []) ?(truncated = false) value =
  match Jsont.Json.encode jsont value with
  | Ok json -> Mentat_tool.Output.make ~text ~json ~media ~truncated ()
  | Error message -> invalid_arg ("Mentat_tools_output.Codec.encode: " ^ message)

let decode jsont output =
  match Mentat_tool.Output.json output with
  | None -> None
  | Some json -> Result.to_option (Jsont.Json.decode jsont json)
