(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let failure : Mentat_workspace_io.File_error.t -> Mentat_tool.Result.failure =
  function
  | Mentat_workspace_io.File_error.Not_found _ -> `Not_found
  | Mentat_workspace_io.File_error.Unknown_root _
  | Mentat_workspace_io.File_error.Escapes_workspace _
  | Mentat_workspace_io.File_error.Too_large _ ->
      `Invalid_input
  | Mentat_workspace_io.File_error.Io _ -> `Failed

let message : Mentat_workspace_io.File_error.t -> string = function
  | Mentat_workspace_io.File_error.Unknown_root key ->
      Mentat_workspace.Root.Key.to_string key ^ ": root is not admitted"
  | Mentat_workspace_io.File_error.Not_found path ->
      Mentat_workspace.Path.display path ^ ": path does not exist"
  | Mentat_workspace_io.File_error.Escapes_workspace path ->
      Mentat_workspace.Path.display path ^ ": path resolves outside workspace"
  | Mentat_workspace_io.File_error.Too_large { path; size; max_bytes } ->
      Printf.sprintf "%s: file is too large (%Ld bytes, max %d)"
        (Mentat_workspace.Path.display path)
        size max_bytes
  | Mentat_workspace_io.File_error.Io { path; _ } ->
      Mentat_workspace.Path.display path ^ ": filesystem I/O error"

let failed ?message:tool_message error =
  let text = Option.value tool_message ~default:(message error) in
  Mentat_tool.Result.failed (failure error) text
