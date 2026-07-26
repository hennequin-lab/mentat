(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let with_resolved workspace_io spelling k =
  match Mentat_workspace_io.resolve_path workspace_io spelling with
  | Error _ -> []
  | Ok path -> k path

let with_resolved_run workspace_io spelling f =
  match Mentat_workspace_io.resolve_path workspace_io spelling with
  | Error error ->
      Mentat_tool.Result.failed `Invalid_input
        (Mentat_workspace.Resolve_error.message error)
  | Ok path -> f path
