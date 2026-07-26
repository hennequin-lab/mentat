(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let merlin_failure error =
  let kind =
    match error with
    | Merlin.Unavailable _ -> `Unavailable
    | Merlin.Timed_out -> `Timed_out
    | Merlin.Cancelled | Merlin.Signaled _ | Merlin.Exited _
    | Merlin.Output_exceeded _ | Merlin.Supervision_failed _
    | Merlin.Incomplete_output _ | Merlin.Query_failure _ | Merlin.Malformed _
      ->
        `Failed
  in
  Mentat_tool.Result.failed kind (Merlin.error_message error)

let workspace_path_of_absolute workspace_io ~source_root ~cwd absolute =
  let spelling = Lpath.Abs.to_string absolute in
  match Mentat_workspace_io.resolve_path workspace_io spelling with
  | Ok path -> Some path
  | Error _ -> (
      match Lpath.Abs.relativize ~root:cwd absolute with
      | None -> None
      | Some relative ->
          Some
            (Mentat_workspace.Path.make
               ~root_key:(Mentat_workspace.Path.root_key source_root)
               relative))

let load_source workspace_io path ~max_bytes =
  match Mentat_workspace_io.File.stat workspace_io path with
  | Error error -> Error (Fs_error.failed error)
  | Ok stat -> (
      match stat.Eio.File.Stat.kind with
      | `Regular_file -> (
          match Mentat_workspace_io.File.load workspace_io path ~max_bytes with
          | Ok source -> Ok source
          | Error error -> Error (Fs_error.failed error))
      | kind ->
          Error
            (Mentat_tool.Result.failed `Invalid_input
               (Mentat_workspace.Path.display path
               ^ ": expected a regular file, found " ^ Stat_kind.kind_name kind
               )))

let resolve_source workspace_io ~path ~max_bytes =
  match Mentat_workspace_io.resolve_path workspace_io path with
  | Error error ->
      Error
        (Mentat_tool.Result.failed `Invalid_input
           (Mentat_workspace.Resolve_error.message error))
  | Ok path -> (
      match load_source workspace_io path ~max_bytes with
      | Ok source -> Ok (path, source)
      | Error e -> Error e)
