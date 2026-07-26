(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let failure : Mentat_edit.Error.t -> Mentat_tool.Result.failure = function
  | Mentat_edit.Error.Conflict _ -> `Stale
  | Mentat_edit.Error.State_mismatch { expected = `Text; actual = `Missing; _ }
    ->
      `Not_found
  | Mentat_edit.Error.State_mismatch _ | Mentat_edit.Error.Invalid_text _
  | Mentat_edit.Error.Duplicate_path _ | Mentat_edit.Error.Too_large _
  | Mentat_edit.Error.Invalid_target _ | Mentat_edit.Error.Read_only_path _
  | Mentat_edit.Error.Protected_path _ ->
      `Invalid_input
  | Mentat_edit.Error.Workspace
      ( _,
        ( Mentat_workspace.Resolve_error.Outside_workspace _
        | Mentat_workspace.Resolve_error.Invalid_input _
        | Mentat_workspace.Resolve_error.Unknown_root _ ) ) ->
      `Invalid_input
  | Mentat_edit.Error.Out_of_workspace _ | Mentat_edit.Error.Io _ -> `Failed

let target_kind = function
  | `Missing -> "missing"
  | `Text -> "text"
  | `Other -> "other"

let message : Mentat_edit.Error.t -> string = function
  | Mentat_edit.Error.Invalid_text (None, reason) -> reason
  | Mentat_edit.Error.Invalid_text (Some path, reason) ->
      Mentat_workspace.Path.display path ^ ": " ^ reason
  | Mentat_edit.Error.Duplicate_path path ->
      Mentat_workspace.Path.display path ^ ": duplicate edit target"
  | Mentat_edit.Error.State_mismatch
      { path; expected = `Text; actual = `Missing; _ } ->
      Mentat_workspace.Path.display path ^ ": path does not exist"
  | Mentat_edit.Error.State_mismatch { path; expected; actual } ->
      Printf.sprintf "%s: expected %s, found %s"
        (Mentat_workspace.Path.display path)
        (target_kind expected) (target_kind actual)
  | Mentat_edit.Error.Too_large { path; size; max_size } ->
      Printf.sprintf "%s: file is too large (%Ld bytes, max %Ld)"
        (Mentat_workspace.Path.display path)
        size max_size
  | Mentat_edit.Error.Invalid_target { path; reason } ->
      Printf.sprintf "%s: %s"
        (Mentat_workspace.Path.display path)
        (Mentat_edit.Error.Target_error.message reason)
  | Mentat_edit.Error.Conflict { path; _ } ->
      Mentat_workspace.Path.display path ^ ": stale write"
  | Mentat_edit.Error.Workspace (None, error) ->
      Mentat_workspace.Resolve_error.message error
  | Mentat_edit.Error.Workspace (Some path, error) ->
      Mentat_workspace.Path.display path
      ^ ": "
      ^ Mentat_workspace.Resolve_error.message error
  | Mentat_edit.Error.Out_of_workspace path ->
      Mentat_workspace.Path.display path
      ^ ": edit target is no longer inside the workspace"
  | Mentat_edit.Error.Read_only_path path ->
      Mentat_workspace.Path.display path
      ^ ": edit target belongs to a read-only workspace root"
  | Mentat_edit.Error.Protected_path (path, name) ->
      Printf.sprintf
        "%s: %s is protected workspace metadata and cannot be modified by \
         tools; change sandbox and workspace policy through configuration or \
         the CLI instead"
        (Mentat_workspace.Path.display path)
        name
  | Mentat_edit.Error.Io (None, _) -> "filesystem I/O error"
  | Mentat_edit.Error.Io (Some path, _) ->
      Mentat_workspace.Path.display path ^ ": filesystem I/O error"

let failed error = Mentat_tool.Result.failed (failure error) (message error)
let failed_apply error = failed (Mentat_edit.Apply_error.error error)

let applied workspace_io edit ~output =
  match Mentat_workspace_io.Edit.apply workspace_io edit with
  | Error error -> failed_apply error
  | Ok _ -> Mentat_tool.Result.completed ~output ()
