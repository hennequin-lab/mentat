(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let absolute workspace_io path =
  match Mentat_workspace_io.to_abs workspace_io path with
  | Ok absolute -> Ok (Lpath.Abs.to_string absolute)
  | Error error -> Error (Mentat_workspace.Resolve_error.message error)

let absolute_exn workspace_io path =
  match Mentat_workspace_io.to_abs workspace_io path with
  | Ok absolute -> Lpath.Abs.to_string absolute
  | Error _ -> assert false

let provider workspace_io path =
  let current = Mentat_workspace_io.cwd workspace_io in
  match Mentat_workspace.Path.relativize ~root:current path with
  | Some relative when Lpath.Rel.is_root relative -> Ok "."
  | Some relative -> Ok (Lpath.Rel.to_string relative)
  | None -> absolute workspace_io path

let display workspace_io path =
  match provider workspace_io path with
  | Ok address -> address
  | Error _ -> assert false

let display_relative workspace_io ~cwd path =
  match Mentat_workspace.Path.relativize ~root:cwd path with
  | Some relative when Lpath.Rel.is_root relative -> "."
  | Some relative -> Lpath.Rel.to_string relative
  | None -> absolute_exn workspace_io path
