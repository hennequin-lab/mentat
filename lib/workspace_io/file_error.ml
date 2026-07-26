(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t =
  | Unknown_root of Mentat_workspace.Root.Key.t
  | Not_found of Mentat_workspace.Path.t
  | Escapes_workspace of Mentat_workspace.Path.t
  | Too_large of {
      path : Mentat_workspace.Path.t;
      size : int64;
      max_bytes : int;
    }
  | Io of { path : Mentat_workspace.Path.t; cause : Eio.Exn.err }

let pp ppf = function
  | Unknown_root key ->
      Format.fprintf ppf "workspace does not admit root %a"
        Mentat_workspace.Root.Key.pp key
  | Not_found path ->
      Format.fprintf ppf "%s: path does not exist"
        (Mentat_workspace.Path.display path)
  | Escapes_workspace path ->
      Format.fprintf ppf "%s: path resolves outside its workspace root"
        (Mentat_workspace.Path.display path)
  | Too_large { path; size; max_bytes } ->
      Format.fprintf ppf "%s: file is %Ld bytes, over the %d-byte read bound"
        (Mentat_workspace.Path.display path)
        size max_bytes
  | Io { path; cause } ->
      Format.fprintf ppf "%s: %a"
        (Mentat_workspace.Path.display path)
        Eio.Exn.pp_err cause
