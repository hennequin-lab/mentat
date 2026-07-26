(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type root_reason =
  | Does_not_exist
  | Not_a_directory
  | Not_a_directory_or_file
  | Not_accessible

type t =
  | Invalid_root of { spelling : string; reason : root_reason }
  | Broad_root of { field : string; spelling : string }
  | Missing_root of Mentat_workspace.Root.t
  | Io of { operation : string; spelling : string; cause : Eio.Exn.err }

let root_reason_message = function
  | Does_not_exist -> "does not exist"
  | Not_a_directory -> "is not a directory"
  | Not_a_directory_or_file -> "is not a directory or regular file"
  | Not_accessible -> "is not accessible"

let pp ppf = function
  | Invalid_root { spelling; reason } ->
      Format.fprintf ppf "invalid root %S: it %s" spelling
        (root_reason_message reason)
  | Broad_root { field; spelling } ->
      Format.fprintf ppf "%s root %S is too broad; choose a narrower directory"
        field spelling
  | Missing_root root ->
      Format.fprintf ppf "workspace root %a does not name an existing directory"
        Mentat_workspace.Root.pp root
  | Io { operation; spelling; cause } ->
      Format.fprintf ppf "%s %S: %a" operation spelling Eio.Exn.pp_err cause
