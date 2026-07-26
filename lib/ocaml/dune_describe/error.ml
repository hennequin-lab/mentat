(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type source = Workspace_describe | Tests_describe

type t =
  | Parse_error of { source : source; offset : int option; message : string }
  | Path_error of { path : string; message : string }
  | Duplicate_library_uid of string
  | Unknown_library_uid of string

let source_text = function
  | Workspace_describe -> "dune describe workspace"
  | Tests_describe -> "dune describe tests"

let message = function
  | Parse_error { source; offset; message } ->
      let where =
        match offset with
        | None -> ""
        | Some offset -> " at byte " ^ string_of_int offset
      in
      source_text source ^ " parse error" ^ where ^ ": " ^ message
  | Path_error { path; message } -> "invalid Dune path " ^ path ^ ": " ^ message
  | Duplicate_library_uid uid -> "duplicate Dune library uid " ^ uid
  | Unknown_library_uid uid -> "unknown Dune library uid " ^ uid

let pp ppf t = Format.pp_print_string ppf (message t)
