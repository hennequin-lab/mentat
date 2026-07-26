(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t =
  | Outside_workspace of Lpath.Abs.t
  | Invalid_input of Lpath.Error.t
  | Unknown_root of Root.Key.t

let message = function
  | Outside_workspace path ->
      Format.asprintf "path is outside workspace: %a" Lpath.Abs.pp path
  | Invalid_input error -> Lpath.Error.message error
  | Unknown_root key ->
      "workspace does not admit root " ^ Root.Key.to_string key

let equal a b =
  match (a, b) with
  | Outside_workspace a, Outside_workspace b -> Lpath.Abs.equal a b
  | Invalid_input a, Invalid_input b -> Lpath.Error.equal a b
  | Unknown_root a, Unknown_root b -> Root.Key.equal a b
  | (Outside_workspace _ | Invalid_input _ | Unknown_root _), _ -> false

let pp ppf error = Format.pp_print_string ppf (message error)
