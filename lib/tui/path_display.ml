(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let home_relative ~home path =
  match home with
  | None -> Lpath.Abs.to_string path
  | Some home -> (
      match Lpath.Abs.relativize ~root:home path with
      | Some relative when Lpath.Rel.is_root relative -> "~"
      | Some relative -> "~/" ^ Lpath.Rel.to_string relative
      | None -> Lpath.Abs.to_string path)
