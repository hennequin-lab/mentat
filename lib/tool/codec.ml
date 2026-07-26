(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let decode_invalid_arg f =
  match f () with
  | value -> value
  | exception Invalid_argument message ->
      Jsont.Error.msg Jsont.Meta.none message
