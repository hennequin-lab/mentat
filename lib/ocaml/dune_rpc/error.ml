(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t =
  | Connection_failed of { endpoint : string; message : string }
  | Protocol_error of { message : string; payload : string option }
