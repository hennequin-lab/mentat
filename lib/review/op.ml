(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t =
  | Add of { path : Lpath.Rel.t; line : int; cr : Cr.t }
  | Replace of { occurrence : Cr.Occurrence.t; cr : Cr.t }
  | Remove of { occurrence : Cr.Occurrence.t }

let path = function
  | Add { path; _ } -> path
  | Replace { occurrence; _ } | Remove { occurrence } ->
      Cr.Occurrence.path occurrence
