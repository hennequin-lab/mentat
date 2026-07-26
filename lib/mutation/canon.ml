(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let rec strictly_sorted compare = function
  | [] | [ _ ] -> true
  | a :: (b :: _ as rest) -> compare a b < 0 && strictly_sorted compare rest

let unique_paths paths =
  let rec loop seen = function
    | [] -> true
    | path :: rest ->
        (not (Mentat_workspace.Path.Set.mem path seen))
        && loop (Mentat_workspace.Path.Set.add path seen) rest
  in
  loop Mentat_workspace.Path.Set.empty paths
