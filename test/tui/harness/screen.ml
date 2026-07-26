(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let contains haystack needle = String.includes ~affix:needle haystack
let has needle screen = contains screen needle

let normalize ~project screen =
  let root = Project.root project in
  Mentat_test_censor.apply ~cwd:root screen

let print ~project screen =
  normalize ~project screen |> String.split_on_char '\n'
  |> List.iteri (fun index line ->
      Printf.printf "%02d | %s\n" (index + 1) (Util.rstrip line))
