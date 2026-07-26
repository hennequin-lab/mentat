(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let contains haystack needle = String.includes ~affix:needle haystack
let has needle screen = contains screen needle

(* A real child canonicalizes its own working directory, so a PTY frame can
   carry the resolved spelling of a root the harness only ever names logically.
   Both fold to the same marker; on hosts where [/tmp] is a real directory the
   two spellings coincide and the extra rule is a no-op. *)
let normalize ~project screen =
  let root = Project.root project in
  let canonical = Project.canonical_root project in
  let extra =
    if String.equal canonical root then []
    else
      [
        {
          Mentat_test_censor.re = Re.str canonical;
          marker = Mentat_test_censor.Fixed "$TESTCASE_ROOT";
        };
      ]
  in
  Mentat_test_censor.apply ~extra ~cwd:root screen

let print ~project screen =
  normalize ~project screen |> String.split_on_char '\n'
  |> List.iteri (fun index line ->
      Printf.printf "%02d | %s\n" (index + 1) (Util.rstrip line))
