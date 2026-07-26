(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* One file change per access. Oversized or pathological inputs render as
   header-plus-omission notes; the limits are display policy, not contract, and
   [max_edit_distance] also bounds the diff computation itself. *)
let limits =
  Textdiff.Limits.make ~max_files:1 ~max_file_bytes:32_768 ~max_lines:4_096
    ~max_edit_distance:10_000 ()

let label path = Textdiff.Label.escaped (Mentat_workspace.Path.display path)

let rendered_diff change =
  let text = Textdiff.to_string (Textdiff.render ~limits [ change ]) in
  if String.length text = 0 then None else Some text

let creation ~path contents =
  Mentat_permission.Request.Change.make
    ?diff:
      (rendered_diff
         (Textdiff.File_change.create ~label:(label path) ~contents))
    ~additions:(Text_helpers.logical_line_count contents)
    ~removals:0 ()

let replacement ~path contents =
  Mentat_permission.Request.Change.make
    ?diff:
      (rendered_diff
         (Textdiff.File_change.create ~label:(label path) ~contents))
    ~additions:(Text_helpers.logical_line_count contents)
    ()

let modify ~path ~before ~after =
  let change = Textdiff.File_change.modify ~label:(label path) ~before ~after in
  let rendered = Textdiff.render ~limits [ change ] in
  let stats = Textdiff.stats rendered in
  let known = Textdiff.omitted rendered = 0 in
  let text = Textdiff.to_string rendered in
  Mentat_permission.Request.Change.make
    ?diff:(if String.length text = 0 then None else Some text)
    ?additions:(if known then Some stats.Textdiff.additions else None)
    ?removals:(if known then Some stats.Textdiff.deletions else None)
    ()
