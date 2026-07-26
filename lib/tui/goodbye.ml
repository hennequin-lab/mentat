(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* The public CLI converges on [mentat resume ID] after the staged TUI landing,
   so the restored-terminal hint remains valid during and after the rebase. *)
let resume_command id = "mentat resume " ^ Mentat_session.Id.to_string id

(* One column of left margin keeps the lockup clear of the terminal edge and
   aligns the continuation hint beneath it. *)
let indent = " "

(* Styling each non-empty row independently closes its SGR state before the
   following newline and leaves the surrounding blank rows unstyled. *)
let line ~color style text =
  if color then Mosaic.Ansi.render [ (style, text) ] else text

let render ~palette ~color ~session =
  let lockup =
    List.map
      (fun row ->
        line ~color (Theme.Palette.accent_style palette) (indent ^ row))
      Theme.lockup
  in
  let resume =
    match session with
    | None -> []
    | Some id ->
        [
          "";
          line ~color
            (Theme.Palette.muted_style palette)
            (indent ^ "continue  " ^ resume_command id);
        ]
  in
  "\n" ^ String.concat "\n" (lockup @ resume) ^ "\n\n"
