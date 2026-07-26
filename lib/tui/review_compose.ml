(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* The CR compose dialog is a plain filled ~60-column box: a muted title, a
   painted single-line draft with an accent cursor glyph, and an error line — no
   rules or border. The panel floats it over the center of the dimmed panes. The
   draft carries the CR grammar itself; parsing happens on submit (in the
   screen), and parse or write problems render as a [!] line under the input
   with the draft preserved.

   The input is app-owned and painted, not a native widget: the review screen
   owns its keyboard, so the screen folds keys into the draft via [append] and
   [backspace] and this module paints the insertion point. *)

type target =
  | Add of { path : Lpath.Rel.t; line : int }
  | Edit of Mentat_review.Cr.View.t
  | Resolve of Mentat_review.Cr.View.t

type t = { target : target; draft : string; problem : string option }

let make ~target ~draft = { target; draft; problem = None }
let target t = t.target
let draft t = t.draft
let with_problem t problem = { t with problem = Some problem }
let with_draft t draft = { t with draft; problem = None }

let cr_location (c : Mentat_review.Cr.View.t) =
  Printf.sprintf "%s:%d"
    (Lpath.Rel.to_string c.Mentat_review.Cr.View.ref.Mentat_review.Cr.Ref.path)
    c.Mentat_review.Cr.View.line

(* [Edit]/[Resolve] carry the CR view itself, so the affordance reads its
   location directly rather than re-resolving a live index that a background
   refresh may have shifted onto a different CR. *)
let affordance t =
  match t.target with
  | Add { path; line } ->
      Printf.sprintf "CR on %s:%d" (Lpath.Rel.to_string path) line
  | Edit c -> "edit CR on " ^ cr_location c
  | Resolve c -> "resolve CR on " ^ cr_location c

let line ?style text = Mosaic.text ?style ~wrap:`None ~flex_shrink:0. text
let dialog_width = 60

(* Enter submits the CR, shift+enter inserts a newline for a multi-line body;
   every other editing gesture — word motions, ctrl+w word-delete, kill-to-line —
   is the textarea's own default. *)
let key_bindings =
  let binding = Mosaic_ui.Textarea.key_binding in
  [
    binding "return" Mosaic_ui.Textarea.Submit;
    binding "enter" Mosaic_ui.Textarea.Submit;
    binding ~shift:true "return" Mosaic_ui.Textarea.Newline;
    binding ~shift:true "enter" Mosaic_ui.Textarea.Newline;
  ]

(* A plain filled dialog: an opaque background with padding, a muted title, the
   editable input, and — on a parse or write failure — a [! …] problem line. No
   rules or border; the fill and padding are the frame. The input is a real
   {!Mosaic.textarea} carrying its own cursor and editing keys; [on_edit] mirrors
   each keystroke into the draft and [on_submit] fires on enter. *)
let view ~palette ?width:_ ?on_edit ?on_submit t =
  let problem =
    match t.problem with
    | None -> []
    | Some message ->
        [ line ~style:(Theme.Palette.error_style palette) ("! " ^ message) ]
  in
  let input =
    Mosaic.textarea ~key:"review-compose-input" ~id:"review-cr-compose"
      ~autofocus:true ~value:t.draft ~placeholder:"handle: comment"
      ~placeholder_color:(Theme.Palette.faint palette)
      ~wrap:`None ~key_bindings
      ~size:{ Mosaic.width = Mosaic.pct 100; height = Mosaic.auto }
      ~min_size:{ Mosaic.width = Mosaic.auto; height = Mosaic.px 1 }
      ~max_size:{ Mosaic.width = Mosaic.auto; height = Mosaic.px 1 }
      ~on_input:(fun value -> Option.map (fun f -> f value) on_edit)
      ~on_submit:(fun value -> Option.map (fun f -> f value) on_submit)
      ()
  in
  let rows =
    [ line ~style:(Theme.Palette.muted_style palette) (affordance t); input ]
    @ problem
  in
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column ~flex_shrink:0.
    ~box_sizing:Mosaic.Box_sizing.Border_box
    ~background:(Theme.Palette.overlay palette)
    ~padding:(Mosaic.padding_lrtb 2 2 1 1)
    ~size:
      {
        Mosaic.width = Mosaic.px dialog_width;
        height = Mosaic.px (List.length rows + 2);
      }
    rows

(* The dialog's row count (title + input + padding, plus an error line), so the
   panel reserves exactly that much space at the diff-pane bottom. *)
let height t = 4 + match t.problem with None -> 0 | Some _ -> 1
