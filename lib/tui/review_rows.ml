(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* The review nav pane: a directory-grouped tree of changed files, each file's
   CR comments always visible as children beneath it. Path-ordered (dirs then
   files sorted); one level of `▾ <dir>` grouping, no nested collapsing. The
   diff pane shows whatever the model cursor selects; this pane renders that
   selection and, when [on_click] is supplied, emits a cursor target per row.

   Rebased onto the thin review waist: it reads a {!Mentat_review.View.t}
   summary and the {!Mentat_review.Cr.View.t} occurrence list rather than a
   whole review value. The CR list is in occurrence order, so a CR's list index
   is its {!Mentat_review.Cursor.Cr} index. *)

(* Waist accessors. *)

let path_string = Lpath.Rel.to_string
let file_path (f : Mentat_review.View.File.t) = f.Mentat_review.View.File.path

let file_status (f : Mentat_review.View.File.t) =
  f.Mentat_review.View.File.status

let file_reviewed (f : Mentat_review.View.File.t) =
  f.Mentat_review.View.File.reviewed

let cr_path (c : Mentat_review.Cr.View.t) =
  c.Mentat_review.Cr.View.ref.Mentat_review.Cr.Ref.path

let cr_line (c : Mentat_review.Cr.View.t) = c.Mentat_review.Cr.View.line
let cr_body (c : Mentat_review.Cr.View.t) = c.Mentat_review.Cr.View.body

let cr_malformed (c : Mentat_review.Cr.View.t) =
  c.Mentat_review.Cr.View.malformed

let cr_resolved (c : Mentat_review.Cr.View.t) = c.Mentat_review.Cr.View.resolved
let occ_path_string c = path_string (cr_path c)
let occ_in_file c path = String.equal (occ_path_string c) (path_string path)

(* Cursor. *)

(* A file row is selected when the cursor sits anywhere in that file — its file
   scope or one of its hunks — since the nav has no hunk rows. *)
let file_selected (cursor : Mentat_review.Cursor.t) path =
  match cursor with
  | Mentat_review.Cursor.Scope scope -> (
      match Mentat_review.Scope.path scope with
      | Some p -> String.equal (path_string p) (path_string path)
      | None -> false)
  | Mentat_review.Cursor.Cr _ -> false

let cr_selected (cursor : Mentat_review.Cursor.t) index =
  match cursor with
  | Mentat_review.Cursor.Cr i -> Int.equal i index
  | Mentat_review.Cursor.Scope _ -> false

(* Tree. *)

(* [grouped] is the one level of depth the tree has: a row under a `▾ <dir>`
   header indents past it. Without it a root-level file — which gets no header —
   renders at the same depth as the previous group's children and reads as one
   of them. *)
type row =
  | Group of string (* A `▾ <dir>` header; not selectable. *)
  | File of {
      path : Lpath.Rel.t;
      status : Mentat_review.Feature.File.status;
      selected : bool;
      reviewed : bool;
      grouped : bool;
    }
  | Cr of {
      cr : Mentat_review.Cr.View.t;
      index : int;
      selected : bool;
      grouped : bool;
    }

let cursor_of_row = function
  | File f ->
      Some (Mentat_review.Cursor.Scope (Mentat_review.Scope.File f.path))
  | Cr c -> Some (Mentat_review.Cursor.Cr c.index)
  | Group _ -> None

type leaf =
  | File_leaf of Mentat_review.View.File.t
  | Cr_leaf of int * Mentat_review.Cr.View.t

let leaf_path = function
  | File_leaf file -> path_string (file_path file)
  | Cr_leaf (_, c) -> occ_path_string c

let dir_of path = match Filename.dirname path with "." -> "" | dir -> dir

let file_row cursor ~grouped file =
  File
    {
      path = file_path file;
      status = file_status file;
      selected = file_selected cursor (file_path file);
      reviewed = file_reviewed file;
      grouped;
    }

let cr_row cursor ~grouped index c =
  Cr { cr = c; index; selected = cr_selected cursor index; grouped }

(* A file's CR children: every occurrence anchored in it, source order. *)
let cr_children cursor ~grouped indexed file =
  let path = file_path file in
  indexed
  |> List.filter (fun (_, c) -> occ_in_file c path)
  |> List.stable_sort (fun (_, a) (_, b) -> Int.compare (cr_line a) (cr_line b))
  |> List.map (fun (index, c) -> cr_row cursor ~grouped index c)

let build (view : Mentat_review.View.t) ~crs =
  let cursor = view.Mentat_review.View.cursor in
  let files =
    List.stable_sort
      (fun a b ->
        String.compare (path_string (file_path a)) (path_string (file_path b)))
      view.Mentat_review.View.files
  in
  let feature_paths = List.map (fun f -> path_string (file_path f)) files in
  let indexed = crs |> List.mapi (fun index c -> (index, c)) in
  (* CRs anchored outside any changed file become their own leaves, grouped
     under their own directory. *)
  let outside =
    List.filter
      (fun (_, c) -> not (List.mem (occ_path_string c) feature_paths))
      indexed
  in
  let leaves =
    List.map (fun f -> File_leaf f) files
    @ List.map (fun (i, c) -> Cr_leaf (i, c)) outside
  in
  let leaves =
    List.stable_sort
      (fun a b -> String.compare (leaf_path a) (leaf_path b))
      leaves
  in
  let rec emit current = function
    | [] -> []
    | leaf :: rest ->
        let dir = dir_of (leaf_path leaf) in
        let grouped = not (String.equal dir "") in
        let header =
          if grouped && not (String.equal dir current) then [ Group dir ]
          else []
        in
        let body =
          match leaf with
          | File_leaf file ->
              file_row cursor ~grouped file
              :: cr_children cursor ~grouped indexed file
          | Cr_leaf (index, c) -> [ cr_row cursor ~grouped index c ]
        in
        header @ body @ emit dir rest
  in
  emit "\000" leaves

let is_selected = function
  | File f -> f.selected
  | Cr c -> c.selected
  | Group _ -> false

let selected_index rows =
  let rec find i = function
    | [] -> None
    | row :: rest -> if is_selected row then Some i else find (i + 1) rest
  in
  find 0 rows

(* The nav's own rows carry the cursor between files and their CR children; the
   [▾ dir] headers and — deliberately — the diff's hunks, which the nav never
   shows, are not stops. A cursor sitting anywhere in a file (its file scope or
   one of its hunks) selects that file's row, so stepping from inside the diff
   still lands on the neighbouring file rather than the next hunk. *)
let nav_step view ~crs direction =
  let rows =
    build view ~crs |> List.filter (function Group _ -> false | _ -> true)
  in
  let count = List.length rows in
  if count = 0 then None
  else
    let selected = selected_index rows in
    let index =
      match (selected, direction) with
      | _, `First -> 0
      | _, `Last -> count - 1
      | None, (`Next | `Previous) -> 0
      | Some i, `Next -> min (count - 1) (i + 1)
      | Some i, `Previous -> max 0 (i - 1)
    in
    Option.bind (List.nth_opt rows index) cursor_of_row

(* Rendering. *)

let status_cell ~palette = function
  | Mentat_review.Feature.File.Added ->
      ("A", Theme.Palette.success_style palette)
  | Mentat_review.Feature.File.Modified ->
      ("M", Theme.Palette.warning_style palette)
  | Mentat_review.Feature.File.Deleted ->
      ("D", Theme.Palette.error_style palette)

let mark_cell ~palette reviewed =
  let glyph = if reviewed then Theme.todo_done else Theme.todo_pending in
  let style =
    if reviewed then Theme.Palette.success_style palette
    else Mosaic.Ansi.Style.default
  in
  (glyph ^ " ", style)

let basename path = Filename.basename (path_string path)

(* A malformed occurrence keeps its raw source text as its body, so the nav
   shows [! <raw>] for it; a well-formed CR shows its rendered body. *)
let cr_text c = if cr_malformed c then Theme.problem ^ cr_body c else cr_body c
let spacer = Mosaic.box ~flex_grow:1. []

(* Everything dims to faint while a compose dialog owns the screen. *)
let dim ~palette ~dimmed style =
  if dimmed then Theme.Palette.faint_style palette else style

(* The cursor glyph: accent when its pane holds focus, muted otherwise, faint
   while dimmed. *)
let cursor_cell ~palette ~focused ~dimmed selected =
  let style =
    if dimmed then Theme.Palette.faint_style palette
    else if focused then Theme.Palette.accent_style palette
    else Theme.Palette.muted_style palette
  in
  Mosaic.text ~style ~wrap:`None ~flex_shrink:0.
    (if selected then Theme.cursor else Theme.cursor_blank)

let on_mouse_of on_click row =
  match (on_click, cursor_of_row row) with
  | Some f, Some cursor ->
      Some
        (fun ev ->
          match Mosaic.Event.Mouse.kind ev with
          | Mosaic.Event.Mouse.Down { button = Mosaic.Event.Mouse.Left } ->
              Some (f cursor)
          | _ -> None)
  | _ -> None

let row_box ?on_mouse nodes =
  Mosaic.box ?on_mouse ~flex_direction:Mosaic.Flex_direction.Row
    ~size:{ Mosaic.width = Mosaic.pct 100; height = Mosaic.px 1 }
    nodes

(* One level of depth: a grouped row hangs under its `▾ <dir>` header. *)
let indent ~grouped base = if grouped then base ^ "  " else base

let render ~palette ~focused ~dimmed ~on_click row =
  match row with
  | Group dir ->
      Mosaic.text
        ~style:(dim ~palette ~dimmed (Theme.Palette.muted_style palette))
        ~wrap:`None ~truncate:true
        (" " ^ Theme.tree_group ^ " " ^ dir)
  | File f ->
      let mark, mark_style = mark_cell ~palette f.reviewed in
      let letter, letter_style = status_cell ~palette f.status in
      let name_style =
        if f.selected && focused then Theme.Palette.accent_style palette
        else Mosaic.Ansi.Style.default
      in
      row_box ?on_mouse:(on_mouse_of on_click row)
        [
          Mosaic.text ~wrap:`None ~flex_shrink:0.
            (indent ~grouped:f.grouped " ");
          cursor_cell ~palette ~focused ~dimmed f.selected;
          Mosaic.text
            ~style:(dim ~palette ~dimmed mark_style)
            ~wrap:`None ~flex_shrink:0. mark;
          (* The name is what yields when the row runs out of width. [wrap:`None]
             leaves a text no break opportunity, so its automatic minimum is its
             whole content and [flex_shrink] alone never makes it give way: a
             long name keeps its full width and pushes the status letter past
             the pane's clip, losing the letter rather than the tail of a name
             the reviewer can already see truncated. The letter carries its own
             leading space so the two cannot abut once the spacer is spent. *)
          Mosaic.text
            ~style:(dim ~palette ~dimmed name_style)
            ~wrap:`None ~truncate:true ~flex_shrink:1.
            ~min_size:{ Mosaic.width = Mosaic.px 0; height = Mosaic.px 0 }
            (basename f.path);
          spacer;
          Mosaic.text
            ~style:(dim ~palette ~dimmed letter_style)
            ~wrap:`None ~flex_shrink:0. (" " ^ letter ^ " ");
        ]
  | Cr c ->
      let malformed = cr_malformed c.cr in
      let base =
        if malformed then Theme.Palette.error_style palette
        else if cr_resolved c.cr then Theme.Palette.faint_style palette
        else Theme.Palette.muted_style palette
      in
      let style =
        if dimmed then Theme.Palette.faint_style palette
        else if c.selected && focused then Theme.Palette.accent_style palette
        else base
      in
      row_box ?on_mouse:(on_mouse_of on_click row)
        [
          Mosaic.text ~wrap:`None ~flex_shrink:0.
            (indent ~grouped:c.grouped "   ");
          cursor_cell ~palette ~focused ~dimmed c.selected;
          Mosaic.text ~style ~wrap:`None ~truncate:true ~flex_shrink:1.
            (cr_text c.cr);
          spacer;
        ]

(* [Mosaic.Scroll_box] declares [reveal] and [scroll_by] sharing [key]/[x]/[y];
   [reveal] is not the last declaration, so a bare literal would rely on
   type-directed disambiguation. Its own module keeps construction unambiguous. *)
module Reveal = struct
  type t = Mosaic.Scroll_box.reveal = {
    key : string;
    x : int option;
    y : int option;
    align_x : Mosaic.Scroll_box.reveal_align;
    align_y : Mosaic.Scroll_box.reveal_align;
    margin : int;
  }
end

(* A one-shot reveal keyed on the selected row, so keyboard navigation re-scrolls
   the selection into view while a manual wheel scroll (which does not change the
   selection) is left untouched — the same contract the diff pane follows. *)
let reveal_of rows =
  match selected_index rows with
  | None -> None
  | Some i ->
      Some
        {
          Reveal.key = Printf.sprintf "review-nav-%d" i;
          x = None;
          y = Some i;
          align_x = `Nearest;
          align_y = `Nearest;
          margin = 1;
        }

let view ~palette ?width:_ ?height ?(focused = true) ?(dimmed = false) ?on_click
    ~crs view =
  let rows = build view ~crs in
  let height_px =
    match height with None -> 16 | Some height -> max 1 height
  in
  (* The rows live in a scroll box so the mouse wheel scrolls the nav pane; the
     reveal keeps the keyboard selection on screen. *)
  [
    Mosaic.scroll_box ~key:"review-nav" ~scroll_y:true ~show_scrollbars:false
      ?reveal:(reveal_of rows)
      ~size:{ Mosaic.width = Mosaic.pct 100; height = Mosaic.px height_px }
      (List.map (render ~palette ~focused ~dimmed ~on_click) rows);
  ]
