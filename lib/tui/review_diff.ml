(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* The review diff pane: the focused file's unified diff with a scope line above
   it, inside a scroll box. The file's hunks lower to one node, so the gutter
   sizes to the widest line number in the file and the content column stays put
   across hunks. The cursor's hunk/line carries the gutter cursor (accent when
   the diff pane holds focus, muted otherwise). A CR anchored outside any hunk
   gets a synthesized context-only view.

   Rebased onto the thin waist: the pane renders one {!Mentat_review.File_diff.t}
   (fetched by the screen for the cursor's focused path) plus the
   {!Mentat_review.Cr.View.t} list, reading orientation from the
   {!Mentat_review.View.t} cursor. The waist carries no per-hunk reviewed state,
   so hunk quieting degrades to whole-file coverage ([View.File.reviewed]) and
   the scope line reports the focused unit's [focus_reviewed]. *)

(* Waist accessors. *)

let path_string = Lpath.Rel.to_string
let fd_path (fd : Mentat_review.File_diff.t) = fd.Mentat_review.File_diff.path

let fd_content (fd : Mentat_review.File_diff.t) =
  fd.Mentat_review.File_diff.content

let fd_before (fd : Mentat_review.File_diff.t) =
  fd.Mentat_review.File_diff.before

let fd_after (fd : Mentat_review.File_diff.t) = fd.Mentat_review.File_diff.after

let file_hunks fd =
  match fd_content fd with
  | Mentat_review.Feature.File.Text hunks -> hunks
  | Mentat_review.Feature.File.Opaque _ -> []

let cr_path (c : Mentat_review.Cr.View.t) =
  c.Mentat_review.Cr.View.ref.Mentat_review.Cr.Ref.path

let cr_line (c : Mentat_review.Cr.View.t) = c.Mentat_review.Cr.View.line
let cr_body (c : Mentat_review.Cr.View.t) = c.Mentat_review.Cr.View.body

let cr_malformed (c : Mentat_review.Cr.View.t) =
  c.Mentat_review.Cr.View.malformed

let cr_resolved (c : Mentat_review.Cr.View.t) = c.Mentat_review.Cr.View.resolved

let cr_recipient (c : Mentat_review.Cr.View.t) =
  c.Mentat_review.Cr.View.recipient

let view_cursor (v : Mentat_review.View.t) = v.Mentat_review.View.cursor

let view_focus_reviewed (v : Mentat_review.View.t) =
  v.Mentat_review.View.focus_reviewed

let file_reviewed_of (v : Mentat_review.View.t) path =
  match
    List.find_opt
      (fun (f : Mentat_review.View.File.t) ->
        String.equal
          (path_string f.Mentat_review.View.File.path)
          (path_string path))
      v.Mentat_review.View.files
  with
  | Some f -> f.Mentat_review.View.File.reviewed
  | None -> false

(* Content text styles. *)

let diff_text_style palette =
  Mosaic.Ansi.Style.make ~fg:(Theme.Palette.muted palette) ()

let dimmed_text_style palette =
  Mosaic.Ansi.Style.make ~fg:(Theme.Palette.faint palette) ()

(* The syntax highlighting seam. The grammar is keyed by the file's name; the
   themed token style lives inside {!Diff_view}, memoized per palette. *)
let language_of_path path =
  Code_highlight.language_of_filename (path_string path)

(* [Mosaic.Diff] declares three record types sharing [side]/[first]/[last], so
   building any at top level would rely on type-directed disambiguation the
   build rejects. Each type gets its own module: within a module only its own
   labels are in scope, so construction and field access stay unambiguous. *)
module Gutter_sign = struct
  type t = Mosaic.Line_number.line_sign = {
    before : string option;
    after : string option;
    before_color : Mosaic.Ansi.Color.t option;
    after_color : Mosaic.Ansi.Color.t option;
  }

  let cursor color =
    {
      before = Some "❯";
      after = None;
      before_color = Some color;
      after_color = None;
    }
end

module Line_color = struct
  type t = Mosaic.Line_number.line_color = {
    gutter : Mosaic.Ansi.Color.t;
    content : Mosaic.Ansi.Color.t option;
  }

  (* A row mark rides the gutter alone. Over the content it would repaint the
     line's add/del background, and on a diff line that background is the
     meaning: an accent wash over a deletion reads brown rather than red. The
     content overlay is spelled out as fully transparent because the diff blends
     a [None] content against the gutter wash, tinting the line's own background
     just the same; only a zero alpha leaves it untouched. *)
  let gutter_mark color =
    { gutter = color; content = Some (Mosaic.Ansi.Color.of_rgba 0 0 0 0) }
end

module Diff_sign = struct
  type t = Mosaic.Diff.line_sign = {
    side : Mosaic.Diff.side;
    first : int;
    last : int;
    sign : Mosaic.Line_number.line_sign;
  }

  let make side n sign = { side; first = n; last = n; sign }
end

module Diff_highlight = struct
  type t = Mosaic.Diff.line_highlight = {
    side : Mosaic.Diff.side;
    first : int;
    last : int;
    color : Mosaic.Line_number.line_color;
  }

  let make side n color = { side; first = n; last = n; color }
end

module Source_line = struct
  type t = Mosaic.Diff.source_line = { side : Mosaic.Diff.side; line : int }

  let side t = t.side
  let line t = t.line
end

(* [Mosaic.Scroll_box] declares [reveal] and [scroll_by] sharing [key]/[x]/[y];
   [reveal] is not the last declaration, so a bare literal would rely on
   type-directed disambiguation. Same wrapper-module discipline as above. *)
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

(* A subtle alpha wash derived from a theme role — no new base colors. The diff
   widget blends it over the line's normal gutter background. *)
let wash base ~alpha =
  let r, g, b = Mosaic.Ansi.Color.to_rgb base in
  Mosaic.Ansi.Color.of_rgba r g b alpha

let cursor_alpha = 56
let compose_alpha = 120

let line_highlight ~base ~alpha side n =
  Diff_highlight.make side n (Line_color.gutter_mark (wash base ~alpha))

let diff_side = function
  | Mentat_review.Scope.Old -> Mosaic.Diff.Old
  | Mentat_review.Scope.New -> Mosaic.Diff.New

let scope_side = function
  | Mosaic.Diff.Old -> Mentat_review.Scope.Old
  | Mosaic.Diff.New -> Mentat_review.Scope.New

(* The cursor in the gutter, on a hunk's first displayed line. It rides the sign
   column's [before] slot, which the diff leaves free for callers. Accent when
   the diff pane holds focus, muted otherwise. *)
let cursor_signs ~color line =
  match line with
  | None -> []
  | Some (side, n) -> [ Diff_sign.make side n (Gutter_sign.cursor color) ]

let hunk_anchor hunk =
  if Textdiff.Hunk.new_count hunk > 0 then
    (Mosaic.Diff.New, Textdiff.Hunk.new_start hunk)
  else (Mosaic.Diff.Old, Textdiff.Hunk.old_start hunk)

(* Cursor target. *)

(* What the cursor points at in the diff pane. [line] is set when the cursor is a
   Line scope (the unit of attention); [hunk] when it is a Hunk scope (the unit
   of coverage). Both drive the gutter cursor and line highlight. *)
type target =
  | File_body of {
      fd : Mentat_review.File_diff.t;
      hunk : Textdiff.Hunk.t option;
      line : (Mosaic.Diff.side * int) option;
    }
  | Cr_anchor of Mentat_review.Cr.View.t * Mentat_review.File_diff.t option
  | Nothing

(* The focused file body matches the cursor's path when supplied; a mismatch
   means the fetch is mid-flight, so nothing renders until it lands. *)
let matching_fd file_diff path =
  match file_diff with
  | Some fd when String.equal (path_string (fd_path fd)) (path_string path) ->
      Some fd
  | Some _ | None -> None

let target view crs file_diff =
  match view_cursor view with
  | Mentat_review.Cursor.Scope scope -> (
      match Mentat_review.Scope.path scope with
      | None -> Nothing
      | Some path -> (
          match matching_fd file_diff path with
          | None -> Nothing
          | Some fd -> (
              match scope with
              | Mentat_review.Scope.Hunk _ ->
                  let hunk =
                    List.find_opt
                      (fun h ->
                        Mentat_review.Scope.equal scope
                          (Mentat_review.Scope.of_hunk ~path h))
                      (file_hunks fd)
                  in
                  File_body { fd; hunk; line = None }
              | Mentat_review.Scope.Line (side, _, n) ->
                  File_body { fd; hunk = None; line = Some (diff_side side, n) }
              | Mentat_review.Scope.Feature | Mentat_review.Scope.File _ ->
                  File_body { fd; hunk = None; line = None })))
  | Mentat_review.Cursor.Cr index -> (
      match List.nth_opt crs index with
      | None -> Nothing
      | Some c -> Cr_anchor (c, matching_fd file_diff (cr_path c)))

(* Scope line. *)

let text ~palette ?style s =
  let style = Option.value style ~default:(Theme.Palette.muted_style palette) in
  Mosaic.text ~style ~wrap:`None ~flex_shrink:0. s

let joined ~palette parts =
  let sep = text ~palette Theme.separator in
  let rec go = function
    | [] -> []
    | [ (s, style) ] -> [ text ~palette ~style s ]
    | (s, style) :: rest -> text ~palette ~style s :: sep :: go rest
  in
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Row
    ~size:{ Mosaic.width = Mosaic.pct 100; height = Mosaic.px 1 }
    (go parts)

let file_counts fd = Textdiff.Hunk.line_counts (file_hunks fd)

let counts_word fd =
  let adds, dels = file_counts fd in
  Printf.sprintf "+%d −%d" adds dels

(* Which re-split hunk holds the cursor's line, by matching the line's number on
   its side against the hunk's lines. *)
let hunk_of_line hunks (side, n) =
  let in_hunk hunk =
    List.exists
      (fun l ->
        match side with
        | Mosaic.Diff.New -> Textdiff.Hunk.Line.new_line l = Some n
        | Mosaic.Diff.Old -> Textdiff.Hunk.Line.old_line l = Some n)
      (Textdiff.Hunk.lines hunk)
  in
  let rec go i = function
    | [] -> None
    | h :: rest -> if in_hunk h then Some i else go (i + 1) rest
  in
  go 0 hunks

(* The scope line reports the file, which hunk the cursor's line sits in
   ([hunk i/N]), the coverage, and the line counts. In the diff the reviewed word
   is the cursor scope's own coverage ([focus_reviewed], so marking the hunk
   flips it); on a nav-focused file it falls back to whole-file coverage. *)
let file_scope_line ~palette view fd cursor_line =
  let muted = Theme.Palette.muted_style palette in
  let path = fd_path fd in
  let reviewed =
    match cursor_line with
    | Some _ -> view_focus_reviewed view
    | None -> file_reviewed_of view path
  in
  let state =
    if reviewed then ("reviewed", Theme.Palette.success_style palette)
    else ("unreviewed", muted)
  in
  let hunk_word =
    match cursor_line with
    | None -> []
    | Some line -> (
        let hunks = file_hunks fd in
        match hunk_of_line hunks line with
        | None -> []
        | Some i ->
            [ (Printf.sprintf "hunk %d/%d" (i + 1) (List.length hunks), muted) ]
        )
  in
  joined ~palette
    (((path_string path, muted) :: hunk_word)
    @ [ state; (counts_word fd, muted) ])

let cr_scope_line ~palette c =
  let muted = Theme.Palette.muted_style palette in
  let location = Printf.sprintf "%s:%d" (path_string (cr_path c)) (cr_line c) in
  let handle =
    if cr_malformed c then "CR"
    else match cr_recipient c with Some h -> "CR " ^ h | None -> "CR"
  in
  let state =
    if cr_malformed c then "malformed"
    else if cr_resolved c then "resolved"
    else "open"
  in
  joined ~palette [ (location, muted); (handle, muted); (state, muted) ]

(* Bodies. *)

let opaque_body ~palette kind =
  let word =
    match kind with `Binary -> "binary file" | `Too_large -> "large file"
  in
  [
    Mosaic.text
      ~style:(Theme.Palette.muted_style palette)
      ~wrap:`Word
      ("  " ^ word ^ " — cannot render as text");
  ]

let cursor_color ~palette ~focused =
  if focused then Theme.Palette.accent palette else Theme.Palette.muted palette

(* The diff widget reports the exact source line clicked; we turn it into a Line
   scope for the screen. The line is the unit of attention, so clicks are lines. *)
let line_click_handler ~path on_line_click =
  match on_line_click with
  | None -> None
  | Some f ->
      Some
        (fun hit ->
          match hit.Mosaic.Diff.source with
          | Some sl ->
              Some
                (f
                   (Mentat_review.Scope.Line
                      ( scope_side (Source_line.side sl),
                        path,
                        Source_line.line sl )))
          | None -> None)

(* The gutter cursor and its subtle highlight sit on the attention line, unless
   the pane is dimmed (a compose dialog owns attention). *)
let cursor_effects ~palette ~focused ~dimmed cursor_line =
  if dimmed then ([], [])
  else
    let signs =
      cursor_signs ~color:(cursor_color ~palette ~focused) cursor_line
    in
    let highlights =
      match cursor_line with
      | Some (side, n) ->
          [
            line_highlight
              ~base:(cursor_color ~palette ~focused)
              ~alpha:cursor_alpha side n;
          ]
      | None -> []
    in
    (signs, highlights)

(* [compose_line] is a [(path_string, line)] anchor; matched by string so an Add
   target's relative path and a CR view's path unify. The anchor carries its own
   gutter cursor in [warning]: it exists only while the compose dialog owns
   attention, which is exactly when {!cursor_effects} withholds the diff's
   cursor, so the landing line would otherwise be marked by a gutter tint alone. *)
let compose_effects ~palette ~path_str compose_line =
  match compose_line with
  | Some (path, n) when String.equal path path_str ->
      let color = Theme.Palette.warning palette in
      ( [ Diff_sign.make Mosaic.Diff.New n (Gutter_sign.cursor color) ],
        [ line_highlight ~base:color ~alpha:compose_alpha Mosaic.Diff.New n ] )
  | _ -> ([], [])

(* One file-level diff node over {!Diff_view}, with review's fixed wrap and
   size. Every consumer computes its own theme, text style, and decorations. *)
let diff_node ~palette ~layout ~theme ~text_style ?language
    ?(line_highlights = []) ~line_signs ?on_line_click source =
  Diff_view.render ~layout ~theme ~text_style
    ~added_emphasis:(Theme.Palette.diff_added_emphasis palette)
    ~removed_emphasis:(Theme.Palette.diff_removed_emphasis palette)
    ~palette ?language ~line_signs ~line_highlights ?on_line_click ~wrap:`Word
    ~size:{ Mosaic.width = Mosaic.pct 100; height = Mosaic.auto }
    source

let body_theme ~palette ~file_reviewed ~dimmed =
  if file_reviewed then Theme.Palette.diff_theme_quieted palette
  else if dimmed then Theme.Palette.diff_theme_dimmed palette
  else Theme.Palette.diff_theme palette

let body_text_style ~palette ~dimmed =
  if dimmed then dimmed_text_style palette else diff_text_style palette

(* The file diff source, at the body's own context or — under [full_context] —
   recomputed at effectively-infinite context so the whole file shows with its
   changed lines highlighted. One node either way. *)
let file_source ~full_context fd hunks =
  if full_context then
    let before = Option.value (fd_before fd) ~default:"" in
    let after = Option.value (fd_after fd) ~default:"" in
    match Textdiff.hunks ~context:max_int ~before ~after () with
    | Some (_ :: _ as hs) -> Diff_view.of_hunks hs
    | None | Some [] -> Diff_view.of_hunks hunks
  else Diff_view.of_hunks hunks

let file_node ~palette ~file_reviewed ~dimmed ~focused ~cursor_line
    ~compose_line ~on_line_click ~layout fd source =
  let path = fd_path fd in
  let cursor_signs, cursor_hl =
    cursor_effects ~palette ~focused ~dimmed cursor_line
  in
  let compose_signs, compose_hl =
    compose_effects ~palette ~path_str:(path_string path) compose_line
  in
  (* No syntax colour while dimmed — the compose dialog owns attention, so the
     diff reads as monochrome-faint. *)
  let language = if dimmed then None else language_of_path path in
  diff_node ~palette ~layout
    ~theme:(body_theme ~palette ~file_reviewed ~dimmed)
    ~text_style:(body_text_style ~palette ~dimmed)
    ?language
    ~line_signs:(cursor_signs @ compose_signs)
    ~line_highlights:(cursor_hl @ compose_hl)
    ?on_line_click:(line_click_handler ~path on_line_click)
    source

(* A CR anchored outside any hunk: a context-only window around the anchor line,
   line numbers on, no add/del backgrounds, always stacked. The anchor line
   keeps its cursor. Without a fetched body (an unchanged file) only the CR text
   is shown. Dimming applies as it does to a file body, but a context window has
   no reviewed coverage to quiet, so its theme is a two-way choice. *)
let cr_context_body ~palette ~focused ~dimmed ~compose_line ~on_line_click c fd
    =
  let line = cr_line c in
  match Option.bind fd fd_after with
  | None ->
      [
        Mosaic.text
          ~style:(Theme.Palette.muted_style palette)
          ~wrap:`Word
          ("  " ^ cr_body c);
      ]
  | Some after ->
      let lines = Array.of_list (String.split_on_char '\n' after) in
      let n = Array.length lines in
      let first = max 1 (line - 6) in
      let last = min n (line + 6) in
      let body =
        List.init
          (max 0 (last - first + 1))
          (fun i ->
            {
              Mosaic.Diff.Patch.tag = Mosaic.Diff.Patch.Context;
              content = lines.(first - 1 + i);
            })
      in
      let hunk =
        {
          Mosaic.Diff.Patch.old_start = first;
          old_lines = List.length body;
          new_start = first;
          new_lines = List.length body;
          lines = body;
        }
      in
      let source = Diff_view.of_patch (Mosaic.Diff.Patch.make [ hunk ]) in
      let cursor_signs, cursor_hl =
        cursor_effects ~palette ~focused ~dimmed (Some (Mosaic.Diff.New, line))
      in
      let compose_signs, compose_hl =
        Option.fold ~none:([], [])
          ~some:(fun fd ->
            compose_effects ~palette
              ~path_str:(path_string (fd_path fd))
              compose_line)
          fd
      in
      let on_click =
        Option.bind fd (fun fd ->
            line_click_handler ~path:(fd_path fd) on_line_click)
      in
      let language =
        if dimmed then None
        else Option.bind fd (fun fd -> language_of_path (fd_path fd))
      in
      [
        diff_node ~palette ~layout:Mosaic.Diff.Unified
          ~theme:
            (if dimmed then Theme.Palette.diff_theme_dimmed palette
             else Theme.Palette.diff_theme palette)
          ~text_style:
            (if dimmed then dimmed_text_style palette
             else diff_text_style palette)
          ?language
          ~line_signs:(cursor_signs @ compose_signs)
          ~line_highlights:(cursor_hl @ compose_hl) ?on_line_click:on_click
          source;
      ]

(* A CR anchored inside a hunk: the file diff, cursor on the anchor line. *)
let anchor_in_hunk fd line =
  List.exists
    (fun hunk ->
      let first = Textdiff.Hunk.new_start hunk in
      let last = first + Textdiff.Hunk.new_count hunk - 1 in
      line >= first && line <= last)
    (file_hunks fd)

(* Auto-scroll. *)

let side_tag = function Mosaic.Diff.Old -> "o" | Mosaic.Diff.New -> "n"

(* A one-shot reveal keyed on the cursor, so it re-fires on every cursor change
   but not after a manual scroll (Scroll_box's reveal contract). The row comes
   from the rendered layout so it is correct across split's blank-alignment
   rows; under [`Word] wrap it is a logical row, imprecise when a line wraps. *)
let reveal_of ~viewport ~layout ~path_str source cursor_line =
  match cursor_line with
  | None -> None
  | Some (side, n) -> (
      match
        Mosaic.Diff.source_line_row (Diff_view.patch source) ~layout
          { Source_line.side; line = n }
      with
      | None -> None
      | Some y ->
          Some
            {
              Reveal.key =
                Printf.sprintf "rl-%s-%s%d" path_str (side_tag side) n;
              x = None;
              y = Some y;
              align_x = `Nearest;
              align_y = `Nearest;
              margin = max 2 (viewport / 3);
            })

(* View. *)

(* The attention line: the Line cursor, else the Hunk cursor's first line. *)
let attention hunk line =
  match line with Some l -> Some l | None -> Option.map hunk_anchor hunk

let nothing_node ~palette =
  Mosaic.text
    ~style:(Theme.Palette.muted_style palette)
    ~wrap:`Word "  nothing to show"

(* The file body node and its reveal share one source, so the reveal row is
   computed from the very patch and layout that render. *)
let file_body ~palette ~view_state ~fd ~cursor_line ~dimmed ~focused ~layout
    ~compose_anchor ~on_line_click ~full_context ~viewport =
  match fd_content fd with
  | Mentat_review.Feature.File.Opaque kind -> (opaque_body ~palette kind, None)
  | Mentat_review.Feature.File.Text hunks ->
      let file_reviewed = file_reviewed_of view_state (fd_path fd) in
      let source = file_source ~full_context fd hunks in
      let node =
        file_node ~palette ~file_reviewed ~dimmed ~focused ~cursor_line
          ~compose_line:compose_anchor ~on_line_click ~layout fd source
      in
      let reveal =
        if full_context then None
        else
          reveal_of ~viewport ~layout
            ~path_str:(path_string (fd_path fd))
            source cursor_line
      in
      ([ node ], reveal)

let view ~palette ?(width = 0) ?height ?(focused = true) ?(dimmed = false)
    ?(layout_pref = Diff_view.Auto) ?compose_anchor ?on_line_click ~crs
    ~file_diff ~full_context view =
  let body_height =
    match height with None -> 20 | Some height -> max 3 (height - 2)
  in
  let layout = Diff_view.resolve layout_pref ~width in
  let scope, body, reveal =
    match target view crs file_diff with
    | Nothing -> (None, [ nothing_node ~palette ], None)
    | File_body { fd; hunk; line } ->
        let cursor_line = attention hunk line in
        let body, reveal =
          file_body ~palette ~view_state:view ~fd ~cursor_line ~dimmed ~focused
            ~layout ~compose_anchor ~on_line_click ~full_context
            ~viewport:body_height
        in
        (Some (file_scope_line ~palette view fd cursor_line), body, reveal)
    | Cr_anchor (c, fd) ->
        let line = cr_line c in
        let in_hunk =
          match fd with Some f -> anchor_in_hunk f line | None -> false
        in
        let body, reveal =
          match fd with
          | Some f when in_hunk ->
              file_body ~palette ~view_state:view ~fd:f
                ~cursor_line:(Some (Mosaic.Diff.New, line))
                ~dimmed ~focused ~layout ~compose_anchor ~on_line_click
                ~full_context ~viewport:body_height
          | _ ->
              ( cr_context_body ~palette ~focused ~dimmed
                  ~compose_line:compose_anchor ~on_line_click c fd,
                None )
        in
        (Some (cr_scope_line ~palette c), body, reveal)
  in
  let scroll =
    Mosaic.scroll_box ~key:"review-diff" ~scroll_y:true ~show_scrollbars:false
      ?reveal
      ~size:{ Mosaic.width = Mosaic.pct 100; height = Mosaic.px body_height }
      body
  in
  (match scope with None -> [] | Some line -> [ line; Mosaic.empty ])
  @ [ scroll ]
