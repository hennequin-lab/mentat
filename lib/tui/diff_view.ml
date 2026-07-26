(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* One diff renderer over Mosaic's split/unified diff renderable. It lowers a
   structured or textual diff to a {!Mosaic.Diff.Patch.t}, resolves the layout,
   builds the syntax highlight through a palette-keyed memo, and forwards every
   decoration. Word emphasis is computed once at lowering time, stored on the
   source, and drawn as per-side {!Mosaic.Diff.line_span} backgrounds. *)

type layout_pref = Auto | Unified | Split

(* [Auto] follows opencode's 120-column threshold applied to the diff region;
   [Split] sits above the 100-column floor below which two ~44-column content
   columns stop being readable. *)
let auto_split_min = 120
let split_floor = 100

let resolve pref ~width =
  match pref with
  | Unified -> Mosaic.Diff.Unified
  | Auto ->
      if width >= auto_split_min then Mosaic.Diff.Split else Mosaic.Diff.Unified
  | Split ->
      if width >= split_floor then Mosaic.Diff.Split else Mosaic.Diff.Unified

(* Source. *)

type emphasis_span = {
  side : Mosaic.Diff.side;
  line : int;
  start_byte : int;
  end_byte : int;
}

type source =
  | Lowered of Mosaic.Diff.Patch.t * emphasis_span list
  | Note of string

(* Patch lowering. *)

let patch_tag = function
  | Textdiff.Hunk.Line.Context -> Mosaic.Diff.Patch.Context
  | Textdiff.Hunk.Line.Added -> Mosaic.Diff.Patch.Added
  | Textdiff.Hunk.Line.Removed -> Mosaic.Diff.Patch.Removed

let patch_line line =
  {
    Mosaic.Diff.Patch.tag = patch_tag (Textdiff.Hunk.Line.kind line);
    content = Textdiff.Hunk.Line.text line;
  }

let patch_hunk hunk =
  {
    Mosaic.Diff.Patch.old_start = Textdiff.Hunk.old_start hunk;
    old_lines = Textdiff.Hunk.old_count hunk;
    new_start = Textdiff.Hunk.new_start hunk;
    new_lines = Textdiff.Hunk.new_count hunk;
    lines = List.map patch_line (Textdiff.Hunk.lines hunk);
  }

(* A changed line-pair diffs to at most this many token edits before word
   emphasis is dropped, so one very long changed line cannot dominate settle
   time. *)
let emphasis_edit_distance = 200

(* Emphasis for one hunk: within each change block the i-th removed line pairs
   with the i-th added line, and the differing word ranges land on the removed
   line's old-side number and the added line's new-side number. *)
let emphasis_of_hunk hunk =
  let spans = ref [] in
  let removed = ref [] in
  let added = ref [] in
  let emit side number ranges =
    match number with
    | None -> ()
    | Some line ->
        List.iter
          (fun (start_byte, end_byte) ->
            spans := { side; line; start_byte; end_byte } :: !spans)
          ranges
  in
  let flush () =
    let rec pair removed_lines added_lines =
      match (removed_lines, added_lines) with
      | r :: removed_rest, a :: added_rest ->
          let removed_ranges, added_ranges =
            Textdiff.word_spans ~max_edit_distance:emphasis_edit_distance
              ~before:(Textdiff.Hunk.Line.text r)
              ~after:(Textdiff.Hunk.Line.text a)
              ()
          in
          emit Mosaic.Diff.Old (Textdiff.Hunk.Line.old_line r) removed_ranges;
          emit Mosaic.Diff.New (Textdiff.Hunk.Line.new_line a) added_ranges;
          pair removed_rest added_rest
      | [], _ | _, [] -> ()
    in
    pair (List.rev !removed) (List.rev !added);
    removed := [];
    added := []
  in
  List.iter
    (fun line ->
      match Textdiff.Hunk.Line.kind line with
      | Textdiff.Hunk.Line.Removed -> removed := line :: !removed
      | Textdiff.Hunk.Line.Added -> added := line :: !added
      | Textdiff.Hunk.Line.Context -> flush ())
    (Textdiff.Hunk.lines hunk);
  flush ();
  List.rev !spans

let of_hunks hunks =
  let spans = List.concat_map emphasis_of_hunk hunks in
  match Mosaic.Diff.Patch.make (List.map patch_hunk hunks) with
  | patch -> Lowered (patch, spans)
  | exception Invalid_argument _ ->
      (* Well-formed hunks always validate; the guard keeps the constructor
         total and preserves the evidence as rendered text if one ever does not. *)
      Note (String.concat "" (List.map Textdiff.Hunk.render hunks))

let of_unified s =
  match Mosaic.Diff.Patch.of_unified s with
  | Ok patch when not (Mosaic.Diff.Patch.is_empty patch) -> Lowered (patch, [])
  | Ok _ | Error _ -> Note s

let of_patch patch = Lowered (patch, [])

let patch = function
  | Lowered (patch, _) -> patch
  | Note _ -> Mosaic.Diff.Patch.make []

(* Syntax seam. *)

(* The diff shares the one code palette and grammar ladder with the transcript's
   fenced code. It is highlighter-backed because the diff synthesizes its buffer
   from patch rows; only the token style is themed, so the highlighter itself is
   palette-independent. *)
let highlighter =
  Mosaic.Code.Highlighter.sync
    (fun { Mosaic.Code.Highlighter.content; language } ->
      match Code_highlight.grammar language with
      | Some highlight -> Mosaic.Syntax_highlight.of_triples (highlight content)
      | None -> Mosaic.Syntax_highlight.of_triples [])

(* Mosaic compares a diff's [highlight] by physical equality when deciding
   whether to re-parse, so the per-language value is memoized on palette
   identity ({!Prims.per_palette}): an unchanged frame reuses one record and a
   runtime theme swap rebuilds the cache under the new palette. *)
let highlight_builder =
  Prims.per_palette (fun palette ->
      let style =
        Code_highlight.style ~palette
          ~base:(Mosaic.Ansi.Style.make ~fg:(Theme.Palette.muted palette) ())
      in
      let cache : (string, Mosaic.Diff.highlight) Hashtbl.t =
        Hashtbl.create 4
      in
      fun language ->
        match Hashtbl.find_opt cache language with
        | Some highlight -> highlight
        | None ->
            let syntax =
              Mosaic.Diff.syntax ~language ~style ~conceal:false highlighter
            in
            let highlight = { Mosaic.Diff.old = syntax; new_ = syntax } in
            Hashtbl.add cache language highlight;
            highlight)

let highlight_for ~palette ?language () =
  match language with
  | None -> None
  | Some language -> Some (highlight_builder palette language)

(* [Mosaic.Diff]'s line_span, line_sign, line_highlight, and source_line records
   all carry a [side] field, so a bare literal would rely on type-directed
   disambiguation. Re-export the record here so its labels resolve unambiguously,
   the same wrapper-module discipline the review diff uses. *)
module Line_span = struct
  type t = Mosaic.Diff.line_span = {
    side : Mosaic.Diff.side;
    line : int;
    start_byte : int;
    end_byte : int;
    color : Mosaic.Ansi.Color.t;
  }
end

(* Emphasis seam. Word emphasis is a pure function of the patch, stored on the
   source; each stored span lowers to a {!Mosaic.Diff.line_span} coloured by its
   side — [removed_emphasis] on the old side, [added_emphasis] on the new — and
   the list forwards to {!Mosaic.diff} as [line_spans]. *)
let emphasis_line_spans ~emphasis ~added_emphasis ~removed_emphasis spans =
  if not emphasis then []
  else
    List.map
      (fun { side; line; start_byte; end_byte } ->
        let color =
          match side with
          | Mosaic.Diff.Old -> removed_emphasis
          | Mosaic.Diff.New -> added_emphasis
        in
        { Line_span.side; line; start_byte; end_byte; color })
      spans

(* Rendering. *)

let note_lines text =
  match List.rev (String.split_on_char '\n' text) with
  | "" :: rest -> List.rev rest
  | lines -> List.rev lines

let note_node ~text_style text =
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column
    ~size:{ Mosaic.width = Mosaic.pct 100; height = Mosaic.auto }
    (List.map
       (fun line -> Mosaic.text ~style:text_style ~wrap:`Word line)
       (note_lines text))

let render ~layout ~theme ~text_style ~added_emphasis ~removed_emphasis ~palette
    ?language ?(emphasis = true) ?(line_signs = []) ?(line_highlights = [])
    ?on_line_click ?(wrap = `Word) ?size source =
  match source with
  | Note text -> note_node ~text_style text
  | Lowered (patch, spans) ->
      let line_spans =
        emphasis_line_spans ~emphasis ~added_emphasis ~removed_emphasis spans
      in
      let highlight = highlight_for ~palette ?language () in
      Mosaic.diff ~layout ~theme ?highlight ~show_line_numbers:true ~wrap
        ~line_signs ~line_highlights ~line_spans ~text_style ?on_line_click
        ?size patch
