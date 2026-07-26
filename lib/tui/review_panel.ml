(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* The review screen frame: a panel over the diff of the worktree against a
   base. A persistent two-pane split — a directory-grouped nav on the left, the
   focused file's diff on the right, a full-height rule between — following the
   panel contract (rule, header, hint) but deliberately waiving the one-column
   law for this surface. Focus (nav or diff) decides which pane the movement
   keys drive and where the accent cursor renders. Below 80 columns the split
   degrades to a single focused pane. The view's cursor is the selection; this
   state holds only view-local orientation.

   Rebased onto the thin waist: the panel reads a {!Mentat_review.View.t}
   summary, the {!Mentat_review.Cr.View.t} list, and the focused
   {!Mentat_review.File_diff.t}; it computes nothing from a whole review value.

   [depth] names the focused pane: [Queue] is nav focus, [Diff] is diff focus.
   The names predate the split and are kept because the screen is wired to
   them. *)

type depth = Queue | Diff
type notice = { text : string; warning : bool }

type state = {
  depth : depth;
  full_context : bool;
  notice : notice option;
  help : bool;
  compose : Review_compose.t option;
}

let init =
  {
    depth = Queue;
    full_context = false;
    notice = None;
    help = false;
    compose = None;
  }

(* Waist accessors. *)

let view_files (v : Mentat_review.View.t) = v.Mentat_review.View.files
let view_base (v : Mentat_review.View.t) = v.Mentat_review.View.base
let view_tip (v : Mentat_review.View.t) = v.Mentat_review.View.tip
let view_cursor (v : Mentat_review.View.t) = v.Mentat_review.View.cursor
let view_units (v : Mentat_review.View.t) = v.Mentat_review.View.units

let view_reviewed_units (v : Mentat_review.View.t) =
  v.Mentat_review.View.reviewed_units

let view_freshness (v : Mentat_review.View.t) =
  v.Mentat_review.View.verdict_freshness

let cursor_has_file (v : Mentat_review.View.t) crs =
  match view_cursor v with
  | Mentat_review.Cursor.Scope scope ->
      Option.is_some (Mentat_review.Scope.path scope)
  | Mentat_review.Cursor.Cr index -> Option.is_some (List.nth_opt crs index)

(* Transitions. *)

(* Focus the diff pane (enter on a nav row). None when already there or the
   cursor has no file to show. *)
let enter state v crs =
  match state.depth with
  | Queue when cursor_has_file v crs -> Some { state with depth = Diff }
  | Queue | Diff -> None

(* The esc ladder: diff focus returns to nav; nav focus returns None so the
   caller closes the panel. *)
let back state =
  match state.depth with
  | Diff -> Some { state with depth = Queue }
  | Queue -> None

let toggle_focus state =
  {
    state with
    depth = (match state.depth with Queue -> Diff | Diff -> Queue);
  }

let set_compose state compose = { state with compose }
let toggle_help state = { state with help = not state.help }
let toggle_context state = { state with full_context = not state.full_context }

let set_notice state ~text ~warning =
  { state with notice = Some { text; warning } }

let clear_notice state = { state with notice = None }

(* Rendering helpers. *)

let hidden_overflow =
  { Mosaic.x = Mosaic.Overflow.Hidden; y = Mosaic.Overflow.Hidden }

let dim ~palette ?(style = Theme.Palette.muted_style palette) line =
  Mosaic.text ~style ~wrap:`Word line

let faint ~palette line =
  Mosaic.text ~style:(Theme.Palette.faint_style palette) ~wrap:`None line

let plain ?style line = Mosaic.text ?style ~wrap:`None ~flex_shrink:0. line
let rule ~palette width = Theme.panel_rule ~palette ?width ()

let frame rows =
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column ~flex_shrink:0.
    ~overflow:hidden_overflow
    ~size:{ Mosaic.width = Mosaic.pct 100; height = Mosaic.auto }
    rows

let spacer = Mosaic.box ~flex_grow:1. []

let verdict_word ~palette v =
  match view_freshness v with
  | `Pending -> ("pending", Theme.Palette.muted_style palette)
  | `Approved -> ("approved", Theme.Palette.success_style palette)
  | `Stale -> ("approved · stale", Theme.Palette.warning_style palette)

(* The loader resolves the base to a full commit hash, so [?range] lets the host
   pass the user's original spec (e.g. "main..worktree") for the header label;
   without it we derive from the view's own labels. *)
let range_label ?range v =
  match range with
  | Some range -> range
  | None -> view_base v ^ ".." ^ view_tip v

(* Header: bold [Review], the muted range, and a right cluster of progress and
   verdict. On the empty state the right cluster drops. *)
let header ~palette ?(counts = true) ?range v =
  let muted = Theme.Palette.muted_style palette in
  let range = range_label ?range v in
  let right =
    if not counts then []
    else
      let progress =
        Printf.sprintf "%d/%d reviewed" (view_reviewed_units v) (view_units v)
      in
      let verdict, verdict_style = verdict_word ~palette v in
      [
        plain ~style:muted progress;
        plain ~style:muted Theme.separator;
        plain ~style:verdict_style verdict;
      ]
  in
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Row
    ~size:{ Mosaic.width = Mosaic.pct 100; height = Mosaic.px 1 }
    ([ plain ~style:Theme.bold "Review"; plain ~style:muted ("  " ^ range) ]
    @ [ spacer ] @ right)

let hint_line_plain ~palette state =
  match state.notice with
  | Some notice ->
      let style =
        if notice.warning then Theme.Palette.warning_style palette
        else Theme.Palette.muted_style palette
      in
      Mosaic.text ~style ~wrap:`None notice.text
  | None ->
      let parts =
        match state.depth with
        | Queue ->
            [
              "tab focus diff";
              "space mark";
              "enter open";
              "c comment";
              "a approve";
              "esc close";
            ]
        | Diff ->
            [
              "tab focus nav";
              "space mark hunk";
              "c comment";
              "]/[ hunk";
              "ctrl+o context";
              "esc nav";
            ]
      in
      faint ~palette (String.concat Theme.separator parts)

let hint_line ~palette state =
  match state.compose with
  | Some compose ->
      let verb =
        match Review_compose.target compose with
        | Review_compose.Add _ -> "enter add CR"
        | Review_compose.Edit _ -> "enter save CR"
        | Review_compose.Resolve _ -> "enter resolve CR"
      in
      faint ~palette (String.concat Theme.separator [ verb; "esc cancel" ])
  | None -> hint_line_plain ~palette state

(* Help table. *)

let help_rows =
  [
    ("tab", "switch focus (nav / diff)");
    ("↑/↓, j/k", "move selection / hunk");
    ("]/[", "next / previous hunk (diff)");
    ("enter", "focus the diff pane");
    ("space", "mark reviewed and advance");
    ("n / p", "next / previous CR");
    ("c / e", "add / edit CR");
    ("x / d", "resolve / remove CR");
    ("a", "toggle approved / pending");
    ("ctrl+o", "cycle diff context");
    ("?", "toggle this table");
    ("esc", "back / close");
  ]

let help_table ~palette () =
  List.map
    (fun (key, description) ->
      faint ~palette ("  " ^ Theme.pad_right 14 key ^ description))
    help_rows

(* Body. *)

(* The human base label: the part before ".." of the range override, else the
   view's own base. *)
let base_label ?range v =
  match range with
  | Some range -> (
      match String.index_opt range '.' with
      | Some i -> String.sub range 0 i
      | None -> range)
  | None -> view_base v

let empty_line ~palette ?range v =
  dim ~palette
    ("  no changes to review — the worktree matches " ^ base_label ?range v)

(* Split layout. *)

let split_min = 80
let nav_width width = min 32 (max 20 (width * 2 / 5))

let separator ~palette height =
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column ~flex_shrink:0.
    ~size:{ Mosaic.width = Mosaic.px 1; height = Mosaic.px height }
    (List.init height (fun _ ->
         Mosaic.text
           ~style:(Theme.Palette.rule_style palette)
           ~wrap:`None Theme.v_separator))

(* Both panes are exact boxes, never grown or shrunk to fit. The diff pane's
   width has to be definite: everything under it sizes itself in percentages,
   and a percentage of an indefinite box falls back to the content's own width,
   which lets one long diff line set the width of a split diff's two halves and
   push the new side outside the clip. *)
let pane ~width ~height rows =
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column ~flex_grow:0.
    ~flex_shrink:0. ~overflow:hidden_overflow
    ~size:{ Mosaic.width = Mosaic.px width; height = Mosaic.px height }
    rows

let split ~palette ~width ~height ~nav ~diff =
  let nav_w = nav_width width in
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Row
    ~size:{ Mosaic.width = Mosaic.pct 100; height = Mosaic.px height }
    [
      pane ~width:nav_w ~height nav;
      separator ~palette height;
      pane ~width:(max 1 (width - nav_w - 1)) ~height diff;
    ]

(* Compose dialog. *)

let compose_cr_anchor (c : Mentat_review.Cr.View.t) =
  ( Lpath.Rel.to_string c.Mentat_review.Cr.View.ref.Mentat_review.Cr.Ref.path,
    c.Mentat_review.Cr.View.line )

(* The line the CR will land on, as a [(path_string, line)] pair the diff pane
   highlights. *)
let compose_anchor state =
  match state.compose with
  | None -> None
  | Some compose -> (
      match Review_compose.target compose with
      | Review_compose.Add { path; line } ->
          Some (Lpath.Rel.to_string path, line)
      | Review_compose.Edit c | Review_compose.Resolve c ->
          Some (compose_cr_anchor c))

(* The composer is a compact opaque dialog floating over the center of the
   dimmed panes. Absolutely positioned with an inset from its fixed size, so
   both panes keep their full height behind it. *)
let dialog_overlay ~palette ~width ~height ?on_compose_edit ?on_compose_submit
    compose =
  let dialog_w = Review_compose.dialog_width in
  let dialog_h = Review_compose.height compose in
  let left = max 0 ((width - dialog_w) / 2) in
  let top = max 0 ((height - dialog_h) / 2) in
  Mosaic.box ~position:Mosaic.Position.Absolute
    ~inset:
      (Mosaic.inset_lrtb left
         (max 0 (width - left - dialog_w))
         top
         (max 0 (height - top - dialog_h)))
    ~z_index:10
    [
      Review_compose.view ~palette ?on_edit:on_compose_edit
        ?on_submit:on_compose_submit compose;
    ]

(* Views. *)

let view ~palette ?width ?height ?range ?layout_pref ?on_click ?on_line_click
    ?on_compose_edit ?on_compose_submit ~crs ~file_diff state v =
  if view_files v = [] then
    frame
      [
        rule ~palette width;
        header ~palette ~counts:false ?range v;
        faint ~palette "esc close";
        Mosaic.empty;
        empty_line ~palette ?range v;
      ]
  else
    let composing = Option.is_some state.compose in
    let width_px = Option.value width ~default:Theme.default_rule_width in
    let total = Option.value height ~default:24 in
    (* Chrome is rule, header, blank, and the bottom legend — four rows. *)
    let pane_h = max 3 (total - 4) in
    let nav_focused = state.depth = Queue in
    let anchor = compose_anchor state in
    let nav_w = if width_px < split_min then width_px else nav_width width_px in
    (* The diff region: the whole width single-pane, else what remains after the
       nav pane and the one-column separator. It resolves the diff's layout. *)
    let diff_width =
      if width_px < split_min then width_px else width_px - nav_w - 1
    in
    let nav () =
      Review_rows.view ~palette ~width:nav_w ~height:pane_h
        ~focused:(nav_focused && not composing)
        ~dimmed:composing ?on_click ~crs v
    in
    let diff () =
      Review_diff.view ~palette ~width:diff_width ~height:pane_h
        ~focused:((not nav_focused) && not composing)
        ~dimmed:composing ?layout_pref ?compose_anchor:anchor ?on_line_click
        ~crs ~file_diff ~full_context:state.full_context v
    in
    let overlay =
      match state.compose with
      | None -> []
      | Some compose ->
          [
            dialog_overlay ~palette ~width:width_px ~height:total
              ?on_compose_edit ?on_compose_submit compose;
          ]
    in
    let body =
      if state.help then help_table ~palette ()
      else if width_px < split_min then if nav_focused then nav () else diff ()
      else
        [
          split ~palette ~width:width_px ~height:pane_h ~nav:(nav ())
            ~diff:(diff ());
        ]
    in
    Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column ~flex_shrink:0.
      ~overflow:hidden_overflow
      ~size:{ Mosaic.width = Mosaic.pct 100; height = Mosaic.px total }
      ([ rule ~palette width; header ~palette ?range v; Mosaic.empty ]
      @ body
      @ [ spacer; hint_line ~palette state ]
      @ overlay)

let loading_view ~palette ?width ?height:_ () =
  frame
    [
      rule ~palette width;
      Mosaic.box ~flex_direction:Mosaic.Flex_direction.Row
        ~size:{ Mosaic.width = Mosaic.pct 100; height = Mosaic.px 1 }
        [
          plain ~style:Theme.bold "Review";
          spacer;
          plain ~style:(Theme.Palette.muted_style palette) "computing…";
        ];
      Mosaic.empty;
    ]

let error_view ~palette ?width ?height:_ ~message () =
  frame
    [
      rule ~palette width;
      plain ~style:Theme.bold "Review";
      Mosaic.text
        ~style:(Theme.Palette.error_style palette)
        ~wrap:`Word (Theme.problem ^ message);
      faint ~palette "esc close";
    ]
