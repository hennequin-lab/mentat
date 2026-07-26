(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Key = Matrix.Input.Key
module Modifier = Matrix.Input.Modifier

(* Commands. *)

(* The review-state effects the screen requests; the application interprets each
   through [Mentat_client] (review_state / review_diff / review_crs queries,
   [review] for a state command, [review_compose] for a CR edit) and feeds the
   result back through the completion messages below. Cursor, marks, and verdict
   live server-side: a navigation or mark is an [Apply] command followed by a
   [Query_state] re-read, so the screen renders only what the waist returns. *)
type command =
  | Query_state of Mentat_review.Scope.t
  | Query_diff of Lpath.Rel.t
  | Query_crs
  | Apply of Mentat_review.Command.t
  | Compose of Mentat_review.Cr.Edit.t

(* Messages. *)

type msg =
  (* Runtime-built completions. *)
  | State_loaded of (Mentat_review.View.t, string) result
  | Diff_loaded of
      Lpath.Rel.t * (Mentat_review.File_diff.t option, string) result
  | Crs_loaded of (Mentat_review.Cr.View.t list, string) result
  | Command_done of (unit, string) result
  | Compose_done of (unit, string) result
  (* User input. *)
  | Moved of Mentat_review.Cursor.move
  | Nav_moved of [ `Next | `Previous | `First | `Last ]
  | Line_moved of [ `Next | `Previous ]
  | Hunk_jumped of [ `Next | `Previous ]
  | Focus_toggled
  | Nav_clicked of Mentat_review.Cursor.t
  | Line_clicked of Mentat_review.Scope.t
  | Entered
  | Back
  | Space_pressed
  | Verdict_toggled
  | Help_toggled
  | Context_toggled
  | Compose_started of [ `Add | `Edit | `Resolve ]
  | Compose_edited of string
  | Compose_cancelled
  | Compose_submitted of string
  | Remove_requested
  | Close_pressed

let state_loaded result = State_loaded result
let diff_loaded path result = Diff_loaded (path, result)
let crs_loaded result = Crs_loaded result
let command_done result = Command_done result
let compose_done result = Compose_done result

type verb =
  [ `Toggle
  | `Verdict
  | `Help
  | `Compose of [ `Add | `Edit | `Resolve ]
  | `Remove
  | `Next_hunk
  | `Prev_hunk
  | `Next_cr
  | `Prev_cr ]

(* The screen verbs the shell resolves through the command registry map onto the
   same internal messages the bare letters produced; these are not focus-aware,
   so the registry owns their chord and the screen owns their effect. *)
let verb = function
  | `Toggle -> Space_pressed
  | `Verdict -> Verdict_toggled
  | `Help -> Help_toggled
  | `Compose which -> Compose_started which
  | `Remove -> Remove_requested
  | `Next_hunk -> Hunk_jumped `Next
  | `Prev_hunk -> Hunk_jumped `Previous
  | `Next_cr -> Moved Mentat_review.Cursor.Next_cr
  | `Prev_cr -> Moved Mentat_review.Cursor.Previous_cr

(* State. *)

type open_state = {
  view : Mentat_review.View.t;
  crs : Mentat_review.Cr.View.t list;
  file_diff : Mentat_review.File_diff.t option;
      (* The focused file body, or None while a fetch is in flight. *)
  diff_path : Lpath.Rel.t option;
      (* The path [file_diff] is (being) fetched for. *)
  panel : Review_panel.state;
  pending_notice : string option;
      (* Settle wording for the in-flight CR mutation, e.g. "CR removed". *)
}

type t = Loading of Lpath.Rel.t option | Failed of string | Open of open_state
type event = Stay of command list | Close of command list

(* On open, land the cursor onto a requested [focus] file when it is among the
   changed files, else off the whole-feature scope onto the first unit (so space
   and enter act immediately); then load the state, CR views, and — once the
   state names a focused file — its diff body. *)
let create ?focus () =
  (* The initial open is read-only — state and CR views only. Landing on the
     focus file or the first unit is a follow-up write serialized after the first
     read (see {!on_state}), so no command races the loader's first load. *)
  (Loading focus, [ Query_state Mentat_review.Scope.Feature; Query_crs ])

(* Waist accessors. *)

let cr_path (c : Mentat_review.Cr.View.t) =
  c.Mentat_review.Cr.View.ref.Mentat_review.Cr.Ref.path

let cr_line (c : Mentat_review.Cr.View.t) = c.Mentat_review.Cr.View.line
let cr_body (c : Mentat_review.Cr.View.t) = c.Mentat_review.Cr.View.body
let cr_ref (c : Mentat_review.Cr.View.t) = c.Mentat_review.Cr.View.ref
let cr_resolved (c : Mentat_review.Cr.View.t) = c.Mentat_review.Cr.View.resolved

let cr_malformed (c : Mentat_review.Cr.View.t) =
  c.Mentat_review.Cr.View.malformed

let view_cursor (v : Mentat_review.View.t) = v.Mentat_review.View.cursor

let view_freshness (v : Mentat_review.View.t) =
  v.Mentat_review.View.verdict_freshness

let view_focus_reviewed (v : Mentat_review.View.t) =
  v.Mentat_review.View.focus_reviewed

let view_focus (v : Mentat_review.View.t) = v.Mentat_review.View.focus
let view_files (v : Mentat_review.View.t) = v.Mentat_review.View.files
let same_path a b = String.equal (Lpath.Rel.to_string a) (Lpath.Rel.to_string b)

(* The path the cursor focuses: a scope's path, or a CR's file. *)
let focus_path (s : open_state) =
  match view_cursor s.view with
  | Mentat_review.Cursor.Scope scope -> Mentat_review.Scope.path scope
  | Mentat_review.Cursor.Cr index ->
      Option.map cr_path (List.nth_opt s.crs index)

(* A CR occupies its own comment line, so a diff-pane click that lands on that
   new-side line selects the CR rather than the bare line — making the CR the
   target for edit, resolve, and remove. The CR view reports its one-based
   new-side source line. *)
let cr_index_at s scope =
  match scope with
  | Mentat_review.Scope.Line (Mentat_review.Scope.New, path, line) ->
      let rec find i = function
        | [] -> None
        | c :: rest ->
            if same_path (cr_path c) path && Int.equal (cr_line c) line then
              Some i
            else find (i + 1) rest
      in
      find 0 s.crs
  | _ -> None

(* The hunks of the focused file, when its body is loaded. The waist serves them
   at git's context (see [Workspace_io.git]), so two edits a few lines apart are
   already two hunks — the pane and the marks share this one hunk model. *)
let focus_hunks (s : open_state) =
  match s.file_diff with
  | Some fd -> (
      match fd.Mentat_review.File_diff.content with
      | Mentat_review.Feature.File.Text hunks -> hunks
      | Mentat_review.Feature.File.Opaque _ -> [])
  | None -> []

(* Line navigation. *)

(* The displayed lines of the focused file, in hunk order: added and context
   lines on the new side, removed lines on the old side. *)
let line_targets s =
  List.concat_map
    (fun hunk ->
      List.filter_map
        (fun line ->
          match Textdiff.Hunk.Line.new_line line with
          | Some n -> Some (Mentat_review.Scope.New, n)
          | None ->
              Option.map
                (fun n -> (Mentat_review.Scope.Old, n))
                (Textdiff.Hunk.Line.old_line line))
        (Textdiff.Hunk.lines hunk))
    (focus_hunks s)

let hunk_first_changed_line ~path hunk =
  let lines = Textdiff.Hunk.lines hunk in
  let changed =
    List.find_opt
      (fun line ->
        match Textdiff.Hunk.Line.kind line with
        | Textdiff.Hunk.Line.Added | Textdiff.Hunk.Line.Removed -> true
        | Textdiff.Hunk.Line.Context -> false)
      lines
  in
  let line =
    match changed with Some line -> Some line | None -> List.nth_opt lines 0
  in
  match line with
  | None -> None
  | Some line -> (
      match Textdiff.Hunk.Line.new_line line with
      | Some n ->
          Some (Mentat_review.Scope.Line (Mentat_review.Scope.New, path, n))
      | None ->
          Option.map
            (fun n ->
              Mentat_review.Scope.Line (Mentat_review.Scope.Old, path, n))
            (Textdiff.Hunk.Line.old_line line))

let containing_hunk s ~path scope =
  List.find_opt
    (fun hunk ->
      Mentat_review.Scope.contains
        (Mentat_review.Scope.of_hunk ~path hunk)
        scope)
    (focus_hunks s)

(* Position of the cursor among the file's line targets. *)
let line_index s path targets =
  let find side line =
    let rec go i = function
      | [] -> None
      | (side', n) :: rest ->
          if
            Int.equal n line
            &&
            match (side', side) with
            | Mentat_review.Scope.New, Mentat_review.Scope.New
            | Mentat_review.Scope.Old, Mentat_review.Scope.Old ->
                true
            | _ -> false
          then Some i
          else go (i + 1) rest
    in
    go 0 targets
  in
  match view_cursor s.view with
  | Mentat_review.Cursor.Scope scope -> (
      match scope with
      | Mentat_review.Scope.Line (side, _, line) -> find side line
      | Mentat_review.Scope.Hunk _ ->
          Option.bind (containing_hunk s ~path scope) (fun hunk ->
              Option.bind (hunk_first_changed_line ~path hunk) (function
                | Mentat_review.Scope.Line (side, _, line) -> find side line
                | _ -> None))
      | _ -> None)
  | Mentat_review.Cursor.Cr index ->
      Option.bind (List.nth_opt s.crs index) (fun c ->
          find Mentat_review.Scope.New (cr_line c))

(* The line the diff cursor should step to. From a non-line cursor the first step
   lands on the file's first changed line rather than moving blindly. *)
let line_step_target s direction =
  match focus_path s with
  | None -> None
  | Some path ->
      let targets = line_targets s in
      if List.is_empty targets then None
      else
        let last = List.length targets - 1 in
        let index =
          match line_index s path targets with
          | Some index -> (
              match direction with
              | `Next -> min last (index + 1)
              | `Previous -> max 0 (index - 1))
          | None -> 0
        in
        let side, line = List.nth targets index in
        Some (Mentat_review.Scope.Line (side, path, line))

(* The first changed line of the hunk adjacent to the cursor's hunk. *)
let hunk_jump_target s direction =
  match focus_path s with
  | None -> None
  | Some path ->
      let hunks = focus_hunks s in
      if List.is_empty hunks then None
      else
        let containing =
          match view_cursor s.view with
          | Mentat_review.Cursor.Scope scope ->
              let rec index i = function
                | [] -> None
                | hunk :: rest ->
                    let hs = Mentat_review.Scope.of_hunk ~path hunk in
                    if
                      Mentat_review.Scope.contains hs scope
                      || Mentat_review.Scope.equal hs scope
                    then Some i
                    else index (i + 1) rest
              in
              index 0 hunks
          | Mentat_review.Cursor.Cr _ -> None
        in
        let last = List.length hunks - 1 in
        let index =
          match (containing, direction) with
          | Some i, `Next -> min last (i + 1)
          | Some i, `Previous -> max 0 (i - 1)
          | None, _ -> 0
        in
        Option.bind (List.nth_opt hunks index) (hunk_first_changed_line ~path)

(* Entering the diff seeds the line cursor at the first changed line of the nav's
   selected file (or the selected hunk), so the pane opens with a line in hand
   rather than nothing highlighted. A cursor already on a line — or on a CR — is
   left where it is. *)
let seed_target s =
  match view_cursor s.view with
  | Mentat_review.Cursor.Cr _ -> None
  | Mentat_review.Cursor.Scope scope -> (
      match scope with
      | Mentat_review.Scope.Line _ | Mentat_review.Scope.Feature -> None
      | Mentat_review.Scope.Hunk { path; _ } ->
          Option.bind
            (containing_hunk s ~path scope)
            (hunk_first_changed_line ~path)
      | Mentat_review.Scope.File path -> (
          match focus_hunks s with
          | hunk :: _ -> hunk_first_changed_line ~path hunk
          | [] -> None))

(* Marks. *)

(* Space marks the unit of coverage: the hunk. A line cursor marks its
   containing hunk; a hunk or file cursor toggles itself. *)
let space_scope s =
  match view_cursor s.view with
  | Mentat_review.Cursor.Cr _ -> None
  | Mentat_review.Cursor.Scope scope -> (
      match scope with
      | Mentat_review.Scope.Feature -> None
      | Mentat_review.Scope.File _ | Mentat_review.Scope.Hunk _ ->
          Some (scope, `Toggle)
      | Mentat_review.Scope.Line (_, path, _) ->
          Option.map
            (fun hunk -> (Mentat_review.Scope.of_hunk ~path hunk, `Mark))
            (containing_hunk s ~path scope))

(* CR anchoring. *)

(* An old-side (removed) line does not exist in the worktree, so a CR anchors on
   the new-side line now sitting where it was. *)
let worktree_line s ~path side line =
  match side with
  | Mentat_review.Scope.New -> Some line
  | Mentat_review.Scope.Old ->
      let scope = Mentat_review.Scope.Line (side, path, line) in
      Option.bind (containing_hunk s ~path scope) (fun hunk ->
          let lines = Textdiff.Hunk.lines hunk in
          let rec find = function
            | [] -> None
            | candidate :: rest -> (
                match Textdiff.Hunk.Line.old_line candidate with
                | Some n when Int.equal n line -> (
                    match Textdiff.Hunk.Line.new_line candidate with
                    | Some new_line -> Some new_line
                    | None -> List.find_map Textdiff.Hunk.Line.new_line rest)
                | _ -> find rest)
          in
          match find lines with
          | Some new_line -> Some new_line
          | None ->
              Some (Textdiff.Hunk.new_start hunk + Textdiff.Hunk.new_count hunk))

let hunk_add_anchor s ~path hunk =
  match hunk_first_changed_line ~path hunk with
  | Some (Mentat_review.Scope.Line (side, path, line)) ->
      Option.map (fun line -> (path, line)) (worktree_line s ~path side line)
  | Some _ | None -> Some (path, Textdiff.Hunk.new_start hunk)

(* Where a new CR anchors: the selected line, hunk, file, or the selected CR. *)
let add_anchor s =
  match view_cursor s.view with
  | Mentat_review.Cursor.Cr index ->
      Option.map (fun c -> (cr_path c, cr_line c)) (List.nth_opt s.crs index)
  | Mentat_review.Cursor.Scope scope -> (
      match scope with
      | Mentat_review.Scope.Feature -> None
      | Mentat_review.Scope.Line (side, path, line) ->
          Option.map
            (fun line -> (path, line))
            (worktree_line s ~path side line)
      | Mentat_review.Scope.Hunk { path; new_start; _ } -> (
          match containing_hunk s ~path scope with
          | Some hunk -> hunk_add_anchor s ~path hunk
          | None -> Some (path, new_start))
      | Mentat_review.Scope.File path -> (
          match focus_hunks s with
          | hunk :: _ -> hunk_add_anchor s ~path hunk
          | [] -> Some (path, 1)))

let cursor_cr s =
  match view_cursor s.view with
  | Mentat_review.Cursor.Cr index ->
      Option.map (fun c -> (index, c)) (List.nth_opt s.crs index)
  | Mentat_review.Cursor.Scope _ -> None

(* CR draft parsing. *)

(* Explicit CR/XCR text is parsed verbatim, a leading handle-colon addresses a
   recipient, and a bare body becomes [CR: body]. *)
let explicit_cr_draft text =
  let starts_token token =
    String.equal text token
    || String.starts_with ~prefix:(token ^ ":") text
    || String.starts_with ~prefix:(token ^ " ") text
  in
  starts_token "CR" || starts_token "CR-soon" || starts_token "XCR"

let parse_draft draft =
  let trimmed = String.trim draft in
  let message error = Format.asprintf "%a" Mentat_review.Cr.Error.pp error in
  if String.equal trimmed "" then Error "the CR body must not be empty"
  else if explicit_cr_draft trimmed then
    Result.map_error message (Mentat_review.Cr.parse trimmed)
  else
    let fallback () =
      Result.map_error message (Mentat_review.Cr.make ~body:trimmed ())
    in
    match String.index_opt trimmed ':' with
    | None -> fallback ()
    | Some i -> (
        let head = String.sub trimmed 0 i in
        let body = String.sub trimmed (i + 1) (String.length trimmed - i - 1) in
        match Mentat_review.Cr.Handle.of_string head with
        | Ok recipient ->
            Result.map_error message (Mentat_review.Cr.make ~recipient ~body ())
        | Error _ -> fallback ())

let compose_edit compose =
  match parse_draft (Review_compose.draft compose) with
  | Error _ as error -> error
  | Ok cr -> (
      match Review_compose.target compose with
      | Review_compose.Add { path; line } ->
          Ok (Mentat_review.Cr.Edit.Add { path; line; cr })
      | Review_compose.Edit c | Review_compose.Resolve c ->
          Ok (Mentat_review.Cr.Edit.Replace { ref = cr_ref c; cr }))

(* Update helpers. *)

(* Clear the settle notice on any deliberate navigation. *)
let clear_notice s = { s with panel = Review_panel.clear_notice s.panel }

(* After a state change, submit the command and re-read the state at
   [Scope.Feature]; {!on_state} learns the moved cursor from the reply and refines
   the focus read, then fetches the focused diff body if the focus file moved. *)
let apply_and_reload command =
  [ Apply command; Query_state Mentat_review.Scope.Feature ]

(* Switch to diff focus with the line cursor seeded onto the file's first changed
   line (see {!seed_target}); a cursor already on a line stays put. [s] carries
   the panel already flipped to {!Review_panel.Diff}. *)
let enter_diff s =
  match seed_target s with
  | Some scope ->
      ( Open s,
        Stay
          (apply_and_reload
             (Mentat_review.Command.Set_cursor
                (Mentat_review.Cursor.Scope scope))) )
  | None -> (Open s, Stay [])

let notify s ~text ~warning =
  Open { s with panel = Review_panel.set_notice s.panel ~text ~warning }

(* The command the open flow lands with on the first read. A requested [focus]
   file present among the changed files sets the cursor there; otherwise the
   cursor advances off the whole-feature scope onto the first unit so space and
   enter act immediately. *)
let first_landing ?focus view =
  let focus_file =
    Option.bind focus (fun path ->
        if
          List.exists
            (fun (f : Mentat_review.View.File.t) ->
              Lpath.Rel.equal f.Mentat_review.View.File.path path)
            (view_files view)
        then Some path
        else None)
  in
  match focus_file with
  | Some path ->
      Mentat_review.Command.Set_cursor
        (Mentat_review.Cursor.Scope (Mentat_review.Scope.File path))
  | None ->
      Mentat_review.Command.Move_cursor
        { move = Mentat_review.Cursor.Next; wrap = false }

(* Fold a freshly read view: install it; refine the read when the queried focus
   is not yet the cursor's own scope (so [focus_reviewed] reports the cursor);
   and fetch the focused file's diff body when the focus moved to a new file. *)
let on_state ?focus s view =
  let s = { s with view } in
  (* First load lands on the focus file or the first unit (see {!first_landing}).
     This is the only place the open flow writes, and it runs strictly after the
     first read. *)
  match view_cursor view with
  | Mentat_review.Cursor.Scope Mentat_review.Scope.Feature
    when view_files view <> [] ->
      ( Open s,
        [
          Apply (first_landing ?focus view);
          Query_state Mentat_review.Scope.Feature;
        ] )
  | _ -> (
      let refine =
        match view_cursor view with
        | Mentat_review.Cursor.Scope sc
          when not (Mentat_review.Scope.equal (view_focus view) sc) ->
            [ Query_state sc ]
        | Mentat_review.Cursor.Scope _ | Mentat_review.Cursor.Cr _ -> []
      in
      match focus_path s with
      | None -> (Open { s with file_diff = None; diff_path = None }, refine)
      | Some path -> (
          match s.diff_path with
          | Some current when same_path current path -> (Open s, refine)
          | _ ->
              ( Open { s with file_diff = None; diff_path = Some path },
                refine @ [ Query_diff path ] )))

let map_compose s f =
  match s.panel.Review_panel.compose with
  | Some compose ->
      Open
        { s with panel = Review_panel.set_compose s.panel (Some (f compose)) }
  | None -> Open s

(* Update. *)

let update msg t =
  match (msg, t) with
  (* Completions. *)
  | State_loaded (Ok view), Loading focus ->
      let s =
        {
          view;
          crs = [];
          file_diff = None;
          diff_path = None;
          panel = Review_panel.init;
          pending_notice = None;
        }
      in
      let t, effects = on_state ?focus s view in
      (t, Stay effects)
  | State_loaded (Ok view), Open s ->
      let t, effects = on_state s view in
      (t, Stay effects)
  | State_loaded (Error message), Loading _ -> (Failed message, Stay [])
  | State_loaded (Error message), Open s ->
      (notify s ~text:("refresh failed: " ^ message) ~warning:true, Stay [])
  | Crs_loaded (Ok crs), Open s -> (Open { s with crs }, Stay [])
  | Crs_loaded (Error message), Open s ->
      (notify s ~text:("CR reload failed: " ^ message) ~warning:true, Stay [])
  | Diff_loaded (path, result), Open s
    when match s.diff_path with
         | Some p ->
             String.equal (Lpath.Rel.to_string p) (Lpath.Rel.to_string path)
         | None -> false -> (
      match result with
      | Ok file_diff -> (Open { s with file_diff }, Stay [])
      | Error message ->
          ( notify s ~text:("diff load failed: " ^ message) ~warning:true,
            Stay [] ))
  | Command_done (Ok ()), _ -> (t, Stay [])
  | Command_done (Error message), Open s ->
      (notify s ~text:message ~warning:true, Stay [])
  | Compose_done (Ok ()), Open s ->
      let notice = Option.value s.pending_notice ~default:"CR written" in
      let panel =
        Review_panel.set_notice
          (Review_panel.set_compose s.panel None)
          ~text:notice ~warning:false
      in
      (Open { s with panel; pending_notice = None }, Stay [])
  | Compose_done (Error message), Open s ->
      let panel =
        match s.panel.Review_panel.compose with
        | Some compose ->
            Review_panel.set_compose s.panel
              (Some (Review_compose.with_problem compose message))
        | None -> Review_panel.set_notice s.panel ~text:message ~warning:true
      in
      (Open { s with panel; pending_notice = None }, Stay [])
  (* Terminal user input. *)
  | Close_pressed, _ -> (t, Close [])
  | Back, Open s -> (
      match Review_panel.back s.panel with
      | Some panel -> (Open { s with panel }, Stay [])
      | None -> (t, Close []))
  | Back, (Loading _ | Failed _) -> (t, Close [])
  (* Navigation. *)
  | Moved move, Open s ->
      ( Open (clear_notice s),
        Stay
          (apply_and_reload
             (Mentat_review.Command.Move_cursor { move; wrap = false })) )
  | Nav_moved direction, Open s -> (
      match Review_rows.nav_step s.view ~crs:s.crs direction with
      | Some cursor ->
          ( Open (clear_notice s),
            Stay (apply_and_reload (Mentat_review.Command.Set_cursor cursor)) )
      | None -> (Open (clear_notice s), Stay []))
  | Line_moved direction, Open s -> (
      match line_step_target s direction with
      | Some scope ->
          ( Open (clear_notice s),
            Stay
              (apply_and_reload
                 (Mentat_review.Command.Set_cursor
                    (Mentat_review.Cursor.Scope scope))) )
      | None -> (Open (clear_notice s), Stay []))
  | Hunk_jumped direction, Open s -> (
      match hunk_jump_target s direction with
      | Some scope ->
          ( Open (clear_notice s),
            Stay
              (apply_and_reload
                 (Mentat_review.Command.Set_cursor
                    (Mentat_review.Cursor.Scope scope))) )
      | None -> (Open (clear_notice s), Stay []))
  | Nav_clicked cursor, Open s ->
      ( Open (clear_notice s),
        Stay (apply_and_reload (Mentat_review.Command.Set_cursor cursor)) )
  | Line_clicked scope, Open s ->
      let panel =
        match s.panel.Review_panel.depth with
        | Review_panel.Diff -> s.panel
        | Review_panel.Queue -> Review_panel.toggle_focus s.panel
      in
      let cursor =
        match cr_index_at s scope with
        | Some index -> Mentat_review.Cursor.Cr index
        | None -> Mentat_review.Cursor.Scope scope
      in
      ( Open (clear_notice { s with panel }),
        Stay (apply_and_reload (Mentat_review.Command.Set_cursor cursor)) )
  | Entered, Open s -> (
      match Review_panel.enter s.panel s.view s.crs with
      | Some panel -> enter_diff { s with panel }
      | None -> (t, Stay []))
  | Focus_toggled, Open s -> (
      let panel = Review_panel.toggle_focus s.panel in
      match panel.Review_panel.depth with
      | Review_panel.Diff -> enter_diff { s with panel }
      | Review_panel.Queue -> (Open { s with panel }, Stay []))
  (* Marks and verdict. *)
  | Space_pressed, Open s -> (
      match space_scope s with
      | None -> (t, Stay [])
      | Some (scope, `Toggle) ->
          let command =
            if view_focus_reviewed s.view then
              Mentat_review.Command.Mark_unreviewed scope
            else Mentat_review.Command.Mark_reviewed scope
          in
          let effects =
            if view_focus_reviewed s.view then apply_and_reload command
            else
              [
                Apply command;
                Apply
                  (Mentat_review.Command.Move_cursor
                     { move = Mentat_review.Cursor.Next; wrap = false });
                Query_state Mentat_review.Scope.Feature;
              ]
          in
          (Open (clear_notice s), Stay effects)
      | Some (scope, `Mark) ->
          ( Open (clear_notice s),
            Stay
              [
                Apply (Mentat_review.Command.Mark_reviewed scope);
                Apply
                  (Mentat_review.Command.Move_cursor
                     { move = Mentat_review.Cursor.Next; wrap = false });
                Query_state Mentat_review.Scope.Feature;
              ] ))
  | Verdict_toggled, Open s ->
      let command =
        match view_freshness s.view with
        | `Approved -> Mentat_review.Command.Set_pending
        | `Pending | `Stale -> Mentat_review.Command.Approve
      in
      (Open (clear_notice s), Stay (apply_and_reload command))
  (* Local orientation. *)
  | Help_toggled, Open s ->
      (Open { s with panel = Review_panel.toggle_help s.panel }, Stay [])
  | Context_toggled, Open s ->
      (Open { s with panel = Review_panel.toggle_context s.panel }, Stay [])
  (* Compose. *)
  | Compose_started kind, Open s -> (
      let open_compose target =
        Open
          {
            s with
            panel =
              Review_panel.set_compose s.panel
                (Some (Review_compose.make ~target ~draft:""));
          }
      in
      match kind with
      | `Add -> (
          match add_anchor s with
          | Some (path, line) ->
              (open_compose (Review_compose.Add { path; line }), Stay [])
          | None ->
              ( notify s ~text:"nothing to comment on here" ~warning:false,
                Stay [] ))
      | `Edit -> (
          match cursor_cr s with
          | Some (_, c) ->
              let draft = cr_body c in
              ( Open
                  {
                    s with
                    panel =
                      Review_panel.set_compose s.panel
                        (Some
                           (Review_compose.make ~target:(Review_compose.Edit c)
                              ~draft));
                  },
                Stay [] )
          | None ->
              (notify s ~text:"select a CR to edit" ~warning:false, Stay []))
      | `Resolve -> (
          match cursor_cr s with
          | Some (_, c) when cr_malformed c ->
              ( notify s
                  ~text:"a malformed CR cannot be resolved; edit or remove it"
                  ~warning:true,
                Stay [] )
          | Some (_, c) when cr_resolved c ->
              ( notify s ~text:"that CR is already resolved" ~warning:false,
                Stay [] )
          | Some (_, c) -> (
              let resolver =
                match Mentat_review.Cr.Handle.of_string "user" with
                | Ok handle -> handle
                | Error _ -> assert false
              in
              match
                Result.bind
                  (Mentat_review.Cr.make ~body:(cr_body c) ())
                  (fun cr -> Mentat_review.Cr.resolve ~resolver cr)
              with
              | Ok resolved ->
                  ( Open
                      {
                        s with
                        panel =
                          Review_panel.set_compose s.panel
                            (Some
                               (Review_compose.make
                                  ~target:(Review_compose.Resolve c)
                                  ~draft:(Mentat_review.Cr.to_string resolved)));
                      },
                    Stay [] )
              | Error error ->
                  ( notify s
                      ~text:
                        (Format.asprintf "%a" Mentat_review.Cr.Error.pp error)
                      ~warning:true,
                    Stay [] ))
          | None ->
              (notify s ~text:"select a CR to resolve" ~warning:false, Stay []))
      )
  | Compose_edited value, Open s ->
      ( map_compose s (fun compose -> Review_compose.with_draft compose value),
        Stay [] )
  | Compose_cancelled, Open s ->
      (Open { s with panel = Review_panel.set_compose s.panel None }, Stay [])
  | Compose_submitted value, Open s -> (
      match s.panel.Review_panel.compose with
      | None -> (t, Stay [])
      | Some compose -> (
          (* Reconcile the textarea's final value before parsing, so an edit
             batched with the enter still counts. *)
          let compose = Review_compose.with_draft compose value in
          let s =
            { s with panel = Review_panel.set_compose s.panel (Some compose) }
          in
          match compose_edit compose with
          | Error problem ->
              ( map_compose s (fun compose ->
                    Review_compose.with_problem compose problem),
                Stay [] )
          | Ok edit ->
              let notice =
                match Review_compose.target compose with
                | Review_compose.Add _ -> "CR added"
                | Review_compose.Edit _ -> "CR updated"
                | Review_compose.Resolve _ -> "CR resolved"
              in
              let effects =
                Compose edit :: Query_state Mentat_review.Scope.Feature
                :: Query_crs
                ::
                (match focus_path s with
                | Some p -> [ Query_diff p ]
                | None -> [])
              in
              (* Close the dialog now, optimistically: enter submits once and the
                 draft is gone, so a second enter cannot re-submit the same edit
                 against the CR its own write is about to change (which the
                 responder would reject as a stale ref). The settle notice
                 confirms on completion; a write failure re-surfaces as a notice
                 rather than the stale draft. *)
              ( Open
                  {
                    s with
                    panel = Review_panel.set_compose s.panel None;
                    pending_notice = Some notice;
                  },
                Stay effects )))
  | Remove_requested, Open s -> (
      match cursor_cr s with
      | Some (_, c) ->
          let effects =
            Compose (Mentat_review.Cr.Edit.Remove { ref = cr_ref c })
            :: Query_state Mentat_review.Scope.Feature :: Query_crs
            ::
            (match focus_path s with
            | Some p -> [ Query_diff p ]
            | None -> [])
          in
          (Open { s with pending_notice = Some "CR removed" }, Stay effects)
      | None -> (notify s ~text:"select a CR to remove" ~warning:false, Stay [])
      )
  (* A diff body for a superseded focus is stale. *)
  | Diff_loaded _, Open _ -> (t, Stay [])
  (* Every other message is inert while loading or failed. *)
  | _, (Loading _ | Failed _) -> (t, Stay [])

(* Keyboard. *)

let plain_text_key (data : Key.event) =
  let modifier = data.Key.modifier in
  (not modifier.Modifier.ctrl)
  && (not modifier.Modifier.alt)
  && not modifier.Modifier.super

let ctrl_char data lower upper code c =
  data.Key.modifier.Modifier.ctrl
  && (Uchar.equal c (Uchar.of_char lower) || Uchar.equal c (Uchar.of_char upper))
  || Uchar.equal c (Uchar.of_int code)

let ctrl_o_char data c = ctrl_char data 'o' 'O' 0x0f c
let ctrl_p_char data c = ctrl_char data 'p' 'P' 0x10 c
let ctrl_n_char data c = ctrl_char data 'n' 'N' 0x0e c

let composing = function
  | Open s -> Option.is_some s.panel.Review_panel.compose
  | Loading _ | Failed _ -> false

let diff_focused = function
  | Open s -> (
      match s.panel.Review_panel.depth with
      | Review_panel.Diff -> true
      | Review_panel.Queue -> false)
  | Loading _ | Failed _ -> false

let helping = function
  | Open s -> s.panel.Review_panel.help
  | Loading _ | Failed _ -> false

let key t (data : Key.event) : msg option =
  let char_is ch c = Uchar.equal c (Uchar.of_char ch) in
  if composing t then
    (* The compose input is a native textarea: it consumes printables, backspace,
       word-delete, and enter itself (enter fires its [on_submit]). Only the keys
       it leaves for the shell reach here — esc cancels the draft. *)
    match data.Key.key with
    | Key.Escape -> Some Compose_cancelled
    | _ -> None
  else
    (* Movement is focus-aware: the nav steps its own rows (files and CRs,
       never a hunk), the diff pane steps lines with ]/[ jumping hunks. *)
    let step_next =
      if diff_focused t then Line_moved `Next else Nav_moved `Next
    in
    let step_previous =
      if diff_focused t then Line_moved `Previous else Nav_moved `Previous
    in
    let jump_first =
      if diff_focused t then Moved Mentat_review.Cursor.First
      else Nav_moved `First
    in
    let jump_last =
      if diff_focused t then Moved Mentat_review.Cursor.Last
      else Nav_moved `Last
    in
    match data.Key.key with
    | Key.Escape -> (
        match t with
        | Open _ when helping t -> Some Help_toggled
        | Open _ -> Some Back
        | Loading _ | Failed _ -> Some Close_pressed)
    | Key.Up -> Some step_previous
    | Key.Down -> Some step_next
    | Key.Char c when ctrl_p_char data c -> Some step_previous
    | Key.Char c when ctrl_n_char data c -> Some step_next
    | Key.Char c when ctrl_o_char data c -> Some Context_toggled
    | Key.Tab -> Some Focus_toggled
    | Key.Enter | Key.Line_feed | Key.KP_enter -> Some Entered
    | Key.Home -> Some jump_first
    | Key.End -> Some jump_last
    | Key.Page_up -> Some step_previous
    | Key.Page_down -> Some step_next
    | Key.Char c when plain_text_key data ->
        (* Focus-contextual movement is the screen's own vocabulary; the review
           verbs (space a ? c e x d ] [ n p) resolve through the registry and
           arrive as {!verb} messages. *)
        if char_is 'j' c then Some step_next
        else if char_is 'k' c then Some step_previous
        else if char_is 'g' c then Some jump_first
        else if char_is 'G' c then Some jump_last
        else None
    | _ -> None

(* View. *)

let view ~palette ?width ?height ?layout_pref ~inject t =
  match t with
  | Loading _ -> Review_panel.loading_view ~palette ?width ?height ()
  | Failed message ->
      Review_panel.error_view ~palette ?width ?height ~message ()
  | Open s ->
      Review_panel.view ~palette ?width ?height ?layout_pref ~crs:s.crs
        ~file_diff:s.file_diff
        ~on_click:(fun cursor -> inject (Nav_clicked cursor))
        ~on_line_click:(fun scope -> inject (Line_clicked scope))
        ~on_compose_edit:(fun value -> inject (Compose_edited value))
        ~on_compose_submit:(fun value -> inject (Compose_submitted value))
        s.panel s.view
