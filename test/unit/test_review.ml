(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap
module Review = Mentat_review

let rel path = Lpath.Rel.of_string_exn path
let expect_some msg = function Some value -> value | None -> failf "%s" msg

let expect_ok msg = function
  | Ok value -> value
  | Error error -> failf "%s: %a" msg Review.Error.pp error

let expect_error msg kind = function
  | Ok _ -> failf "%s: expected an error" msg
  | Error error ->
      let same =
        match (Review.Error.kind error, kind) with
        | Review.Error.Invalid_scope, Review.Error.Invalid_scope
        | Review.Error.Invalid_cursor, Review.Error.Invalid_cursor
        | Review.Error.Invalid_file, Review.Error.Invalid_file
        | Review.Error.Busy, Review.Error.Busy
        | Review.Error.Stale_snapshot, Review.Error.Stale_snapshot ->
            true
        | _ -> false
      in
      is_true ~msg same

let file ?(path = "lib/a.ml") ~before ~after () =
  expect_ok "file change"
    (Review.Feature.File.make ~path:(rel path) ~before ~after ())

let feature ?(base = "main") ?(tip = "WORKTREE") files =
  Review.Feature.v ~base ~tip files

let scan_crs ?(path = "lib/a.ml") text =
  Review.Cr.scan ~syntax:Review.Cr.Syntax.ocaml ~path:(rel path) ~text

let numbered n =
  String.concat "" (List.init n (fun i -> Printf.sprintf "line %02d\n" (i + 1)))

let edit_line text index replacement =
  String.concat "\n"
    (List.mapi
       (fun i line -> if i = index then replacement else line)
       (String.split_on_char '\n' text))

let insert_after text index replacement =
  String.concat "\n"
    (List.concat_map
       (fun (i, line) -> if i = index then [ line; replacement ] else [ line ])
       (List.mapi (fun i line -> (i, line)) (String.split_on_char '\n' text)))

(* Two edits 50 lines apart stay two hunks at the 12-line review context. *)
let simple_before = numbered 60
let simple_after = edit_line (edit_line simple_before 3 "EDIT A") 54 "EDIT B"

let simple_review () =
  let change =
    file ~before:(Some simple_before) ~after:(Some simple_after) ()
  in
  Review.v ~feature:(feature [ change ]) ~crs:[]

let first_hunk_scope review =
  match Review.unit_scopes review with
  | (Review.Scope.Hunk _ as scope) :: _ -> scope
  | _ -> failf "expected a hunk review unit"

let feature_construction_boundaries () =
  expect_error "file needs one side" Review.Error.Invalid_file
    (Review.Feature.File.make ~path:(rel "lib/missing.ml") ~before:None
       ~after:None ());
  expect_error "context must be non-negative" Review.Error.Invalid_file
    (Review.Feature.File.make ~context:(-1) ~path:(rel "lib/a.ml")
       ~before:(Some "a\n") ~after:(Some "b\n") ());
  expect_error "max edit distance must be non-negative"
    Review.Error.Invalid_file
    (Review.Feature.File.make ~max_edit_distance:(-1) ~path:(rel "lib/a.ml")
       ~before:(Some "a\n") ~after:(Some "b\n") ());
  let a_first =
    file ~path:"lib/a.ml" ~before:(Some "a\n") ~after:(Some "first\n") ()
  in
  let a_duplicate =
    file ~path:"lib/a.ml" ~before:(Some "a\n") ~after:(Some "duplicate\n") ()
  in
  let b = file ~path:"lib/b.ml" ~before:(Some "b\n") ~after:(Some "b'\n") () in
  let feature = feature [ b; a_first; a_duplicate ] in
  let files = Review.Feature.files feature in
  equal (list string) ~msg:"feature files are path sorted"
    [ "lib/a.ml"; "lib/b.ml" ]
    (List.map
       (fun file -> Lpath.Rel.to_string (Review.Feature.File.path file))
       files);
  equal (option string) ~msg:"duplicate paths keep the first entry"
    (Some "first\n")
    (Option.bind
       (Review.Feature.find_file feature ~path:(rel "lib/a.ml"))
       Review.Feature.File.after)

(* Scopes *)

let scope_containment () =
  let path = rel "lib/a.ml" in
  let other = rel "lib/b.ml" in
  let hunk =
    Review.Scope.Hunk
      { path; old_start = 3; old_count = 4; new_start = 3; new_count = 5 }
  in
  is_true ~msg:"feature contains files"
    (Review.Scope.contains Review.Scope.Feature (Review.Scope.File path));
  is_true ~msg:"file contains its hunks"
    (Review.Scope.contains (Review.Scope.File path) hunk);
  is_true ~msg:"other files do not contain the hunk"
    (not (Review.Scope.contains (Review.Scope.File other) hunk));
  is_true ~msg:"hunk contains its new-side lines"
    (Review.Scope.contains hunk (Review.Scope.Line (Review.Scope.New, path, 7)));
  is_true ~msg:"hunk excludes lines outside its new range"
    (not
       (Review.Scope.contains hunk
          (Review.Scope.Line (Review.Scope.New, path, 8))));
  is_true ~msg:"hunk contains its old-side lines"
    (Review.Scope.contains hunk (Review.Scope.Line (Review.Scope.Old, path, 6)));
  is_true ~msg:"line contains only itself"
    (Review.Scope.contains
       (Review.Scope.Line (Review.Scope.New, path, 7))
       (Review.Scope.Line (Review.Scope.New, path, 7)))

(* Marks and effective marks *)

let marks_and_effective_marks () =
  let review = simple_review () in
  let path = rel "lib/a.ml" in
  let hunk = first_hunk_scope review in
  let review =
    expect_ok "mark file" (Review.mark_reviewed review (Review.Scope.File path))
  in
  is_true ~msg:"file mark covers its hunks" (Review.is_reviewed review hunk);
  let review = expect_ok "unmark hunk" (Review.mark_unreviewed review hunk) in
  is_true ~msg:"hunk override beats the file mark"
    (not (Review.is_reviewed review hunk));
  is_true ~msg:"file itself stays reviewed"
    (Review.is_reviewed review (Review.Scope.File path));
  let review =
    expect_ok "re-mark file"
      (Review.mark_reviewed review (Review.Scope.File path))
  in
  is_true ~msg:"covering mark replaces inner overrides"
    (Review.is_reviewed review hunk);
  expect_error "marking a missing file fails" Review.Error.Invalid_scope
    (Review.mark_reviewed review (Review.Scope.File (rel "lib/missing.ml")))

let summary_progress () =
  let review = simple_review () in
  equal int ~msg:"one file" 1 (Review.files review);
  equal int ~msg:"two units" 2 (Review.units review);
  equal int ~msg:"unit scopes agree with unit count" 2
    (List.length (Review.unit_scopes review));
  equal (option int) ~msg:"file unit scopes agree with unit count" (Some 2)
    (Option.map List.length
       (Review.file_unit_scopes review ~path:(rel "lib/a.ml")));
  equal int ~msg:"none reviewed" 0 (Review.reviewed_units review);
  is_true ~msg:"pending verdict"
    (match Review.verdict_freshness review with `Pending -> true | _ -> false);
  let hunk = first_hunk_scope review in
  let review = expect_ok "mark hunk" (Review.mark_reviewed review hunk) in
  equal int ~msg:"one reviewed unit" 1 (Review.reviewed_units review);
  is_true ~msg:"progress is a half"
    (Float.abs (Review.progress review -. 0.5) < 1e-9);
  is_true ~msg:"not complete" (not (Review.is_complete review))

let open_crs_counts_unresolved () =
  (* [open_crs] projects the unresolved CR count off the occurrence array:
     resolved (XCR) and unparseable comments do not count. *)
  let text =
    "let a = 1\n(* CR alice: fix this *)\n(* XCR alice: done *)\nlet b = 2\n"
  in
  let crs = scan_crs text in
  equal int ~msg:"both CR comments scanned" 2 (List.length crs);
  let change =
    file ~before:(Some "let a = 1\nlet b = 2\n") ~after:(Some text) ()
  in
  let review = Review.v ~feature:(feature [ change ]) ~crs in
  equal int ~msg:"only the open CR is counted" 1 (Review.open_crs review);
  equal int ~msg:"no CRs means none open" 0 (Review.open_crs (simple_review ()))

(* Verdict freshness *)

let verdict_staleness () =
  let review = simple_review () in
  let review = Review.approve review in
  is_true ~msg:"approved is fresh"
    (match Review.verdict_freshness review with
    | `Approved -> true
    | _ -> false);
  let changed =
    file ~before:(Some simple_before)
      ~after:(Some (edit_line simple_after 10 "ANOTHER EDIT"))
      ()
  in
  let review' = Review.refresh review ~feature:(feature [ changed ]) ~crs:[] in
  is_true ~msg:"content change stales the verdict"
    (match Review.verdict_freshness review' with `Stale -> true | _ -> false);
  let review' = Review.approve review' in
  is_true ~msg:"re-approval is fresh again"
    (match Review.verdict_freshness review' with
    | `Approved -> true
    | _ -> false)

(* Refresh carry-forward *)

let refresh_keeps_untouched_files () =
  let a = file ~path:"lib/a.ml" ~before:(Some "a\n") ~after:(Some "b\n") () in
  let b = file ~path:"lib/b.ml" ~before:(Some "x\n") ~after:(Some "y\n") () in
  let review = Review.v ~feature:(feature [ a; b ]) ~crs:[] in
  let review =
    expect_ok "mark a"
      (Review.mark_reviewed review (Review.Scope.File (rel "lib/a.ml")))
  in
  let b' = file ~path:"lib/b.ml" ~before:(Some "x\n") ~after:(Some "z\n") () in
  let review' = Review.refresh review ~feature:(feature [ a; b' ]) ~crs:[] in
  is_true ~msg:"untouched file keeps its mark"
    (Review.is_reviewed review' (Review.Scope.File (rel "lib/a.ml")));
  is_true ~msg:"edited file was never marked"
    (not (Review.is_reviewed review' (Review.Scope.File (rel "lib/b.ml"))))

let refresh_drops_edited_file_marks () =
  let review = simple_review () in
  let path = rel "lib/a.ml" in
  let review =
    expect_ok "mark file" (Review.mark_reviewed review (Review.Scope.File path))
  in
  let changed =
    file ~before:(Some simple_before)
      ~after:(Some (simple_after ^ "EXTRA\n"))
      ()
  in
  let review' = Review.refresh review ~feature:(feature [ changed ]) ~crs:[] in
  is_true ~msg:"edited file drops its mark"
    (not (Review.is_reviewed review' (Review.Scope.File path)))

let refresh_relocates_shifted_hunks () =
  (* The reviewed hunk's content is untouched; lines are inserted above it,
     so its position shifts. The mark must relocate by content evidence. *)
  let before =
    "a\n\
     b\n\
     c\n\
     d\n\
     e\n\
     f\n\
     g\n\
     h\n\
     i\n\
     j\n\
     k\n\
     l\n\
     m\n\
     n\n\
     o\n\
     p\n\
     q\n\
     r\n\
     s\n\
     t\n\
     u\n\
     v\n\
     w\n\
     1\n\
     2\n\
     3\n"
  in
  let after =
    "a\n\
     b\n\
     c\n\
     d\n\
     e\n\
     f\n\
     g\n\
     h\n\
     i\n\
     j\n\
     k\n\
     l\n\
     m\n\
     n\n\
     o\n\
     p\n\
     q\n\
     r\n\
     s\n\
     t\n\
     u\n\
     v\n\
     w\n\
     1\n\
     CHANGED\n\
     3\n"
  in
  let review =
    Review.v
      ~feature:(feature [ file ~before:(Some before) ~after:(Some after) () ])
      ~crs:[]
  in
  let hunk = first_hunk_scope review in
  let review = expect_ok "mark hunk" (Review.mark_reviewed review hunk) in
  let before' = "NEW\n" ^ before in
  let after' = "NEW\n" ^ after in
  let review' =
    Review.refresh review
      ~feature:(feature [ file ~before:(Some before') ~after:(Some after') () ])
      ~crs:[]
  in
  let hunk' = first_hunk_scope review' in
  is_true ~msg:"shifted hunk keeps its mark by evidence"
    (Review.is_reviewed review' hunk');
  is_true ~msg:"the relocated scope differs from the original"
    (not (Review.Scope.equal hunk hunk'))

let refresh_drops_ambiguous_hunks () =
  (* Two pure insertions of identical text produce identical changed-line
     evidence; the carried mark would be ambiguous and must drop. *)
  let base = numbered 60 in
  let after = insert_after base 5 "EXTRA" in
  let review =
    Review.v
      ~feature:(feature [ file ~before:(Some base) ~after:(Some after) () ])
      ~crs:[]
  in
  let hunk = first_hunk_scope review in
  let review = expect_ok "mark hunk" (Review.mark_reviewed review hunk) in
  let after' = insert_after after 50 "EXTRA" in
  let review' =
    Review.refresh review
      ~feature:(feature [ file ~before:(Some base) ~after:(Some after') () ])
      ~crs:[]
  in
  let feature' = Review.feature review' in
  let changed = List.hd (Review.Feature.files feature') in
  match Review.Feature.File.content changed with
  | Review.Feature.File.Text hunks ->
      equal int ~msg:"two hunks after refresh" 2 (List.length hunks);
      List.iter
        (fun hunk ->
          is_true ~msg:"ambiguous hunk marks drop"
            (not
               (Review.is_reviewed review'
                  (Review.Scope.of_hunk
                     ~path:(Review.Feature.File.path changed)
                     hunk))))
        hunks
  | _ -> failf "expected text hunks"

let refresh_reanchors_cr_cursor () =
  let text = "let a = 1\n(* CR alice: rename this *)\nlet b = 2\n" in
  let crs = scan_crs text in
  equal int ~msg:"one CR scanned" 1 (List.length crs);
  let change = file ~before:(Some "let a = 1\n") ~after:(Some text) () in
  let review = Review.v ~feature:(feature [ change ]) ~crs in
  let review =
    expect_ok "select cr" (Review.set_cursor review (Review.Cursor.Cr 0))
  in
  (* New text shifts the CR down. *)
  let text' =
    "let z = 0\nlet a = 1\n(* CR alice: rename this *)\nlet b = 2\n"
  in
  let crs' = scan_crs text' in
  let change' = file ~before:(Some "let a = 1\n") ~after:(Some text') () in
  let review' =
    Review.refresh review ~feature:(feature [ change' ]) ~crs:crs'
  in
  (match Review.cursor review' with
  | Review.Cursor.Cr 0 -> ()
  | _ ->
      failf "expected the cursor to re-anchor to CR 0, got %a" Review.Cursor.pp
        (Review.cursor review'));
  (* When the CR vanishes the cursor falls back to the containing file. *)
  let text'' = "let a = 1\nlet b = 2\n" in
  let review'' =
    Review.refresh review
      ~feature:
        (feature [ file ~before:(Some "let a = 1\n") ~after:(Some text'') () ])
      ~crs:[]
  in
  match Review.cursor review'' with
  | Review.Cursor.Scope scope ->
      is_true ~msg:"cursor falls back to the file"
        (Review.Scope.equal scope (Review.Scope.File (rel "lib/a.ml")))
  | Review.Cursor.Cr _ ->
      failf "unexpected cursor %a" Review.Cursor.pp (Review.cursor review'')

(* Navigation *)

let navigation_order () =
  let text = "let a = 1\n(* CR alice: rename this *)\nlet b = 2\n" in
  let crs = scan_crs text in
  let change =
    file ~before:(Some "let a = 1\nlet b = 2\n") ~after:(Some text) ()
  in
  let review = Review.v ~feature:(feature [ change ]) ~crs in
  (* feature -> file -> hunk(s) -> cr *)
  let step review = Review.move_cursor review Review.Cursor.Next in
  let review = step review in
  (match Review.cursor review with
  | Review.Cursor.Scope scope ->
      is_true ~msg:"first stop is the file"
        (Review.Scope.equal scope (Review.Scope.File (rel "lib/a.ml")))
  | _ -> failf "expected a scope");
  let review = step review in
  (match Review.cursor review with
  | Review.Cursor.Scope scope -> (
      match scope with
      | Review.Scope.Hunk _ -> ()
      | _ -> failf "expected a hunk stop")
  | _ -> failf "expected a scope");
  let review = Review.move_cursor review Review.Cursor.Next_cr in
  (match Review.cursor review with
  | Review.Cursor.Cr 0 -> ()
  | _ -> failf "expected the CR stop");
  (* Without wrap the cursor stays at the last stop. *)
  let review = step review in
  let review = step review in
  match Review.cursor review with
  | Review.Cursor.Cr 0 -> ()
  | _ ->
      failf "expected to stay at the CR, got %a" Review.Cursor.pp
        (Review.cursor review)

let navigation_jumps_and_wraps () =
  let a =
    file ~path:"lib/a.ml" ~before:(Some "let a = 1\n")
      ~after:(Some "let a = 2\n") ()
  in
  let b =
    file ~path:"lib/b.ml" ~before:(Some "let b = 1\n")
      ~after:(Some "let b = 2\n") ()
  in
  let crs = scan_crs ~path:"notes.ml" "(* CR: outside changed files *)\n" in
  let review = Review.v ~feature:(feature [ b; a ]) ~crs in
  let expect_cursor msg expected review =
    is_true ~msg (Review.Cursor.equal expected (Review.cursor review))
  in
  let file_a = Review.Cursor.Scope (Review.Scope.File (rel "lib/a.ml")) in
  let file_b = Review.Cursor.Scope (Review.Scope.File (rel "lib/b.ml")) in
  let outside_cr = Review.Cursor.Cr 0 in
  let review = Review.move_cursor review Review.Cursor.Next_file in
  expect_cursor "Next_file lands on the first file" file_a review;
  let review = Review.move_cursor review Review.Cursor.Next_file in
  expect_cursor "Next_file skips to the next file" file_b review;
  let review = Review.move_cursor review Review.Cursor.Previous_file in
  expect_cursor "Previous_file returns to the previous file" file_a review;
  let review = Review.move_cursor review Review.Cursor.First in
  expect_cursor "First returns to feature" Review.Cursor.feature review;
  let review = Review.move_cursor review Review.Cursor.Next_cr in
  expect_cursor "Next_cr reaches CRs outside changed files" outside_cr review;
  let review = Review.move_cursor review Review.Cursor.Previous_cr in
  expect_cursor "Previous_cr without wrap stays on the first CR" outside_cr
    review;
  let review = Review.move_cursor review Review.Cursor.Last in
  expect_cursor "Last reaches the final stop" outside_cr review;
  let review = Review.move_cursor ~wrap:true review Review.Cursor.Next in
  expect_cursor "Next wraps from the final stop to feature"
    Review.Cursor.feature review;
  let review = Review.move_cursor ~wrap:true review Review.Cursor.Previous in
  expect_cursor "Previous wraps from feature to the final stop" outside_cr
    review

(* Live protocol *)

let live_debounce_and_load () =
  let review = simple_review () in
  let live = Review.Live.make ~review ~fingerprint:"fp0" () in
  let live, actions =
    Review.Live.step live (Review.Live.Fs_changed { now = 0. })
  in
  let request, seconds =
    match actions with
    | [ Review.Live.Sleep { request; seconds } ] -> (request, seconds)
    | _ -> failf "expected a sleep action"
  in
  is_true ~msg:"default debounce" (Float.abs (seconds -. 0.5) < 1e-9);
  (* A burst extends the deadline; the early tick re-arms. *)
  let live, actions =
    Review.Live.step live (Review.Live.Fs_changed { now = 0.3 })
  in
  is_true ~msg:"burst schedules nothing new" (List.is_empty actions);
  let live, actions =
    Review.Live.step live (Review.Live.Tick { now = 0.5; request })
  in
  (match actions with
  | [ Review.Live.Sleep { seconds; _ } ] ->
      is_true ~msg:"re-armed for the remainder"
        (Float.abs (seconds -. 0.3) < 1e-9)
  | _ -> failf "expected a re-armed sleep");
  let live, actions =
    Review.Live.step live (Review.Live.Tick { now = 0.81; request })
  in
  let load_request =
    match actions with
    | [ Review.Live.Load { request; known = Some "fp0" } ] -> request
    | _ -> failf "expected a load action against fp0"
  in
  (* A stale tick is ignored while loading. *)
  let live, actions =
    Review.Live.step live (Review.Live.Tick { now = 1.0; request })
  in
  is_true ~msg:"stale tick ignored" (List.is_empty actions);
  (* Unchanged load returns to idle. *)
  let live, actions =
    Review.Live.step live (Review.Live.Loaded (load_request, Ok `Unchanged))
  in
  is_true ~msg:"unchanged load is quiet" (List.is_empty actions);
  ignore live

let live_replace_and_dirty_reload () =
  let review = simple_review () in
  let live = Review.Live.make ~review ~fingerprint:"fp0" () in
  let live, actions =
    Review.Live.step live (Review.Live.Fs_changed { now = 0. })
  in
  let sleep_request =
    match actions with
    | [ Review.Live.Sleep { request; _ } ] -> request
    | _ -> failf "expected sleep"
  in
  let live, actions =
    Review.Live.step live
      (Review.Live.Tick { now = 0.5; request = sleep_request })
  in
  let load_request =
    match actions with
    | [ Review.Live.Load { request; _ } ] -> request
    | _ -> failf "expected load"
  in
  (* Changes arrive while loading: reload once the load completes. *)
  let live, _ = Review.Live.step live (Review.Live.Fs_changed { now = 0.6 }) in
  let changed =
    file ~before:(Some "a\n") ~after:(Some "b\n") ~path:"lib/c.ml" ()
  in
  let load =
    { Review.Live.feature = feature [ changed ]; crs = []; fingerprint = "fp1" }
  in
  let live, actions =
    Review.Live.step live (Review.Live.Loaded (load_request, Ok (`Loaded load)))
  in
  (match actions with
  | [ Review.Live.Replace replaced; Review.Live.Load { known = Some "fp1"; _ } ]
    ->
      is_true ~msg:"replaced review has the new feature"
        (Review.Feature.equal (Review.feature replaced) (feature [ changed ]))
  | _ -> failf "expected replace followed by a dirty reload");
  ignore live

let live_mutation_guard () =
  let review = simple_review () in
  let live = Review.Live.make ~review ~fingerprint:"fp0" () in
  expect_error "stale mutation refused" Review.Error.Stale_snapshot
    (Result.map
       (fun _ -> ())
       (Review.Live.mutation_started live ~fingerprint:"other"));
  let live, request =
    expect_ok "mutation starts"
      (Review.Live.mutation_started live ~fingerprint:"fp0")
  in
  expect_error "second mutation refused" Review.Error.Busy
    (Result.map
       (fun _ -> ())
       (Review.Live.mutation_started live ~fingerprint:"fp0"));
  (* Watch events during a mutation are ignored. *)
  let live, actions =
    Review.Live.step live (Review.Live.Fs_changed { now = 0. })
  in
  is_true ~msg:"watching pauses during mutation" (List.is_empty actions);
  (* Failure clears the fingerprint so the next cycle recovers. *)
  let live, outcome =
    Review.Live.mutation_loaded live request (Error "write failed")
  in
  (match outcome with
  | `Failed _ -> ()
  | _ -> failf "expected a failed mutation");
  is_true ~msg:"fingerprint cleared on failure"
    (Option.is_none (Review.Live.fingerprint live));
  (* Recovery load runs unconditionally. *)
  let live, actions =
    Review.Live.step live (Review.Live.Fs_changed { now = 1. })
  in
  let sleep_request =
    match actions with
    | [ Review.Live.Sleep { request; _ } ] -> request
    | _ -> failf "expected sleep"
  in
  let _, actions =
    Review.Live.step live
      (Review.Live.Tick { now = 1.5; request = sleep_request })
  in
  match actions with
  | [ Review.Live.Load { known = None; _ } ] -> ()
  | _ -> failf "expected an unconditional load"

(* Persistence *)

let persist_round_trip_and_validation () =
  let review = simple_review () in
  let hunk = first_hunk_scope review in
  let review = expect_ok "mark hunk" (Review.mark_reviewed review hunk) in
  let review = Review.approve review in
  let review =
    expect_ok "set cursor" (Review.set_cursor review (Review.Cursor.Scope hunk))
  in
  let record = Review.Persist.of_review review in
  equal string ~msg:"base observes the projected feature base" "main"
    (Review.Persist.base record);
  let encoded =
    match Jsont_bytesrw.encode_string Review.Persist.jsont record with
    | Ok text -> text
    | Error message -> failf "encode failed: %s" message
  in
  let decoded =
    match Jsont_bytesrw.decode_string Review.Persist.jsont encoded with
    | Ok record -> record
    | Error message -> failf "decode failed: %s" message
  in
  equal string ~msg:"base survives the codec"
    (Review.Persist.base record)
    (Review.Persist.base decoded);
  (* Restore onto identical content: everything survives. *)
  let fresh = simple_review () in
  let restored = Review.Persist.restore decoded fresh in
  is_true ~msg:"mark restored" (Review.is_reviewed restored hunk);
  is_true ~msg:"approval restored fresh"
    (match Review.verdict_freshness restored with
    | `Approved -> true
    | _ -> false);
  is_true ~msg:"cursor restored"
    (Review.Cursor.equal (Review.cursor restored) (Review.Cursor.Scope hunk));
  (* Restore onto changed content: stale marks drop, approval shows stale. *)
  let changed =
    file ~before:(Some simple_before)
      ~after:(Some (edit_line simple_after 10 "DIFFERENT"))
      ()
  in
  let fresh' = Review.v ~feature:(feature [ changed ]) ~crs:[] in
  let restored' = Review.Persist.restore decoded fresh' in
  is_true ~msg:"stale approval is visible"
    (match Review.verdict_freshness restored' with
    | `Stale -> true
    | _ -> false);
  (* Restore against another base restores nothing. *)
  let other_base =
    Review.v ~feature:(feature ~base:"other" [ changed ]) ~crs:[]
  in
  let restored'' = Review.Persist.restore decoded other_base in
  is_true ~msg:"other base keeps pending"
    (match Review.verdict_freshness restored'' with
    | `Pending -> true
    | _ -> false);
  equal int ~msg:"other base restores no marks" 0
    (List.length (Review.marks restored''))

let persist_rejects_future_versions () =
  let json =
    {|{"version": 2, "base": "main", "marks": [], "verdict": {"kind": "pending"}, "cursor": {"kind": "scope", "scope": {"kind": "feature"}}}|}
  in
  match Jsont_bytesrw.decode_string Review.Persist.jsont json with
  | Ok _ -> failf "expected a version error"
  | Error _ -> ()

(* Opaque files *)

let opaque_files_are_whole_file_units () =
  let binary =
    expect_some "binary change"
      (Result.to_option
         (Review.Feature.File.make ~path:(rel "img/logo.png")
            ~before:(Some "\xff\xfe\x00binary")
            ~after:(Some "\xff\xfe\x01binary") ()))
  in
  (match Review.Feature.File.content binary with
  | Review.Feature.File.Opaque `Binary -> ()
  | _ -> failf "expected opaque binary content");
  let review = Review.v ~feature:(feature [ binary ]) ~crs:[] in
  equal int ~msg:"one whole-file unit" 1 (Review.units review);
  let review =
    expect_ok "mark opaque file"
      (Review.mark_reviewed review (Review.Scope.File (rel "img/logo.png")))
  in
  is_true ~msg:"opaque file unit reviewed" (Review.is_complete review)

(* CR comments — folded from test_cr.ml; cr is now the private Mentat_review.Cr. *)
module Cr_tests = struct
  module Cr = Review.Cr
  module Path = Lpath
  module Content_ref = Mentat_digest.Content_ref

  let error_kind =
    let pp ppf = function
      | Cr.Error.Invalid_handle -> Format.pp_print_string ppf "Invalid_handle"
      | Cr.Error.Invalid_body -> Format.pp_print_string ppf "Invalid_body"
      | Cr.Error.Invalid_syntax -> Format.pp_print_string ppf "Invalid_syntax"
      | Cr.Error.Invalid_comment -> Format.pp_print_string ppf "Invalid_comment"
      | Cr.Error.Invalid_anchor -> Format.pp_print_string ppf "Invalid_anchor"
      | Cr.Error.Stale_occurrence ->
          Format.pp_print_string ppf "Stale_occurrence"
    in
    testable ~pp ~equal:( = ) ()

  let handle_value = testable ~pp:Cr.Handle.pp ~equal:Cr.Handle.equal ()
  let status_value = testable ~pp:Cr.Status.pp ~equal:Cr.Status.equal ()

  (* CR digests are [Mentat_digest.Content_ref.t] values whose canonical token is
   [sha256:<64 hex>:<byte length>]. The token grammar is covered by
   [test_digest]; here we assert that CR values produce well-formed references
   and use them for equality. *)
  let digest_value = testable ~pp:Content_ref.pp ~equal:Content_ref.equal ()

  let is_hex64 s =
    String.length s = 64
    && String.for_all
         (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
         s

  let check_identity_form msg identity =
    let digest_hex = Mentat_digest.to_hex (Content_ref.digest identity) in
    is_true
      ~msg:(msg ^ ": digest is 64 lowercase hex characters")
      (is_hex64 digest_hex);
    equal string
      ~msg:(msg ^ ": to_token is the canonical sha256:<hex>:<len> token")
      (Printf.sprintf "sha256:%s:%d" digest_hex (Content_ref.length identity))
      (Content_ref.to_token identity)

  let syntax_value = testable ~pp:Cr.Syntax.pp ~equal:Cr.Syntax.equal ()

  let rel path =
    match Path.Rel.of_string path with
    | Ok path -> path
    | Error error -> failf "invalid relative path: %a" Path.Error.pp error

  let source_path = rel "lib/a.ml"

  let expect_ok msg = function
    | Ok value -> value
    | Error error -> failf "%s: %a" msg Cr.Error.pp error

  let expect_error msg expected = function
    | Ok _ -> failf "%s: expected error" msg
    | Error error -> equal error_kind ~msg expected (Cr.Error.kind error)

  let handle text = expect_ok text (Cr.Handle.of_string text)

  let make ?priority ?recipient body =
    expect_ok body (Cr.make ?priority ?recipient ~body ())

  let parse text = expect_ok text (Cr.parse text)

  let expect_comment msg occurrence =
    expect_ok msg (Cr.Occurrence.comment occurrence)

  let expect_occurrence_error msg expected occurrence =
    expect_error msg expected (Cr.Occurrence.comment occurrence)

  let nth_occurrence msg n occurrences =
    match List.nth_opt occurrences n with
    | Some occurrence -> occurrence
    | None -> failf "%s: missing occurrence %d" msg n

  let handle_validation () =
    let mentat = handle "mentat" in
    equal string ~msg:"handle source form" "mentat" (Cr.Handle.to_string mentat);
    equal handle_value ~msg:"handles compare by source form" mentat
      (handle "mentat");
    List.iter
      (fun text ->
        expect_error
          ("invalid handle " ^ String.escaped text)
          Cr.Error.Invalid_handle (Cr.Handle.of_string text))
      [
        "";
        "two words";
        "agent:one";
        "agent\000one";
        "agent\011one";
        "agent\012one";
      ]

  let comment_construction_and_resolution () =
    let recipient = handle "mentat" in
    let resolver = handle "agent" in
    let cr = make ~recipient "  tighten this  " in
    equal string ~msg:"default priority is now" "now"
      (Cr.Priority.to_string Cr.Priority.default);
    equal status_value ~msg:"default status is open now"
      (Cr.Status.Open Cr.Priority.Now) (Cr.status cr);
    equal (option handle_value) ~msg:"recipient is retained" (Some recipient)
      (Cr.recipient cr);
    equal string ~msg:"body is trimmed" "tighten this" (Cr.body cr);
    equal string ~msg:"open CR renders canonically" "CR mentat: tighten this"
      (Cr.to_string cr);
    let resolved = expect_ok "resolve" (Cr.resolve ~resolver cr) in
    equal status_value ~msg:"resolve records resolver"
      (Cr.Status.Resolved { resolver })
      (Cr.status resolved);
    equal string ~msg:"resolve retains body and recipient"
      "XCR agent for mentat: tighten this" (Cr.to_string resolved);
    let rewritten =
      expect_ok "resolve with body" (Cr.resolve ~resolver ~body:" fixed " cr)
    in
    equal string ~msg:"resolve can replace body" "XCR agent for mentat: fixed"
      (Cr.to_string rewritten);
    expect_error "empty body" Cr.Error.Invalid_body (Cr.make ~body:"  " ());
    expect_error "NUL body" Cr.Error.Invalid_body (Cr.make ~body:"a\000b" ())

  let parsing_and_rendering () =
    let cases =
      [
        (" CR: fix it ", "CR: fix it", Cr.Status.Open Cr.Priority.Now, None);
        ( "CR mentat: fix it",
          "CR mentat: fix it",
          Cr.Status.Open Cr.Priority.Now,
          Some "mentat" );
        ( "CR-soon mentat: fix it",
          "CR-soon mentat: fix it",
          Cr.Status.Open Cr.Priority.Soon,
          Some "mentat" );
        ( "XCR agent: fixed",
          "XCR agent: fixed",
          Cr.Status.Resolved { resolver = handle "agent" },
          None );
        ( "XCR agent for mentat: fixed",
          "XCR agent for mentat: fixed",
          Cr.Status.Resolved { resolver = handle "agent" },
          Some "mentat" );
      ]
    in
    List.iter
      (fun (input, rendered, status, recipient) ->
        let cr = parse input in
        equal string ~msg:input rendered (Cr.to_string cr);
        equal status_value ~msg:(input ^ " status") status (Cr.status cr);
        equal (option string) ~msg:(input ^ " recipient") recipient
          (Option.map Cr.Handle.to_string (Cr.recipient cr)))
      cases;
    List.iter
      (fun (input, kind) -> expect_error input kind (Cr.parse input))
      [
        ("TODO: fix", Cr.Error.Invalid_comment);
        ("CR", Cr.Error.Invalid_comment);
        ("CR bad handle: fix", Cr.Error.Invalid_comment);
        ("XCR agent while mentat: fix", Cr.Error.Invalid_comment);
        ("XCR agent for: fix", Cr.Error.Invalid_comment);
        ("CR:  ", Cr.Error.Invalid_body);
      ]

  let digest_validation () =
    let cr = parse "CR: fix it" in
    let identity = Cr.digest cr in
    check_identity_form "CR digest" identity;
    (* The identity is over the normalized source text ([to_string]), so its byte
     length is that text's length. *)
    equal int ~msg:"digest byte length matches normalized text"
      (String.length (Cr.to_string cr))
      (Content_ref.length identity);
    equal digest_value ~msg:"identical comments share a digest" identity
      (Cr.digest (parse "CR: fix it"));
    is_true ~msg:"different bodies digest differently"
      (not (Content_ref.equal identity (Cr.digest (parse "CR: other"))));
    let resolved =
      expect_ok "resolve" (Cr.resolve ~resolver:(handle "agent") cr)
    in
    is_true ~msg:"resolving changes the digest"
      (not (Content_ref.equal identity (Cr.digest resolved)))

  let syntax_validation () =
    let line = expect_ok "line syntax" (Cr.Syntax.line ~prefix:"//") in
    let block =
      expect_ok "block syntax" (Cr.Syntax.block ~open_:"/*" ~close:"*/")
    in
    equal syntax_value ~msg:"line syntax equality" line
      (expect_ok "line syntax copy" (Cr.Syntax.line ~prefix:"//"));
    equal syntax_value ~msg:"block syntax equality" block
      (expect_ok "block syntax copy" (Cr.Syntax.block ~open_:"/*" ~close:"*/"));
    List.iter
      (fun (msg, result) -> expect_error msg Cr.Error.Invalid_syntax result)
      [
        ("empty line prefix", Cr.Syntax.line ~prefix:"");
        ("space-prefixed line prefix", Cr.Syntax.line ~prefix:" //");
        ("tab-prefixed line prefix", Cr.Syntax.line ~prefix:"\t//");
        ("line prefix with LF", Cr.Syntax.line ~prefix:"//\n");
        ("line prefix with CR", Cr.Syntax.line ~prefix:"//\r");
        ("empty block opener", Cr.Syntax.block ~open_:"" ~close:"*/");
        ("block closer with NUL", Cr.Syntax.block ~open_:"/*" ~close:"*\000/");
        ("block closer with CR", Cr.Syntax.block ~open_:"/*" ~close:"*/\r");
      ]

  let syntax_of_path_conventions () =
    let some msg expected path =
      match Cr.Syntax.of_path (rel path) with
      | Some syntax -> equal syntax_value ~msg expected syntax
      | None -> failf "%s: expected a syntax for %s" msg path
    in
    let line prefix = expect_ok "line syntax" (Cr.Syntax.line ~prefix) in
    let block open_ close =
      expect_ok "block syntax" (Cr.Syntax.block ~open_ ~close)
    in
    some "dune files use lisp line comments" (line ";") "lib/host/dune";
    some "dune-project uses lisp line comments" (line ";") "dune-project";
    some "dune-workspace uses lisp line comments" (line ";") "dune-workspace";
    some "OCaml sources use block comments" Cr.Syntax.ocaml
      "lib/cr/mentat_cr.ml";
    some "OCaml interfaces use block comments" Cr.Syntax.ocaml "a.mli";
    some "C-family sources use slash comments" (line "//") "src/main.rs";
    some "scripts use hash comments" (line "#") "setup.sh";
    some "yaml uses hash comments" (line "#") ".github/workflows/ci.yml";
    some "css uses block comments" (block "/*" "*/") "web/site.css";
    is_true ~msg:"unknown extensions have no syntax"
      (Option.is_none (Cr.Syntax.of_path (rel "README.md")));
    is_true ~msg:"extensionless files have no syntax"
      (Option.is_none (Cr.Syntax.of_path (rel "LICENSE")))

  let scan_ocaml_block_comments () =
    let text =
      String.concat "\n"
        [
          "let x = 1";
          "  (* CR mentat: tighten this *)";
          "let y = 2";
          "(* CR bad handle: report this *)";
          "(* not CR: ignored *)";
          "";
        ]
    in
    let occurrences = Cr.scan ~syntax:Cr.Syntax.ocaml ~path:source_path ~text in
    equal int ~msg:"block scanner includes valid and malformed CRs" 2
      (List.length occurrences);
    let first = nth_occurrence "block scan" 0 occurrences in
    let second = nth_occurrence "block scan" 1 occurrences in
    equal string ~msg:"valid raw block" "(* CR mentat: tighten this *)"
      (Cr.Occurrence.raw first);
    equal string ~msg:"valid parsed block" "CR mentat: tighten this"
      (Cr.to_string (expect_comment "valid block" first));
    equal int ~msg:"occurrence line is one-based" 2 (Cr.Occurrence.line first);
    check_identity_form "valid occurrence digest" (Cr.Occurrence.digest first);
    equal digest_value ~msg:"valid occurrence digests its parsed comment"
      (Cr.digest (expect_comment "valid block" first))
      (Cr.Occurrence.digest first);
    expect_occurrence_error "malformed block is preserved"
      Cr.Error.Invalid_comment second;
    check_identity_form "malformed occurrence digest"
      (Cr.Occurrence.digest second);
    is_true ~msg:"malformed occurrence has a distinct, stable digest"
      (not
         (Content_ref.equal
            (Cr.Occurrence.digest first)
            (Cr.Occurrence.digest second)))

  let scan_respects_payload_boundaries () =
    let syntax =
      expect_ok "custom block syntax" (Cr.Syntax.block ~open_:"/*" ~close:"R*/")
    in
    let text = "/* CR*/\n/* CR: real */R*/\n" in
    let occurrences = Cr.scan ~syntax ~path:source_path ~text in
    equal int ~msg:"scanner does not read CR marker across close delimiter" 1
      (List.length occurrences);
    let occurrence = nth_occurrence "custom block scan" 0 occurrences in
    equal string ~msg:"scanner keeps the real payload" "CR: real */"
      (Cr.to_string (expect_comment "custom block" occurrence))

  let scan_ocaml_nested_block_comments () =
    let text = "let x = 1\n(* CR: outer (* nested *) done *)\nlet y = 2\n" in
    let occurrences = Cr.scan ~syntax:Cr.Syntax.ocaml ~path:source_path ~text in
    equal int ~msg:"nested OCaml block yields one occurrence" 1
      (List.length occurrences);
    let occurrence = nth_occurrence "nested block scan" 0 occurrences in
    equal string ~msg:"nested raw block" "(* CR: outer (* nested *) done *)"
      (Cr.Occurrence.raw occurrence);
    equal string ~msg:"nested payload parses through outer close"
      "CR: outer (* nested *) done"
      (Cr.to_string (expect_comment "nested block" occurrence))

  let scan_ocaml_nested_cr_inside_non_cr_comment () =
    let text = "let x = 1\n(* ignored (* CR: nested *) ignored *)\n" in
    let occurrences = Cr.scan ~syntax:Cr.Syntax.ocaml ~path:source_path ~text in
    equal int ~msg:"nested CR inside non-CR block is scanned" 1
      (List.length occurrences);
    let occurrence = nth_occurrence "nested inner CR scan" 0 occurrences in
    equal string ~msg:"nested inner raw block" "(* CR: nested *)"
      (Cr.Occurrence.raw occurrence);
    equal string ~msg:"nested inner payload parses" "CR: nested"
      (Cr.to_string (expect_comment "nested inner block" occurrence))

  let scan_ocaml_ignores_string_literals () =
    let text =
      String.concat "\n"
        [
          "let ordinary = \"(* CR: not a comment *)\"";
          "let quoted = {| (* CR: not a comment *) |}";
          "let tagged = {fixture| (* CR: not a comment *) |fixture}";
          "let unterminated = {| (* CR: not a comment *)";
        ]
    in
    let occurrences = Cr.scan ~syntax:Cr.Syntax.ocaml ~path:source_path ~text in
    equal int ~msg:"OCaml scanner ignores string literals" 0
      (List.length occurrences)

  let scan_line_comments () =
    let syntax = expect_ok "line syntax" (Cr.Syntax.line ~prefix:"//") in
    let text =
      String.concat "\n"
        [
          "  // CR: first";
          "let x = 1 // CR: inline is ignored";
          "// XCR agent: done";
          "// CR bad handle: malformed";
          "";
        ]
    in
    let occurrences = Cr.scan ~syntax ~path:source_path ~text in
    equal int ~msg:"line scanner finds line-start CR-looking comments" 3
      (List.length occurrences);
    let first = nth_occurrence "line scan" 0 occurrences in
    let second = nth_occurrence "line scan" 1 occurrences in
    let third = nth_occurrence "line scan" 2 occurrences in
    equal string ~msg:"line raw excludes indentation" "// CR: first"
      (Cr.Occurrence.raw first);
    equal int ~msg:"line occurrence records first line" 1
      (Cr.Occurrence.line first);
    equal string ~msg:"resolved line parses" "XCR agent: done"
      (Cr.to_string (expect_comment "resolved line" second));
    expect_occurrence_error "malformed line is preserved"
      Cr.Error.Invalid_comment third

  let counts_open_and_addressed () =
    let text =
      String.concat "\n"
        [
          "let a = 1";
          "(* CR alice: tighten *)";
          "(* CR bob: document *)";
          "(* CR: unaddressed *)";
          "(* XCR alice: already resolved *)";
          "(* CR *)";
          "";
        ]
    in
    let occurrences = Cr.scan_file ~path:source_path ~text in
    equal int ~msg:"scan finds every CR-looking comment, valid or not" 5
      (List.length occurrences);
    let counts handle = Cr.Occurrence.counts ~handle occurrences in
    let alice = counts (handle "alice") in
    equal int ~msg:"open counts valid unresolved CRs only" 3
      alice.Cr.Occurrence.open_;
    equal int ~msg:"addressed counts open CRs recipient-matched to alice" 1
      alice.Cr.Occurrence.addressed;
    let bob = counts (handle "bob") in
    equal int ~msg:"open is recipient-independent" 3 bob.Cr.Occurrence.open_;
    equal int ~msg:"bob is addressed once" 1 bob.Cr.Occurrence.addressed;
    let carol = counts (handle "carol") in
    equal int ~msg:"an unaddressed handle counts no addressed CRs" 0
      carol.Cr.Occurrence.addressed;
    let empty = Cr.Occurrence.counts ~handle:(handle "alice") [] in
    equal int ~msg:"no occurrences means no open CRs" 0
      empty.Cr.Occurrence.open_

  let scan_file_uses_path_convention () =
    let text = "let x = 1\n(* CR mentat: fix this *)\n" in
    equal int ~msg:"scan_file scans OCaml sources by convention" 1
      (List.length (Cr.scan_file ~path:source_path ~text));
    let unknown_path =
      match Path.Rel.of_string "notes.md" with
      | Ok path -> path
      | Error error -> failf "invalid path: %a" Path.Error.pp error
    in
    equal int ~msg:"scan_file yields nothing for unconventional paths" 0
      (List.length (Cr.scan_file ~path:unknown_path ~text:"(* CR: fix *)\n"))

  let render_source_comments () =
    let cr = make "review this" in
    equal (result string error_kind) ~msg:"OCaml block render"
      (Ok "(* CR: review this *)")
      (Result.map_error Cr.Error.kind (Cr.render ~syntax:Cr.Syntax.ocaml cr));
    expect_error "line render rejects newline" Cr.Error.Invalid_body
      (Cr.render
         ~syntax:(expect_ok "line syntax" (Cr.Syntax.line ~prefix:"//"))
         (make "first\nsecond"));
    expect_error "block render rejects delimiter" Cr.Error.Invalid_body
      (Cr.render ~syntax:Cr.Syntax.ocaml (make "contains *) close"))

  let text_insertions () =
    let cr = make "review this" in
    let source = "let f x =\n  x + 1\n" in
    let expected = "let f x =\n  (* CR: review this *)\n  x + 1\n" in
    equal string ~msg:"insert before line uses target indentation" expected
      (expect_ok "before line"
         (Cr.add_before_line ~syntax:Cr.Syntax.ocaml ~text:source ~line:2 cr));
    equal string ~msg:"insert after line uses following indentation" expected
      (expect_ok "after line"
         (Cr.add_after_line ~syntax:Cr.Syntax.ocaml ~text:source ~line:1 cr));
    equal string ~msg:"append after trailing newline"
      "let x = 1\n(* CR: review this *)\n"
      (expect_ok "append trailing newline"
         (Cr.add_at_end ~syntax:Cr.Syntax.ocaml ~text:"let x = 1\n" cr));
    equal string ~msg:"append adds a separator newline"
      "let x = 1\n(* CR: review this *)\n"
      (expect_ok "append missing trailing newline"
         (Cr.add_at_end ~syntax:Cr.Syntax.ocaml ~text:"let x = 1" cr));
    equal string ~msg:"insert after final line uses no indentation"
      "let f x =\n  x + 1\n(* CR: review this *)\n"
      (expect_ok "after final line"
         (Cr.add_after_line ~syntax:Cr.Syntax.ocaml ~text:"let f x =\n  x + 1"
            ~line:2 cr));
    expect_error "line outside source" Cr.Error.Invalid_anchor
      (Cr.add_before_line ~syntax:Cr.Syntax.ocaml ~text:source ~line:99 cr);
    expect_error "line comments reject multi-line CRs" Cr.Error.Invalid_body
      (Cr.add_at_end
         ~syntax:(expect_ok "line syntax" (Cr.Syntax.line ~prefix:"//"))
         ~text:"" (make "first\nsecond"));
    expect_error "block comments reject closing delimiter" Cr.Error.Invalid_body
      (Cr.add_at_end ~syntax:Cr.Syntax.ocaml ~text:"" (make "contains *) close"));
    expect_error "OCaml block comments reject opening delimiter"
      Cr.Error.Invalid_body
      (Cr.add_at_end ~syntax:Cr.Syntax.ocaml ~text:"" (make "contains (* open"))

  let replace_and_remove_occurrences () =
    let text = "let x = 1\n  (* CR mentat: fix this *)\nlet y = 2\n" in
    let occurrence =
      match Cr.scan ~syntax:Cr.Syntax.ocaml ~path:source_path ~text with
      | [ occurrence ] -> occurrence
      | occurrences ->
          failf "expected one occurrence, got %d" (List.length occurrences)
    in
    let cr = expect_comment "replace target" occurrence in
    let resolved =
      expect_ok "resolve target" (Cr.resolve ~resolver:(handle "agent") cr)
    in
    equal string ~msg:"replace preserves surrounding indentation"
      "let x = 1\n  (* XCR agent for mentat: fix this *)\nlet y = 2\n"
      (expect_ok "replace" (Cr.replace ~text occurrence resolved));
    equal string ~msg:"removing a comment alone on its line removes the line"
      "let x = 1\nlet y = 2\n"
      (expect_ok "remove" (Cr.remove ~text occurrence));
    (let inline = "let x = 1 (* CR mentat: fix this *)\nlet y = 2\n" in
     let occurrence =
       match Cr.scan ~syntax:Cr.Syntax.ocaml ~path:source_path ~text:inline with
       | [ occurrence ] -> occurrence
       | occurrences ->
           failf "expected one inline occurrence, got %d"
             (List.length occurrences)
     in
     equal string ~msg:"removing a trailing comment keeps the line"
       "let x = 1 \nlet y = 2\n"
       (expect_ok "inline remove" (Cr.remove ~text:inline occurrence)));
    let stale = "let x = 1\n  (* CR mentat: changed *)\nlet y = 2\n" in
    expect_error "stale replace" Cr.Error.Stale_occurrence
      (Cr.replace ~text:stale occurrence resolved);
    expect_error "stale remove" Cr.Error.Stale_occurrence
      (Cr.remove ~text:stale occurrence)

  let replace_line_comment_preserves_crlf () =
    let syntax = expect_ok "line syntax" (Cr.Syntax.line ~prefix:"//") in
    let text = "// CR: first\r\nlet x = 1\r\n" in
    let occurrence =
      match Cr.scan ~syntax ~path:source_path ~text with
      | [ occurrence ] -> occurrence
      | occurrences ->
          failf "expected one occurrence, got %d" (List.length occurrences)
    in
    let cr = expect_comment "CRLF line comment" occurrence in
    let resolved =
      expect_ok "resolve CRLF line comment"
        (Cr.resolve ~resolver:(handle "agent") cr)
    in
    equal string ~msg:"replace preserves CRLF line ending"
      "// XCR agent: first\r\nlet x = 1\r\n"
      (expect_ok "replace CRLF line comment"
         (Cr.replace ~text occurrence resolved))

  let line_comment_boundaries () =
    let syntax = expect_ok "line syntax" (Cr.Syntax.line ~prefix:"//") in
    let final_line = "let x = 1\n// CR: final" in
    let occurrence =
      match Cr.scan ~syntax ~path:source_path ~text:final_line with
      | [ occurrence ] -> occurrence
      | occurrences ->
          failf "expected one final-line occurrence, got %d"
            (List.length occurrences)
    in
    equal string ~msg:"scanner includes final line without newline"
      "// CR: final"
      (Cr.Occurrence.raw occurrence);
    let crlf = "let x = 1\r\n  // CR: remove\r\nlet y = 2\r\n" in
    let occurrence =
      match Cr.scan ~syntax ~path:source_path ~text:crlf with
      | [ occurrence ] -> occurrence
      | occurrences ->
          failf "expected one CRLF occurrence, got %d" (List.length occurrences)
    in
    equal string ~msg:"removing whole CRLF line preserves surrounding endings"
      "let x = 1\r\nlet y = 2\r\n"
      (expect_ok "remove CRLF line comment" (Cr.remove ~text:crlf occurrence))

  let rendered_comments_parse_back (soon, recipient_text, body) =
    let priority = if soon then Cr.Priority.Soon else Cr.Priority.Now in
    let recipient = Option.map handle recipient_text in
    let cr = make ~priority ?recipient body in
    equal string ~msg:"rendered comment parses canonically" (Cr.to_string cr)
      (Cr.to_string (parse (Cr.to_string cr)))

  let handle_text =
    testable ~pp:Format.pp_print_string ~equal:String.equal
      ~gen:(Gen.string_size (Gen.int_range 1 8) (Gen.char_range 'a' 'z'))
      ()

  let body_text =
    testable ~pp:Format.pp_print_string ~equal:String.equal
      ~gen:(Gen.string_size (Gen.int_range 1 30) (Gen.char_range 'a' 'z'))
      ()

  let suite =
    [
      group "comments"
        [
          test "validates handles" handle_validation;
          test "constructs and resolves comments"
            comment_construction_and_resolution;
          test "parses and renders source syntax" parsing_and_rendering;
          test "digests comments" digest_validation;
          prop' "rendered comments parse back"
            (triple bool (option handle_text) body_text)
            rendered_comments_parse_back;
        ];
      group "source syntax"
        [
          test "validates syntax delimiters" syntax_validation;
          test "maps paths to conventional syntaxes" syntax_of_path_conventions;
          test "renders source comments" render_source_comments;
        ];
      group "scanning"
        [
          test "scans OCaml block comments" scan_ocaml_block_comments;
          test "respects payload boundaries" scan_respects_payload_boundaries;
          test "scans nested OCaml block comments"
            scan_ocaml_nested_block_comments;
          test "scans nested CR inside non-CR OCaml comments"
            scan_ocaml_nested_cr_inside_non_cr_comment;
          test "ignores OCaml string literals"
            scan_ocaml_ignores_string_literals;
          test "scans line comments" scan_line_comments;
          test "scans by path convention" scan_file_uses_path_convention;
          test "counts open and addressed occurrences" counts_open_and_addressed;
        ];
      group "text transformations"
        [
          test "inserts comments" text_insertions;
          test "replaces and removes occurrences" replace_and_remove_occurrences;
          test "preserves CRLF line endings when replacing line comments"
            replace_line_comment_preserves_crlf;
          test "handles line comment boundaries" line_comment_boundaries;
        ];
    ]
end

(* Commands *)

let command_value =
  testable ~pp:Review.Command.pp ~equal:Review.Command.equal ()

let command_round_trip () =
  let review = simple_review () in
  let hunk = first_hunk_scope review in
  let commands =
    [
      Review.Command.Mark_reviewed Review.Scope.Feature;
      Review.Command.Mark_unreviewed (Review.Scope.File (rel "lib/a.ml"));
      Review.Command.Clear_mark hunk;
      Review.Command.Approve;
      Review.Command.Set_pending;
      Review.Command.Set_cursor (Review.Cursor.Scope Review.Scope.Feature);
      Review.Command.Set_cursor (Review.Cursor.Cr 0);
      Review.Command.Move_cursor { move = Review.Cursor.Next; wrap = true };
      Review.Command.Move_cursor { move = Review.Cursor.Last; wrap = false };
    ]
  in
  List.iter
    (fun command ->
      let encoded =
        match Jsont_bytesrw.encode_string Review.Command.jsont command with
        | Ok text -> text
        | Error message -> failf "encode failed: %s" message
      in
      let decoded =
        match Jsont_bytesrw.decode_string Review.Command.jsont encoded with
        | Ok command -> command
        | Error message -> failf "decode failed: %s" message
      in
      equal command_value ~msg:"command round-trips" command decoded)
    commands

let command_strict_decode () =
  match
    Jsont_bytesrw.decode_string Review.Command.jsont
      {|{"kind": "approve", "scope": {"kind": "feature"}}|}
  with
  | Ok _ -> fail "expected a strict-decode rejection of a foreign member"
  | Error _ -> ()

let command_apply_matches_transforms () =
  let review = simple_review () in
  let hunk = first_hunk_scope review in
  let same msg a b = is_true ~msg (Review.equal a b) in
  same "mark_reviewed"
    (expect_ok "apply mark"
       (Review.Command.apply review (Review.Command.Mark_reviewed hunk)))
    (expect_ok "direct mark" (Review.mark_reviewed review hunk));
  same "clear_mark"
    (expect_ok "apply clear"
       (Review.Command.apply review (Review.Command.Clear_mark hunk)))
    (Review.clear_mark review hunk);
  same "approve"
    (expect_ok "apply approve"
       (Review.Command.apply review Review.Command.Approve))
    (Review.approve review);
  same "set_pending"
    (expect_ok "apply pending"
       (Review.Command.apply review Review.Command.Set_pending))
    (Review.set_pending review);
  same "set_cursor"
    (expect_ok "apply cursor"
       (Review.Command.apply review
          (Review.Command.Set_cursor (Review.Cursor.Scope hunk))))
    (expect_ok "direct cursor"
       (Review.set_cursor review (Review.Cursor.Scope hunk)));
  same "move_cursor"
    (expect_ok "apply move"
       (Review.Command.apply review
          (Review.Command.Move_cursor
             { move = Review.Cursor.Next; wrap = false })))
    (Review.move_cursor ~wrap:false review Review.Cursor.Next);
  expect_error "apply surfaces an invalid-scope transform error"
    Review.Error.Invalid_scope
    (Review.Command.apply review
       (Review.Command.Mark_reviewed (Review.Scope.File (rel "lib/missing.ml"))))

(* View *)

let view_value = testable ~pp:Review.View.pp ~equal:Review.View.equal ()

let view_projects_review () =
  let review = simple_review () in
  let hunk = first_hunk_scope review in
  let review = expect_ok "mark hunk" (Review.mark_reviewed review hunk) in
  let view = Review.View.of_review review ~focus:Review.Scope.Feature in
  equal string ~msg:"base" "main" view.Review.View.base;
  equal string ~msg:"tip" "WORKTREE" view.Review.View.tip;
  is_true ~msg:"focus recorded"
    (Review.Scope.equal Review.Scope.Feature view.Review.View.focus);
  equal int ~msg:"units match" (Review.units review) view.Review.View.units;
  equal int ~msg:"reviewed units match"
    (Review.reviewed_units review)
    view.Review.View.reviewed_units;
  match view.Review.View.files with
  | [ file ] ->
      is_true ~msg:"file path"
        (Lpath.Rel.equal (rel "lib/a.ml") file.Review.View.File.path);
      is_true ~msg:"file is text, not opaque" (not file.Review.View.File.opaque);
      equal int ~msg:"file units" 2 file.Review.View.File.units;
      equal int ~msg:"file reviewed units" 1
        file.Review.View.File.reviewed_units;
      is_true ~msg:"file not fully reviewed"
        (not file.Review.View.File.reviewed)
  | _ -> fail "expected exactly one changed file"

let view_round_trip () =
  let review = simple_review () in
  let hunk = first_hunk_scope review in
  let review = expect_ok "mark hunk" (Review.mark_reviewed review hunk) in
  let review = Review.approve review in
  let view = Review.View.of_review review ~focus:hunk in
  let encoded =
    match Jsont_bytesrw.encode_string Review.View.jsont view with
    | Ok text -> text
    | Error message -> failf "encode failed: %s" message
  in
  let decoded =
    match Jsont_bytesrw.decode_string Review.View.jsont encoded with
    | Ok view -> view
    | Error message -> failf "decode failed: %s" message
  in
  equal view_value ~msg:"view round-trips" view decoded

(* File diff *)

let file_diff_round_trip () =
  let change =
    file ~path:"lib/a.ml" ~before:(Some simple_before)
      ~after:(Some simple_after) ()
  in
  let fd = Review.File_diff.of_file change in
  is_true ~msg:"path projected"
    (Lpath.Rel.equal (rel "lib/a.ml") fd.Review.File_diff.path);
  is_true ~msg:"content is text"
    (match fd.Review.File_diff.content with
    | Review.Feature.File.Text _ -> true
    | Review.Feature.File.Opaque _ -> false);
  let encoded =
    match Jsont_bytesrw.encode_string Review.File_diff.jsont fd with
    | Ok text -> text
    | Error message -> failf "encode failed: %s" message
  in
  let decoded =
    match Jsont_bytesrw.decode_string Review.File_diff.jsont encoded with
    | Ok fd -> fd
    | Error message -> failf "decode failed: %s" message
  in
  is_true ~msg:"file diff round-trips" (Review.File_diff.equal fd decoded)

(* CR views and refs *)

let cr_views_and_refs () =
  let text =
    "(* CR alice: fix *)\nlet a = 1\n(* CR alice: fix *)\nlet b = 2\n"
  in
  let occurrences = scan_crs text in
  equal int ~msg:"two occurrences" 2 (List.length occurrences);
  let views = Review.Cr.views occurrences in
  match views with
  | [ v0; v1 ] ->
      equal int ~msg:"first ordinal" 0
        v0.Review.Cr.View.ref.Review.Cr.Ref.ordinal;
      equal int ~msg:"duplicate CRs get distinct ordinals" 1
        v1.Review.Cr.View.ref.Review.Cr.Ref.ordinal;
      is_true ~msg:"body projected" (String.length v0.Review.Cr.View.body > 0);
      (match Review.Cr.resolve_ref occurrences v1.Review.Cr.View.ref with
      | Some occurrence ->
          is_true ~msg:"resolve_ref recovers the exact occurrence"
            (Review.Cr.Occurrence.equal occurrence (List.nth occurrences 1))
      | None -> fail "resolve_ref should find the second occurrence");
      let stale =
        Review.Cr.Ref.make ~path:(rel "lib/a.ml")
          ~digest:(Mentat_digest.Content_ref.of_contents "no-such-cr")
          ~ordinal:0
      in
      is_true ~msg:"a stale ref resolves to None"
        (Option.is_none (Review.Cr.resolve_ref occurrences stale));
      let encoded =
        match Jsont_bytesrw.encode_string Review.Cr.View.jsont v0 with
        | Ok text -> text
        | Error message -> failf "encode failed: %s" message
      in
      let decoded =
        match Jsont_bytesrw.decode_string Review.Cr.View.jsont encoded with
        | Ok view -> view
        | Error message -> failf "decode failed: %s" message
      in
      is_true ~msg:"cr view round-trips" (Review.Cr.View.equal v0 decoded)
  | _ -> fail "expected two views"

let cr_edit_round_trip () =
  let cr =
    match Review.Cr.make ~body:"fix this" () with
    | Ok cr -> cr
    | Error error -> failf "cr make: %a" Review.Cr.Error.pp error
  in
  let reference =
    Review.Cr.Ref.make ~path:(rel "lib/a.ml")
      ~digest:(Mentat_digest.Content_ref.of_contents "cr-digest")
      ~ordinal:2
  in
  let edits =
    [
      Review.Cr.Edit.Add { path = rel "lib/a.ml"; line = 3; cr };
      Review.Cr.Edit.Replace { ref = reference; cr };
      Review.Cr.Edit.Remove { ref = reference };
    ]
  in
  List.iter
    (fun edit ->
      let encoded =
        match Jsont_bytesrw.encode_string Review.Cr.Edit.jsont edit with
        | Ok text -> text
        | Error message -> failf "encode failed: %s" message
      in
      let decoded =
        match Jsont_bytesrw.decode_string Review.Cr.Edit.jsont encoded with
        | Ok edit -> edit
        | Error message -> failf "decode failed: %s" message
      in
      is_true ~msg:"cr edit round-trips" (Review.Cr.Edit.equal edit decoded))
    edits

let () =
  run "mentat.review"
    (Cr_tests.suite
    @ [
        group "features"
          [ test "construction boundaries" feature_construction_boundaries ];
        group "scopes" [ test "containment" scope_containment ];
        group "marks"
          [
            test "explicit and effective marks" marks_and_effective_marks;
            test "summary progress" summary_progress;
            test "open CR count excludes resolved" open_crs_counts_unresolved;
          ];
        group "verdict" [ test "staleness" verdict_staleness ];
        group "refresh"
          [
            test "keeps untouched files" refresh_keeps_untouched_files;
            test "drops edited file marks" refresh_drops_edited_file_marks;
            test "relocates shifted hunks" refresh_relocates_shifted_hunks;
            test "drops ambiguous hunks" refresh_drops_ambiguous_hunks;
            test "re-anchors CR cursors" refresh_reanchors_cr_cursor;
          ];
        group "navigation"
          [
            test "canonical order" navigation_order;
            test "jumps and wrapping" navigation_jumps_and_wraps;
          ];
        group "live"
          [
            test "debounce and load" live_debounce_and_load;
            test "replace and dirty reload" live_replace_and_dirty_reload;
            test "mutation guard" live_mutation_guard;
          ];
        group "persist"
          [
            test "round trip and validation" persist_round_trip_and_validation;
            test "rejects future versions" persist_rejects_future_versions;
            test "opaque files are whole-file units"
              opaque_files_are_whole_file_units;
          ];
        group "command"
          [
            test "round trips under its pinned tags" command_round_trip;
            test "strict decode rejects foreign members" command_strict_decode;
            test "apply matches the direct transforms"
              command_apply_matches_transforms;
          ];
        group "view"
          [
            test "projects a review" view_projects_review;
            test "round trips" view_round_trip;
          ];
        group "file diff"
          [ test "projects a file and round trips" file_diff_round_trip ];
        group "cr views"
          [ test "views, refs, and resolution" cr_views_and_refs ];
        group "cr edits" [ test "round trips" cr_edit_round_trip ];
      ])
