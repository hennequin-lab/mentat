(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Shared JSON codecs for review value types. The persistence record, the review
   command flow, and the review_state view all cross the wire over the same
   scope, verdict, and cursor encodings, so the encoding is defined once here and
   composed by each of those owners rather than re-derived per surface. *)

let decode_error message = Jsont.Error.msg Jsont.Meta.none message

let rel_path =
  Jsont.map ~kind:"relative path"
    ~dec:(fun raw ->
      match Lpath.Rel.of_string raw with
      | Ok path -> path
      | Error error -> decode_error (Lpath.Error.message error))
    ~enc:Lpath.Rel.to_string Jsont.string

let side =
  Jsont.enum ~kind:"diff side" [ ("old", Scope.Old); ("new", Scope.New) ]

let mark_state =
  Jsont.enum ~kind:"mark state"
    [ ("reviewed", Mark.Reviewed); ("unreviewed", Mark.Unreviewed) ]

let file_status =
  Jsont.enum ~kind:"file status"
    [
      ("added", Feature.File.Added);
      ("deleted", Feature.File.Deleted);
      ("modified", Feature.File.Modified);
    ]

let scope =
  let feature_case =
    Jsont.Object.map ~kind:"feature scope" Scope.Feature
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "feature" ~dec:Fun.id
  in
  let file_case =
    Jsont.Object.map ~kind:"file scope" (fun path -> Scope.File path)
    |> Jsont.Object.mem "path" rel_path ~enc:(function
      | Scope.File path -> path
      | _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "file" ~dec:Fun.id
  in
  let hunk_case =
    Jsont.Object.map ~kind:"hunk scope"
      (fun path old_start old_count new_start new_count ->
        Scope.Hunk { path; old_start; old_count; new_start; new_count })
    |> Jsont.Object.mem "path" rel_path ~enc:(function
      | Scope.Hunk { path; _ } -> path
      | _ -> assert false)
    |> Jsont.Object.mem "old_start" Jsont.int ~enc:(function
      | Scope.Hunk { old_start; _ } -> old_start
      | _ -> assert false)
    |> Jsont.Object.mem "old_count" Jsont.int ~enc:(function
      | Scope.Hunk { old_count; _ } -> old_count
      | _ -> assert false)
    |> Jsont.Object.mem "new_start" Jsont.int ~enc:(function
      | Scope.Hunk { new_start; _ } -> new_start
      | _ -> assert false)
    |> Jsont.Object.mem "new_count" Jsont.int ~enc:(function
      | Scope.Hunk { new_count; _ } -> new_count
      | _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "hunk" ~dec:Fun.id
  in
  let line_case =
    Jsont.Object.map ~kind:"line scope" (fun side path line ->
        Scope.Line (side, path, line))
    |> Jsont.Object.mem "side" side ~enc:(function
      | Scope.Line (side, _, _) -> side
      | _ -> assert false)
    |> Jsont.Object.mem "path" rel_path ~enc:(function
      | Scope.Line (_, path, _) -> path
      | _ -> assert false)
    |> Jsont.Object.mem "line" Jsont.int ~enc:(function
      | Scope.Line (_, _, line) -> line
      | _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "line" ~dec:Fun.id
  in
  let cases =
    List.map Jsont.Object.Case.make
      [ feature_case; file_case; hunk_case; line_case ]
  in
  let enc_case = function
    | Scope.Feature -> Jsont.Object.Case.value feature_case Scope.Feature
    | Scope.File _ as scope -> Jsont.Object.Case.value file_case scope
    | Scope.Hunk _ as scope -> Jsont.Object.Case.value hunk_case scope
    | Scope.Line _ as scope -> Jsont.Object.Case.value line_case scope
  in
  Jsont.Object.map ~kind:"scope" Fun.id
  |> Jsont.Object.case_mem "kind" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let verdict =
  let pending_case =
    Jsont.Object.map ~kind:"pending verdict" Verdict.Pending
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "pending" ~dec:Fun.id
  in
  let approved_case =
    Jsont.Object.map ~kind:"approved verdict" (fun feature ->
        Verdict.Approved { feature })
    |> Jsont.Object.mem "feature" Mentat_digest.Content_ref.jsont ~enc:(function
      | Verdict.Approved { feature } -> feature
      | _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "approved" ~dec:Fun.id
  in
  let cases = List.map Jsont.Object.Case.make [ pending_case; approved_case ] in
  let enc_case = function
    | Verdict.Pending -> Jsont.Object.Case.value pending_case Verdict.Pending
    | Verdict.Approved _ as verdict ->
        Jsont.Object.Case.value approved_case verdict
  in
  Jsont.Object.map ~kind:"verdict" Fun.id
  |> Jsont.Object.case_mem "kind" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let cursor =
  let scope_case =
    Jsont.Object.map ~kind:"scope cursor" (fun scope -> Cursor.Scope scope)
    |> Jsont.Object.mem "scope" scope ~enc:(function
      | Cursor.Scope scope -> scope
      | _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "scope" ~dec:Fun.id
  in
  let cr_case =
    Jsont.Object.map ~kind:"cr cursor" (fun index -> Cursor.Cr index)
    |> Jsont.Object.mem "index" Jsont.int ~enc:(function
      | Cursor.Cr index -> index
      | _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "cr" ~dec:Fun.id
  in
  let cases = List.map Jsont.Object.Case.make [ scope_case; cr_case ] in
  let enc_case = function
    | Cursor.Scope _ as cursor -> Jsont.Object.Case.value scope_case cursor
    | Cursor.Cr _ as cursor -> Jsont.Object.Case.value cr_case cursor
  in
  Jsont.Object.map ~kind:"cursor" Fun.id
  |> Jsont.Object.case_mem "kind" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
