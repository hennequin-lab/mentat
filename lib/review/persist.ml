(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type review = Review.t

type mark_record = {
  scope : Scope.t;
  state : Mark.state;
  evidence : Mentat_digest.Content_ref.t;
}

type t = {
  base : string;
  marks : mark_record list;
  verdict : Verdict.t;
  cursor : Cursor.t;
}

let base record = record.base
let version = 1
let decode_error message = Jsont.Error.msg Jsont.Meta.none message

let mark_jsont =
  Jsont.Object.map ~kind:"review mark" (fun scope state evidence ->
      { scope; state; evidence })
  |> Jsont.Object.mem "scope" Codec.scope ~enc:(fun mark -> mark.scope)
  |> Jsont.Object.mem "state" Codec.mark_state ~enc:(fun mark -> mark.state)
  |> Jsont.Object.mem "evidence" Mentat_digest.Content_ref.jsont
       ~enc:(fun mark -> mark.evidence)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let jsont =
  Jsont.Object.map ~kind:"review state"
    (fun decoded_version base marks verdict cursor ->
      if not (Int.equal decoded_version version) then
        decode_error
          (Printf.sprintf "unsupported review state version %d" decoded_version)
      else { base; marks; verdict; cursor })
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun _ -> version)
  |> Jsont.Object.mem "base" Jsont.string ~enc:(fun record -> record.base)
  |> Jsont.Object.mem "marks" (Jsont.list mark_jsont) ~enc:(fun record ->
      record.marks)
  |> Jsont.Object.mem "verdict" Codec.verdict ~enc:(fun record ->
      record.verdict)
  |> Jsont.Object.mem "cursor" Codec.cursor ~enc:(fun record -> record.cursor)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let of_review review =
  {
    base = Feature.base (Review.feature review);
    marks =
      List.map
        (fun mark ->
          {
            scope = Mark.scope mark;
            state = Mark.state mark;
            evidence = Mark.evidence mark;
          })
        (Review.marks review);
    verdict = Review.verdict review;
    cursor = Review.cursor review;
  }

let restore record review =
  if not (String.equal record.base (Feature.base (Review.feature review))) then
    review
  else
    let marks =
      List.map
        (fun { scope; state; evidence } -> Mark.make ~scope ~state ~evidence)
        record.marks
    in
    Review.apply_persisted review ~marks ~verdict:record.verdict
      ~cursor:record.cursor
