(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type content = Feature.File.content

type t = {
  path : Lpath.Rel.t;
  status : Feature.File.status;
  content : content;
  before : string option;
  after : string option;
}

let of_file file =
  {
    path = Feature.File.path file;
    status = Feature.File.status file;
    content = Feature.File.content file;
    before = Feature.File.before file;
    after = Feature.File.after file;
  }

let content_equal (a : content) (b : content) =
  match (a, b) with
  | Feature.File.Text a, Feature.File.Text b ->
      List.equal Textdiff.Hunk.equal a b
  | Feature.File.Opaque a, Feature.File.Opaque b -> a = b
  | (Feature.File.Text _ | Feature.File.Opaque _), _ -> false

let status_equal (a : Feature.File.status) (b : Feature.File.status) =
  match (a, b) with
  | Feature.File.Added, Feature.File.Added
  | Feature.File.Deleted, Feature.File.Deleted
  | Feature.File.Modified, Feature.File.Modified ->
      true
  | (Feature.File.Added | Feature.File.Deleted | Feature.File.Modified), _ ->
      false

let equal a b =
  Lpath.Rel.equal a.path b.path
  && status_equal a.status b.status
  && content_equal a.content b.content
  && Option.equal String.equal a.before b.before
  && Option.equal String.equal a.after b.after

let pp ppf t = Format.fprintf ppf "file_diff %a" Lpath.Rel.pp t.path

let opaque_reason_jsont =
  Jsont.enum ~kind:"opaque reason"
    [ ("binary", `Binary); ("too_large", `Too_large) ]

let content_jsont =
  let text_case =
    Jsont.Object.map ~kind:"text content" (fun hunks -> Feature.File.Text hunks)
    |> Jsont.Object.mem "hunks" (Jsont.list Textdiff.Hunk.jsont) ~enc:(function
      | Feature.File.Text hunks -> hunks
      | Feature.File.Opaque _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "text" ~dec:Fun.id
  in
  let opaque_case =
    Jsont.Object.map ~kind:"opaque content" (fun reason ->
        Feature.File.Opaque reason)
    |> Jsont.Object.mem "reason" opaque_reason_jsont ~enc:(function
      | Feature.File.Opaque reason -> reason
      | Feature.File.Text _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "opaque" ~dec:Fun.id
  in
  let cases = List.map Jsont.Object.Case.make [ text_case; opaque_case ] in
  let enc_case = function
    | Feature.File.Text _ as c -> Jsont.Object.Case.value text_case c
    | Feature.File.Opaque _ as c -> Jsont.Object.Case.value opaque_case c
  in
  Jsont.Object.map ~kind:"content" Fun.id
  |> Jsont.Object.case_mem "kind" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let jsont =
  Jsont.Object.map ~kind:"file diff" (fun path status content before after ->
      { path; status; content; before; after })
  |> Jsont.Object.mem "path" Codec.rel_path ~enc:(fun t -> t.path)
  |> Jsont.Object.mem "status" Codec.file_status ~enc:(fun t -> t.status)
  |> Jsont.Object.mem "content" content_jsont ~enc:(fun t -> t.content)
  |> Jsont.Object.opt_mem "before" Jsont.string ~enc:(fun t -> t.before)
  |> Jsont.Object.opt_mem "after" Jsont.string ~enc:(fun t -> t.after)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
