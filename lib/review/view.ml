(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type review = Review.t

module File = struct
  type t = {
    path : Lpath.Rel.t;
    status : Feature.File.status;
    opaque : bool;
    units : int;
    reviewed_units : int;
    reviewed : bool;
  }

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
    && Bool.equal a.opaque b.opaque
    && Int.equal a.units b.units
    && Int.equal a.reviewed_units b.reviewed_units
    && Bool.equal a.reviewed b.reviewed

  let status_jsont =
    Jsont.enum ~kind:"file status"
      [
        ("added", Feature.File.Added);
        ("deleted", Feature.File.Deleted);
        ("modified", Feature.File.Modified);
      ]

  let jsont =
    Jsont.Object.map ~kind:"review file view"
      (fun path status opaque units reviewed_units reviewed ->
        { path; status; opaque; units; reviewed_units; reviewed })
    |> Jsont.Object.mem "path" Codec.rel_path ~enc:(fun f -> f.path)
    |> Jsont.Object.mem "status" status_jsont ~enc:(fun f -> f.status)
    |> Jsont.Object.mem "opaque" Jsont.bool ~enc:(fun f -> f.opaque)
    |> Jsont.Object.mem "units" Jsont.int ~enc:(fun f -> f.units)
    |> Jsont.Object.mem "reviewed_units" Jsont.int ~enc:(fun f ->
        f.reviewed_units)
    |> Jsont.Object.mem "reviewed" Jsont.bool ~enc:(fun f -> f.reviewed)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

type t = {
  title : string option;
  base : string;
  tip : string;
  verdict : Verdict.t;
  verdict_freshness : Verdict.freshness;
  cursor : Cursor.t;
  focus : Scope.t;
  focus_reviewed : bool;
  files : File.t list;
  units : int;
  reviewed_units : int;
  open_crs : int;
  progress : float;
  is_complete : bool;
}

let file_view review file =
  let path = Feature.File.path file in
  let unit_scopes =
    Option.value (Review.file_unit_scopes review ~path) ~default:[]
  in
  let reviewed_units =
    List.length (List.filter (Review.is_reviewed review) unit_scopes)
  in
  let opaque =
    match Feature.File.content file with
    | Feature.File.Opaque _ -> true
    | Feature.File.Text _ -> false
  in
  {
    File.path;
    status = Feature.File.status file;
    opaque;
    units = List.length unit_scopes;
    reviewed_units;
    reviewed = Review.file_reviewed review ~path;
  }

let of_review review ~focus =
  let feature = Review.feature review in
  {
    title = Feature.title feature;
    base = Feature.base feature;
    tip = Feature.tip feature;
    verdict = Review.verdict review;
    verdict_freshness = Review.verdict_freshness review;
    cursor = Review.cursor review;
    focus;
    focus_reviewed = Review.is_reviewed review focus;
    files = List.map (file_view review) (Feature.files feature);
    units = Review.units review;
    reviewed_units = Review.reviewed_units review;
    open_crs = Review.open_crs review;
    progress = Review.progress review;
    is_complete = Review.is_complete review;
  }

let freshness_equal a b =
  match (a, b) with
  | `Pending, `Pending | `Approved, `Approved | `Stale, `Stale -> true
  | (`Pending | `Approved | `Stale), _ -> false

let equal a b =
  Option.equal String.equal a.title b.title
  && String.equal a.base b.base && String.equal a.tip b.tip
  && Verdict.equal a.verdict b.verdict
  && freshness_equal a.verdict_freshness b.verdict_freshness
  && Cursor.equal a.cursor b.cursor
  && Scope.equal a.focus b.focus
  && Bool.equal a.focus_reviewed b.focus_reviewed
  && List.equal File.equal a.files b.files
  && Int.equal a.units b.units
  && Int.equal a.reviewed_units b.reviewed_units
  && Int.equal a.open_crs b.open_crs
  && Float.equal a.progress b.progress
  && Bool.equal a.is_complete b.is_complete

let pp ppf t =
  Format.fprintf ppf
    "@[<v>review %s..%s (%d/%d units, %d files, %d open CRs, %.2f)@]" t.base
    t.tip t.reviewed_units t.units (List.length t.files) t.open_crs t.progress

let freshness_jsont =
  Jsont.enum ~kind:"verdict freshness"
    [ ("pending", `Pending); ("approved", `Approved); ("stale", `Stale) ]

let jsont =
  Jsont.Object.map ~kind:"review state"
    (fun
      title
      base
      tip
      verdict
      verdict_freshness
      cursor
      focus
      focus_reviewed
      files
      units
      reviewed_units
      open_crs
      progress
      is_complete
    ->
      {
        title;
        base;
        tip;
        verdict;
        verdict_freshness;
        cursor;
        focus;
        focus_reviewed;
        files;
        units;
        reviewed_units;
        open_crs;
        progress;
        is_complete;
      })
  |> Jsont.Object.opt_mem "title" Jsont.string ~enc:(fun t -> t.title)
  |> Jsont.Object.mem "base" Jsont.string ~enc:(fun t -> t.base)
  |> Jsont.Object.mem "tip" Jsont.string ~enc:(fun t -> t.tip)
  |> Jsont.Object.mem "verdict" Codec.verdict ~enc:(fun t -> t.verdict)
  |> Jsont.Object.mem "verdict_freshness" freshness_jsont ~enc:(fun t ->
      t.verdict_freshness)
  |> Jsont.Object.mem "cursor" Codec.cursor ~enc:(fun t -> t.cursor)
  |> Jsont.Object.mem "focus" Codec.scope ~enc:(fun t -> t.focus)
  |> Jsont.Object.mem "focus_reviewed" Jsont.bool ~enc:(fun t ->
      t.focus_reviewed)
  |> Jsont.Object.mem "files" (Jsont.list File.jsont) ~enc:(fun t -> t.files)
  |> Jsont.Object.mem "units" Jsont.int ~enc:(fun t -> t.units)
  |> Jsont.Object.mem "reviewed_units" Jsont.int ~enc:(fun t ->
      t.reviewed_units)
  |> Jsont.Object.mem "open_crs" Jsont.int ~enc:(fun t -> t.open_crs)
  |> Jsont.Object.mem "progress" Jsont.number ~enc:(fun t -> t.progress)
  |> Jsont.Object.mem "is_complete" Jsont.bool ~enc:(fun t -> t.is_complete)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
