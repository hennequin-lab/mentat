(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

(* Line counts are computed from the two texts while the event is constructed,
   because replay must never render a real edit as "unchanged" and the diff
   engine does not own serialized framing. *)
let line_counts ~path ~before ~after =
  let label = Textdiff.Label.escaped (Mentat_workspace.Path.display path) in
  match Textdiff.File_change.of_states ~label ~before ~after with
  | None -> (0, 0)
  | Some change ->
      let stats = Textdiff.Stats.of_changes [ change ] in
      (stats.Textdiff.additions, stats.Textdiff.deletions)

module Id =
  String_id.Make
    (struct
      let module_path = "Mentat_mutation.Change.Id"
      let kind = "change id"
    end)
    ()

let invalid fn message = invalid_arg' "Mentat_mutation.Change" fn message

type t = {
  id : Id.t;
  path : Mentat_workspace.Path.t;
  before : Image.t;
  after : Image.t;
  additions : int;
  deletions : int;
}

(* Internal constructor: the introduction forms are [Event.of_edit], revert
   settlement, and decode, which all run this validation. *)
let v ~id ~path ~before ~after ~additions ~deletions =
  (match ((before : Image.t), (after : Image.t)) with
  | Image.Missing, Image.Missing ->
      invalid "v" "missing-to-missing transition is unrepresentable"
  | Image.Text b, Image.Text a when Mentat_digest.Content_ref.equal b a ->
      invalid "v" "change must alter the file"
  | (Image.Missing | Image.Text _), (Image.Missing | Image.Text _) -> ());
  if additions < 0 then invalid "v" "additions must be non-negative";
  if deletions < 0 then invalid "v" "deletions must be non-negative";
  { id; path; before; after; additions; deletions }

let id t = t.id
let path t = t.path
let before t = t.before
let after t = t.after
let additions t = t.additions
let deletions t = t.deletions

let kind t : Mentat_edit.kind =
  match (t.before, t.after) with
  | Image.Missing, Image.Text _ -> `Create
  | Image.Text _, Image.Text _ -> `Modify
  | Image.Text _, Image.Missing -> `Delete
  | Image.Missing, Image.Missing -> assert false

let of_changes changes =
  let paths, added, deleted =
    List.fold_left
      (fun (paths, added, deleted) change ->
        ( Mentat_workspace.Path.Set.add (path change) paths,
          added + additions change,
          deleted + deletions change ))
      (Mentat_workspace.Path.Set.empty, 0, 0)
      changes
  in
  Textdiff.Stats.v
    ~files:(Mentat_workspace.Path.Set.cardinal paths)
    ~additions:added ~deletions:deleted

module Net = struct
  type entry = {
    path : Mentat_workspace.Path.t;
    before : Image.t;
    after : Image.t;
    contiguous : bool;
    sources : Id.t list;
  }
end

type net_acc = {
  first : Image.t;
  last : Image.t;
  contiguous : bool;
  rev_sources : Id.t list;
}

let net changes =
  let rev_order, map =
    List.fold_left
      (fun (rev_order, map) (change : t) ->
        match Mentat_workspace.Path.Map.find_opt change.path map with
        | None ->
            let acc =
              {
                first = change.before;
                last = change.after;
                contiguous = true;
                rev_sources = [ change.id ];
              }
            in
            ( change.path :: rev_order,
              Mentat_workspace.Path.Map.add change.path acc map )
        | Some acc ->
            let acc =
              {
                first = acc.first;
                last = change.after;
                contiguous =
                  acc.contiguous && Image.equal acc.last change.before;
                rev_sources = change.id :: acc.rev_sources;
              }
            in
            (rev_order, Mentat_workspace.Path.Map.add change.path acc map))
      ([], Mentat_workspace.Path.Map.empty)
      changes
  in
  List.rev rev_order
  |> List.filter_map (fun path ->
      let acc = Mentat_workspace.Path.Map.find path map in
      if Image.equal acc.first acc.last then None
      else
        Some
          {
            Net.path;
            before = acc.first;
            after = acc.last;
            contiguous = acc.contiguous;
            sources = List.rev acc.rev_sources;
          })

module Hunks_error = struct
  type t = Missing_blob of Mentat_digest.Content_ref.t | Bound_exceeded

  let pp ppf = function
    | Missing_blob identity ->
        Format.fprintf ppf "missing blob %a" Mentat_digest.Content_ref.pp
          identity
    | Bound_exceeded ->
        Format.pp_print_string ppf "edit distance bound exceeded"
end

let hunks ?max_edit_distance ~blob t =
  let resolve = function
    | Image.Missing -> Ok ""
    | Image.Text identity -> (
        match blob identity with
        | Some contents -> Ok contents
        | None -> Error (Hunks_error.Missing_blob identity))
  in
  match resolve t.before with
  | Error e -> Error e
  | Ok before -> (
      match resolve t.after with
      | Error e -> Error e
      | Ok after -> (
          match Textdiff.hunks ?max_edit_distance ~before ~after () with
          | Some hunks -> Ok hunks
          | None -> Error Hunks_error.Bound_exceeded))

let equal a b =
  Id.equal a.id b.id
  && Mentat_workspace.Path.equal a.path b.path
  && Image.equal a.before b.before
  && Image.equal a.after b.after
  && Int.equal a.additions b.additions
  && Int.equal a.deletions b.deletions

let pp ppf t =
  let kind =
    match kind t with
    | `Create -> "create"
    | `Modify -> "modify"
    | `Delete -> "delete"
  in
  Format.fprintf ppf "change(%a, %s %a)" Id.pp t.id kind
    Mentat_workspace.Path.pp t.path

let jsont =
  Jsont.Object.map ~kind:"change"
    (fun id path before after additions deletions ->
      decode_invalid_arg (fun () ->
          v ~id ~path ~before ~after ~additions ~deletions))
  |> Jsont.Object.mem "id" Id.jsont ~enc:(fun c -> c.id)
  |> Jsont.Object.mem "path" Mentat_workspace.Path.jsont ~enc:(fun c -> c.path)
  |> Jsont.Object.mem "before" Image.jsont ~enc:(fun c -> c.before)
  |> Jsont.Object.mem "after" Image.jsont ~enc:(fun c -> c.after)
  |> Jsont.Object.mem "additions" Jsont.int ~enc:(fun c -> c.additions)
  |> Jsont.Object.mem "deletions" Jsont.int ~enc:(fun c -> c.deletions)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

(* The proven two-case narrowing: a successfully applied entry never carries a
   non-text side, so lowering it to durable images is total. *)
let of_entry ~context ~id entry =
  let image_of observed =
    match (observed : Mentat_edit.Observed.t) with
    | Mentat_edit.Observed.Missing -> Image.Missing
    | Mentat_edit.Observed.Text contents ->
        Image.Text (Mentat_digest.Content_ref.of_contents contents)
    | Mentat_edit.Observed.Other ->
        invalid_arg' "Mentat_mutation" context
          "applied entry carries a non-text state"
  in
  let path = Mentat_edit.Result.Entry.target_path entry in
  let before_state = Mentat_edit.Result.Entry.before entry in
  let after_state = Mentat_edit.Result.Entry.after entry in
  let before = image_of before_state in
  let after = image_of after_state in
  let additions, deletions =
    line_counts ~path
      ~before:(Mentat_edit.Observed.text before_state)
      ~after:(Mentat_edit.Observed.text after_state)
  in
  v ~id:(Id.of_string id) ~path ~before ~after ~additions ~deletions
