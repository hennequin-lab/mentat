(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Merge_status = struct
  type t =
    | Not_superseded
    | Clean of Mentat_digest.Content_ref.t
    | Conflict of Textdiff.Merge.Region.t list
    | Unresolvable of string
end

module Entry = struct
  type t = {
    path : Mentat_workspace.Path.t;
    operation : [ `Added | `Deleted | `Modified ];
    contiguous : bool;
    hunks : Textdiff.Hunk.t list option;
    merge : Merge_status.t;
    sources : Change.Id.t list;
  }
end

type t = {
  entries : Entry.t list;
  revertability : Revertability.t;
  additions : int;
  deletions : int;
}

(* The forward transition [before -> after] the ledger recorded. *)
let operation (entry : Change.Net.entry) =
  match (entry.Change.Net.before, entry.Change.Net.after) with
  | Image.Missing, _ -> `Added
  | _, Image.Missing -> `Deleted
  | _ -> `Modified

(* The bytes of one recorded image: the absent side is empty text; a text image
   resolves through the port, and an image the port cannot supply collapses the
   whole entry's hunks to [None]. *)
let image_side resolve = function
  | Image.Missing -> Some ""
  | Image.Text reference -> resolve reference

(* Preview the three-way merge a superseded selection would apply: [base] is
   the net-after, [ours] the current file, [theirs] the
   net-before. [current] supplies the current read for [ours]; absent, or absent
   for this path, it falls back to the recorded head — an advisory answer, since
   an unrecorded writer would make the real merge differ. A non-superseded entry
   is [Not_superseded]; a path whose bytes cannot be resolved is
   [Unresolvable]. *)
let merge_status ~state ~resolve ~current (entry : Change.Net.entry) =
  let path = entry.Change.Net.path in
  match State.head state path with
  | Some head when not (Image.equal (Change.after head) entry.Change.Net.after)
    -> (
      let ours =
        let from_head () =
          match image_side resolve (Change.after head) with
          | Some contents -> Ok contents
          | None -> Error "current bytes unavailable"
        in
        match current with
        | None -> from_head ()
        | Some observe -> (
            match observe path with
            | Some (Mentat_edit.Observed.Text contents) -> Ok contents
            | Some Mentat_edit.Observed.Missing -> Ok ""
            | Some Mentat_edit.Observed.Other ->
                Error "current target is not an editable text file"
            | None -> from_head ())
      in
      match
        ( image_side resolve entry.Change.Net.after,
          image_side resolve entry.Change.Net.before,
          ours )
      with
      | Some base, Some theirs, Ok ours -> (
          match Textdiff.Merge.v ~base ~ours ~theirs () with
          | Some merge when Textdiff.Merge.is_clean merge -> (
              match Textdiff.Merge.resolved merge with
              | Some text ->
                  Merge_status.Clean
                    (Mentat_digest.Content_ref.of_contents text)
              | None ->
                  Merge_status.Unresolvable "clean merge without a resolution")
          | Some merge -> Merge_status.Conflict (Textdiff.Merge.conflicts merge)
          | None -> Merge_status.Unresolvable "merge distance bound exceeded")
      | _, _, Error reason -> Merge_status.Unresolvable reason
      | _ -> Merge_status.Unresolvable "merge base or restore bytes unavailable"
      )
  | Some _ | None -> Merge_status.Not_superseded

let compute ~state ~selection ?current ~resolve () =
  let net = State.net state selection in
  let revertability = State.revertability state selection in
  let entries =
    List.map
      (fun (entry : Change.Net.entry) ->
        let hunks =
          match
            ( image_side resolve entry.Change.Net.before,
              image_side resolve entry.Change.Net.after )
          with
          | Some before, Some after -> Textdiff.hunks ~before ~after ()
          | _ -> None
        in
        {
          Entry.path = entry.Change.Net.path;
          operation = operation entry;
          contiguous = entry.Change.Net.contiguous;
          hunks;
          merge = merge_status ~state ~resolve ~current entry;
          sources = entry.Change.Net.sources;
        })
      net
  in
  let additions, deletions =
    List.fold_left
      (fun (a, d) (entry : Entry.t) ->
        match entry.Entry.hunks with
        | Some hunks ->
            let a', d' = Textdiff.Hunk.line_counts hunks in
            (a + a', d + d')
        | None -> (a, d))
      (0, 0) entries
  in
  { entries; revertability; additions; deletions }

module Feasibility = struct
  type outcome =
    | Clean of Mentat_digest.Content_ref.t
    | Conflict of Textdiff.Merge.Region.t list
    | Unresolvable of string

  type path_merge = { path : Mentat_workspace.Path.t; outcome : outcome }
  type t = Trivial | Mergeable of path_merge list | Blocked of path_merge list

  let ready = function Trivial | Mergeable _ -> true | Blocked _ -> false
end

let feasibility t =
  let path_merges =
    List.filter_map
      (fun (entry : Entry.t) ->
        let wrap outcome =
          Some { Feasibility.path = entry.Entry.path; outcome }
        in
        match entry.Entry.merge with
        | Merge_status.Not_superseded -> None
        | Merge_status.Clean reference -> wrap (Feasibility.Clean reference)
        | Merge_status.Conflict regions -> wrap (Feasibility.Conflict regions)
        | Merge_status.Unresolvable reason ->
            wrap (Feasibility.Unresolvable reason))
      t.entries
  in
  match path_merges with
  | [] -> Feasibility.Trivial
  | _ ->
      if
        List.for_all
          (fun (path_merge : Feasibility.path_merge) ->
            match path_merge.Feasibility.outcome with
            | Feasibility.Clean _ -> true
            | Feasibility.Conflict _ | Feasibility.Unresolvable _ -> false)
          path_merges
      then Feasibility.Mergeable path_merges
      else Feasibility.Blocked path_merges
