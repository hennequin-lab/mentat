(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type item = File of Lpath.Rel.t | Directory of Lpath.Rel.t

let compare_items left right =
  match (left, right) with
  | File left, File right | Directory left, Directory right ->
      Lpath.Rel.compare left right
  | File _, Directory _ -> -1
  | Directory _, File _ -> 1

type load_state =
  | Not_requested
  | Loading
  | Loaded of item list
  | Failed of string

type t = { query : string; selected : int; files : load_state }

let make ?(query = "") () = { query; selected = 0; files = Not_requested }
let max_visible = 200
let path = function File path | Directory path -> path
let replacement = Uchar.rep

(* Capability diagnostics and path bytes are terminal input. Keep them on one
   inert UTF-8 line before passing them to Mosaic. *)
let sanitize_inline text =
  let buffer = Buffer.create (String.length text) in
  let rec loop offset =
    if offset < String.length text then begin
      let decoded = String.get_utf_8_uchar text offset in
      let length = max 1 (Uchar.utf_decode_length decoded) in
      if Uchar.utf_decode_is_valid decoded then begin
        let uchar = Uchar.utf_decode_uchar decoded in
        let code = Uchar.to_int uchar in
        if
          code < 0x20
          || (code >= 0x7f && code <= 0x9f)
          || code = 0x2028 || code = 0x2029
        then Buffer.add_utf_8_uchar buffer replacement
        else Buffer.add_utf_8_uchar buffer uchar
      end
      else Buffer.add_utf_8_uchar buffer replacement;
      loop (offset + length)
    end
  in
  loop 0;
  Buffer.contents buffer

let normalize_query query = query |> String.trim |> String.lowercase_ascii

let contains ~needle haystack =
  String.includes ~affix:needle (String.lowercase_ascii haystack)

(* Match strings carry the directory suffix, so a [dir/] query matches the
   directory row itself alongside its contents. *)
let match_path item =
  let text = Lpath.Rel.to_string (path item) in
  match item with File _ -> text | Directory _ -> text ^ "/"

let match_basename item =
  match Lpath.Rel.basename (path item) with
  | None -> None
  | Some basename -> (
      match item with
      | File _ -> Some basename
      | Directory _ -> Some (basename ^ "/"))

let hidden item =
  List.exists
    (fun component ->
      String.length component > 0 && Char.equal component.[0] '.')
    (Lpath.Rel.components (path item))

(* Rank 0: the query names the entry itself. Rank 1: the query only matches
   earlier in the path, so the entry merely lives under a matching ancestor.
   Within a rank, visible entries precede hidden ones and enumeration order
   holds, so equal states and queries produce equal row orders. *)
let match_rank ~query item =
  if not (contains ~needle:query (match_path item)) then None
  else
    let basename_matched =
      match match_basename item with
      | Some basename -> contains ~needle:query basename
      | None -> false
    in
    let name_rank = if basename_matched then 0 else 1 in
    let hidden_rank = if hidden item then 1 else 0 in
    Some ((name_rank * 2) + hidden_rank)

let take count items =
  let rec loop taken count = function
    | [] -> List.rev taken
    | _ :: _ when count = 0 -> List.rev taken
    | item :: rest -> loop (item :: taken) (count - 1) rest
  in
  loop [] count items

(* One directory's immediate children, directories first — the quiet ls-like
   listing used while browsing rather than matching. *)
let children_of dir items =
  items
  |> List.filter (fun item ->
      match Lpath.Rel.parent (path item) with
      | Some parent -> Lpath.Rel.equal parent dir
      | None -> false)
  |> List.stable_sort (fun left right ->
      match (left, right) with
      | Directory _, File _ -> -1
      | File _, Directory _ -> 1
      | File _, File _ | Directory _, Directory _ -> 0)

(* An empty query browses the root; a [dir/] query naming an enumerated
   directory browses that directory. Anything else is a recursive match. *)
let browse_target ~query items =
  if String.equal query "" then Some Lpath.Rel.root
  else if String.length query > 1 && String.ends_with ~suffix:"/" query then
    let prefix = String.sub query 0 (String.length query - 1) in
    List.find_map
      (function
        | Directory dir
          when String.equal
                 (String.lowercase_ascii (Lpath.Rel.to_string dir))
                 prefix ->
            Some dir
        | Directory _ | File _ -> None)
      items
  else None

let ranked_matches ~query items =
  items
  |> List.filter_map (fun item ->
      match match_rank ~query item with
      | None -> None
      | Some rank -> Some (rank, item))
  |> List.stable_sort (fun (left, _) (right, _) -> Int.compare left right)
  |> List.map snd

let visible_items t =
  match t.files with
  | Not_requested | Loading | Failed _ -> []
  | Loaded items ->
      let query = normalize_query t.query in
      let visible =
        match browse_target ~query items with
        | Some dir -> children_of dir items
        | None -> ranked_matches ~query items
      in
      take max_visible visible

let clamp t =
  let count = List.length (visible_items t) in
  let selected = if count = 0 then 0 else min (count - 1) (max 0 t.selected) in
  { t with selected }

let with_query query t =
  let t =
    if String.equal query t.query then t else { t with query; selected = 0 }
  in
  clamp t

let move offset t =
  let count = List.length (visible_items t) in
  if count = 0 then t
  else
    let selected = Option_list.wrap ~count t.selected offset in
    { t with selected }

let select_next t = move 1 t
let select_previous t = move (-1) t

let request_load t =
  match t.files with
  | Not_requested -> ({ t with files = Loading }, true)
  | Loading | Loaded _ | Failed _ -> (t, false)

(* Every proper ancestor of an enumerated file is a completable directory; the
   root itself is not an item. *)
let ancestor_dirs file dirs =
  let rec loop dirs candidate =
    match Lpath.Rel.parent candidate with
    | None -> dirs
    | Some parent ->
        if Lpath.Rel.equal parent Lpath.Rel.root then dirs
        else loop (Lpath.Rel.Set.add parent dirs) parent
  in
  loop dirs file

let items_of_files files =
  let files =
    let _, reversed =
      List.fold_left
        (fun (seen, kept) file ->
          if Lpath.Rel.Set.mem file seen then (seen, kept)
          else (Lpath.Rel.Set.add file seen, file :: kept))
        (Lpath.Rel.Set.empty, []) files
    in
    List.rev reversed
  in
  let dirs =
    List.fold_left
      (fun dirs file -> ancestor_dirs file dirs)
      Lpath.Rel.Set.empty files
  in
  let items =
    List.map (fun file -> File file) files
    @ List.map (fun dir -> Directory dir) (Lpath.Rel.Set.elements dirs)
  in
  (* Path order interleaves each directory immediately before its contents. A
     directory and a file cannot share a path, so the string order is total. *)
  List.sort
    (fun left right ->
      String.compare
        (Lpath.Rel.to_string (path left))
        (Lpath.Rel.to_string (path right)))
    items

let loaded result t =
  let files =
    match result with
    | Ok files -> Loaded (items_of_files files)
    | Error diagnostic ->
        Failed (diagnostic |> Mentat_diagnostic.to_string |> sanitize_inline)
  in
  clamp { t with files }

let selected_item t = List.nth_opt (visible_items t) t.selected
let enter = selected_item

type tab_result = Chosen of item | Browse of Lpath.Rel.t | No_selection

let tab t =
  match selected_item t with
  | None -> No_selection
  | Some (File _ as item) -> Chosen item
  | Some (Directory dir) -> Browse dir

type msg = Select of item | Activate of item
type event = Stay | Activated of item

let index_of item items =
  List.find_index (fun candidate -> compare_items candidate item = 0) items

let select item t =
  match index_of item (visible_items t) with
  | None -> None
  | Some selected -> Some { t with selected }

let update message t =
  match message with
  | Select item -> (
      match select item t with None -> (t, Stay) | Some t -> (t, Stay))
  | Activate item -> (
      match select item t with
      | None -> (t, Stay)
      | Some t -> (t, Activated item))

let segment ~palette ~selected text =
  Completion_list.segment
    ?style:
      (if selected then Some (Theme.Palette.accent_style palette) else None)
    (sanitize_inline text)

(* A path is one semantic value, so keep all of its segments in the same
   flexible cell. Mosaic can then clip or ellipsize the value without laying a
   basename out independently from its parent. *)
let item_row ~palette ~selected item =
  let path = path item in
  let directory_suffix = match item with File _ -> "" | Directory _ -> "/" in
  let primary = segment ~palette ~selected (Theme.kind_file ^ " ") in
  let detail =
    segment ~palette ~selected (Lpath.Rel.to_string path ^ directory_suffix)
  in
  Completion_list.row ~detail primary

let view ~palette t =
  match t.files with
  | Not_requested | Loading -> Completion_list.note ~palette "loading files…"
  | Failed message -> Completion_list.error ~palette message
  | Loaded [] -> Completion_list.note ~palette "no files"
  | Loaded (_ :: _) -> (
      match visible_items t with
      | [] -> Completion_list.note ~palette "no matching files"
      | items ->
          let indexed = Array.of_list items in
          items
          |> List.mapi (fun index item ->
              item_row ~palette ~selected:(index = t.selected) item)
          |> Completion_list.view ~palette ~selected:t.selected
               ~on_select:(fun index -> Select indexed.(index))
               ~on_activate:(fun index -> Activate indexed.(index)))
