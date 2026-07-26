(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module File = struct
  type status = Added | Deleted | Modified

  type content =
    | Text of Textdiff.Hunk.t list
    | Opaque of [ `Binary | `Too_large ]

  type t = {
    path : Lpath.Rel.t;
    status : status;
    content : content;
    before : string option;
    after : string option;
    digest : Mentat_digest.Content_ref.t;
  }

  let identity ~before ~after =
    let buffer = Buffer.create 512 in
    Buffer.add_string buffer "file";
    let side = function
      | None -> Buffer.add_string buffer "\x00-"
      | Some text ->
          Buffer.add_string buffer "\x00+";
          Mentat_digest.frame buffer text
    in
    side before;
    side after;
    Mentat_digest.Content_ref.of_contents (Buffer.contents buffer)

  let make ?(context = 12) ?(max_edit_distance = 4096) ~path ~before ~after () =
    if context < 0 then
      Error (Error.make Error.Invalid_file "context must be non-negative")
    else if max_edit_distance < 0 then
      Error
        (Error.make Error.Invalid_file "max_edit_distance must be non-negative")
    else
      match (before, after) with
      | None, None ->
          Error
            (Error.make Error.Invalid_file
               "file change must have at least one side")
      | _ ->
          let status =
            match (before, after) with
            | None, Some _ -> Added
            | Some _, None -> Deleted
            | _ -> Modified
          in
          let text_side = function
            | None -> true
            | Some text -> String.is_valid_utf_8 text
          in
          let content =
            if not (text_side before && text_side after) then Opaque `Binary
            else
              match
                Textdiff.hunks ~context ~max_edit_distance
                  ~before:(Option.value before ~default:"")
                  ~after:(Option.value after ~default:"")
                  ()
              with
              | None -> Opaque `Too_large
              | Some hunks -> Text hunks
          in
          Ok
            {
              path;
              status;
              content;
              before;
              after;
              digest = identity ~before ~after;
            }

  let path t = t.path
  let status t = t.status
  let content t = t.content
  let before t = t.before
  let after t = t.after
  let digest t = t.digest

  let equal a b =
    Lpath.Rel.equal a.path b.path
    && Mentat_digest.Content_ref.equal a.digest b.digest

  let pp ppf t =
    Format.fprintf ppf "%s %s"
      (match t.status with
      | Added -> "added"
      | Deleted -> "deleted"
      | Modified -> "modified")
      (Lpath.Rel.to_string t.path)
end

type t = {
  title : string option;
  base : string;
  tip : string;
  files : File.t list;
  digest : Mentat_digest.Content_ref.t;
}

let identity files =
  let buffer = Buffer.create 512 in
  Buffer.add_string buffer "feature";
  List.iter
    (fun file ->
      Buffer.add_char buffer '\x00';
      Mentat_digest.frame buffer (Lpath.Rel.to_string (File.path file));
      Mentat_digest.frame buffer
        (Mentat_digest.Content_ref.to_token (File.digest file)))
    files;
  Mentat_digest.Content_ref.of_contents (Buffer.contents buffer)

let v ?title ~base ~tip files =
  let files =
    let sorted =
      List.stable_sort
        (fun a b -> Lpath.Rel.compare (File.path a) (File.path b))
        files
    in
    let rec dedup = function
      | a :: b :: rest when Lpath.Rel.equal (File.path a) (File.path b) ->
          dedup (a :: rest)
      | a :: rest -> a :: dedup rest
      | [] -> []
    in
    dedup sorted
  in
  { title; base; tip; files; digest = identity files }

let title t = t.title
let base t = t.base
let tip t = t.tip
let files t = t.files

let find_file t ~path =
  List.find_opt (fun file -> Lpath.Rel.equal (File.path file) path) t.files

let digest t = t.digest
let is_empty t = List.is_empty t.files
let equal a b = Mentat_digest.Content_ref.equal a.digest b.digest

let pp ppf t =
  Format.fprintf ppf "%s..%s (%d files)" t.base t.tip (List.length t.files)
