(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Result.Syntax

let invalid fn message = Import.invalid_arg' "Mentat_store.Review" fn message

module Key = struct
  type t = { root : Mentat_workspace.Root.Key.t; base : string }

  (* No non-empty rule: base validity is [Mentat_review]'s concern (a record's
     base is {!Mentat_review.Persist.base}), not an invariant the store invents.
     [Key.make] builds a lookup key; a stored record's base is derived from the
     record, never from a free string a caller could disagree with. *)
  let make ~root ~base = { root; base }
  let root t = t.root
  let base t = t.base

  let equal a b =
    Mentat_workspace.Root.Key.equal a.root b.root && String.equal a.base b.base

  let pp ppf t =
    Format.fprintf ppf "%a:%s" Mentat_workspace.Root.Key.pp t.root t.base
end

module Error = struct
  type t =
    | Already_exists of Key.t
    | Conflict of {
        key : Key.t;
        expected : Mentat_digest.t;
        actual : Mentat_digest.t;
      }
    | Vanished of Key.t
    | Base_mismatch of { key : Key.t; base : string }
    | Non_regular_file of string
    | Invalid_record of { path : string; detail : string }
    | Io of Io.t

  let message = function
    | Already_exists key ->
        Format.asprintf "review record already exists: %a" Key.pp key
    | Conflict { key; expected; actual } ->
        Format.asprintf
          "review conflict for %a: expected revision %a but found %a" Key.pp key
          Mentat_digest.pp expected Mentat_digest.pp actual
    | Vanished key ->
        Format.asprintf "review record no longer exists: %a" Key.pp key
    | Base_mismatch { key; base } ->
        Format.asprintf
          "review base mismatch for %a: record base %s does not match %s" Key.pp
          key base (Key.base key)
    | Non_regular_file path -> path ^ ": is not a regular file"
    | Invalid_record { path; detail } -> path ^ ": " ^ detail
    | Io io -> Io.message io

  let pp ppf t = Format.pp_print_string ppf (message t)

  let diagnostic = function
    | Invalid_record { path; detail } ->
        Mentat_diagnostic.make
          ~context:(path ^ "\n" ^ detail)
          "review record is invalid"
    | error -> Mentat_diagnostic.make (message error)
end

module Document = struct
  type t = {
    key : Key.t;
    value : Mentat_review.Persist.t;
    revision : Mentat_digest.t;
  }

  let make ~key ~value ~revision = { key; value; revision }
  let key t = t.key
  let value t = t.value
  let revision t = t.revision
end

let key_dir_rel key =
  Layout.reviews_dir ^ "/"
  ^ Disk.escaped_component (Mentat_workspace.Root.Key.to_string (Key.root key))

let record_name key = Disk.escaped_component (Key.base key) ^ ".json"
let record_rel key = key_dir_rel key ^ "/" ^ record_name key

let lock_rel key =
  key_dir_rel key ^ "/" ^ Disk.escaped_component (Key.base key) ^ ".lock"

let io ~op ~path f =
  Result.map_error (fun payload -> Error.Io payload) (Disk.guard ~op ~path f)

let path_kind t rel =
  Result.map_error
    (fun payload -> Error.Io payload)
    (Disk.path_kind ~path:rel (Handle.path t rel))

(* Writers for one key serialize across fibers on the keyed registry mutex and
   across processes on a sibling advisory lock, so the CAS comparison in [commit]
   never races a concurrent reviewer. The key directory must exist before its
   lock file can. *)
let locked t key f =
  let dir = key_dir_rel key in
  let mutex_key =
    "review:" ^ dir ^ "/" ^ Disk.escaped_component (Key.base key)
  in
  match
    Handle.Registry.with_keyed (Handle.registry t) mutex_key (fun () ->
        let created = Disk.mkdir ~path:dir (Handle.path t dir) in
        if created then
          Disk.fsync_dir ~path:Layout.reviews_dir
            (Handle.path t Layout.reviews_dir);
        Disk.with_flock ~path:(lock_rel key) (Handle.path t (lock_rel key)) f)
  with
  | result -> result
  | exception Disk.Io_error payload -> Error (Error.Io payload)

let encode_record value =
  match Jsont_bytesrw.encode_string Mentat_review.Persist.jsont value with
  | Ok text -> text ^ "\n"
  | Error message -> invalid "encode" ("record does not encode: " ^ message)

let load_bytes t rel =
  io ~op:Io.Read ~path:rel (fun () -> Eio.Path.load (Handle.path t rel))

let write_record t key text =
  let dir = key_dir_rel key in
  io ~op:Io.Write ~path:(record_rel key) (fun () ->
      Disk.replace ~dir:(Handle.path t dir) ~dir_path:dir
        ~name:(record_name key) text)

let load t key =
  let path = record_rel key in
  match path_kind t path with
  | Error _ as error -> error
  | Ok `Missing -> Ok None
  | Ok (`Directory | `Other) -> Error (Error.Non_regular_file path)
  | Ok `File -> (
      let* text = load_bytes t path in
      match Jsont_bytesrw.decode_string Mentat_review.Persist.jsont text with
      | Error detail -> Error (Error.Invalid_record { path; detail })
      | Ok value ->
          let base = Mentat_review.Persist.base value in
          if not (String.equal (Key.base key) base) then
            Error (Error.Base_mismatch { key; base })
          else
            Ok
              (Some
                 (Document.make ~key ~value
                    ~revision:(Mentat_digest.string text))))

(* The stored record's base is derived from the record itself: a record
   captured against [main] is keyed under [main], so it can never be filed under
   a [develop] key and later restore nothing. The caller supplies only the
   workspace root. *)
let create t ~root value =
  let key = Key.make ~root ~base:(Mentat_review.Persist.base value) in
  locked t key (fun () ->
      let path = record_rel key in
      match path_kind t path with
      | Error _ as error -> error
      | Ok `File -> Error (Error.Already_exists key)
      | Ok (`Directory | `Other) -> Error (Error.Non_regular_file path)
      | Ok `Missing ->
          let text = encode_record value in
          let* () = write_record t key text in
          Ok (Document.make ~key ~value ~revision:(Mentat_digest.string text)))

let commit t document value =
  let key = Document.key document in
  (* The replacement must be a record for the same base as the document's key: a
     commit can never change the base a record is stored under. A
     disagreement is a structured error, never a silent overwrite. *)
  if not (String.equal (Key.base key) (Mentat_review.Persist.base value)) then
    Error (Error.Base_mismatch { key; base = Mentat_review.Persist.base value })
  else
    locked t key (fun () ->
        let path = record_rel key in
        match path_kind t path with
        | Error _ as error -> error
        | Ok `Missing ->
            (* No record store op removes records, so a missing target here is
             external interference with durable state — loud, not absence. *)
            Error (Error.Vanished key)
        | Ok (`Directory | `Other) -> Error (Error.Non_regular_file path)
        | Ok `File ->
            let* bytes = load_bytes t path in
            let actual = Mentat_digest.string bytes in
            let expected = Document.revision document in
            if not (Mentat_digest.equal expected actual) then
              Error (Error.Conflict { key; expected; actual })
            else
              let text = encode_record value in
              let* () =
                Eio.Cancel.protect (fun () -> write_record t key text)
              in
              Ok
                (Document.make ~key ~value
                   ~revision:(Mentat_digest.string text)))
