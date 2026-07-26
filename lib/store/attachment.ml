(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Result.Syntax

module Error = struct
  type t =
    | Blob_mismatch of {
        expected : Mentat_digest.Content_ref.t;
        actual : Mentat_digest.Content_ref.t;
      }
    | Non_regular_file of string
    | Io of Io.t

  let message = function
    | Blob_mismatch { expected; actual } ->
        Format.asprintf "attachment %a holds bytes hashing to %a"
          Mentat_digest.Content_ref.pp expected Mentat_digest.Content_ref.pp
          actual
    | Non_regular_file path -> path ^ ": is not a regular file"
    | Io io -> Io.message io

  let pp ppf t = Format.pp_print_string ppf (message t)

  let diagnostic ~session error =
    match error with
    | Blob_mismatch _ | Non_regular_file _ ->
        Mentat_diagnostic.make ~context:(message error)
          (Format.asprintf "attachment store for session %a is invalid"
             Mentat_session.Id.pp session)
    | Io io -> Mentat_diagnostic.make (Io.message io)
end

let io ~op ~path f =
  Result.map_error (fun payload -> Error.Io payload) (Disk.guard ~op ~path f)

let path_kind t rel =
  Result.map_error
    (fun payload -> Error.Io payload)
    (Disk.path_kind ~path:rel (Handle.path t rel))

let get t ~session reference =
  let path = Layout.attachment session reference in
  match path_kind t path with
  | Error _ as error -> error
  | Ok `Missing -> Ok None
  | Ok `File -> (
      let* bytes =
        io ~op:Io.Read ~path (fun () -> Eio.Path.load (Handle.path t path))
      in
      match Mentat_digest.Content_ref.verify reference bytes with
      | Ok () -> Ok (Some bytes)
      | Error actual ->
          Error (Error.Blob_mismatch { expected = reference; actual }))
  | Ok (`Directory | `Other) -> Error (Error.Non_regular_file path)

(* Write-once: identical bytes converge to a no-op — but a pre-existing regular
   file is verified against the reference, never trusted, so an externally
   damaged blob is [Blob_mismatch]. A fresh write goes parent syncs + tmp + fsync
   + rename + shard-dir fsync, mirroring the mutation blob store's durability. *)
let put t ~session text =
  let reference = Mentat_digest.Content_ref.of_contents text in
  let rel = Layout.attachment session reference in
  match path_kind t rel with
  | Error _ as error -> error
  | Ok `File -> (
      let* bytes =
        io ~op:Io.Read ~path:rel (fun () -> Eio.Path.load (Handle.path t rel))
      in
      match Mentat_digest.Content_ref.verify reference bytes with
      | Ok () -> Ok reference
      | Error actual ->
          Error (Error.Blob_mismatch { expected = reference; actual }))
  | Ok `Missing ->
      let session_dir = Layout.session_dir session in
      let attachments_dir = Layout.attachments_dir session in
      let shard_dir = Layout.attachment_shard_dir session reference in
      let* () =
        io ~op:Io.Write ~path:attachments_dir (fun () ->
            let (_ : bool) =
              Disk.mkdir ~path:attachments_dir (Handle.path t attachments_dir)
            in
            let (_ : bool) =
              Disk.mkdir ~path:shard_dir (Handle.path t shard_dir)
            in
            Disk.fsync_dir ~path:attachments_dir (Handle.path t attachments_dir);
            Disk.fsync_dir ~path:session_dir (Handle.path t session_dir);
            Disk.replace ~dir:(Handle.path t shard_dir) ~dir_path:shard_dir
              ~name:(Layout.blob_name reference)
              text)
      in
      Ok reference
  | Ok (`Directory | `Other) -> Error (Error.Non_regular_file rel)
