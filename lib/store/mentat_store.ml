(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Io = Io
module Session = Session
module Run_lock = Run_lock
module Mutation = Mutation
module Attachment = Attachment
module Review = Review
module Export = Export
module Bundle = Bundle
module Capture = Capture

module Error = struct
  type t = Layout of { path : string; message : string } | Io of Io.t

  let message = function
    | Layout { path; message } -> path ^ ": " ^ message
    | Io io -> Io.message io

  let pp ppf t = Format.pp_print_string ppf (message t)
end

type t = Handle.t

let last_exn_diagnostic = Disk.last_exn_diagnostic

let open_ ~sw path =
  match
    Handle.open_ ~sw path ~children:[ Layout.sessions_dir; Layout.reviews_dir ]
  with
  | Ok t -> Ok t
  | Error (`Layout (path, message)) -> Error (Error.Layout { path; message })
  | Error (`Io io) -> Error (Error.Io io)

(* A seed's failure surfaces through the branch's document-creation surface, so
   it speaks that surface's error. A native IO failure passes through; a damaged
   or absent parent blob — reading the parent's mutation store to copy it did not
   yield the bytes the branch needs — is reported as an [Io] read failure of that
   parent history, its structured detail preserved in the message. Copy performs
   no document load or history validation, so its other arms cannot arise; they
   fold into the same read failure for totality. *)
let seed_error_as_session ~from (error : Mutation.Error.t) : Session.Error.t =
  match error with
  | Mutation.Error.Io io -> Session.Error.Io io
  | Mutation.Error.Document error -> error
  | Mutation.Error.Invalid_line _ | Mutation.Error.Invalid_history _
  | Mutation.Error.Correlation _ | Mutation.Error.Blob_mismatch _
  | Mutation.Error.Missing_blob _ | Mutation.Error.Non_regular_file _ ->
      Session.Error.Io
        {
          Io.op = Io.Read;
          path = Layout.session_dir from;
          cause = Io.Message (Mutation.Error.message error);
        }

let attachment_seed_error ~from (error : Attachment.Error.t) : Session.Error.t =
  match error with
  | Attachment.Error.Io io -> Session.Error.Io io
  | Attachment.Error.Blob_mismatch _ | Attachment.Error.Non_regular_file _ ->
      Session.Error.Io
        {
          Io.op = Io.Read;
          path = Layout.session_dir from;
          cause = Io.Message (Attachment.Error.message error);
        }

(* A branch's document-media [`Ref]s are document refs, not mutation-event refs,
   so [Mutation.copy_into] never touches them; walk the child document's media
   refs and copy each blob from the parent's attachments namespace into the
   child's, so a branch is self-contained. A referenced blob absent from
   the parent is a read failure of that parent history, never a laundered gap. *)
let copy_attachment_blobs t ~from ~into child =
  let missing reference =
    Session.Error.Io
      {
        Io.op = Io.Read;
        path = Layout.session_dir from;
        cause =
          Io.Message
            (Format.asprintf "referenced attachment %a is missing"
               Mentat_digest.Content_ref.pp reference);
      }
  in
  let rec loop = function
    | [] -> Ok ()
    | reference :: rest -> (
        match Attachment.get t ~session:from reference with
        | Error error -> Error (attachment_seed_error ~from error)
        | Ok None -> Error (missing reference)
        | Ok (Some bytes) -> (
            match Attachment.put t ~session:into bytes with
            | Error error -> Error (attachment_seed_error ~from error)
            | Ok (_ : Mentat_digest.Content_ref.t) -> loop rest))
  in
  loop (Mentat_session.media_refs child)

let fork t ~from ~events child =
  let into = Mentat_session.id child in
  Session.create_seeded t child ~seed:(fun () ->
      match
        Result.map_error
          (seed_error_as_session ~from)
          (Mutation.copy_into t ~from ~into ~events)
      with
      | Error _ as error -> error
      | Ok () -> copy_attachment_blobs t ~from ~into child)

(* Undo commit truncation: drop the crossed turns from both durable halves of one
   session under its document lock. The ledger is rewritten in place first —
   ledger-first ordering keeps the session loadable across a crash between the two
   writes (see {!Mutation.rewrite_ledger}) — then the truncated document is
   committed by whole-document CAS against the still-current [document]. Both name
   the surviving turns: [ledger] is the prefix the driver derived through
   [prefix_for_turns], [session] the document replayed up to but not including the
   first crossed turn. On [Ok (doc, mstate)] both halves are durable and coherent
   and [mstate] is the post-truncate mutation anchor the driver adopts. A mutation
   error surfaces on the session error surface, exactly as {!fork}'s seed does. *)
let truncate t ~fence ~document ~ledger session =
  let from = Session.Document.id document in
  match
    Result.map_error
      (seed_error_as_session ~from)
      (Mutation.rewrite_ledger t ~fence ~document ledger)
  with
  | Error _ as error -> error
  | Ok mstate -> (
      match Session.commit t ~fence document session with
      | Error _ as error -> error
      | Ok doc -> Ok (doc, mstate))
