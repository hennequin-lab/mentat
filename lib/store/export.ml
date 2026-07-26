(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid fn message = Import.invalid_arg' "Mentat_store.Export" fn message

(* A loaded document, a validated event, or plain manifest data that fails to
   encode is impossible-by-construction — programmer error, never IO or
   corruption. *)
let encode_line line =
  match Jsont_bytesrw.encode_string Bundle.Line.jsont line with
  | Ok text -> text
  | Error message -> invalid "write" ("bundle line does not encode: " ^ message)

let write ~root ~fence ~write =
  Handle.check_live root fence;
  let id = Run_lock.session fence in
  (* Snapshot both ledgers under their one write lock, then release it before
     invoking the arbitrary output callback. A shared guard is not linear: the
     lock, not the caller's discipline, excludes a same-process commit between
     these two reads. *)
  let snapshot =
    match
      Session_lock.with_ root id (fun () ->
          Handle.check_live root fence;
          match Session.load root id with
          | Error error -> Error (`Session error)
          | Ok document -> (
              match Mutation.read root document with
              | Error error -> Error (`Mutation error)
              | Ok state -> Ok (document, state)))
    with
    | result -> result
    | exception Disk.Io_error payload ->
        Error (`Mutation (Mutation.Error.Io payload))
  in
  match snapshot with
  | Error _ as error -> error
  | Ok (document, state) -> (
      let events = Mentat_mutation.State.events state in
      Handle.check_live root fence;
      let document_line =
        encode_line (Bundle.Line.Document (Session.Document.session document))
      in
      let document_version =
        match
          Jsont_bytesrw.decode_string Bundle.document_version_jsont
            document_line
        with
        | Ok version -> version
        | Error message ->
            invalid "write" ("encoded document carries no version: " ^ message)
      in
      let header =
        match
          Jsont_bytesrw.encode_string Bundle.header_jsont
            (Bundle.tag, Bundle.format_version, document_version)
        with
        | Ok text -> text
        | Error message ->
            invalid "write" ("bundle header does not encode: " ^ message)
      in
      (* The manifest digest covers the exact bytes of every preceding line;
             {!Mentat_digest.Fold} hashes them incrementally as they are emitted,
             so no copy of the stream is retained. *)
      let covered = Mentat_digest.Fold.create () in
      let emit line =
        Mentat_digest.Fold.add covered line;
        Mentat_digest.Fold.add covered "\n";
        write (line ^ "\n")
      in
      emit header;
      emit document_line;
      List.iter
        (fun event -> emit (encode_line (Bundle.Line.Event event)))
        events;
      let references =
        Bundle.referenced_blobs_all (Session.Document.session document) events
      in
      (* A referenced image is either a mutation-event blob (file-change bytes)
         or a document-media attachment; read whichever namespace holds it. Under
         the no-GC blob rule a referenced image can only be missing through
         integrity damage. *)
      let read_blob reference =
        match Mutation.blob root ~session:id reference with
        | Error error -> Error (`Mutation error)
        | Ok (Some _ as bytes) -> Ok bytes
        | Ok None -> (
            match Attachment.get root ~session:id reference with
            | Ok (Some _ as bytes) -> Ok bytes
            | Ok None -> Ok None
            | Error (Attachment.Error.Io payload) ->
                Error (`Mutation (Mutation.Error.Io payload))
            | Error
                ( Attachment.Error.Blob_mismatch _
                | Attachment.Error.Non_regular_file _ ) ->
                Error (`Mutation (Mutation.Error.Missing_blob reference)))
      in
      let rec blobs count = function
        | [] -> Ok count
        | reference :: rest -> (
            Handle.check_live root fence;
            match read_blob reference with
            | Error _ as error -> error
            | Ok None ->
                Error (`Mutation (Mutation.Error.Missing_blob reference))
            | Ok (Some bytes) ->
                emit
                  (encode_line
                     (Bundle.Line.Blob
                        { reference; bytes = Base64.encode_string bytes }));
                blobs (count + 1) rest)
      in
      match blobs 0 references with
      | Error _ as error -> error
      | Ok blob_count ->
          (* The terminal manifest is the completeness claim: it is not
                 emitted over a capture whose fence has already fallen. *)
          Handle.check_live root fence;
          let digest =
            Mentat_digest.to_hex (Mentat_digest.Fold.digest covered)
          in
          write
            (encode_line
               (Bundle.Line.End
                  {
                    events = List.length events;
                    blobs = blob_count;
                    digest = "sha256:" ^ digest;
                  })
            ^ "\n");
          Ok ())
