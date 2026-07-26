(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let tag = "mentat.session"
let format_version = 2

let image_refs images =
  List.filter_map
    (function
      | Mentat_mutation.Image.Text reference -> Some reference
      | Mentat_mutation.Image.Missing -> None)
    images

let refs_of_event = function
  | Mentat_mutation.Event.Checkpoint _ | Mentat_mutation.Event.Tool_observed _
    ->
      []
  | Mentat_mutation.Event.Edit { changes; _ } ->
      image_refs
        (List.concat_map
           (fun change ->
             [
               Mentat_mutation.Change.before change;
               Mentat_mutation.Change.after change;
             ])
           changes)
  | Mentat_mutation.Event.Revert_started started ->
      let { Mentat_mutation.Revert.Started.targets; _ } = started in
      image_refs
        (List.concat_map
           (fun target ->
             let { Mentat_mutation.Revert.Target.expected; restore; _ } =
               target
             in
             [ expected; restore ])
           targets)
  | Mentat_mutation.Event.Revert_settled settled ->
      let { Mentat_mutation.Revert.Settled.changes; _ } = settled in
      image_refs
        (List.concat_map
           (fun change ->
             [
               Mentat_mutation.Change.before change;
               Mentat_mutation.Change.after change;
             ])
           changes)

let referenced_blobs events =
  let seen = Hashtbl.create 16 in
  List.concat_map
    (fun event ->
      List.filter_map
        (fun reference ->
          let token = Mentat_digest.Content_ref.to_token reference in
          if Hashtbl.mem seen token then None
          else begin
            Hashtbl.add seen token ();
            Some reference
          end)
        (refs_of_event event))
    events

(* The complete blob set a bundle closes over: the mutation events' images
   unioned with the document's media attachments, deduplicated with the
   mutation-event refs first and any new document-media refs after — the exact
   order {!Export.write} emits and {!decode} requires. *)
let referenced_blobs_all document events =
  let mutation = referenced_blobs events in
  let seen = Hashtbl.create 16 in
  List.iter
    (fun reference ->
      Hashtbl.replace seen (Mentat_digest.Content_ref.to_token reference) ())
    mutation;
  let media =
    List.filter
      (fun reference ->
        let token = Mentat_digest.Content_ref.to_token reference in
        if Hashtbl.mem seen token then false
        else (
          Hashtbl.add seen token ();
          true))
      (Mentat_session.media_refs document)
  in
  mutation @ media

module Error = struct
  type t = Truncated | Corrupt of string

  let message = function
    | Truncated -> "truncated export bundle"
    | Corrupt message -> "corrupt export bundle: " ^ message

  let pp ppf t = Format.pp_print_string ppf (message t)
end

type t = {
  document_version : int;
  session : Mentat_session.t;
  events : Mentat_mutation.Event.t list;
  blobs : (Mentat_digest.Content_ref.t * string) list;
}

(* The embedded document's own version member, read back from a document section
   line so the header's [document_version] can be verified against the document
   the stream actually carries. *)
let embedded_version_jsont =
  Jsont.Object.map ~kind:"session document version" Fun.id
  |> Jsont.Object.mem "version" Jsont.int ~enc:Fun.id
  |> Jsont.Object.skip_unknown |> Jsont.Object.finish

let document_version_jsont =
  Jsont.Object.map ~kind:"document section version" Fun.id
  |> Jsont.Object.mem "document" embedded_version_jsont ~enc:Fun.id
  |> Jsont.Object.skip_unknown |> Jsont.Object.finish

(* Header triple: export tag, format version, document version. *)
let header_jsont =
  Jsont.Object.map ~kind:"export header"
    (fun export format_version document_version ->
      (export, format_version, document_version))
  |> Jsont.Object.mem "export" Jsont.string ~enc:(fun (export, _, _) -> export)
  |> Jsont.Object.mem "format_version" Jsont.int ~enc:(fun (_, format, _) ->
      format)
  |> Jsont.Object.mem "document_version" Jsont.int ~enc:(fun (_, _, document) ->
      document)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

module Line = struct
  type t =
    | Document of Mentat_session.t
    | Event of Mentat_mutation.Event.t
    | Blob of { reference : Mentat_digest.Content_ref.t; bytes : string }
    | End of { events : int; blobs : int; digest : string }

  let jsont =
    let document_case =
      Jsont.Object.map ~kind:"document section" (fun session ->
          Document session)
      |> Jsont.Object.mem "document" Mentat_session.jsont ~enc:(function
        | Document session -> session
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "document" ~dec:Fun.id
    in
    let event_case =
      Jsont.Object.map ~kind:"event section" (fun event -> Event event)
      |> Jsont.Object.mem "event" Mentat_mutation.Event.jsont ~enc:(function
        | Event event -> event
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "event" ~dec:Fun.id
    in
    let blob_case =
      Jsont.Object.map ~kind:"blob section" (fun reference bytes ->
          Blob { reference; bytes })
      |> Jsont.Object.mem "ref" Mentat_digest.Content_ref.jsont ~enc:(function
        | Blob { reference; _ } -> reference
        | _ -> assert false)
      |> Jsont.Object.mem "bytes" Jsont.string ~enc:(function
        | Blob { bytes; _ } -> bytes
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "blob" ~dec:Fun.id
    in
    let end_case =
      Jsont.Object.map ~kind:"end section" (fun events blobs digest ->
          End { events; blobs; digest })
      |> Jsont.Object.mem "events" Jsont.int ~enc:(function
        | End { events; _ } -> events
        | _ -> assert false)
      |> Jsont.Object.mem "blobs" Jsont.int ~enc:(function
        | End { blobs; _ } -> blobs
        | _ -> assert false)
      |> Jsont.Object.mem "digest" Jsont.string ~enc:(function
        | End { digest; _ } -> digest
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "end" ~dec:Fun.id
    in
    let cases =
      List.map Jsont.Object.Case.make
        [ document_case; event_case; blob_case; end_case ]
    in
    let enc_case = function
      | Document _ as line -> Jsont.Object.Case.value document_case line
      | Event _ as line -> Jsont.Object.Case.value event_case line
      | Blob _ as line -> Jsont.Object.Case.value blob_case line
      | End _ as line -> Jsont.Object.Case.value end_case line
    in
    Jsont.Object.map ~kind:"export line" Fun.id
    |> Jsont.Object.case_mem "section" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

(* Lines with the byte offset at which each starts; a final [\n]-less
   fragment cannot prove completeness and is [Error.Truncated]. *)
let lines input =
  let length = String.length input in
  let rec go acc position =
    if position >= length then Ok (List.rev acc)
    else
      match String.index_from_opt input position '\n' with
      | None -> Error Error.Truncated
      | Some newline ->
          go
            ((String.sub input position (newline - position), position) :: acc)
            (newline + 1)
  in
  go [] 0

let manifest_digest input ~end_offset =
  Mentat_digest.to_hex (Mentat_digest.string (String.sub input 0 end_offset))

let decode_blob reference bytes =
  match Base64.decode bytes with
  | Error (`Msg message) -> Error (Error.Corrupt ("blob bytes: " ^ message))
  | Ok bytes ->
      if Mentat_digest.Content_ref.matches reference bytes then
        Ok (reference, bytes)
      else Error (Error.Corrupt "blob bytes do not match their reference")

(* The blob section must be exactly the images the events and the document's
   media reference: one line per reference, none extra, none absent. *)
let closed ~document ~events ~blobs =
  let referenced = Hashtbl.create 16 in
  List.iter
    (fun reference ->
      Hashtbl.replace referenced
        (Mentat_digest.Content_ref.to_token reference)
        ())
    (referenced_blobs_all document events);
  let present = Hashtbl.create 16 in
  let rec check = function
    | [] ->
        if Hashtbl.length present < Hashtbl.length referenced then
          Error (Error.Corrupt "referenced blob missing from the bundle")
        else Ok ()
    | (reference, _) :: rest ->
        let token = Mentat_digest.Content_ref.to_token reference in
        if Hashtbl.mem present token then
          Error (Error.Corrupt "duplicate blob for one reference")
        else if not (Hashtbl.mem referenced token) then
          Error (Error.Corrupt "blob not referenced by any event")
        else begin
          Hashtbl.add present token ();
          check rest
        end
  in
  check blobs

let decode input =
  match lines input with
  | Error _ as error -> error
  | Ok [] -> Error Error.Truncated
  | Ok ((header_line, _) :: rest) -> (
      match Jsont_bytesrw.decode_string header_jsont header_line with
      | Error message -> Error (Error.Corrupt ("header: " ^ message))
      | Ok (export, format, document_version) ->
          if not (String.equal export tag) then
            Error (Error.Corrupt ("unknown export tag: " ^ export))
          else if format <> format_version then
            Error
              (Error.Corrupt
                 ("unsupported format version: " ^ string_of_int format))
          else
            (* Sections in stream order: document, events, blobs, end. *)
            let rec walk ~session ~events ~blobs = function
              | [] -> Error Error.Truncated
              | (text, offset) :: rest -> (
                  match Jsont_bytesrw.decode_string Line.jsont text with
                  | Error message -> Error (Error.Corrupt message)
                  | Ok (Line.Document decoded) -> (
                      if Option.is_some session || events <> [] || blobs <> []
                      then Error (Error.Corrupt "document section out of order")
                      else
                        (* The header's [document_version] must equal the
                           embedded document's own version: a bundle whose header
                           promises one schema and whose document is another is
                           corrupt, not a version the decoder guesses. *)
                        match
                          Jsont_bytesrw.decode_string document_version_jsont
                            text
                        with
                        | Error message ->
                            Error
                              (Error.Corrupt ("document version: " ^ message))
                        | Ok embedded when embedded <> document_version ->
                            Error
                              (Error.Corrupt
                                 (Printf.sprintf
                                    "document version %d does not match header \
                                     version %d"
                                    embedded document_version))
                        | Ok _ ->
                            walk ~session:(Some decoded) ~events ~blobs rest)
                  | Ok (Line.Event event) ->
                      if Option.is_none session || blobs <> [] then
                        Error (Error.Corrupt "event section out of order")
                      else walk ~session ~events:(event :: events) ~blobs rest
                  | Ok (Line.Blob { reference; bytes }) -> (
                      if Option.is_none session then
                        Error (Error.Corrupt "blob section out of order")
                      else
                        match decode_blob reference bytes with
                        | Error _ as error -> error
                        | Ok blob ->
                            walk ~session ~events ~blobs:(blob :: blobs) rest)
                  | Ok
                      (Line.End
                         { events = event_count; blobs = blob_count; digest })
                    -> (
                      match session with
                      | None ->
                          Error (Error.Corrupt "bundle carries no document")
                      | Some session -> (
                          if rest <> [] then
                            Error
                              (Error.Corrupt
                                 "content after the terminal manifest")
                          else if
                            event_count <> List.length events
                            || blob_count <> List.length blobs
                          then Error Error.Truncated
                          else if
                            not
                              (String.equal digest
                                 ("sha256:"
                                 ^ manifest_digest input ~end_offset:offset))
                          then Error Error.Truncated
                          else
                            let events = List.rev events in
                            let blobs = List.rev blobs in
                            match closed ~document:session ~events ~blobs with
                            | Error _ as error -> error
                            | Ok () -> (
                                (* The events must replay: a bundle is only
                                   as valid as the history it installs. *)
                                match
                                  Mentat_mutation.State.of_events events
                                with
                                | Error error ->
                                    Error
                                      (Error.Corrupt
                                         ("events do not replay: "
                                         ^ Mentat_mutation.State.Error.message
                                             error))
                                | Ok _ -> (
                                    match
                                      Correlation.check
                                        ~mode:Correlation.History session events
                                    with
                                    | Error error ->
                                        Error
                                          (Error.Corrupt
                                             (Correlation.message error))
                                    | Ok () ->
                                        Ok
                                          {
                                            document_version;
                                            session;
                                            events;
                                            blobs;
                                          })))))
            in
            walk ~session:None ~events:[] ~blobs:[] rest)
