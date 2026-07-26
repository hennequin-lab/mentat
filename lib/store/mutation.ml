(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Result.Syntax

let invalid fn message = Import.invalid_arg' "Mentat_store.Mutation" fn message

module Error = struct
  module Correlation = Correlation

  type t =
    | Invalid_line of { path : string; line : int; detail : string }
    | Invalid_history of Mentat_mutation.State.Error.t
    | Correlation of Correlation.t
    | Document of Session.Error.t
    | Blob_mismatch of {
        expected : Mentat_digest.Content_ref.t;
        actual : Mentat_digest.Content_ref.t;
      }
    | Missing_blob of Mentat_digest.Content_ref.t
    | Non_regular_file of string
    | Io of Io.t

  let message = function
    | Invalid_line { path; line; detail } ->
        Format.sprintf "%s:%d: invalid mutation event: %s" path line detail
    | Invalid_history error ->
        "invalid mutation history: " ^ Mentat_mutation.State.Error.message error
    | Correlation error ->
        "mutation/session correlation failed: " ^ Correlation.message error
    | Document error -> Session.Error.message error
    | Blob_mismatch { expected; actual } ->
        Format.asprintf "blob %a holds bytes hashing to %a"
          Mentat_digest.Content_ref.pp expected Mentat_digest.Content_ref.pp
          actual
    | Missing_blob reference ->
        Format.asprintf "referenced blob %a is missing"
          Mentat_digest.Content_ref.pp reference
    | Non_regular_file path -> path ^ ": is not a regular file"
    | Io io -> Io.message io

  let pp ppf t = Format.pp_print_string ppf (message t)

  let diagnostic ~session error =
    let subject =
      Format.asprintf "mutation history for session %a is invalid"
        Mentat_session.Id.pp session
    in
    match error with
    | Invalid_line _ | Invalid_history _ | Correlation _ | Blob_mismatch _
    | Missing_blob _ | Non_regular_file _ ->
        Mentat_diagnostic.make ~context:(message error) subject
    | Document error -> Session.Error.diagnostic error
    | Io io -> Mentat_diagnostic.make (Io.message io)
end

let io ~op ~path f =
  Result.map_error (fun payload -> Error.Io payload) (Disk.guard ~op ~path f)

let path_kind t rel =
  Result.map_error
    (fun payload -> Error.Io payload)
    (Disk.path_kind ~path:rel (Handle.path t rel))

(* Reading. *)

(* [segments] is a [\n]-split: every intermediate segment was a terminated line;
   a non-empty final segment is a torn tail and is dropped. *)
let events_of_text ~path text =
  let segments = String.split_on_char '\n' text in
  let rec go line events = function
    | [] | [ _ ] -> Ok (List.rev events)
    | segment :: rest -> (
        match
          Jsont_bytesrw.decode_string Mentat_mutation.Event.jsont segment
        with
        | Ok event -> go (line + 1) (event :: events) rest
        | Error detail -> Error (Error.Invalid_line { path; line; detail }))
  in
  go 1 [] segments

let state_of_events events =
  match Mentat_mutation.State.of_events events with
  | Ok state -> Ok state
  | Error error -> Error (Error.Invalid_history error)

let read_state t ~session =
  let path = Layout.ledger session in
  match path_kind t path with
  | Error _ as error -> error
  | Ok `Missing -> state_of_events []
  | Ok `File ->
      let* text =
        io ~op:Io.Read ~path (fun () -> Eio.Path.load (Handle.path t path))
      in
      let* events = events_of_text ~path text in
      state_of_events events
  | Ok (`Directory | `Other) -> Error (Error.Non_regular_file path)

let read t document =
  let session = Session.Document.id document in
  let* state = read_state t ~session in
  match
    Correlation.check ~mode:Correlation.History
      (Session.Document.session document)
      (Mentat_mutation.State.events state)
  with
  | Ok () -> Ok state
  | Error error -> Error (Error.Correlation error)

(* Appending. *)

let encode_event event =
  match Jsont_bytesrw.encode_string Mentat_mutation.Event.jsont event with
  | Ok text -> text ^ "\n"
  | Error message -> invalid "append" ("event does not encode: " ^ message)

let ensure_document ~fence document =
  let fenced = Run_lock.session fence in
  let supplied = Session.Document.id document in
  if not (Mentat_session.Id.equal fenced supplied) then
    invalid "append"
      (Format.asprintf "fence is for session %a, not document %a"
         Mentat_session.Id.pp fenced Mentat_session.Id.pp supplied)

let current_document t document =
  let id = Session.Document.id document in
  match Session.load t id with
  | Error error -> Error (Error.Document error)
  | Ok current ->
      let expected = Session.Document.revision document in
      let actual = Session.Document.revision current in
      if Mentat_digest.equal expected actual then Ok current
      else
        Error (Error.Document (Session.Error.Conflict { id; expected; actual }))

let anchor t ~fence ~session =
  let cell = fence.Handle.anchor in
  match !cell with
  | Some state -> Ok state
  | None ->
      let* state = read_state t ~session in
      let dir = Layout.session_dir session in
      let* () =
        io ~op:Io.Sync ~path:dir (fun () ->
            Disk.fsync_dir ~path:dir (Handle.path t dir))
      in
      Ok state

let write_events t ~fence ~session ~next events =
  let cell = fence.Handle.anchor in
  let lines = String.concat "" (List.map encode_event events) in
  let ledger = Layout.ledger session in
  let dir = Layout.session_dir session in
  match
    io ~op:Io.Write ~path:ledger (fun () ->
        Disk.append ~ledger:(Handle.path t ledger) ~ledger_path:ledger
          ~dir:(Handle.path t dir) ~dir_path:dir lines)
  with
  | Ok () ->
      cell := Some next;
      Ok ()
  | Error _ as error ->
      cell := None;
      error
  | exception exn ->
      cell := None;
      raise exn

(* Exact document comparison, correlation, blob persistence, and event
   durability are one critical section. [persist] runs only after every semantic
   check has succeeded, so rejected appends leave both ledger and blob tree
   untouched. *)
let append_batch t ~fence ~document ~persist events =
  Handle.check_live t fence;
  ensure_document ~fence document;
  let session = Session.Document.id document in
  match events with
  | [] -> anchor t ~fence ~session
  | events -> (
      let run () =
        Handle.check_live t fence;
        let* current = current_document t document in
        let* anchored = anchor t ~fence ~session in
        let* next =
          match Mentat_mutation.State.append anchored events with
          | Ok next -> Ok next
          | Error error -> Error (Error.Invalid_history error)
        in
        let session_value = Session.Document.session current in
        let* () =
          match
            Correlation.check ~mode:Correlation.History session_value
              (Mentat_mutation.State.events next)
          with
          | Ok () -> Ok ()
          | Error error -> Error (Error.Correlation error)
        in
        let* () =
          match
            Correlation.check ~mode:Correlation.Live session_value events
          with
          | Ok () -> Ok ()
          | Error error -> Error (Error.Correlation error)
        in
        Eio.Cancel.protect (fun () ->
            let* () = persist session in
            let* () = write_events t ~fence ~session ~next events in
            Ok next)
      in
      match Session_lock.with_ t session run with
      | result -> result
      | exception Disk.Io_error payload -> Error (Error.Io payload))

(* Blob-referencing arms are rejected here so their blobs-before-event law cannot
   be bypassed: their bytes enter only through the composite ops below. Only the
   blob-free arms — checkpoints and tool observations — cross the plain append. *)
let append t ~fence ~document events =
  List.iter
    (fun event ->
      match event with
      | Mentat_mutation.Event.Edit _ ->
          invalid "append" "edit events append through append_edit"
      | Mentat_mutation.Event.Revert_started _ ->
          invalid "append"
            "revert started events append through append_revert_started"
      | Mentat_mutation.Event.Revert_settled _ ->
          invalid "append"
            "revert settled events append through append_revert_settled"
      | Mentat_mutation.Event.Checkpoint _
      | Mentat_mutation.Event.Tool_observed _ ->
          ())
    events;
  append_batch t ~fence ~document ~persist:(fun _ -> Ok ()) events

(* Blobs. *)

let blob t ~session reference =
  let path = Layout.blob session reference in
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

(* Write-once: same bytes converge to a no-op — but a pre-existing regular file
   is verified against the reference, never trusted, so an externally damaged
   blob is [Blob_mismatch] and no event that references it is appended. A
   distinct write goes parent syncs + tmp + fsync + rename + shard-dir fsync. The
   parent entry syncs are unconditional: an entry inherited unsynced from a
   previous process becomes durable before this write relies on it. *)
let put_blob t ~session text =
  let reference = Mentat_digest.Content_ref.of_contents text in
  let rel = Layout.blob session reference in
  match path_kind t rel with
  | Error _ as error -> error
  | Ok `File -> (
      let* bytes =
        io ~op:Io.Read ~path:rel (fun () -> Eio.Path.load (Handle.path t rel))
      in
      match Mentat_digest.Content_ref.verify reference bytes with
      | Ok () -> Ok ()
      | Error actual ->
          Error (Error.Blob_mismatch { expected = reference; actual }))
  | Ok `Missing ->
      let session_dir = Layout.session_dir session in
      let blobs_dir = Layout.blobs_dir session in
      let shard_dir = Layout.blob_shard_dir session reference in
      io ~op:Io.Write ~path:blobs_dir (fun () ->
          let (_ : bool) =
            Disk.mkdir ~path:blobs_dir (Handle.path t blobs_dir)
          in
          let (_ : bool) =
            Disk.mkdir ~path:shard_dir (Handle.path t shard_dir)
          in
          Disk.fsync_dir ~path:blobs_dir (Handle.path t blobs_dir);
          Disk.fsync_dir ~path:session_dir (Handle.path t session_dir);
          Disk.replace ~dir:(Handle.path t shard_dir) ~dir_path:shard_dir
            ~name:(Layout.blob_name reference)
            text)
  | Ok (`Directory | `Other) -> Error (Error.Non_regular_file rel)

(* Blobs land before their event. [append_batch] calls this only after document,
   semantic, and correlation validation and while still holding the shared
   session document lock, so a rejected batch leaves no unreferenced blob. *)
let put_texts t ~session texts =
  List.fold_left
    (fun acc text ->
      let* () = acc in
      put_blob t ~session text)
    (Ok ()) texts

let entry_texts entries =
  List.concat_map
    (fun entry ->
      List.filter_map Mentat_edit.Observed.text
        [
          Mentat_edit.Result.Entry.before entry;
          Mentat_edit.Result.Entry.after entry;
        ])
    entries

let append_edit t ~fence ~document ~entries event =
  Handle.check_live t fence;
  (* The derivation predicate is mutation-owned: the event must be exactly the
     lowering of [entries] under its own correlation ids, ordinal, and stopping
     target. A successful apply lowers with no target ([uncertain = None]); a
     commit-phase attempt carries its uncertain target. The event's own
     [uncertain] field selects between them, so both cross this one seam. *)
  (match event with
  | Mentat_mutation.Event.Edit
      { turn; claim; ordinal; checkpoint; uncertain; _ } ->
      let expected =
        Mentat_mutation.Event.of_attempt ~turn ~claim ~ordinal ~checkpoint
          ~applied:entries ~uncertain
      in
      if not (Mentat_mutation.Event.equal event expected) then
        invalid "append_edit" "event does not derive from entries"
  | _ -> invalid "append_edit" "event is not an edit event");
  append_batch t ~fence ~document
    ~persist:(fun session -> put_texts t ~session (entry_texts entries))
    [ event ]

(* Verify each referenced blob is present and intact before appending, so a
   started event never lands referencing a blob external damage has removed or
   corrupted. Recorded restore images (net-before) are durable since the events
   that recorded them and are verified here; a clean merge's restore image is
   fresh and is put, not verified (see {!append_revert_started}). *)
let verify_present t ~session references =
  List.fold_left
    (fun acc reference ->
      let* () = acc in
      match blob t ~session reference with
      | Ok (Some _) -> Ok ()
      | Ok None -> Error (Error.Missing_blob reference)
      | Error _ as error -> error)
    (Ok ()) references

let image_ref = function
  | Mentat_mutation.Image.Text reference -> Some reference
  | Mentat_mutation.Image.Missing -> None

let append_revert_started t ~fence ~document ~plan ~current event =
  Handle.check_live t fence;
  let started =
    match event with
    | Mentat_mutation.Event.Revert_started started -> started
    | _ -> invalid "append_revert_started" "event is not a revert started event"
  in
  if
    not
      (Mentat_mutation.Revert.Started.equal started
         (Mentat_mutation.Revert.Plan.started plan))
  then invalid "append_revert_started" "event does not derive from plan";
  let targets =
    (Mentat_mutation.Revert.Plan.started plan)
      .Mentat_mutation.Revert.Started.targets
  in
  (* The started fact's expected images are the plan-time reads; for an
     overridden non-contiguous target those bytes are durable nowhere else. Each
     read must derive its target's frozen expected image, so the bytes stored
     here are exactly the images the event references. *)
  let texts =
    List.filter_map
      (fun (target : Mentat_mutation.Revert.Target.t) ->
        let read =
          match
            List.find_opt
              (fun (path, _) ->
                Mentat_workspace.Path.equal path
                  target.Mentat_mutation.Revert.Target.path)
              current
          with
          | Some (_, read) -> read
          | None ->
              invalid "append_revert_started"
                "current reads do not cover the plan targets"
        in
        match (target.Mentat_mutation.Revert.Target.expected, read) with
        | Mentat_mutation.Image.Missing, Mentat_edit.Observed.Missing -> None
        | Mentat_mutation.Image.Text reference, Mentat_edit.Observed.Text text
          when Mentat_digest.Content_ref.matches reference text ->
            Some text
        | _ ->
            invalid "append_revert_started"
              "current read does not derive the target's expected image")
      targets
  in
  (* Restore images are recorded net-before images whose durability is verified
     rather than assumed — except a clean merge's restore image, whose fresh
     bytes are supplied here through [merge_blobs] and made durable
     before the event lands. So the merge refs are put and the remaining
     (recorded) restore refs verified. *)
  let merge_blobs = Mentat_mutation.Revert.Plan.merge_blobs plan in
  let merge_refs = List.map fst merge_blobs in
  let restore_refs =
    targets
    |> List.filter_map (fun (target : Mentat_mutation.Revert.Target.t) ->
        image_ref target.Mentat_mutation.Revert.Target.restore)
    |> List.filter (fun reference ->
        not (List.exists (Mentat_digest.Content_ref.equal reference) merge_refs))
  in
  let* (_ : Mentat_mutation.State.t) =
    append_batch t ~fence ~document
      ~persist:(fun session ->
        let* () = put_texts t ~session texts in
        let* () = put_texts t ~session (List.map snd merge_blobs) in
        verify_present t ~session restore_refs)
      [ event ]
  in
  Ok ()

let append_revert_settled t ~fence ~document ~started ?outcome event =
  Handle.check_live t fence;
  let settled =
    match event with
    | Mentat_mutation.Event.Revert_settled settled -> settled
    | _ -> invalid "append_revert_settled" "event is not a revert settled event"
  in
  (* Re-derive the settlement the outcome classifies to: an absent outcome is
     crash recovery's, which may mint only the ambiguous settlement of the frozen
     started record. *)
  let expected =
    match outcome with
    | Some outcome -> Mentat_mutation.Revert.settle started outcome
    | None -> Mentat_mutation.Revert.settle_ambiguous started
  in
  if not (Mentat_mutation.Revert.Settled.equal settled expected) then
    invalid "append_revert_settled" "event does not derive from outcome";
  (* The restoration rows' images are the confirmed entries' text sides: frozen
     expected befores and restored afters, both stored write-once before the
     event lands. A recovered settlement carries no rows and no bytes. *)
  let entries =
    match outcome with
    | None -> []
    | Some (Ok result) -> Mentat_edit.Result.entries result
    | Some (Error apply_error) -> Mentat_edit.Apply_error.applied apply_error
  in
  let* (_ : Mentat_mutation.State.t) =
    append_batch t ~fence ~document
      ~persist:(fun session -> put_texts t ~session (entry_texts entries))
      [ event ]
  in
  Ok ()

(* Reverting. The whole fenced revert lifecycle: the workspace effects — the
   plan-time read, the conservative capture, and the confined edit application —
   enter as ports so this library links no workspace or edit-application backend,
   while the netting, planning, and the ordered three-fact append stay here where
   the ledger's own invariants live. *)

type revert_outcome =
  | Nothing_to_revert
  | Refused of Mentat_mutation.Revert.Problem.t list
  | Applied of Mentat_mutation.Revert.Settled.t

(* The plan's evidence: each candidate path's current read through the [observe]
   port, and the bytes of each net-before restore image from the blob store.
   Assembling it here keeps [Revert.prepare] pure — the planner performs no IO
   and is deterministic in its arguments. *)
let revert_evidence t ~session ~observe net =
  let current =
    List.map
      (fun (entry : Mentat_mutation.Change.Net.entry) ->
        ( entry.Mentat_mutation.Change.Net.path,
          observe entry.Mentat_mutation.Change.Net.path ))
      net
  in
  let blobs =
    (* Both the net-before restore image and the net-after merge base: the
       planner reads the before for an ordinary revert and, for a superseded
       entry, the after as the three-way merge's common ancestor. *)
    List.concat_map
      (fun (entry : Mentat_mutation.Change.Net.entry) ->
        List.filter_map
          (function
            | Mentat_mutation.Image.Text reference -> (
                match blob t ~session reference with
                | Ok (Some bytes) -> Some (reference, bytes)
                | Ok None | Error _ -> None)
            | Mentat_mutation.Image.Missing -> None)
          [
            entry.Mentat_mutation.Change.Net.before;
            entry.Mentat_mutation.Change.Net.after;
          ])
      net
  in
  (current, Mentat_mutation.Revert.Evidence.make ~current ~blobs)

let revert_apply ?(merge = true) ?override t ~fence ~document ~selection
    ~observe ~checkpoint ~apply ~new_id =
  let session = Session.Document.id document in
  let* current =
    Result.map_error (fun e -> Error.Document e) (Session.load t session)
  in
  let* state = read t current in
  match selection with
  | None -> Ok Nothing_to_revert
  | Some selection -> (
      let net = Mentat_mutation.State.net state selection in
      let current_reads, evidence = revert_evidence t ~session ~observe net in
      let revert_id = new_id () in
      match
        Mentat_mutation.Revert.prepare ~merge ?override ~id:revert_id ~evidence
          state selection
      with
      | Error problems -> Ok (Refused problems)
      | Ok plan ->
          let started = Mentat_mutation.Revert.Plan.started plan in
          (* The started fact freezes only ready targets; hand the composite
             exactly their plan-time reads. *)
          let target_paths =
            List.map
              (fun (target : Mentat_mutation.Revert.Target.t) ->
                target.Mentat_mutation.Revert.Target.path)
              started.Mentat_mutation.Revert.Started.targets
          in
          let plan_current =
            List.filter
              (fun (path, _) ->
                List.exists (Mentat_workspace.Path.equal path) target_paths)
              current_reads
          in
          let checkpoint_fact =
            checkpoint
              ~boundary:(Mentat_mutation.Checkpoint.Before_revert revert_id)
          in
          let* (_ : Mentat_mutation.State.t) =
            append t ~fence ~document:current
              [ Mentat_mutation.Event.checkpoint checkpoint_fact ]
          in
          let* () =
            append_revert_started t ~fence ~document:current ~plan
              ~current:plan_current
              (Mentat_mutation.Event.revert_started plan)
          in
          let outcome = apply (Mentat_mutation.Revert.Plan.edit plan) in
          let settled = Mentat_mutation.Revert.settle started outcome in
          let* () =
            append_revert_settled t ~fence ~document:current ~started ~outcome
              (Mentat_mutation.Event.revert_settled settled)
          in
          Ok (Applied settled))

(* Branching. Seed a freshly minted child's mutation store from its parent's:
   blobs land before the ledger that references them — the intra-domain ordering
   law carried into the child — and the ledger write is a plain append onto an
   absent file, so no fence or anchor is involved. The caller (a branch's atomic
   document creation) holds the child's document lock and guarantees the child
   has no concurrent writer, so this op takes no lock of its own. *)
let copy_into t ~from ~into ~events =
  let* () =
    List.fold_left
      (fun acc reference ->
        let* () = acc in
        match blob t ~session:from reference with
        | Ok (Some bytes) -> put_blob t ~session:into bytes
        | Ok None -> Error (Error.Missing_blob reference)
        | Error _ as error -> error)
      (Ok ())
      (Bundle.referenced_blobs events)
  in
  match events with
  | [] -> Ok ()
  | _ :: _ ->
      let lines = String.concat "" (List.map encode_event events) in
      let ledger = Layout.ledger into in
      let dir = Layout.session_dir into in
      io ~op:Io.Write ~path:ledger (fun () ->
          Disk.append ~ledger:(Handle.path t ledger) ~ledger_path:ledger
            ~dir:(Handle.path t dir) ~dir_path:dir lines)

(* Truncation. Rewrite the fenced session's ledger in place to [events] — the
   surviving-turn prefix an undo commit keeps, which the caller derives through
   {!Mentat_mutation.State.prefix_for_turns}. This is the one deliberate break in
   the true-append contract: not an append but a whole-file replace, the same
   prefix shape {!copy_into} writes for a branch, applied over the session's own
   ledger under its document lock. The caller commits the truncated document only
   after this returns (ledger-first ordering), so a crash between the two writes
   leaves the still-full document referencing a ledger that dropped a suffix —
   the prefix references a subset of the full document's turns, so correlation
   holds and the session stays loadable; a leftover armed boundary whose revert
   the prefix dropped is what resume reconciles to Released. On [Ok next] the
   ledger is durable and [next] is the refolded post-truncate anchor. *)
let rewrite_ledger t ~fence ~document events =
  Handle.check_live t fence;
  ensure_document ~fence document;
  let session = Session.Document.id document in
  let run () =
    Handle.check_live t fence;
    let* current = current_document t document in
    let* next = state_of_events events in
    let* () =
      match
        Correlation.check ~mode:Correlation.History
          (Session.Document.session current)
          (Mentat_mutation.State.events next)
      with
      | Ok () -> Ok ()
      | Error error -> Error (Error.Correlation error)
    in
    let cell = fence.Handle.anchor in
    let dir = Layout.session_dir session in
    let lines = String.concat "" (List.map encode_event events) in
    Eio.Cancel.protect (fun () ->
        match
          io ~op:Io.Write ~path:(Layout.ledger session) (fun () ->
              Disk.replace ~dir:(Handle.path t dir) ~dir_path:dir
                ~name:Layout.ledger_name lines)
        with
        | Ok () ->
            cell := Some next;
            Ok next
        | Error _ as error ->
            cell := None;
            error)
  in
  match Session_lock.with_ t session run with
  | result -> result
  | exception Disk.Io_error payload -> Error (Error.Io payload)
