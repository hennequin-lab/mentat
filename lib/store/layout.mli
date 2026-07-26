(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The [sessions/<id>] path vocabulary.

    Private machinery: every store-relative path under the [sessions/] tree is
    derived here, so the layout — which components are escaped, where each
    artifact lives — is stated once. Session ids are percent-escaped with
    {!Disk.escaped_component}; the directory-name inverse used by the list scan
    is {!Disk.unescaped_component}. *)

val sessions_dir : string
(** [sessions_dir] is the layout child holding one directory per session. *)

val reviews_dir : string
(** [reviews_dir] is the layout child holding review records, keyed by workspace
    root and base. *)

val snapshots_dir : string
(** [snapshots_dir] is the layout child holding the workspace-boundary capture
    stores, one per-workspace subdirectory (see {!Capture}). Unlike
    {!sessions_dir} and {!reviews_dir} it is not validated or created at
    {!Mentat_store.open_}; {!Capture} mints it under the root on first capture.
*)

val session_dir : Mentat_session.Id.t -> string
(** [session_dir id] is [sessions/<escaped id>] — the session's directory, the
    one physical reclamation unit. *)

val document : Mentat_session.Id.t -> string
(** [document id] is the session's whole-document CAS file, [session.json]. *)

val doc_lock : Mentat_session.Id.t -> string
(** [doc_lock id] is the document writers' advisory lock,
    [sessions/<escaped id>.lock] — a sibling of {!session_dir}, so [create] can
    take it before the directory exists. *)

val run_lock : Mentat_session.Id.t -> string
(** [run_lock id] is the session's run-fence lock file, [run.lock]. *)

val ledger_name : string
(** [ledger_name] is the basename of the mutation event log within a session
    directory, [ledger.jsonl] — the file name an in-place ledger replace targets
    inside the session-directory capability. *)

val ledger : Mentat_session.Id.t -> string
(** [ledger id] is the session's mutation event log, [ledger.jsonl]. *)

val blobs_dir : Mentat_session.Id.t -> string
(** [blobs_dir id] is the session's content-addressed blob tree root. *)

val blob_shard_dir :
  Mentat_session.Id.t -> Mentat_digest.Content_ref.t -> string
(** [blob_shard_dir id ref] is the two-hex-digit shard directory that holds
    [ref]'s blob: a fresh keyed hash {e over} the reference token, not a prefix
    of it — every token shares the constant [sha256:] prefix, so a prefix shard
    would collapse all blobs into one bucket. *)

val blob_name : Mentat_digest.Content_ref.t -> string
(** [blob_name ref] is [ref]'s blob file basename: the reference token with [:]
    mapped to [-] (a portable path component). *)

val blob : Mentat_session.Id.t -> Mentat_digest.Content_ref.t -> string
(** [blob id ref] is [ref]'s blob file: {!blob_name} under its shard. *)

val attachments_dir : Mentat_session.Id.t -> string
(** [attachments_dir id] is the session's attachment blob tree root,
    [sessions/<escaped id>/attachments] — a namespace distinct from
    {!blobs_dir}, holding model-visible media externalized from the transcript
    rather than file-change evidence. *)

val attachment_shard_dir :
  Mentat_session.Id.t -> Mentat_digest.Content_ref.t -> string
(** [attachment_shard_dir id ref] is the two-hex-digit shard directory holding
    [ref]'s attachment blob, keyed by the same reference-token hash as
    {!blob_shard_dir}, under {!attachments_dir}. *)

val attachment : Mentat_session.Id.t -> Mentat_digest.Content_ref.t -> string
(** [attachment id ref] is [ref]'s attachment blob file: {!blob_name} under its
    attachment shard. *)
