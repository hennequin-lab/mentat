(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let sessions_dir = "sessions"
let reviews_dir = "reviews"
let snapshots_dir = "snapshots"
let escaped_id id = Disk.escaped_component (Mentat_session.Id.to_string id)
let session_dir id = sessions_dir ^ "/" ^ escaped_id id
let document id = session_dir id ^ "/session.json"
let doc_lock id = sessions_dir ^ "/" ^ escaped_id id ^ ".lock"
let run_lock id = session_dir id ^ "/run.lock"
let ledger_name = "ledger.jsonl"
let ledger id = session_dir id ^ "/" ^ ledger_name
let blobs_dir id = session_dir id ^ "/blobs"

let shard reference =
  Mentat_digest.key ~length:2 ~domain:"mentat.store.blob-shard.v1"
    [ Mentat_digest.Content_ref.to_token reference ]

let blob_shard_dir id reference = blobs_dir id ^ "/" ^ shard reference

(* Reference tokens contain [:], which is not portable in path components. *)
let blob_name reference =
  String.map
    (fun c -> if Char.equal c ':' then '-' else c)
    (Mentat_digest.Content_ref.to_token reference)

let blob id reference = blob_shard_dir id reference ^ "/" ^ blob_name reference
let attachments_dir id = session_dir id ^ "/attachments"

let attachment_shard_dir id reference =
  attachments_dir id ^ "/" ^ shard reference

let attachment id reference =
  attachment_shard_dir id reference ^ "/" ^ blob_name reference
