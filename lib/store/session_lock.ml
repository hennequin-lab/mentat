(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* The one serialization domain for every write belonging to a session. The
   process-local half is shared by every handle opened over the same physical
   root; the advisory half excludes writers in other processes. *)
let with_ root id f =
  let key = "doc:" ^ Disk.escaped_component (Mentat_session.Id.to_string id) in
  Handle.Registry.with_keyed (Handle.registry root) key (fun () ->
      Disk.with_flock ~path:(Layout.doc_lock id)
        (Handle.path root (Layout.doc_lock id))
        f)
