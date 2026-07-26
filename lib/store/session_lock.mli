(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The one write-serialization domain for a session's durable state.

    Private to the store handles. Every write that belongs to a session — the
    document commit, a mutation append, an export capture — runs inside this
    single critical section, so none of them interleave. The domain has two
    halves that hold together: the process-local {!Handle.Registry} mutex keyed
    by the session id serializes writers that share one opened root, and the
    POSIX advisory lock on the session's document lock file excludes writers in
    other processes. *)

val with_ : Handle.t -> Mentat_session.Id.t -> (unit -> 'a) -> 'a
(** [with_ root id f] runs [f] holding [id]'s write-serialization domain under
    [root] — the process-local keyed mutex and then the cross-process advisory
    lock, both released when [f] returns or raises. *)
