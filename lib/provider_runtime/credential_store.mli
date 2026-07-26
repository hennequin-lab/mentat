(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The bespoke credential file machinery for [auth.json].

    Credentials are secret-bearing and never enter the byte-opaque session
    store. The runtime owns the file directly: a [Credential.Store.jsont]
    decode, an atomic temp + [0o600] + rename write, and filesystem
    store/credential locks, all bound to a [config_dir] capability the
    executable resolves. This module is private behind root account discovery
    and [Login]; there is no public secret-bearing read or write.

    A loaded {!Mentat_provider.Credential.Store.t} is secret-bearing;
    maintainers must not log or format it. The locks serialize concurrent access
    both in-process (an [Eio.Mutex] per canonical lock path) and across
    processes (an advisory [lockf]), so save, refresh, and logout operations on
    one slot serialize without holding the global store lock during network I/O.
*)

type t
(** A credential store bound to its [config_dir/auth.json] file. *)

val make : config_dir:Eio.Fs.dir_ty Eio.Path.t -> t
(** [make ~config_dir] binds the store to [config_dir/auth.json]. Performs no
    I/O; the file is read lazily. Raises [Invalid_argument] if [config_dir] is
    not a native filesystem path (locks require one). *)

val load : t -> (Mentat_provider.Credential.Store.t, Store_error.t) result
(** [load t] is the current store snapshot, or [Store.empty] when the file does
    not exist. Reads without locking. [Error (Decode _)] when [auth.json] is
    present but not decodable, [Error (Io _)] on a read failure. The returned
    store is secret-bearing. *)

val with_store_lock :
  t ->
  env:Eio_unix.Stdenv.base ->
  (unit -> ('a, Store_error.t) result) ->
  ('a, Store_error.t) result
(** [with_store_lock t ~env f] runs [f] holding the store-file lock, retrying
    with a capped backoff for as long as the lock is contended and releasing it
    even if [f] raises. [Error (Locked _)] when the lock cannot be taken for a
    reason other than contention (for example a permission failure on the
    [.lock] file). Fiber cancellation while waiting is re-raised. *)

val with_credential_lock :
  t ->
  env:Eio_unix.Stdenv.base ->
  provider:Mentat_llm.Provider.t ->
  name:Mentat_provider.Credential.Name.t ->
  (unit -> ('a, Store_error.t) result) ->
  ('a, Store_error.t) result
(** [with_credential_lock t ~env ~provider ~name f] is {!with_store_lock} over
    the per-credential lock keyed by [provider] and [name], so operations racing
    on the same slot serialize. *)

val save :
  t ->
  env:Eio_unix.Stdenv.base ->
  Mentat_provider.Credential.Store.t ->
  (unit, Store_error.t) result
(** [save t ~env store] writes [store] atomically ([0o600] temp then rename).
    The caller holds the store lock. *)

val edit :
  t ->
  env:Eio_unix.Stdenv.base ->
  f:
    (Mentat_provider.Credential.Store.t ->
    Mentat_provider.Credential.Store.t * 'a) ->
  ('a, Store_error.t) result
(** [edit t ~env ~f] loads the store, applies [f], and saves the returned store,
    all under the store lock. It returns [f]'s accompanying value only after the
    save succeeds, so callers can carry facts derived from the exact committed
    snapshot without reloading it. *)
