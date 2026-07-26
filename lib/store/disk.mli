(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Shared durable filesystem mechanics.

    Private machinery beneath the store handles: durable replace, ledger append
    with torn-tail repair, directory sync, and advisory locking with cancellable
    backoff. Every operation goes through the opened-root directory capability
    ([openat]-based), never a native path string: a native path resolves through
    the filesystem namespace and can name a different tree after a rename, so an
    op split across [native_exn] strings and the capability could compare bytes
    in one physical tree and replace bytes in another. Failures raise
    {!Io_error} with the failing primitive, the store-relative path, and the
    structured cause; domain modules catch it and wrap their own [Io] arm.
    Cancellation ([Eio.Cancel.Cancelled]) is never wrapped.

    Directory fsync is explicit and required to succeed on the first-class
    targets (Linux, macOS): there is no blanket [EINVAL] suppression.

    {b macOS durability bound.} On macOS, [fsync] flushes to the drive but does
    not force the drive's own volatile cache to stable media — that is
    [fcntl F_FULLFSYNC], which this store deliberately does not issue. Every
    durability claim built on {!fsync_fd} and {!fsync_dir} is therefore relative
    to [fsync]'s platform guarantee: on macOS, power loss (not process or kernel
    crash) can still lose a flushed write the drive had not yet committed. *)

exception Io_error of Io.t
(** The internal failure channel for every primitive below. *)

val raise_io : op:Io.op -> path:string -> exn -> 'a
(** [raise_io ~op ~path exn] rethrows [exn] as {!Io_error} with the given
    operation context, retaining a [Unix.error] structurally and rendering any
    other cause. [Eio.Cancel.Cancelled] and {!Io_error} pass through unwrapped.
*)

val guard : op:Io.op -> path:string -> (unit -> 'a) -> ('a, Io.t) result
(** [guard ~op ~path f] runs [f], turning an {!Io_error} payload — or any other
    non-cancellation exception, tagged with [op] and [path] — into [Error].
    [Eio.Cancel.Cancelled] re-raises. Each store domain injects its own [Io] arm
    over this neutral form. *)

val observe : what:string -> path:string -> exn -> unit
(** [observe ~what ~path exn] records [exn]'s raise context — called at the
    catch, before any further raise — as the store's last exception diagnostic
    and logs it at error level. {!raise_io} and {!guard} call it whenever they
    convert an exception into a fact; the fact keeps the message, this keeps the
    trace. *)

val last_exn_diagnostic : unit -> string option
(** [last_exn_diagnostic ()] is the most recent {!observe} record — operation,
    path, exception, and backtrace — for a crash report written after the
    converted error has propagated as a plain message. Process-global,
    last-writer-wins. *)

val path_kind :
  path:string ->
  _ Eio.Path.t ->
  ([ `Missing | `File | `Directory | `Other ], Io.t) result
(** [path_kind ~path cap] is the kind of [cap] via [Eio.Path.kind ~follow:true]:
    [`Missing] for a non-existent target, [`File] for a regular file,
    [`Directory] for a directory, and [`Other] for anything else. A filesystem
    failure is [Error] under [op = Read] and [path]; [Eio.Cancel.Cancelled]
    re-raises. *)

val escaped_component : string -> string
(** [escaped_component s] is [s] percent-escaped to a safe single path
    component: unreserved bytes ([A-Za-z0-9_-]) pass through, everything else
    becomes [%XX]. *)

val unescaped_component : string -> string option
(** [unescaped_component s] inverts {!escaped_component}, or [None] if [s]
    contains an invalid escape. *)

val fsync_fd : path:string -> Unix.file_descr -> unit
(** [fsync_fd ~path fd] flushes [fd] to stable storage, retrying [EINTR].
    Callable from a systhread against a descriptor an {!Eio_unix.Fd.t} lends. *)

val write_all : path:string -> Unix.file_descr -> string -> unit
(** [write_all ~path fd s] writes all of [s] at [fd]'s current position,
    retrying [EINTR] and short writes. Whether a failure changed the caller's
    target depends on the caller's seam — a temporary file's write leaves the
    target untouched until the following rename, while a ledger append writes
    the target in place — so a caller that requires coherence past its own
    linearization point reloads. Callable from a systhread. *)

val fd_of : _ Eio.Resource.t -> Eio_unix.Fd.t
(** [fd_of resource] is the {!Eio_unix.Fd.t} underlying an opened file
    capability, for descriptor-level locking and syncing. *)

val fsync_dir : path:string -> Eio.Fs.dir_ty Eio.Path.t -> unit
(** [fsync_dir ~path dir] opens the directory capability [dir] read-only and
    fsyncs it, making its entries durable. [path] labels the directory for
    diagnostics. *)

val mkdir : path:string -> Eio.Fs.dir_ty Eio.Path.t -> bool
(** [mkdir ~path dir] creates the directory capability [dir] with permissions
    [0o700], returning [true] iff this call created it. The parent must exist.
*)

val with_flock : path:string -> Eio.Fs.dir_ty Eio.Path.t -> (unit -> 'a) -> 'a
(** [with_flock ~path lock f] runs [f] holding the POSIX advisory lock on the
    lock file capability [lock], creating the file if needed. Acquisition is
    non-blocking [F_TLOCK] with exponential sleep backoff, so the wait is an Eio
    cancellation point and never parks the domain. POSIX record locks exclude
    other processes only; the caller supplies intra-process exclusion. *)

val replace :
  dir:Eio.Fs.dir_ty Eio.Path.t ->
  dir_path:string ->
  name:string ->
  string ->
  unit
(** [replace ~dir ~dir_path ~name contents] durably replaces the file [name]
    inside the directory capability [dir]: a fresh temporary sibling is written,
    fsynced, renamed over [name] through the same capability, and [dir] is
    fsynced. [dir_path] labels [dir] for diagnostics. A failed attempt unlinks
    its temporary. After the rename the new bytes are already published, so a
    directory-sync failure leaves durable state changed and a caller that
    requires coherence reloads. *)

val append :
  ledger:Eio.Fs.dir_ty Eio.Path.t ->
  ledger_path:string ->
  dir:Eio.Fs.dir_ty Eio.Path.t ->
  dir_path:string ->
  string ->
  unit
(** [append ~ledger ~ledger_path ~dir ~dir_path lines] appends [lines] to the
    file capability [ledger], repairing a torn tail first: any final [\n]-less
    fragment from a prior crash is truncated at the last record boundary, then
    [lines] is written at end-of-file, fsynced, and [dir] fsynced. Once the
    write begins the pre-append boundary is restored best-effort on failure so a
    retry cannot duplicate records, but the on-disk tail is nonetheless unknown,
    so a caller re-anchors from disk after any failure from the write onward. *)
