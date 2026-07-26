(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Durable persistence — the one library that touches durable bytes on behalf
    of the engine.

    [mentat.store] is one package opened once as a single root capability
    ({!t}). Over that one root, {!Session} (session documents under
    whole-document CAS), {!Mutation} (the mutation event ledger and its
    content-addressed blobs), {!Review} (review records), the run fence
    ({!Run_lock}), and the export bridge ({!Export}) are independent views: each
    operation takes the root, and the root's device/inode identity keys the one
    lock registry every view shares. There is deliberately no cross-domain error
    type: the domains have different identities, concurrency laws, corruption
    meanings, and retention, and each surface states its own. Only opening the
    root — where a layout child of the wrong kind is the one shared failure —
    has a store-level {!Error.t}. The handles share private filesystem mechanics
    — capability-based durable replace, device/inode root identity, keyed locks,
    JSONL tail repair — that expose no product concept and no native path
    string.

    {b What the store deliberately does not own.} No clock: every op writes
    exactly the documents it is given — semantic metadata, [updated_at]
    included, is authored by its owning library before the byte-opaque save
    boundary. No session index: {!Session.scan} is an exact full scan, and
    lifecycle filtering, recency ordering, and search are combinators above it.
    No import: {!Export} only streams out — the bundle decoder is private to the
    library's round-trip tests, so nothing public can half-install a session.

    {b The load-bearing laws.} Every filesystem operation goes through the
    opened root's directory capability ([openat]-based), never a native path
    string, so a root rename can never split one op across two physical trees.
    Two locks with distinct lifetimes serve distinct jobs and neither
    substitutes for the other: a per-session document lock serializes each
    compare-and-set for milliseconds, so no update is lost even if the fence
    discipline is ever violated, while the run fence — held across a whole turn
    — guarantees one driver even though every CAS write is already individually
    safe. Same-process fence exclusion is reserve-before-open: POSIX record
    locks never conflict within one process and closing any descriptor drops the
    lock, so the fence holds exactly one [O_CLOEXEC] descriptor and an atomic
    registry reservation is taken before any descriptor is opened. Mutation
    appends and existing-session commits are fence-typed: the {!Run_lock.guard}
    parameter is the proof that this process is the session's sole driver, and a
    released or wrong-root guard raises. {!Mutation.append_edit} makes blobs
    durable before the event that references them; the settlement commit that
    references the event follows both. Corruption is fail-loud everywhere:
    corrupt is never missing and missing is never corrupt — {!Review.load}'s
    [Ok None] means missing, and only missing. Durability is local to each op:
    an op that creates a file fsyncs the directory entry it created before
    returning, and an op whose failure can leave durable state changed says so
    in its own contract. *)

module Io = Io
(** Shared filesystem-failure payloads. *)

(** {1:root Opening a root} *)

module Error : sig
  (** Errors from opening the store root. *)

  type t =
    | Layout of { path : string; message : string }
        (** A layout child ([sessions/], [reviews/]) exists and is not a
            directory: adopting it would misplace every op under it. *)
    | Io of Io.t  (** A filesystem primitive failed while opening the root. *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic for [e]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats {!message} output. *)
end

type t = Handle.t
(** The type for an opened store root: the single directory capability every
    domain view operates through, plus its interned per-root lock registry.
    Every {!Session}, {!Mutation}, {!Review}, {!Run_lock}, and {!Export}
    operation takes this root. Two opens of the same physical root (reached by
    different paths) share one registry; the registry is released when the last
    open over the root finishes.

    The concrete identity is the private opened-root handle; a consumer holds it
    opaquely — its accessors ([Handle] is not a public module) are the store's
    own, so no caller can compose the fence liveness check or reach a lock
    registry. *)

val open_ : sw:Eio.Switch.t -> Eio.Fs.dir_ty Eio.Path.t -> (t, Error.t) result
(** [open_ ~sw path] opens the existing directory capability [path] under [sw]
    as the store root and keeps it for the switch's lifetime. The layout
    children [sessions/] and [reviews/] are validated and created if absent — a
    child of the wrong kind is {!Error.Layout} — and directory-synced before
    success. Operations on the root are affine to the Eio domain that opened it.
*)

val last_exn_diagnostic : unit -> string option
(** [last_exn_diagnostic ()] is the raise context — operation, path, exception,
    and backtrace — of the most recent exception a store primitive converted
    into an error fact, recorded at the catch. The fact carries only the
    rendered message, and logging can be disabled, so a host writing a crash
    report after such an error propagates reads the trace here. Process-global,
    last-writer-wins. *)

(** {1:views Domain views}

    Each module is a set of operations over the {!t} returned by {!open_}; they
    share its registry and its single-domain affinity. *)

module Session = Session
(** Whole-document CAS persistence for session documents. *)

module Run_lock = Run_lock
(** The run fence — one driver per session. *)

module Mutation = Mutation
(** The per-session mutation event log and blob store. *)

module Attachment = Attachment
(** The per-session attachment blob store: content-addressed, write-once,
    fence-free media bytes externalized from the transcript, in a namespace
    distinct from {!Mutation}'s file-change blobs. *)

module Review = Review
(** CAS persistence for review records. *)

module Export = Export
(** The per-session export bridge. *)

module Capture = Capture
(** Content-addressed whole-workspace boundary captures, resolved under the
    single store root. Unlike the fence-typed session-writing views, a capture
    is a boundary observation — it appends no ledger event and takes no fence —
    so it is a plain view over a per-workspace {!Capture.Store.t} keyed under
    [snapshots/]. *)

(** {1:branch Branching}

    The one operation that spans two views, because branching a session is
    atomic across them: the child's mutation facts and the document that
    references them are one durable act. *)

val fork :
  t ->
  from:Mentat_session.Id.t ->
  events:Mentat_mutation.Event.t list ->
  Mentat_session.t ->
  (Session.Document.t, Session.Error.t) result
(** [fork root ~from ~events child] atomically branches [from]'s durable state
    into the new session [child] — the persistence half of a fork or rewind,
    whose document half ({!Mentat_session.fork}, {!Mentat_session.rewind}) the
    caller has already computed into [child]. It seeds [child]'s mutation store
    from [from]'s through {!Mutation.copy_into} — copying [events] (the ledger
    prefix the child retains, from {!Mentat_mutation.State.prefix_for_turns})
    and every blob they reference — and only then, once those facts are durable,
    writes [child]'s document. The document is the commit point that references
    the facts, so [child] never exists holding a settlement whose fact is
    missing, and a failure to seed leaves no [child] at all.

    Errors are {!Session.create}'s (notably {!Session.Error.Already_exists} for
    a taken id and {!Session.Error.Corrupt} for a document-less non-empty target
    directory — including a prior branch's crash residue). A parent-side blob
    that is absent or damaged, or a copy IO failure, surfaces as
    {!Session.Error.Corrupt} or {!Session.Error.Io}: the branch cannot launder
    corrupt parent history into the child.

    Raises [Invalid_argument] as {!Session.create}. *)

val truncate :
  t ->
  fence:Run_lock.guard ->
  document:Session.Document.t ->
  ledger:Mentat_mutation.Event.t list ->
  Mentat_session.t ->
  (Session.Document.t * Mentat_mutation.State.t, Session.Error.t) result
(** [truncate root ~fence ~document ~ledger session] drops an undo commit's
    crossed turns from both durable halves of one session under its document
    lock, the destructive twin of {!fork}: where a fork seeds a new session from
    a copied prefix, a truncate shrinks the current session to a surviving
    prefix in place. It rewrites the session's mutation ledger to [ledger] — the
    surviving-turn prefix the caller derived through
    {!Mentat_mutation.State.prefix_for_turns} — through
    {!Mutation.rewrite_ledger}, and only then, once the ledger is durable,
    commits [session] (the document replayed up to but not including the first
    crossed turn's start) by whole-document CAS ({!Session.commit}) against the
    still-current [document].

    Ledger-first ordering is deliberate: a crash between the two writes leaves
    the still-full document referencing a truncated ledger, which stays loadable
    (the prefix references a subset of the document's turns) and whose leftover
    armed boundary resume reconciles to Released — never the reverse, which
    would orphan ledger facts against turns the document no longer owns. On
    [Ok (doc, mstate)] both halves are durable and coherent, [doc] carries the
    truncated document's new revision, and [mstate] is the post-truncate
    mutation anchor the driver adopts.

    Errors are {!Mutation.rewrite_ledger}'s (surfaced on the session error
    surface, as {!fork}'s seed failures are) and {!Session.commit}'s — notably
    {!Session.Error.Conflict} if [document] ceased to be current under the
    fence. Raises [Invalid_argument] as {!Session.commit} and
    {!Mutation.rewrite_ledger}. *)

(**/**)

module Bundle = Bundle
(** Internal: the export-bundle wire codecs and decoder, exposed so the
    library's round-trip tests can prove export completeness. Not a public
    import surface. *)

(**/**)
