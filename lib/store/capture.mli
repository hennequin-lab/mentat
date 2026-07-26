(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The workspace-boundary capture view — content-addressed whole-workspace
    snapshots resolved under the single store root.

    This is the durable backend behind the executable's checkpoint capability
    ([bin/workspace_adapter]): it takes a whole-workspace {e capture} at a
    mutation boundary and stores it so a later revert or opaque-writer
    attribution step can read a path's pre-image. The checkpoint {e fact} — its
    boundary identity, its degradation vocabulary — is [mentat.mutation]'s; this
    view owns only the bytes the fact's opaque
    {!Mentat_mutation.Checkpoint.Snapshot.t} reference resolves to. It is not
    fence-typed: a capture is a boundary observation, not a session write, and
    it never appends to the ledger the fence protects.

    {b Why content-addressed and not git.} Mentat's only process-spawn boundary
    is [Mentat_workspace_io.Command.run], sealed and confined to the workspace
    roots. A git bare repo would have to be written outside the workspace, where
    a confined child cannot write; a second unsandboxed [git] would breach the
    single-spawn-boundary invariant. So the trusted host reads the workspace
    through the capability's pure [File] operations and stores content-addressed
    blobs plus a manifest with its own host-side IO — the sandbox confines child
    processes, never the host's own bookkeeping.

    {b Storage shape.} Every capture store resolves under the opened store root
    (never a second native-path capability), keyed by the workspace root:

    {v   <root>/snapshots/<key>/blobs/<AB>/<content-token> v}

    A capture stores each covered regular file's bytes as a {e write-once}
    content-addressed blob, builds a manifest of [(path, entry)] rows, and
    stores the manifest as one more content-addressed blob; the manifest's
    reference token {e is} the snapshot reference. Because blob names are their
    content, an unchanged file across captures re-stores nothing and an
    unchanged tree yields the same reference — captures are deterministic in the
    bytes they cover.

    {b Durability.} Every blob is written temp then fsync then rename then
    directory-fsync, and the manifest is written {e last}, after every blob it
    references is durable — so a returned {!t} reference names only durable
    bytes. Power-loss durability carries the same platform bound as the store's
    edit commits (macOS [fsync] does not issue [F_FULLFSYNC]); a crash before a
    capture is durable is covered by the engine's fresh post-recovery
    re-capture, so a lost pre-crash capture is never trusted.

    These durable mechanics mirror {!Mentat_store.Disk}'s temp-fsync-rename
    shape but run on a {e presentation-only} failure channel: a capture failure
    degrades a checkpoint to [Degraded] rather than raising a typed store [Io]
    error, so this view keeps its own flat {!Error.t} and never threads the
    domain views' [Io]-typed primitives. *)

(** {1:errors Errors} *)

module Error : sig
  type t
  (** A filesystem or integrity failure: the store-relative path that failed and
      a rendered cause. Presentation only; no caller matches on it — a capture
      failure becomes a durable [Degraded] checkpoint, and a read failure is a
      structured refusal at its own consumer. *)

  val message : t -> string
  (** [message t] is a human-readable diagnostic. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats {!message} output. *)
end

(** {1:store The store} *)

module Store : sig
  type t
  (** A per-workspace content-addressed capture store, resolved under one opened
      store root. Construction is pure and touches no filesystem; directories
      are created under the root on the first capture. *)

  val create : Handle.t -> key:string -> t
  (** [create root ~key] is the capture store resolved at [snapshots/<key>]
      under the opened store [root] — the same [openat]-based capability every
      store view operates through, never a second native-path capability. [key]
      is the workspace-root-derived subdirectory, so distinct workspaces never
      share a store and repeated resolutions of one workspace do.

      Raises [Invalid_argument] if [key] is empty. *)
end

(** {1:capture Capturing} *)

(** The walk's classification of one candidate path. Only regular files are
    covered — symlinks and special files are {!Skip}, so a capture records file
    content, the substrate a text revert restores from. *)
type loaded =
  | Loaded of string  (** a covered regular file's complete bytes *)
  | Skip
      (** silently omitted from the capture: the entry vanished, is a directory,
          a symlink, or an unsupported kind. Not a coverage gap. *)
  | Excluded
      (** attempted but not coverable — too large, or unreadable. Counted in
          {!t.excluded} as coverage honesty. *)

type t = {
  reference : string;
      (** The manifest's content-reference token — the opaque value the adapter
          lifts into {!Mentat_mutation.Checkpoint.Snapshot.t}. *)
  excluded : int;  (** Count of {!Excluded} paths. Non-negative. *)
}
(** The outcome of a successful capture. *)

val capture :
  Store.t ->
  paths:Lpath.Rel.t list ->
  load:(Lpath.Rel.t -> loaded) ->
  (t, Error.t) result
(** [capture store ~paths ~load] captures the workspace described by [paths].
    For each path, in order, [load] classifies it: {!Loaded} entries are stored
    (regular-file bytes as a write-once blob), {!Skip} entries are omitted, and
    {!Excluded} entries are counted. The resulting manifest — the covered
    entries sorted by path — is stored as one more content-addressed blob, and
    its reference is returned. Streaming: [load] is called once per path and its
    bytes are stored immediately, so at most one covered file is held at a time.

    [Error] on any filesystem or integrity failure; the adapter maps it to a
    [Degraded] checkpoint. A capture that stores nothing but covers the empty
    tree still succeeds with a valid (empty-manifest) reference. *)

(** {1:reading Reading a capture} *)

(** The state a capture recorded for one path. *)
type observed =
  | File of string  (** the regular file's bytes at capture time *)
  | Absent  (** the path was not covered by this capture *)

val read :
  Store.t -> reference:string -> Lpath.Rel.t -> (observed, Error.t) result
(** [read store ~reference path] is the state [path] held in the capture named
    by [reference] (a token returned by {!capture}). Blob bytes are verified
    against their content reference before return — a mismatch is [Error], never
    silently returned bytes. [Error] if [reference] is malformed or names a
    manifest whose bytes are absent or corrupt (integrity failure). This is the
    substrate a later revert or attribution step reads pre-images through. *)
