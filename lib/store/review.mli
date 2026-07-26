(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** CAS persistence for review records.

    Review records are user-authored durable state — marks, verdict, cursor —
    keyed by typed workspace and base identity and held to the same standard as
    session documents: content-revision CAS, atomic replacement with directory
    sync, and fail-loud corruption. Missing is a value ([Ok None]); damaged
    durable bytes are an error, never laundered into "never saved".

    Review state is workspace-scoped, not session-scoped (a review is over a
    worktree diff, not a conversation), so it is a flat keyed store outside
    [sessions/] and never appears in a session export. These are views over one
    opened store root ({!Mentat_store.open_}); every operation takes that root.
*)

(** {1:keys Keys} *)

module Key : sig
  type t
  (** The type for the identity of one review: a typed workspace root plus the
      review's base. Never a caller-composed path string — the store owns the
      path encoding of the typed parts, and workspace identity never launders
      through a string seam. *)

  val make : root:Mentat_workspace.Root.Key.t -> base:string -> t
  (** [make ~root ~base] is a review key {e for lookup}. A stored record's base
      is derived from the record on {!create} (via
      {!Mentat_review.Persist.base}), never from a free string, so a [main]
      record can never be filed under a [develop] key; [make] is how a caller
      names the record it wants to load. There is no non-empty-base rule — base
      validity is [Mentat_review]'s concern, not one the store invents. *)

  val root : t -> Mentat_workspace.Root.Key.t
  (** [root t] is the workspace root identity. *)

  val base : t -> string
  (** [base t] is the review's base label. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] name the same review. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a key for diagnostics. *)
end

(** {1:errors Errors} *)

module Error : sig
  (** Review store errors. *)

  type t =
    | Already_exists of Key.t
        (** {!create} found an existing record for the key. *)
    | Conflict of {
        key : Key.t;
        expected : Mentat_digest.t;
        actual : Mentat_digest.t;
      }
        (** The record changed after [expected] was observed — a concurrent
            reviewer; never last-writer-wins. *)
    | Vanished of Key.t
        (** The record was gone at {!commit} time. No store op removes review
            records, so this is external interference with durable state — loud,
            never treated as first-save or laundered into an opaque IO failure.
        *)
    | Base_mismatch of { key : Key.t; base : string }
        (** A decoded or replacement record's base ([base]) disagrees with its
            key base. A record's base is derived at create and can never change,
            so {!load} refuses valid bytes moved under a different base-derived
            path, and {!commit} refuses a [develop] record over a [main]
            document, rather than minting an incoherent document or silently
            overwriting one that later restores nothing. *)
    | Non_regular_file of string
        (** The target path exists and is not a regular file. *)
    | Invalid_record of { path : string; detail : string }
        (** Persisted review bytes that will not decode. This is an error, never
            [Ok None]: evidence-based restore may drop stale marks, but damaged
            durable bytes are not "never saved". *)
    | Io of Io.t  (** A filesystem or lock primitive failed. *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic for [e]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats {!message} output. *)

  val diagnostic : t -> Mentat_diagnostic.t
  (** [diagnostic e] is the user-facing diagnostic for [e]. *)
end

(** {1:documents Documents} *)

module Document : sig
  type t
  (** The type for a loaded review record with its private exact-byte CAS
      revision — the same pattern as {!Session.Document}. *)

  val key : t -> Key.t
  (** [key t] is the record's identity. *)

  val value : t -> Mentat_review.Persist.t
  (** [value t] is the persisted review record. *)

  val revision : t -> Mentat_digest.t
  (** [revision t] is the content identity of the persisted bytes — diagnostics
      only. *)
end

(** {1:operations Operations} *)

val load : Handle.t -> Key.t -> (Document.t option, Error.t) result
(** [load root key] is [Ok None] iff no record exists for [key] — missing, and
    only missing. A non-file target is {!Error.Non_regular_file}; bytes that
    will not decode are {!Error.Invalid_record}; decoded bytes whose record base
    does not agree with [key] are {!Error.Base_mismatch}; an IO failure is
    {!Error.Io}. *)

val create :
  Handle.t ->
  root:Mentat_workspace.Root.Key.t ->
  Mentat_review.Persist.t ->
  (Document.t, Error.t) result
(** [create root ~root:workspace value] is the first save for [value] under
    [workspace], keyed by the record's own base ({!Mentat_review.Persist.base})
    — the record decides where it is filed, so a mismatched key can never be
    invented. {!Error.Already_exists} if a record is present. Durable on return:
    data and the containing directory entry.

    Raises [Invalid_argument] if [value] will not encode. *)

val commit :
  Handle.t ->
  Document.t ->
  Mentat_review.Persist.t ->
  (Document.t, Error.t) result
(** [commit root doc value] replaces the record iff [doc]'s revision is still
    current; a concurrent reviewer gets {!Error.Conflict}, never a
    last-writer-wins loss of marks. Atomic rename, then containing-directory
    fsync. No store op removes review records, so a target missing at commit
    time is external interference with durable state and is {!Error.Vanished} —
    loud, never absence. {!Error.Base_mismatch} if [value]'s base disagrees with
    [doc]'s key base — a commit can never change the base a record is stored
    under, and a disagreement is a structured error, never a silent overwrite.

    Raises [Invalid_argument] if [value] will not encode. *)
