(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Durable workspace mutation evidence — events, state, and revert.

    [mentat.mutation] is the durable mutation-evidence vocabulary: what changed,
    under which claim and turn, with what content-addressed images — and the
    revert lifecycle over that evidence. Authoritative [Mentat_edit] results
    lower into one append-only event vocabulary ({!Event.t}), the vocabulary
    folds into one validated state ({!State.of_events}, the sole replay entry
    point), and change history, revert safety ({!State.revertability}), and
    revert plans ({!Revert.prepare}) derive from that state. Tools never author
    mutation facts: tool results are model-history text plus optional summary
    facts, never mutation authority.

    The library is pure and inert; three adjacent concerns are deliberately not
    its own. Bytes persistence is the store's: its composite [append_edit]
    receives the byte-carrying value together with the derived event, and
    blobs-before-event is the {e store's} ordering law, not this library's code.
    Attribution windows are the engine's claim scopes: this library only records
    their yield ({!Event.of_edit} for exact evidence, {!Event.tool_observed} for
    the observational rest). Projection is the protocol's: it reads {!State}'s
    projections directly, and no protocol-shaped view lives here.

    The load-bearing laws:
    - The ledger records occurrences as they occur: one claim scope with several
      applies records several {!Event.Edit} events, each carrying a dense
      0-based apply ordinal, and a claim's evidence is derived by folding its
      events — never stored as an aggregate. A commit-phase failed apply records
      its confirmed prefix exactly, plus the stopping target as a durable
      uncertain fact ({!Event.of_attempt}); an apply that touched nothing
      records nothing.
    - {!Event.Revert_started} persists the frozen targets before any revert
      filesystem IO, and replay re-validates every history-derived field of a
      start against its prefix ({!State.Error.Invalid_start}).
    - Crash recovery settles an unresolved revert from the frozen started record
      ({!Revert.settle_ambiguous}), never by re-resolving the selection.
    - Derived ids — checkpoint ids from their boundary, change ids from their
      enclosing source coordinates (claim and apply ordinal, or revert) and path
      — are minted internally, and decoders re-derive and verify them; no
      constructor accepts an id alongside the fields it derives from, so a
      continuation process re-deriving after a crash lands on the same values
      and detects existing facts through {!State}.
    - Events carry no session identity: the ledger is per-session, so session
      identity is a storage-lookup input, never a member of a fact. The
      [mentat.session] dependency is the correlation-id vocabulary only —
      {!Mentat_session.Turn.Id} and {!Mentat_session.Tool_claim.Id} with their
      codecs — the sole accepted reason the mutation ledger depends on the
      session's identity vocabulary.
    - Old wire shapes fail loudly as structured decode errors; there are no
      compatibility readers.
    - Confirm-with-override: a non-contiguous target is never silently minted
      into a plan — only an explicit, path-specific {!Revert.Override.t} lets
      {!Revert.prepare} proceed, and the consent is frozen into the started fact
      for audit. *)

module Image = Image
(** Durable file images. *)

(** Durable file-level change facts.

    A change is one applied file transition with content-addressed before/after
    images. It is a leaf value with no supported constructor: introduction is
    the library's own, through the event lowerings, revert settlement, and
    decode. The facade recites {!Change} to keep the lowering constructor
    {!Change.of_entry} off the supported surface; id spelling and derivation
    stay behind the library's private lowering seam. *)
module Change : sig
  module Id = Change.Id
  (** Change identifiers, derived internally from the enclosing source
      coordinates and the path — never accepted from callers. *)

  type t = Change.t
  (** The type for one applied file transition.

      Invariant: the image pair is one of the three valid transitions —
      [Missing -> Text], [Text -> Text], or [Text -> Missing] — and the line
      counts are non-negative. A change must alter the file:
      [Missing -> Missing] and a byte-identical [Text -> Text] pair are
      unrepresentable — constructors never build them and decode rejects them.
  *)

  val id : t -> Id.t
  (** [id t] is [t]'s derived change id. *)

  val path : t -> Mentat_workspace.Path.t
  (** [path t] is the path [t] mutated. *)

  val before : t -> Image.t
  (** [before t] is the path's image before the transition. *)

  val after : t -> Image.t
  (** [after t] is the path's image after the transition. *)

  val additions : t -> int
  (** [additions t] is [t]'s recorded added line count, computed from the two
      texts when the change is constructed. *)

  val deletions : t -> int
  (** [deletions t] is [t]'s recorded deleted line count, computed from the two
      texts when the change is constructed. *)

  val kind : t -> Mentat_edit.kind
  (** [kind t] is the applied transition kind, an eliminator over the image
      pair: [Missing -> Text] is [`Create], [Text -> Text] is [`Modify], and
      [Text -> Missing] is [`Delete]. There is no stored operation member to
      disagree with the images. *)

  val of_changes : t list -> Textdiff.stats
  (** [of_changes changes] is the aggregate {!Textdiff.stats} for [changes]:
      [files] counts distinct row paths; [additions] and [deletions] sum the
      recorded line counts. This is the run-cumulative trailer input. *)

  (** {1:netting Netting} *)

  module Net = Change.Net
  (** Netted per-path endpoints — for each path the first before image and the
      last after image, with contributing sources and the contiguity fact. *)

  val net : t list -> Net.entry list
  (** [net changes] is the per-path endpoint netting of [changes] in list order:
      for each path, the first before image and the final after image, with
      contributing sources and the contiguity fact. Entries are in first-seen
      path order; paths whose endpoint images are equal are dropped. Netting is
      total: no sequence of valid transitions fails to fold. *)

  (** {1:presentation Presentation} *)

  module Hunks_error = Change.Hunks_error
  (** Structured {!hunks} refusals — a missing blob (integrity corruption under
      the no-GC blob rule: blobs are never garbage collected, so a referenced
      image's bytes are always present) or a bounded-diff refusal, never
      conflated. *)

  val hunks :
    ?max_edit_distance:int ->
    blob:(Mentat_digest.Content_ref.t -> string option) ->
    t ->
    (Textdiff.Hunk.t list, Hunks_error.t) result
  (** [hunks ?max_edit_distance ~blob t] is the before/after line diff of one
      change, resolving its text images through [blob] and grouping with
      {!Textdiff.hunks}. [max_edit_distance] is forwarded to that bounded diff.
      [Error (Hunks_error.Missing_blob id)] means [blob] returned [None] for a
      referenced image; [Error Hunks_error.Bound_exceeded] means the diff
      exceeded the bound. [blob] must return the exact recorded bytes for a
      reference it resolves. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same change. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a change for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps changes to JSON objects, validating image pairs and
      non-negative counts on decode. The id member is re-validated against its
      derivation by the enclosing decoders — {!Event.jsont}'s edit case and the
      revert settlement codec — which know the enclosing source. *)
end

module Checkpoint = Checkpoint
(** Durable boundary capture facts. Identifiers derive deterministically from
    capture boundaries; see {!Checkpoint.id_of_boundary}. *)

module Revertability = Revertability
(** The single-owner revert-safety answer. *)

module Revert = Revert
(** Revert selection, pure preparation, and lifecycle evidence. *)

module Event = Event
(** The durable mutation vocabulary. *)

module State = State
(** The sole replay fold and its durable mutation projections, including pure
    selection resolution through {!State.resolve}. *)

module Diff = Diff
(** Netted display diffs over the mutation ledger: a selection collapsed to net
    per-path transitions with resolved line hunks, summed line counts, and the
    carried {!Revertability} answer. *)
