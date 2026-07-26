(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The per-session mutation event log and blob store.

    Events and blobs share one set of operations, because their durability
    ordering — blobs durable before the event that references them, both durable
    before the settlement that references the event — is one workflow with one
    enforcement seam ({!append_edit}). These are views over one opened store
    root ({!Mentat_store.open_}); every operation takes that root.

    The event log is a true append: each append writes [\n]-terminated JSON
    lines and fsyncs before returning, so a crash can tear only a final
    [\n]-less fragment, which repair drops — exactly the orphan events replay
    ignores. Appends are enforced at one ownership boundary: the fence guard
    proves the active driver, the exact {!Session.Document.t} proves which
    session history the mutation facts describe, and the session document lock
    serializes the revision check, correlation validation, blob persistence, and
    event durability with every document write. Each batch is also folded into a
    per-fence-tenure {!Mentat_mutation.State.t} anchor before it is written — a
    batch the validator rejects returns {!Error.Invalid_history} and writes
    nothing.

    Blobs are content-addressed, per-session, and write-once: same bytes
    converge to a no-op, a pre-existing file is verified against its reference
    (never trusted), and reclamation is the session's physical removal only.
    Blob writes have no public primitive — bytes enter through the composite ops
    {!append_edit}, {!append_revert_started}, and {!append_revert_settled}, and
    {!append} rejects every blob-referencing arm outright, which is what makes
    the blobs-before-event law enforceable at all. *)

(** {1:errors Errors} *)

module Error : sig
  (** Mutation store errors. *)

  module Correlation : sig
    (** Structured failures joining mutation facts to their exact session
        document. *)

    type t =
      | Unknown_turn of Mentat_session.Turn.Id.t
      | Turn_without_tool_claim of Mentat_session.Turn.Id.t
      | Turn_not_settled of Mentat_session.Turn.Id.t
      | Unknown_claim of Mentat_session.Tool_claim.Id.t
      | Claim_turn_mismatch of {
          claim : Mentat_session.Tool_claim.Id.t;
          expected : Mentat_session.Turn.Id.t;
          actual : Mentat_session.Turn.Id.t;
        }
      | Claim_not_open of Mentat_session.Tool_claim.Id.t
      | Recovery_without_ambiguity of Mentat_session.Turn.Id.t
      | Before_tools_after_ambiguity of Mentat_session.Turn.Id.t

    val message : t -> string
    val pp : Format.formatter -> t -> unit
  end

  type t =
    | Invalid_line of { path : string; line : int; detail : string }
        (** A [\n]-terminated event-log line that will not decode — genuine
            damage with a location, never silently skipped. *)
    | Invalid_history of Mentat_mutation.State.Error.t
        (** Decoded events that do not replay: the semantic validator's own
            error, preserved. On append this is the incremental rejection of a
            conflicting batch; on read it is a damaged history. *)
    | Correlation of Correlation.t
        (** Mutation facts reference a turn or claim not owned by the supplied
            session document. History reads accept open or settled claims; live
            appends additionally require the exact open tool suspension. *)
    | Document of Session.Error.t
        (** Loading the current document under the shared lock failed, or the
            supplied document ceased to be current. A nested
            {!Session.Error.Conflict} is the latter case; no blob or event was
            written. *)
    | Blob_mismatch of {
        expected : Mentat_digest.Content_ref.t;
        actual : Mentat_digest.Content_ref.t;
      }
        (** A blob's bytes hash differently than the path claims; verified on
            every {!blob} read, on a pre-existing file at put time, and at
            export. *)
    | Missing_blob of Mentat_digest.Content_ref.t
        (** A referenced blob that does not exist at all. Minted at export and
            by the composite preflight for a started fact's restore images —
            referenced net-before images whose bytes the composite does not
            supply and whose durability external damage can have broken. {!blob}
            keeps [Ok None]: absence is an answer there, not damage. *)
    | Non_regular_file of string
        (** A ledger or blob path that exists and is not a regular file —
            structural damage to the store tree, never an IO failure. *)
    | Io of Io.t  (** A filesystem or lock primitive failed. *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic for [e]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats {!message} output. *)

  val diagnostic : session:Mentat_session.Id.t -> t -> Mentat_diagnostic.t
  (** [diagnostic ~session e] is the user-facing diagnostic for [e] in
      [session]'s mutation store. *)
end

(** {1:appending Appending} *)

val append :
  Handle.t ->
  fence:Run_lock.guard ->
  document:Session.Document.t ->
  Mentat_mutation.Event.t list ->
  (Mentat_mutation.State.t, Error.t) result
(** [append root ~fence ~document events] appends [events] to the fenced
    session's [ledger.jsonl] as [\n]-terminated JSON lines, in one buffered
    write with the final newline written last, fsynced before return, followed
    by a [sessions/<id>/] directory fsync — on [Ok state] the events and the
    ledger's directory entry are durable, and [state] is the advanced
    mutation-state anchor (a full replay of the persisted history plus [events],
    or the unchanged current anchor for empty [events]). The target session is
    [fence]'s: appending to an unrelated session is unrepresentable. [document]
    must be the exact current document for that session. Its revision is
    compared while holding the same session document lock as {!Session.commit};
    if a commit wins first, the append returns [Error (Document (Conflict _))]
    without writing a blob or event. Empty [events] is a no-op. Before writing,
    any torn tail from a prior crash is truncated at the last record boundary.

    Only the blob-free arms cross here: the admissible events are [Checkpoint]
    and [Tool_observed]. Every blob-referencing arm is rejected — [Edit] enters
    through {!append_edit}, the one seam that persists its bytes;
    [Revert_started] through {!append_revert_started}, which makes its images
    durable before revert IO begins; [Revert_settled] through
    {!append_revert_settled} — so an event can never land referencing blobs the
    store was never given.

    Validation is anchored per fence tenure and incremental: the existing log is
    read and folded once, at the first append under a given guard, into a
    {!Mentat_mutation.State.t} anchor carried by the guard itself — one disk
    read per tenure, with one [sessions/<id>/] sync so entries inherited
    unsynced from a previous holder are durable before the tenure builds on them
    — and each batch is validated under the document lock by folding only the
    batch into the anchor through {!Mentat_mutation.State.append}, whose law
    makes the result equal to a full replay of the anchored history plus the
    batch. The anchor advances only on a successful append. On any failed append
    whose write may have begun, the ledger is restored to its pre-append
    boundary best-effort and the anchor is invalidated unconditionally, so the
    next append re-anchors from disk. The anchor dies with the guard.

    {!Error.Invalid_history} if the batch does not extend the mutation history;
    {!Error.Correlation} if an event does not belong to [document]'s turn and
    tool-claim history or is not valid at its current live suspension;
    {!Error.Document} if [document] is stale or cannot be loaded;
    {!Error.Invalid_line} if the existing log is damaged;
    {!Error.Non_regular_file} if the ledger path exists and is not a regular
    file; {!Error.Io} on failure. Durability granularity is this op's contract;
    batching is the engine's call.

    Raises [Invalid_argument] if [fence] has been released or does not hold this
    store root's run lock — a dead fence is a programmer error, not an IO
    outcome — if an event is a blob-referencing arm ([Edit], [Revert_started],
    [Revert_settled]), or if an event will not encode. *)

val append_edit :
  Handle.t ->
  fence:Run_lock.guard ->
  document:Session.Document.t ->
  entries:Mentat_edit.Result.Entry.t list ->
  Mentat_mutation.Event.t ->
  (Mentat_mutation.State.t, Error.t) result
(** [append_edit root ~fence ~document ~entries event] is the composite
    edit-persistence op — the intra-domain blobs-before-event law enforced at
    one seam. It receives the confirmed {!Mentat_edit.Result.Entry.t} list — the
    byte carrier — and verifies that [event] derives from [entries] under its
    own correlation ids, ordinal, and stopping target, stores every text-side
    blob the event references (write-once), then appends [event] through the
    same fenced, serialized, validated path as {!append}. Both a successful
    apply's result entries and a commit-phase attempt's confirmed prefix cross
    here — [entries] is a list, not a {!Mentat_edit.Result.t}, precisely so
    passing an attempt's prefix asserts no whole-plan success that did not
    happen. On [Ok state] blobs and event are durable, in that order, and
    [state] is the advanced mutation-state anchor; the settlement commit that
    references them follows.

    Raises [Invalid_argument] if [event] does not derive from [entries] — the
    engine composed unrelated values — or as {!append}. *)

val append_revert_started :
  Handle.t ->
  fence:Run_lock.guard ->
  document:Session.Document.t ->
  plan:Mentat_mutation.Revert.Plan.t ->
  current:(Mentat_workspace.Path.t * Mentat_edit.Observed.t) list ->
  Mentat_mutation.Event.t ->
  (unit, Error.t) result
(** [append_revert_started root ~fence ~document ~plan ~current event] is the
    composite revert-start persistence op. The started fact's expected images
    are the plan-time reads — for an overridden non-contiguous target the
    unrecorded writer's text, durable nowhere else — so the op receives those
    reads ([current], the same pairs the engine assembled into the plan's
    {!Mentat_mutation.Revert.Evidence.t}), verifies that [event] freezes exactly
    [plan]'s started record and that each target's read derives its frozen
    expected image, stores every expected text as a blob (write-once), stores
    the plan's {!Mentat_mutation.Revert.Plan.merge_blobs} — a clean merge's
    fresh restore images, durable nowhere else — write-once, then verifies every
    {e other} restore image the started fact references is present and intact
    ({!Error.Missing_blob}/{!Error.Blob_mismatch} otherwise), and appends
    [event] through the same fenced, serialized, validated path as {!append}.
    Those verified restore images are recorded net-before images, durable since
    the events that recorded them; the merge blobs are the sole exception,
    supplied here rather than assumed. On [Ok ()] the started fact and every
    image it references are durable — the precondition for beginning revert IO.

    Raises [Invalid_argument] if [event] is not [plan]'s started event, if
    [current] does not cover the plan targets, or if a read does not derive its
    target's expected image — the engine composed unrelated values — or as
    {!append}. *)

val append_revert_settled :
  Handle.t ->
  fence:Run_lock.guard ->
  document:Session.Document.t ->
  started:Mentat_mutation.Revert.Started.t ->
  ?outcome:(Mentat_edit.Result.t, Mentat_edit.Apply_error.t) result ->
  Mentat_mutation.Event.t ->
  (unit, Error.t) result
(** [append_revert_settled root ~fence ~document ~started ?outcome event] is the
    composite revert-settlement persistence op. [outcome] is the apply evidence
    — the byte carrier whose confirmed entries hold the restoration rows' text
    sides; omitted, the settlement is crash recovery's, which may mint only
    {!Mentat_mutation.Revert.settle_ambiguous} of the frozen [started] record
    and carries no rows and no bytes. The op verifies that [event] is exactly
    the settlement of [started] under [outcome] (re-derived through
    {!Mentat_mutation.Revert.settle}), stores every confirmed entry's text side
    as a blob (write-once), then appends [event] through the same fenced,
    serialized, validated path as {!append}. On [Ok ()] blobs and event are
    durable, in that order.

    Raises [Invalid_argument] if [event] does not derive from [started] and
    [outcome] — including an [outcome] that does not cover [started]'s targets —
    or as {!append}. *)

(** {1:reading Reading} *)

val read :
  Handle.t -> Session.Document.t -> (Mentat_mutation.State.t, Error.t) result
(** [read root document] replays [document]'s event log into a validated
    {!Mentat_mutation.State.t} — the semantic value, not a re-projected event
    list; a caller that needs the raw events takes
    {!Mentat_mutation.State.
    events}. A final [\n]-less fragment is a torn
    tail and is dropped. A [\n]-terminated line that will not decode is
    {!Error.Invalid_line} — genuinely damaged, not crash-truncated, never
    silently skipped. A history that does not replay is
    {!Error.Invalid_history}; a history that names a turn or claim not owned by
    [document] is {!Error.Correlation}. Historical correlation accepts claims
    that are open or settled — unlike live append correlation, it does not
    require the claim still be the current suspension. This full re-validation
    lives here, on the cold read/recovery path. A missing log is the empty
    state; a ledger path that is not a regular file is
    {!Error.Non_regular_file}. Reads take no lock: they are safe against the
    single locked appender because a torn or in-progress final line is exactly
    what tail-repair drops. Callers that need a document/event pair from one
    instant use {!Export.write}, which snapshots both under the shared document
    lock. *)

val blob :
  Handle.t ->
  session:Mentat_session.Id.t ->
  Mentat_digest.Content_ref.t ->
  (string option, Error.t) result
(** [blob root ~session ref] is [None] if no blob for [ref] exists under
    [session]; the bytes otherwise, verified against [ref] before return — a
    hash mismatch is {!Error.Blob_mismatch}, never silently returned bytes. A
    blob path that is not a regular file is {!Error.Non_regular_file}. This
    resolves the net-before images revert lowering needs. *)

(** {1:reverting Reverting} *)

(** The outcome of {!revert_apply}. *)
type revert_outcome =
  | Nothing_to_revert
      (** The selection named no revertable work; no file was touched and no
          fact was minted. *)
  | Refused of Mentat_mutation.Revert.Problem.t list
      (** Preparation refused with every problem it found; no file was touched
          and no fact was minted. *)
  | Applied of Mentat_mutation.Revert.Settled.t
      (** The revert ran; the settlement records the per-path outcomes. *)

val revert_apply :
  ?merge:bool ->
  ?override:Mentat_mutation.Revert.Override.t ->
  Handle.t ->
  fence:Run_lock.guard ->
  document:Session.Document.t ->
  selection:Mentat_mutation.Revert.Selection.t option ->
  observe:(Mentat_workspace.Path.t -> Mentat_edit.Observed.t) ->
  checkpoint:
    (boundary:Mentat_mutation.Checkpoint.boundary ->
    Mentat_mutation.Checkpoint.t) ->
  apply:
    (Mentat_edit.t -> (Mentat_edit.Result.t, Mentat_edit.Apply_error.t) result) ->
  new_id:(unit -> Mentat_mutation.Revert.Id.t) ->
  (revert_outcome, Error.t) result
(** [revert_apply root ~fence ~document ~selection ~observe ~checkpoint ~apply
     ~new_id] runs the whole fenced revert lifecycle for [selection] against the
    fenced session's current history. A [None] selection is [Nothing_to_revert]:
    the flags named nothing revertable, and nothing is read or written.

    [merge] (default [true]) is threaded to {!Mentat_mutation.Revert.prepare}:
    when set, a superseded selection at the recorded head is cleanly merged
    rather than refused; when [false] the lifecycle is byte-identical to the
    pre-merge engine.

    Otherwise it re-reads the current document under the fence (so the appends
    revision-check against the live head, not the possibly-stale [document]),
    replays its history, nets [selection], and assembles the plan's evidence —
    each candidate path's current read through [observe], and each net-before
    restore image's bytes from the blob store, plus each net-after image's bytes
    as the three-way merge base a superseded path needs. It mints the revert id
    through [new_id] — never derived from session, scope, or ordinal, the
    mutation library's own rule — and prepares the all-or-nothing plan; a
    preparation refusal is [Refused] and touches no file. On a ready plan it
    captures a [Before_revert] checkpoint through [checkpoint] (so the revert is
    itself undoable), then under the fence appends the three ledger facts in
    order — the checkpoint, then the started fact (freezing the plan's expected
    images durable before any IO through {!append_revert_started}), then the
    settlement ({!append_revert_settled}) — applying the lowered edit through
    [apply] between the last two. [apply] re-observes each target under the
    write lock, so a target changed since planning fails stale and the
    settlement records it. On [Ok (Applied settled)] the whole lifecycle is
    durable.

    The three workspace effects cross as ports so the store links no workspace
    or edit-application library: [observe] is the plan-time read of one path (an
    unreadable target enters as {!Mentat_edit.Observed.Other}); [checkpoint] is
    the conservative capture at a boundary, whose degradation is a value, not a
    failure; and [apply] is the confined edit application. [new_id] mints the
    command-boundary revert id.

    Errors are the fenced appends' ({!append}, {!append_revert_started},
    {!append_revert_settled}), a {!Error.Document} if the current document
    cannot be re-loaded, or a replay {!Error.t} of the current history. Raises
    [Invalid_argument] as the appends do — notably a released or wrong-root
    [fence]. *)

(** {1:copying Branching} *)

val copy_into :
  Handle.t ->
  from:Mentat_session.Id.t ->
  into:Mentat_session.Id.t ->
  events:Mentat_mutation.Event.t list ->
  (unit, Error.t) result
(** [copy_into root ~from ~into ~events] seeds [into]'s mutation store from
    [from]'s for a branch or rewind: it copies every blob [events] reference
    (read from [from] and verified, written write-once under [into]) and then
    writes [events] as [into]'s [ledger.jsonl], blobs durable before the ledger
    that references them — the same intra-domain ordering the live appends keep,
    carried into the child. [events] is the exact ledger prefix to copy, which
    the caller derives from [from]'s validated history through
    {!Mentat_mutation.State.prefix_for_turns}; a fork copies the whole ledger, a
    rewind the prefix its kept turns retain. Empty [events] writes no ledger —
    the child's mutation history is the empty state.

    Unfenced and unlocked: [into] is a freshly minted branch target with no
    driver and no concurrent writer, and this op is the seed of its atomic
    creation (see {!Session.create_seeded}), which already holds [into]'s
    document lock — so it never re-acquires it. Blob reads against [from] are
    fence-free and verified, safe against [from]'s live driver because blobs are
    write-once.

    {!Error.Missing_blob} if a referenced blob is absent under [from] — under
    the no-GC rule a referenced image's bytes are always present, so absence is
    [from]'s integrity corruption; {!Error.Blob_mismatch} or
    {!Error.Non_regular_file} on damaged [from] blob bytes; {!Error.Io} on a
    read or write failure. On [Ok ()] the copied blobs and ledger are durable
    under [into].

    Raises [Invalid_argument] if an event will not encode. *)

(** {1:truncating Truncating} *)

val rewrite_ledger :
  Handle.t ->
  fence:Run_lock.guard ->
  document:Session.Document.t ->
  Mentat_mutation.Event.t list ->
  (Mentat_mutation.State.t, Error.t) result
(** [rewrite_ledger root ~fence ~document events] replaces the fenced session's
    [ledger.jsonl] whole with [events] — the surviving-turn prefix an undo
    commit keeps, which the caller derives from the current validated history
    through {!Mentat_mutation.State.prefix_for_turns}. This is the one
    deliberate break in the true-append contract: not an append but an atomic
    whole-file replace (the same prefix shape {!copy_into} writes, applied in
    place), serialized on the same session document lock as every other write.
    On [Ok state] the truncated ledger is durable and [state] is the advanced
    mutation-state anchor — a full replay of [events], adopted by the fence so
    the next append builds on it.

    The intended caller is the session-store truncate composite
    ({!Mentat_store.truncate}), which commits the truncated document only
    {e after} this returns: ledger-first ordering keeps the session loadable
    across a crash between the two writes, because the prefix references a
    subset of the still-full document's turns, so correlation holds; a leftover
    armed boundary whose revert the prefix dropped is what resume reconciles to
    Released. Empty [events] writes an empty ledger.

    {!Error.Invalid_history} if [events] does not replay; {!Error.Correlation}
    if it names a turn the current document does not own; {!Error.Document} if
    [document] is stale or cannot be loaded; {!Error.Non_regular_file} or
    {!Error.Io} on a damaged ledger or filesystem failure.

    Raises [Invalid_argument] if [fence] has been released or does not hold this
    root's run lock, or if an event will not encode. *)
