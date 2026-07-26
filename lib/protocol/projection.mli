(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The one projector — engine/replay-facing; frontends never call it.

    Both entry points fold the whole journal once, joining per-claim mutation
    evidence from [mutation] at tool settlements. The fold is pure: the facts
    emitted for the event at sequence [n] depend only on durable state up to and
    including [n], so live emission over cumulative state and replay over the
    saved document produce position-tagged facts with identical durable
    projections, and {!after} of a position equals the strict suffix of {!all}
    after it.

    Three recovery-only journal facts project to nothing: the provider-request
    claim, the durable interrupt request, and an ambiguous provider settlement.
    A known failed provider settlement instead emits
    {!Fact.Turn_provider_failed} with the owner error whole; the following
    {!Fact.Turn_settled} closes the generic turn lifecycle. The projector emits
    positions only at emitted facts, so a non-emitting event's sequence is never
    a legitimate token.

    [mutation] is the session's replayed mutation state
    ([Mentat_mutation.State.of_events] over the session's mutation events). A
    claim's settlement revertability is computed against the claim's
    mutation-ledger prefix ({!Mentat_mutation.State.at_claim}) — never the full
    replayed ledger, whose answer changes when later changes supersede the
    claim's, breaking the durable encoded identity above. The other evidence
    fields read full state safely: a claim's change rows and observed paths are
    immutable once recorded (mutation replay forbids a second edit per claim),
    and the run-cumulative totals accumulate in journal settlement order. An
    orphan change whose claim never settled appears in no projection. *)

val all :
  session:Mentat_session.t ->
  mutation:Mentat_mutation.State.t ->
  (Position.t * Fact.t) list
(** [all ~session ~mutation] is every position-tagged committed fact of
    [session]'s journal, in order, joining per-claim mutation evidence from
    [mutation] at tool settlements.

    It is a pure left fold: the fact emitted at sequence [n] depends only on
    durable state through [n]. Live emission over cumulative state and replay
    over a saved document therefore produce position-tagged facts with
    byte-identical durable encodings.

    Most journal events map one-to-one to a {!Fact.t}. Exactly three do not:
    provider-request claims, durable interrupt requests, and ambiguous provider
    settlements. Known failed provider settlements emit a structured
    {!Fact.Turn_provider_failed}; their terminal turn settlement emits the
    following generic {!Fact.Turn_settled}. The projector emits positions only
    for emitted facts.

    A claim's settlement revertability is computed against its mutation-ledger
    prefix ({!Mentat_mutation.State.at_claim}), never the full replayed ledger.
    Change rows and observed paths are immutable after settlement, and
    run-cumulative totals follow journal settlement order. An orphan mutation
    whose claim never settled appears in no projection. *)

val after :
  Position.t ->
  session:Mentat_session.t ->
  mutation:Mentat_mutation.State.t ->
  ((Position.t * Fact.t) list, Position.Invalid.t) result
(** [after position ~session ~mutation] first proves membership: [position] must
    name a fact-emitting event of [session]'s own feed. It then is the strict
    suffix of {!all} after [position]. A candidate naming another session is
    [Error (Foreign_session _)]; one whose sequence names no emitted fact —
    forged past the head, or a non-emitting event — is [Error (Not_in_feed _)].
    Neither is a token this projector ever produced, so returning the suffix of
    a forged position (which would silently skip every fact below it) is
    impossible by construction. *)

(** {1:resumable Resumable projection}

    {!all} and {!after} fold the whole journal on every call — the O(J) one-shot
    and replay API. A materialized reader that grows its projection one commit
    at a time instead carries {!type:resume}, the fold's accumulator lifted out,
    and {!advance}s it over each commit's events in O(delta). *)

type resume
(** The type for the projector's carried fold state: how many journal events
    have been folded, the active turn's settled change rows, and whether the
    fold is inside a compaction turn. It is exactly {!all}'s left-fold
    accumulator, exposed so a projection can advance across calls rather than
    restart. *)

val start : resume
(** [start] is the fold state before any event: nothing consumed, no turn scope,
    outside compaction. {!advance} from [start] over a whole journal is {!all}.
*)

val consumed : resume -> int
(** [consumed resume] is the number of journal events [resume] has folded — the
    zero-based sequence of the next event {!advance} will project. A reader uses
    it to slice the unmaterialized tail of a freshly loaded journal into the
    [delta] that catches its projection up. *)

val advance :
  resume ->
  session:Mentat_session.t ->
  mutation:Mentat_mutation.State.t ->
  delta:Mentat_session.Event.t list ->
  resume * (Position.t * Fact.t) list
(** [advance resume ~session ~mutation ~delta] folds [delta] — the journal
    events at sequences [consumed resume ..] — onto [resume], returning the
    advanced state and the facts [delta] emits, position-tagged from where
    [resume] left off.

    It is {!all}'s own per-event fold split across calls: [advance] applies the
    identical function to the identical events in the identical order, carrying
    the identical accumulator. So for any partition of a journal into successive
    deltas, chaining [advance] from {!start} through them and concatenating the
    emitted facts equals {!all} over the whole — the incremental projection is
    byte-identical to the from-scratch one, at every split.

    [session] supplies only the owning id for positions; its own event list is
    not read, [delta] is the events to fold. [mutation] joins per-claim
    settlement evidence exactly as {!all} does, and because that evidence reads
    each claim's immutable ledger prefix ({!Mentat_mutation.State.at_claim}) —
    never the full replayed ledger — advancing a settlement once with the
    mutation state at its own commit yields the same fact a later full re-fold
    would, so the emitted facts never need rewriting. *)
