(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The durable mutation vocabulary. Every fact this library persists is one of
    these five arms, appended to the store's per-session mutation ledger and
    interpreted only by {!State.of_events}. {!jsont} is the sole durable
    mutation codec entry point.

    Exact and observed effects are two arms, deliberately: both append before
    the claim's settlement commit, so a crash between them leaves the claim
    unsettled and recovery bounds the two together — while one merged arm would
    force every exact edit to carry an empty observed list.

    Blobs-before-event ordering is the store's obligation: its composite append
    operations receive the authoritative byte-carrying value together with the
    derived event and persist every text side before the event that references
    it. This library only defines the values. *)

(** The type for durable mutation events. The arms are exposed [private] so
    replay folds can match them; introduction is through the per-arm
    constructors and decode, so every in-memory event passed its checked
    constructor — the fold validates only cross-event facts. *)
type t = private
  | Checkpoint of Checkpoint.t
      (** A boundary capture attempt, available or degraded. *)
  | Edit of {
      turn : Mentat_session.Turn.Id.t;
      claim : Mentat_session.Tool_claim.Id.t;
      ordinal : int;
          (** The 0-based index of this event among the claim's recorded apply
              events — the ledger records occurrences as they occur, so one
              claim scope with several applies records several [Edit] events.
              The fold enforces density in order: the k-th recorded apply
              carries ordinal k. *)
      checkpoint : Checkpoint.Id.t option;
          (** The turn's [Available] conservative checkpoint, when one exists;
              [None] durably records that the capture yielded no snapshot. *)
      changes : Change.t list;
          (** The confirmed transitions, unique paths, mutation order. Empty
              only when [uncertain] is present — a commit-phase failure whose
              very first write did not confirm. *)
      uncertain : Mentat_workspace.Path.t option;
          (** The stopping target of a commit-phase failed apply: mutation of it
              was attempted under this claim and did not confirm, so its on-disk
              state is uncertain. A per-event field, not a [Tool_observed] row,
              deliberately: the causal tie to the claim and apply is part of the
              fact. It records no images and never contributes a {!Change.t}; it
              degrades revertability to [Incomplete]
              ({!Revertability.Uncertain}). *)
    }
      (** One recorded apply under the run-stage tool claim: exact transitions
          lowered from the applied [Mentat_edit] outcome — the whole result on
          success, the confirmed prefix plus the uncertain stopping target on a
          commit-phase failure. *)
  | Tool_observed of {
      turn : Mentat_session.Turn.Id.t;
      claim : Mentat_session.Tool_claim.Id.t;
      paths : Mentat_workspace.Path.t list;
          (** Deduplicated, sorted, non-empty; normalization is
              {!Mentat_workspace.Path.t}'s own construction invariant. *)
    }
      (** Epistemically modest, deliberately: the paths changed while the claim
          window was active. The event does not assert the tool caused the
          changes, records no images, and never contributes a {!Change.t}. It
          contributes [Incomplete] revertability and an observational protocol
          notice. *)
  | Revert_started of Revert_data.Started.t
      (** Persisted {e before} any revert filesystem IO. *)
  | Revert_settled of Revert_data.Settled.t
      (** Per-path outcomes plus the apply evidence. *)

val checkpoint : Checkpoint.t -> t
(** [checkpoint capture] is the durable event for the boundary capture attempt
    [capture]. *)

val of_edit :
  turn:Mentat_session.Turn.Id.t ->
  claim:Mentat_session.Tool_claim.Id.t ->
  ordinal:int ->
  checkpoint:Checkpoint.Id.t option ->
  Mentat_edit.Result.t ->
  t
(** [of_edit ~turn ~claim ~ordinal ~checkpoint result] is the pure lowering the
    engine calls at the edit application boundary for a successful apply. For
    each applied entry, in mutation order: images from the entry's observed
    before and planned after states (the proven two-case narrowing), line counts
    from a [Textdiff] diff of the two texts, and the change id derived from
    [claim], [ordinal], and the entry's target path. It inspects no tool output
    and downcasts nothing — the engine lowers exact transitions without reading
    what the tool returned; it is a total function of a non-empty result. An
    empty result records no event — the engine appends nothing.

    [ordinal] is the event's index among the claim's recorded apply events —
    dense, 0-based; the fold rejects a gap or a repeat. [checkpoint] is required
    because [None] is a durable fact — the turn's conservative capture yielded
    no snapshot — never a defaulted omission.

    Raises [Invalid_argument] if [result] is empty or [ordinal] is negative. *)

val of_attempt :
  turn:Mentat_session.Turn.Id.t ->
  claim:Mentat_session.Tool_claim.Id.t ->
  ordinal:int ->
  checkpoint:Checkpoint.Id.t option ->
  applied:Mentat_edit.Result.Entry.t list ->
  uncertain:Mentat_workspace.Path.t option ->
  t
(** [of_attempt ~turn ~claim ~ordinal ~checkpoint ~applied ~uncertain] lowers
    one commit-phase failed apply's workspace effect to an [Edit] event:
    [applied], the confirmed committed prefix
    ({!Mentat_edit.Apply_error.applied}), lowers to exact change rows in
    mutation order, and [uncertain] is the stopping target whose on-disk state
    did not confirm ({!Mentat_edit.Apply_error.Phase.Commit}). The apply error
    itself never crosses this boundary — the attribution port hands over the
    confirmed prefix and the target, not {!Mentat_edit.Apply_error.t}.

    [ordinal] is the event's index among the claim's recorded apply events —
    dense, 0-based; the fold rejects a gap or a repeat. [checkpoint] is required
    because [None] is a durable fact — the turn's conservative capture yielded
    no snapshot — never a defaulted omission. There is no successful-apply
    overload: the exact yield lowers through {!of_edit}, and an attempt exists
    only when it left workspace effect.

    Raises [Invalid_argument] if [ordinal] is negative, or if [applied] is empty
    and [uncertain] is [None] — a non-effectful attempt is not an occurrence. *)

val tool_observed :
  turn:Mentat_session.Turn.Id.t ->
  claim:Mentat_session.Tool_claim.Id.t ->
  Mentat_workspace.Path.t list ->
  t
(** [tool_observed ~turn ~claim paths] deduplicates and sorts [paths]
    (normalization is {!Mentat_workspace.Path.t}'s own invariant). The engine
    derives the path set from the workspace attribution service at claim-scope
    close; this is not a revival of tool receipts — no tool reports its own
    effect.

    Raises [Invalid_argument] if [paths] is empty. *)

val revert_started : Revert_data.Plan.t -> t
(** [revert_started plan] freezes [plan] into its durable started record.
    Construction touches no filesystem: the event is a function of the plan
    alone, so it can — and must — be persisted before any revert IO. *)

val revert_settled : Revert_data.Settled.t -> t
(** [revert_settled settled] is the settlement event for [settled]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same event. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats an event for diagnostics. *)

val jsont : t Jsont.t
(** [jsont] is the sole durable mutation codec entry point. Every variant uses
    an explicit stable tag; unknown tags and unknown members fail loudly as
    structured decode errors — never a silent skip. Decode checks local
    representation invariants, including derived-id validity. *)
