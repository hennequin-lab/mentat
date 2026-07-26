(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The fact-to-HTML projection — the heart of {!Mentat_web}.

    This module is the web analogue of [lib/tui/turn.ml]: it folds the same
    committed {!Mentat_protocol.Fact.t} stream and droppable
    {!Mentat_protocol.Progress.t} pulses that drive the terminal transcript,
    holding the same per-session accumulator, but projects to {!Html.t}
    fragments rather than Mosaic boxes. The projection is a pure function with
    no I/O, so it is directly snapshot-testable without a browser.

    {b Two DOM regions.} A rendered page carries a [#transcript] container of
    permanent, append-only committed blocks and a [#live] region re-rendered
    wholesale on every event. A committed fact contributes zero or more
    {e committed fragments} (a settled assistant article, a settled tool row, a
    compaction seam, an outcome notice) which {!fact} returns; the daemon
    appends them to [#transcript]. The ephemeral tail — the streaming open
    block, running tool rows, the reasoning ticker, the pending decision form,
    the task board, ambient notices, the working line — is {!live}, a single
    node the daemon morph-replaces into [#live]. {!progress} only advances the
    accumulator; the daemon re-renders {!live} after it. This mirrors the TUI,
    which re-renders its whole tail each frame.

    {b Determinism keys.} A block minted by a committed fact carries the id
    [f-<seq>] from the fact's {!Mentat_protocol.Position.t}; blocks a later fact
    must find and mutate carry an owner-correlation id instead
    ([tool-<claimid>], [decision-<decisionid>], [turn-<turnid>-open]).
    Cold-loaded HTML and a later live SSE fragment independently compute the
    same id. Fragment roots carry a [data-swap] attribute ([append] or [morph])
    the SSE dispatcher reads.

    {b Firewall.} All fact and progress text reaches HTML only through
    {!Html.El.txt} / {!Html.At.v}. The {!Mentat_protocol.Fact.t} [Turn_message]
    arm carrying an assistant message is rejected as {!Error.Assistant_message},
    exactly as [turn.assistant] owns that lane in the TUI — a rendering of it
    would be a firewall miss. *)

(** {1:errors Fold errors} *)

module Error : sig
  (** Why a fact fold rejects a fact. These mirror [lib/tui/turn.ml]'s reducer
      errors: a fact arriving against an inconsistent accumulator is a projector
      or engine bug, surfaced as a finding rather than rendered. *)
  type t =
    | No_active_turn  (** A turn-scoped fact arrived with no active turn. *)
    | Turn_already_active of {
        active : Mentat_session.Turn.Id.t;
        incoming : Mentat_session.Turn.Id.t;
      }  (** A turn started while another is active. *)
    | Turn_mismatch of {
        active : Mentat_session.Turn.Id.t;
        incoming : Mentat_session.Turn.Id.t;
      }  (** A fact named a turn other than the active one. *)
    | Duplicate_tool_claim of Mentat_session.Tool_claim.Id.t
        (** A tool claim opened more than once. *)
    | Unknown_tool_claim of Mentat_session.Tool_claim.Id.t
        (** A settlement referenced an unopened claim. *)
    | Wrong_tool_stage of {
        claim : Mentat_session.Tool_claim.Id.t;
        expected : Mentat_tool.Stage.t;
        actual : Mentat_tool.Stage.t;
      }  (** A prepared settlement arrived at a non-prepare stage. *)
    | Decision_already_pending of {
        pending : Mentat_session.Decision.Id.t;
        incoming : Mentat_session.Decision.Id.t;
      }  (** A decision opened while one is already pending. *)
    | Unknown_decision of {
        pending : Mentat_session.Decision.Id.t option;
        incoming : Mentat_session.Decision.Id.t;
      }  (** A resolution did not match the pending decision. *)
    | Open_state_at_settlement of {
        turn : Mentat_session.Turn.Id.t;
        tool_claims : int;
        decision_pending : bool;
      }  (** A turn settled with open tool claims or a pending decision. *)
    | Assistant_message
        (** A [turn.message] fact carried an assistant message; [turn.assistant]
            owns that lane. *)

  val message : t -> string
  (** [message t] is a human-readable diagnostic for [t]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same error. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats {!message} output. *)
end

(** {1:accumulator The accumulator} *)

type acc
(** The type for the per-session render accumulator — the ephemeral state a live
    tail is drawn from, re-homed from [turn.ml]'s [Turn.t] with the
    Mosaic-specific wrap chunk lists dropped. It is reconstructible cold from
    committed facts alone, which is what makes an SSE skip-to-settled a
    guarantee rather than a degraded mode. Consume one accumulator per feed,
    linearly. *)

val initial : acc
(** [initial] is the idle accumulator: no turn, no open block, no tools, no
    pending decision. *)

(** {1:folding Folding} *)

val fact :
  now:float ->
  acc ->
  Mentat_protocol.Position.t ->
  Mentat_protocol.Fact.t ->
  (acc * Html.t list, Error.t) result
(** [fact ~now acc pos f] advances [acc] by the committed fact [f] at [pos] and
    returns the committed fragments [f] contributes, each a self-describing
    append or morph node the daemon dispatches (and cold load nests in
    [#transcript]). [now] is a wall-clock reading in seconds, used only for the
    running-tool start stamp and the settled-turn elapsed receipt; passing a
    fixed value keeps the projection deterministic. It is [Error _] when [f] is
    inconsistent with [acc] (see {!Error.t}), exactly as [turn.ml]'s reducer
    rejects it. *)

val progress : acc -> Mentat_protocol.Progress.t -> acc
(** [progress acc p] advances [acc] by the droppable pulse [p]. A pulse
    contributes no committed fragment; it only updates the live tail, which the
    daemon re-renders through {!live}. A pulse for an inactive turn is ignored.
*)

val live : now:float -> session:Mentat_session.Id.t -> acc -> Html.t
(** [live ~now ~session acc] is the [#live] region for [acc]: the pending user
    echo, the reasoning ticker, the streaming open assistant block (plain
    escaped text, never markdown mid-stream), running and awaiting tool rows,
    the pending decision form (whose action posts to [session]), the task board,
    ambient notices, download and compaction banners, and the working line. The
    node is a single morph target; an idle empty accumulator yields an empty
    region. [now] stamps client-ticked elapsed times. *)

(** {1:cold Cold load and windowed reads} *)

val cold :
  now:float ->
  session:Mentat_session.Id.t ->
  Mentat_protocol.Transcript.Tail.t ->
  (acc * Html.t, Error.t) result
(** [cold ~now ~session view] folds [view]'s page of committed facts and
    assembles the full initial page body: the [#transcript] container of
    committed blocks followed by the [#live] region of the final accumulator
    (seeded with [view]'s authoritative pending decision). A bounded tail
    ([Transcript.Tail.default_n] is 100) routinely begins partway through a
    turn; those leading facts, whose [Turn_started] fell above the window, are
    absorbed and rendered as one truncation marker rather than rejected, so the
    complete in-window turns render and the read never fails on the happy path.
    It is [Error _] only for a genuine firewall violation {e after} a turn
    context is established in the window (a projector bug, not a window split) —
    the firewall stays intact for what the page shows. *)

val page :
  now:float ->
  (Mentat_protocol.Position.t * Mentat_protocol.Fact.t) list ->
  (acc * Html.t list, Error.t) result
(** [page ~now facts] folds a bounded [facts] window to its committed transcript
    blocks — the backward-paging read behind a scroll-up. Its leading edge is
    absorbed with the same window-boundary tolerance as {!cold} (the older-page
    affordance conveys the truncation, so no inline marker is emitted here). It
    is [Error _] on the same genuine post-context firewall violation as {!cold}.
*)

val attach :
  now:float -> (Mentat_protocol.Position.t * Mentat_protocol.Fact.t) list -> acc
(** [attach ~now window] folds a bounded seed [window] of committed facts into
    the accumulator a live feed resumes from, so a reconnect can seed from a
    bounded suffix instead of an O(history) fold from the feed's beginning. The
    window ordinarily ends at the resume position, so a mid-turn resume seeds a
    [Running] accumulator (established by the window's own [Turn_started]) and
    the subsequent live {!fact} calls — which stay strict — fold against it
    rather than tripping {!Error.No_active_turn}. It is total: the seed window
    is trusted committed history, and its leading edge and any inconsistency are
    absorbed rather than raised, since only the live stream after the seed can
    carry a genuine firewall violation. *)
