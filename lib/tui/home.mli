(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The launch stage shown before a conversation begins.

    Home composes the animated brand, an inert welcome, the caller's composer,
    and the newest resumable session supplied by the session query. It is a pure
    view: it performs no client, store, workspace, or account operation. Live
    workspace health belongs to progress notices, not to a home-only pull.

    The shell owns query scheduling and interprets the exact session identifier
    returned by {!Recents.most_recent}. {!Motion} owns only the lockup's local
    playback state. *)

(** {1:motion Brand motion} *)

module Motion : sig
  type t
  (** The type for the lockup's immutable playback state. *)

  val init : reduced:bool -> t
  (** [init ~reduced] starts the looping pour. When [reduced] is [true], it
      starts at the static lockup and requires no timer. *)

  val tick : t -> t
  (** [tick t] advances the pour by one frame. The last frame rests for four
      ticks before the pour repeats. A static value is unchanged. *)

  val freeze : t -> t
  (** [freeze t] is the static lockup. It is idempotent; the shell calls it on
      the first user input so the brand does not keep moving while they type. *)

  val animating : t -> bool
  (** [animating t] is [true] iff the shell should keep a frame subscription
      mounted for [t]. *)

  val lockup_rows : t -> string list
  (** [lockup_rows t] is exactly two rows. A frozen or reduced-motion value is
      {!Theme.lockup} byte-for-byte. *)
end

(** {1:recents Recent sessions} *)

module Recents : sig
  type t
  (** The type for the home projection of the saved-session query.

      Values distinguish initial loading, ready, retained refresh, initial
      failure, and failed refresh. Successful rows remain exact
      {!Mentat_session.Summary.t} values. Only active, top-level summaries are
      resumable from Home; archived, deleted, forked, and delegated-child
      summaries supplied by an over-broad responder are ignored. *)

  val loading : t
  (** [loading] is a query that has not produced its first response. *)

  val loaded : Mentat_session.Summary.t list -> t
  (** [loaded summaries] is a successful response.

      Eligible summaries are sorted with
      {!Mentat_session.Summary.compare_recency}; duplicate identifiers are
      discarded. Eligibility is applied before selecting the newest summary, so
      callers must not impose a result limit before lineage filtering. A
      successful response, including an empty response, clears a prior failure
      or refresh state. *)

  val refreshing : t -> t
  (** [refreshing t] marks a new query in flight. An earlier successful
      response, including a known-empty response, remains available and visible.
      Without an earlier response this is {!loading}. *)

  val failed : string -> t -> t
  (** [failed message t] records a query failure. An earlier successful response
      remains available and visible. [message] is normalized to inert one-line
      UTF-8 for display; blank diagnostics become [session history unavailable].
  *)

  val most_recent : t -> Mentat_session.Id.t option
  (** [most_recent t] is the identifier of the newest retained resumable
      summary, including during a refresh or after a failed refresh. It is
      [None] before the first successful response and after a successful empty
      response. *)
end

(** {1:layout Stage layout} *)

val stage :
  palette:Theme.Palette.t ->
  snapshot:Snapshot.t ->
  recents:Recents.t ->
  now:Mentat_session.Time.t ->
  account_absent:bool ->
  permission_review:Mentat_permission.Review_behavior.t ->
  notice:string list ->
  motion:Motion.t ->
  composer:'msg Mosaic.t option ->
  'msg Mosaic.t
(** [stage ~snapshot ~recents ~now ~account_absent ~permission_review ~notice
     ~motion ~composer] is the complete Home region above the shell-owned
    footer.

    [snapshot] contains launch version, workspace, and sandbox facts. Its model,
    effort, and known context window are the launch fallback before any turn and
    the last-started contract afterwards. [recents] is the retained result of
    the session query; Home derives its title, age, phase, turn count, and
    resume target directly from the owner summary. [now] is the caller's clock
    for age formatting. Future timestamps read [just now].

    [account_absent] is the caller's exact account-readiness-derived fact. When
    [true], Home points visibly to [/login]. [permission_review] is the exact
    session posture; bypass renders a loud warning. Home never interprets
    {!Snapshot.sandbox} as a safety fact.

    [notice] is the standing announcement content for the stage's notice slot,
    rendered as an accent-barred block between the launch facts and the
    composer. It is mentat speaking to its user, not transient shell chrome:
    armed prompts and flashes belong to the caller's footer. The lead line is
    default foreground with the product name an unbolded-accent atom; every
    supporting line is muted. The block sizes to its widest line so the stage
    centers it as a unit, lines left-aligned within it. An empty or blank
    [notice] renders no block.

    Notice lines, query diagnostics, summary titles and previews, and launch
    labels are normalized to one-line UTF-8; terminal controls cannot escape
    into the frame. Mosaic owns intrinsic measurement, word wrapping,
    truncation, and grapheme-safe clipping at the width assigned by the parent
    layout.

    A present [composer] keeps its natural height and is centered in a flex item
    whose maximum width is 60 columns. Home never premeasures it. When
    [composer] is [None], a caller-owned panel occupies the bottom surface and
    launch facts and the permission warning are omitted with the composer.

    Growing flex spacers center the stage vertically. When height is scarce,
    flex shrink weights make the notice yield most readily, followed by
    recent-session facts and brand decoration; the composer and safety warning
    do not shrink. The stage clips any final overflow to the allocation supplied
    by its parent. *)
