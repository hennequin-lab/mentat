(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Immutable launch facts supplied to the terminal application.

    A startup value is the complete, launch-fixed input crossed from the
    executable into the pure application shell. It contains no client, callback,
    store, or other effect handle. Construct one with {!make} and inspect its
    facts through the direct accessors below.

    {b Note.} Only launch-fixed values cross here: a value belongs in a startup
    value iff it is frozen for the terminal lifetime, or superseded by the
    session feed the instant a turn exists — the version, resolved startup
    model, startup sandbox label, and cwd. A live queryable projection the
    client owns a query for — the resolved configuration view, the
    model-readiness catalog, or session summaries — never enters here, even when
    the composition root already holds it in memory; such a value reaches the
    shell only through the client's query seam. A startup value must be one a
    remote daemon could supply identically from its own session view; a live
    projection injected past the client waist lives daemon-side and never
    reaches a remote frontend, silently making that slice of the UI
    in-process-only. Carrying no effect handle is therefore necessary but not
    sufficient to cross here. *)

(** {1:types Types} *)

(** The initial composer action requested by the executable. *)
type input =
  | Empty  (** Start with an empty composer. *)
  | Draft of string  (** Seed the composer without submitting it. *)
  | Submit of string
      (** Submit the text before the first frame. On a resumed session this is
          conservatively treated as a draft so it cannot race replay. *)

type t
(** The type for a complete set of launch facts. Values remain fixed for one
    terminal lifetime; replayed and live session facts may supersede their
    presentation fallbacks in the application model. *)

(** {1:constructors Constructor} *)

val make :
  ?launch_review:bool ->
  snapshot:Snapshot.t ->
  mode:Mentat_session.Contract.Mode.t ->
  session:Mentat_session.Id.t option ->
  input:input ->
  providers:Mentat_provider.t list ->
  permission_review:Mentat_permission.Review_behavior.t ->
  unit ->
  t
(** [make ~snapshot ~mode ~session ~input ~providers ~permission_review ()] is a
    complete launch value. [snapshot], [mode], [providers], and
    [permission_review] are fallbacks fixed at launch. [session] selects the
    session to replay from the beginning, when present, and [input] specifies
    the initial composer action. [launch_review] opens the review surface over
    the workspace review before the first frame ([mentat review]); it defaults
    to [false]. *)

(** {1:queries Launch facts} *)

val snapshot : t -> Snapshot.t
(** [snapshot t] is the launch fallback rendered before a turn supplies exact
    session facts. *)

val mode : t -> Mentat_session.Contract.Mode.t
(** [mode t] is the mode requested for the first fresh prompt. It does not
    mutate a resumed turn. *)

val session : t -> Mentat_session.Id.t option
(** [session t] is [Some id] when [id] must be replayed from the beginning and
    [None] for a fresh launch. *)

val input : t -> input
(** [input t] is the initial composer action. *)

val providers : t -> Mentat_provider.t list
(** [providers t] is the launch-fixed provider declaration metadata used by
    authentication surfaces. It is not a live catalog projection. *)

val permission_review : t -> Mentat_permission.Review_behavior.t
(** [permission_review t] is the acknowledged permission posture rendered before
    a session query supplies exact settings. *)

val launch_review : t -> bool
(** [launch_review t] is [true] iff the executable launched onto the review
    surface ([mentat review]); the application opens review before the first
    frame. *)
