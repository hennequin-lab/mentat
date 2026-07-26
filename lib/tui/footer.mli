(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The single-row footer beneath the composer.

    The footer presents only facts supplied through the next client boundary or
    fixed at launch: permission posture, account absence, composer input mode,
    workspace, model, launch sandbox posture, and observed context use. It
    deliberately has no workspace-health verdict. Health changes arrive in the
    transcript as ambient progress notices; this module performs no workspace
    read and does not invent an idle verdict while that feed has no health
    cadence.

    Plain input shows workspace facts and the available shortcut or way-home
    affordance. Shell and history-search input replace those facts with their
    mode-specific key hints and badge. Left-hand facts form one contiguous run:
    every fact past the first owns its leading separator in the same text node,
    so flex layout cannot detach or collapse that join. The right affordance
    sits past a growing spacer that holds it apart from those facts; it carries
    no separator of its own, since nothing precedes it in that group. Mosaic
    owns all width allocation and display-column-safe truncation; Footer passes
    the complete home-relative path rather than shortening path strings itself.
*)

val view :
  palette:Theme.Palette.t ->
  permission_review:Mentat_permission.Review_behavior.t ->
  input_mode:Composer.Input_mode.t ->
  account_absent:bool ->
  ?home_badge:string ->
  ?context:int * int option ->
  Snapshot.t ->
  'msg Mosaic.t
(** [view ~permission_review ~input_mode ~account_absent ?context snapshot]
    renders one row that fills its parent's available width.

    [permission_review] is the session's acknowledged posture.
    {!Mentat_permission.Review_behavior.Enforce} adds nothing;
    {!Mentat_permission.Review_behavior.Bypass} adds a loud safety warning. Its
    [!] status mark is intrinsic while its explanation yields to higher-priority
    content.

    [input_mode] controls the rest of the row. {!Composer.Input_mode.Plain}
    shows the home-relative workspace and the full {!Snapshot.model_line}; the
    provider, model, and optional effort reach Mosaic as one semantic value.
    When [account_absent] is [true], the caller has derived from account
    readiness that no provider is connected, and a [! not logged in · /login]
    warning leads those facts. {!Composer.Input_mode.Shell} and
    {!Composer.Input_mode.History_search} instead show their owned key hints and
    right-aligned badges. Input-mode and safety marks remain intrinsic while
    explanatory labels truncate.

    [home_badge], when it contains non-whitespace text, replaces the ordinary
    [? for shortcuts] hint in plain mode. The application uses it for the way
    back from a drilled-in agent thread. Malformed UTF-8 and terminal controls
    are replaced and line breaks are flattened before layout.

    [context] is [Some (used, percent)] where [used] is the current context
    occupancy in tokens and [percent] its share of the model window when known,
    both precomputed by the caller so this compact glance and the pane's context
    section agree. It defaults to absence. The muted [used (percent%)] fact —
    abbreviated as [9.4K (2%)], or [9.4K] alone when [percent] is [None] —
    appears only when [used] is positive. It is the lowest-priority left fact,
    shed before the workspace path and model line under width pressure.

    The launch sandbox posture from [snapshot.sandbox] appears in plain mode as
    an ambient fact after the model. The permissive [workspace-write] default
    stays silent; [read-only] and [external-sandbox] read as quiet facts; only
    [danger-full-access] carries a loud [!] safety mark, worded [full access] so
    it never reads as a duplicate of the permission-review warning. An
    unrecognized spelling is silent. The mark is intrinsic while its label
    yields to higher-priority facts.

    Workspace spelling follows [snapshot.home]: equality renders [~], a strict
    descendant begins with [~/], and an outside path or absent home remains
    absolute. The optional separator and complete path are one flexible text
    node. Mosaic's prefix-first ellipsis therefore cannot detach that separator
    or drop parent components while retaining a misleading root-relative leaf.
    Mosaic owns allocation and truncation without Footer measuring or slicing
    display text.

    Plain-mode degradation is expressed through flex priority: the context
    glance yields first, then the workspace path and shortcut, then the full
    model line; safety facts retain the highest priority. Under severe pressure
    Mosaic may remove the whole workspace allocation rather than presenting a
    misleading path fragment. The full model line and its leading separator form
    one truncating text node, so Mosaic cannot lay out a detached delimiter.
    Mosaic truncates without splitting grapheme clusters. The row clips any
    residual overflow rather than wrapping. *)
