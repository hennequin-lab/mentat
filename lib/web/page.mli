(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The document shell for the web frontend.

    {!Render} projects a session's fact stream to the two swappable DOM regions
    ([#transcript] and [#live]); this module wraps that body in a complete HTML
    document — the static [<head>] chrome, the strict in-page CSP meta (the
    second line behind the edge's authoritative header), the [<link>] and
    [<script>] references to the embedded static assets, the session navigation,
    and the always-present bottom composer form. Every dynamic value — a session
    title, a resume token — reaches the page through {!Html.El.txt} /
    {!Html.At.v}; only the fixed chrome uses {!Html.El.unsafe_raw}.

    The composer is a plain HTML form that posts to the command routes, so the
    surface works with no browser scripting at all: a submit is a POST that the
    daemon answers with a redirect back to the session, and the streamed live
    tail is an enhancement the embedded script layers on top. *)

val content_security_policy : string
(** The strict CSP the in-page meta declares. The edge's authoritative header
    must be byte-identical, and the two libraries share no linkable home for one
    constant (the layering boundary forbids a shared link in either direction),
    so the equality is enforced by the drift guard in [test/web], not by
    construction. *)

val document : head_title:string -> body:Html.t list -> Html.t
(** [document ~head_title ~body] is the bare document shell — the [<head>]
    chrome (charset, viewport, the in-page CSP meta, the stylesheet and script
    references) wrapping [body] as the [<body>] children. It is the primitive
    the richer entry points below build on, exposed for one-off pages (such as
    the review read) that reuse the chrome without the session composer.
    [head_title] is escaped owner text. *)

val session_list :
  sessions:Mentat_session.Summary.t list -> unreadable:int -> Html.t
(** [session_list ~sessions ~unreadable] is the complete session-index document:
    the shell wrapping a navigation list of [sessions] (newest first, each a
    link to its transcript with its lifecycle actions) and a new-session form. A
    non-zero [unreadable] — the count of store entries a scan could not classify
    — is surfaced as a single notice rather than dropped silently. *)

val session :
  title:string ->
  session:Mentat_session.Id.t ->
  resume:Mentat_protocol.Position.t option ->
  earlier:Mentat_protocol.Position.t option ->
  body:Html.t ->
  Html.t
(** [session ~title ~session ~resume ~earlier ~body] is the complete transcript
    document for [session]: the shell wrapping [body] (the {!Render.cold} node
    carrying [#transcript] and [#live]) in a [<main>] that records [session] and
    the [resume] token the embedded script opens its event stream from ([resume]
    is the feed head, or [None] for a session with no committed fact yet). Above
    [body] sits the {!earlier_control} for [earlier] (the tail's backward
    continuation), and below the [<main>] the bottom composer form with its
    interrupt and compaction controls. [title] is the page and header title,
    escaped as owner text. *)

val earlier_control :
  session:Mentat_session.Id.t ->
  before:Mentat_protocol.Position.t option ->
  Html.t
(** [earlier_control ~session ~before] is the [#earlier] region carrying the
    scroll-up pagination trigger: a load-earlier button whose [data-before]
    records the [before] continuation cursor when older facts remain, or an
    empty region when the feed's beginning is reached. The backward-page route
    returns a fresh copy of this node so the embedded script can advance or
    retire the trigger. *)

val error : status:int -> message:string -> Html.t
(** [error ~status ~message] is a minimal document reporting an HTTP-level
    failure — an unrouted path, a malformed request, or a client operation error
    — with [status] in the heading and [message] as escaped body text. *)
