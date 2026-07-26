(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The HTTP surface of the web frontend, as transport-light routing values.

    This module maps a parsed HTTP request — a method, a split path, a query,
    and a decoded form body — onto a {!Mentat_client} call and a {!response}
    value, and it drives the live event stream ({!feed}). It touches no HTTP
    library: the daemon's network edge decodes the request, hands the parts to
    {!handle} (or {!feed} for the stream), sets the security headers it owns
    (the authoritative CSP, the session cookie), and serialises the {!response}
    through {!to_http}. The frontend's whole effect surface is a single
    {!Mentat_client.t}; it reaches no engine, store, or transport.

    Every command route follows post-redirect-get: a form POST commits through
    {!Mentat_client.submit} (or a request/completion flow) and answers with a
    redirect back to the session, so the surface works with browser scripting
    disabled. The embedded script upgrades those same routes with a live stream
    and in-place swaps, but adds no route the no-script path lacks. *)

(** {1:responses Responses} *)

type response =
  | Html of Html.t  (** A complete HTML document; [200], [text/html]. *)
  | Fragment of Html.t list
      (** An HTML fragment (one or more sibling nodes) for a client-side swap or
          a scroll-up page; [200], [text/html]. *)
  | Redirect of string
      (** A [303 See Other] to a same-origin path — the post-redirect-get
          landing a no-script form follows after a command. *)
  | Asset of { media_type : string; body : string }
      (** A served static asset with its own media type; [200]. *)
  | Not_found  (** [404]; an unrouted path. *)
  | Bad_request of string
      (** [400]; a request the frontend rejects before the client — an
          unparseable id, a missing form field, or a malformed answer. *)
  | Failed of Mentat_protocol.Error.t
      (** A client operation returned a structured error; {!to_http} maps it to
          an HTTP status. *)

module Http : sig
  (** The transport-ready shape of a {!response}. *)

  type t = {
    status : int;  (** The HTTP status code. *)
    headers : (string * string) list;
        (** Response headers this surface owns: the content type, and a
            [location] for a redirect. The daemon's edge adds the ones it owns
            (the CSP, cookies) and never expects them here. *)
    body : string;  (** The response body bytes. *)
  }
end

val to_http : response -> Http.t
(** [to_http response] is the status, owned headers, and body bytes for
    [response] — the total mapping the daemon's edge serialises. Error and
    not-found responses render a minimal HTML page; a redirect carries an empty
    body and a [location] header. *)

(** {1:environment The connection environment} *)

module Env : sig
  (** The stable per-connection injections a request handler reads: the client,
      a clock, and the client-minted id sources. The daemon builds one and
      reuses it across requests. *)

  type t

  val make :
    client:Mentat_client.t ->
    now:(unit -> float) ->
    new_session:(unit -> Mentat_session.Id.t) ->
    new_turn:(unit -> Mentat_session.Turn.Id.t) ->
    t
  (** [make ~client ~now ~new_session ~new_turn] is the environment backed by
      [client]. [now] reads a wall clock in seconds (the running-tool stamp and
      the settled-turn receipt); [new_session] and [new_turn] mint the
      client-minted ids a new session and a prompt turn require, so the routing
      layer stays deterministic under test. *)
end

(** {1:handling Request handling} *)

val handle :
  Env.t ->
  meth:string ->
  path:string list ->
  query:(string * string list) list ->
  body:(string * string) list ->
  response
(** [handle env ~meth ~path ~query ~body] routes one non-streaming request.
    [path] is the URL path split on ['/'] with empty segments dropped, [query]
    is the decoded query string (a key to its values), and [body] is the decoded
    [application/x-www-form-urlencoded] form (a key to its last value). The live
    stream path ([GET /session/<id>/feed]) is {b not} handled here: the daemon
    routes it to {!feed}. An unroutable method or path is {!Not_found}. *)

(** {1:feed The live event stream} *)

module Frame : sig
  (** One server-sent event frame. *)

  type t = {
    id : int option;
        (** The committed position sequence for a transcript-append frame, which
            the daemon writes as the SSE [id:] so the browser's native
            [EventSource] resumes from it; [None] for a droppable live-region
            morph, which carries no [id:]. *)
    html : string;
        (** The rendered fragment bytes: the committed blocks of one fact, or
            the whole re-rendered live region. The daemon splits it across
            [data:] lines per the SSE rule and owns the heartbeat. *)
  }
end

type fault =
  | Attach of Mentat_protocol.Error.t
      (** The stream could not attach, or a catch-up position was rejected: no
          frame was emitted for it, so the daemon may still answer with an HTTP
          error. *)
  | Render_fault of Render.Error.t
      (** A committed fact was inconsistent with the fold — a projector or
          engine bug, or the [turn.message] assistant firewall miss. The stream
          is aborted after the frames already emitted; the daemon logs it. *)

val feed :
  Env.t ->
  sw:Eio.Switch.t ->
  session:Mentat_session.Id.t ->
  from:[ `Beginning | `After of Mentat_protocol.Position.t ] ->
  emit:(Frame.t -> unit) ->
  (unit, fault) result
(** [feed env ~sw ~session ~from ~emit] streams [session]'s live render to
    [emit], one {!Frame.t} per event, until the feed closes or faults. It
    follows the session from its beginning to rebuild the render accumulator
    faithfully (a bounded resume with a fresh accumulator would mis-handle a
    turn already in flight), and emits frames only for facts strictly after
    [from]: a committed fact contributes one append frame carrying its sequence
    followed by the re-rendered live region, and a droppable pulse contributes
    the re-rendered live region alone. The feed is bound to [sw]; releasing it
    closes the stream. *)
