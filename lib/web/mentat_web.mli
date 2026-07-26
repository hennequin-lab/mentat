(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Server-rendered HTML for the daemon's web frontend.

    The daemon's co-located web frontend. {!Html} is the
    escaping-by-construction combinator surface (the one XSS boundary),
    {!Render} the pure projection that folds the same {!Mentat_protocol.Fact.t}
    stream the TUI transcript folds into HTML fragments, {!Page} the document
    shell, {!Assets} the embedded browser script and stylesheet, and {!Routes}
    the HTTP surface that maps requests onto a single {!Mentat_client.t} and
    drives the live event stream. The frontend links no HTTP server and no
    engine: its whole effect surface is the client, and the fact-to-HTML
    projection is directly snapshot-testable without a browser. The daemon's
    network edge decodes requests onto {!Routes.handle} / {!Routes.feed} and
    owns the response headers (the authoritative CSP, cookies) this library does
    not. *)

module Html = Html
(** The escaping HTML combinator surface — the one XSS boundary. *)

module Render = Render
(** The fact-to-HTML projection and its per-session accumulator. *)

module Page = Page
(** The document shell: the [<head>] chrome, the session index, and the
    transcript page with its composer. *)

module Assets = Assets
(** The embedded browser script and stylesheet served under [/static]. *)

module Routes = Routes
(** The HTTP surface as transport-light routing values, and the live feed. *)
