(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Provider-neutral LLM values and client interface for one model turn.

    [Mentat_llm] defines inert values for provider-neutral model requests,
    model-visible transcripts, completed provider responses, live progress
    events, and structured errors. {!Client} is the injected effect boundary: it
    invokes an adapter-supplied interpreter but owns no transport and depends on
    no effect runtime. The library's data dependencies are [jsont] for codecs
    and [mentat.digest] for request and content identity.

    {b The waist.} The library is a closed loop over one spine. A checked
    {!Transcript.t} is assembled into a {!Request.t}; a {!Client.t} interprets
    the request, emits ordered {!Event.t} progress, and returns an authoritative
    terminal {!Response.t}; the response's assistant message is appended back
    into the transcript with {!Transcript.add_response}. The common path is
    these five concepts — {!module:Transcript}, {!module:Request},
    {!module:Client}, {!module:Response}, {!module:Event}. The payload and
    identity leaves ({!module:Content}, {!module:Tool}, {!module:Message},
    {!module:Usage}, {!module:Model}, {!module:Provider}) and {!module:Error}
    are reached through them.

    {b Off the spine.} {!module:Schema} is not on the turn loop: it validates a
    JSON instance against the supported subset schema a {!Tool.input_schema}
    carries — a structured-output conformance check paired with tools, run
    outside the request-response cycle rather than as a step in it.

    {b Terminal authority.} The terminal {!Response.t} is the durable transcript
    fact; {!Event.t} values are display and early-execution progress only and
    carry no replay authority. Only {!Response.message} is appended to a
    transcript, never a live event.

    {b Above the library.} Concrete provider adapter libraries supply the
    effectful clients: each implements a {!Client.run} owning authentication,
    transport, retries, and protocol translation. Executable tools, sessions,
    permissions, storage, model catalog, and pricing live in libraries that
    depend {e into} [mentat.llm], never the reverse.

    A typical turn builds or updates a {!Transcript.t}, constructs a checked
    {!Request.t} with {!Request.make}, sends it through {!Client.response},
    consumes {!Event.t} progress and the terminal {!Response.t}, and appends
    {!Response.message} back with {!Transcript.add_response}. {!Request.digest}
    projects a request to a canonical {!Mentat_digest.t} identity for durable
    billing claims. *)

module Provider = Provider
(** Provider namespaces. *)

module Model = Model
(** Provider model identities. *)

module Content = Content
(** Model-visible content blocks. *)

module Image = Image
(** Pure raster-image recognition (format and dimensions). *)

module Tool = Tool
(** Model-visible tools, calls, and results. *)

module Schema = Jsonschema
(** JSON-Schema subset validator for {!Tool.input_schema}, off the turn loop.
    The validator is the standalone [jsonschema] library; this is its
    mentat-side surface, paired here with {!Tool.input_schema}. *)

module Usage = Usage
(** Token usage. *)

module Message = Message
(** Replayable model-visible messages. *)

module Transcript = Transcript
(** Checked model-visible transcripts. *)

module Request = Request
(** Model requests and their canonical digest. *)

module Response = Response
(** Completed provider responses. *)

module Error = Error
(** Provider-boundary errors. *)

module Event = Event
(** Live non-terminal provider progress. *)

module Client = Client
(** Provider interpreters. *)
