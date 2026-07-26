(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Public web search tool over a remote MCP search backend.

    [web_search] accepts a strict JSON object with required string [query] and
    optional positive integer [num_results], bounded by host policy. Unknown and
    duplicate members are rejected. It issues one JSON-RPC [tools/call] POST to
    the selected {!Search_service} backend over the same hardened transport as
    {!Web_fetch}: the endpoint host and port are vetted before connecting, TLS
    peers are verified against the system trust roots, the response body is
    size-bounded and read under a whole-call deadline, and the call is
    cancellable. No redirect is followed.

    The backend and its optional API key are host configuration
    ([web.search_provider], [web.exa_api_key], [web.parallel_api_key]); a key
    travels only in the request (Exa in the endpoint query, Parallel in an
    Authorization header) and never reaches the model, the permission request,
    or durable output. The remote server formats the result text; the tool
    sanitizes and bounds it to the policy character limit. A non-2xx response,
    an unparseable body, an oversized body, or a private resolved address fails
    with a bounded diagnostic and never contacts anything else. *)

val name : string
(** [name] is ["web_search"]. *)

val make :
  policy:Policy.t ->
  backend:Search_service.t ->
  ?api_key:string ->
  fetch:Transport.t ->
  unit ->
  Mentat_tool.t
(** [make ~policy ~backend ~fetch ()] is the immutable [web_search] definition.
    Construction starts no work and opens no network resource.

    [backend] selects the remote search service and [api_key], when present, is
    its optional credential — both search keyless. [fetch] is the shared web
    transport, invoked once beneath a fresh call-scoped switch with a
    {!Transport.Request.Post} carrying the JSON-RPC search envelope.

    Malformed input produces no permission request and fails as
    [`Invalid_input]. A private resolved endpoint address fails as
    [`Permission_denied]; timeout as [`Timed_out]; backend unavailability as
    [`Unavailable]; and non-2xx, response-limit, protocol, and parse failures as
    [`Failed]. Cooperative cancellation returns a cancelled interruption. *)
