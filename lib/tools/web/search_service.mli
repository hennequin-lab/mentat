(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Remote MCP web-search backends: the wire form for {!Web_search}.

    This module isolates the hand-rolled JSON-RPC [tools/call] request envelope
    and the plain-JSON / server-sent-event response parsing for the supported
    keyless search endpoints (Exa, Parallel). It is {b not an MCP client}: there
    is no initialize handshake, session, or capability negotiation — only the
    encode and decode of one stateless HTTP POST. Confining the envelope here
    keeps {!Web_search} and its transport backend-agnostic, so a future backend
    swap changes only this module. It performs no I/O. *)

type t =
  | Exa
  | Parallel
      (** The supported search backends. Both answer keyless; an API key is
          optional and only raises limits. *)

val of_string : string -> t option
(** [of_string s] is the backend spelled [s] (["exa"], ["parallel"]), or [None].
    ["off"] is not a backend; it is the config sentinel that withholds the tool.
*)

val to_string : t -> string
(** [to_string t] is [t]'s stable spelling. *)

val endpoint : t -> ?api_key:string -> unit -> Uri.t
(** [endpoint t ()] is [t]'s absolute HTTPS MCP endpoint. Exa carries an
    optional key as a query parameter; Parallel's endpoint is key-free (its key
    rides an Authorization header — see {!headers}). *)

val headers :
  t -> ?api_key:string -> user_agent:string -> unit -> (string * string) list
(** [headers t ~user_agent ()] are the request headers: a [user-agent], a JSON +
    event-stream [accept], a JSON [content-type], and — for Parallel with a key
    — an [authorization] bearer. Every name is a valid HTTP token; only
    [content-type] and [authorization] fall outside the shared transport's
    default header allowlist. *)

val request_body : t -> query:string -> num_results:int -> string
(** [request_body t ~query ~num_results] is the JSON-RPC 2.0 [tools/call]
    request body invoking [t]'s remote search tool with a backend-shaped
    argument object. *)

val parse_response : string -> (string, string) result
(** [parse_response body] extracts the model-facing search result text from an
    MCP response [body], accepting either a plain JSON object or a server-sent-
    event stream whose [data:] lines each carry one. It returns the first result
    content item's text, or [Error] when no parseable payload carries result
    text. The text is untrusted backend output; the caller sanitizes and bounds
    it. *)
