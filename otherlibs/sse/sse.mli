(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Server-Sent Events framing: a pure writer and reader.

    This is the wire framing only — the [id:]/[event:]/[data:] lines of the SSE
    text format
    ({{:https://html.spec.whatwg.org/multipage/server-sent-events.html} the
      EventSource protocol}). It carries no transport: the {!Writer} produces
    the bytes of one event, and the {!Reader} pulls one event from a
    caller-owned line source, so the same core serves an HTTP server emitting
    events, an HTTP client decoding a model stream, and a test harness — each
    supplying its own byte plumbing. The Eio line adapter for a byte stream
    lives in the companion [sse_eio] library. *)

module Event : sig
  type t = { id : string option; name : string; data : string }
  (** One decoded event. [id] is the last [id:] field seen, if any; [name] is
      the [event:] type ([""] when the event carried no [event:] field, i.e. the
      default [message] type); [data] joins the event's [data:] fields with a
      single newline between them. *)
end

module Writer : sig
  val frame : ?id:string -> ?event:string -> data:string -> unit -> string
  (** [frame ?id ?event ~data ()] is the bytes of one SSE event. [id] is emitted
      only when given, so a committed-frames-only [id:] discipline lets a
      [Last-Event-ID] always name a committed position; [event] names a
      non-default event type. A [data] spanning newlines is split across one
      [data:] line per segment, which {!Reader.next} rejoins with a newline. *)

  val heartbeat : string
  (** An idle keep-alive: a single SSE comment frame, invisible to [EventSource]
      and cheap for proxies to pass through. *)
end

module Reader : sig
  val next : (unit -> string option) -> Event.t option
  (** [next read_line] pulls one event from a line source, where [read_line]
      yields the next line without its terminator, or [None] at end of stream.
      Comment lines (a leading [:]) are skipped; a blank line dispatches the
      accumulated event; and [None] is returned only at end of stream with no
      pending event. A field line without a colon, and any field other than
      [id], [event], or [data], is ignored. *)
end
