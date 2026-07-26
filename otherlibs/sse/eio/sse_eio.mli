(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The Eio byte-stream adapter for {!Sse}.

    {!Reader} decodes SSE events incrementally from an {!Eio.Flow.source} — the
    body is parsed one 4 KiB window at a time and never buffered whole — so a
    long-lived model or feed stream is consumed as its events arrive. This is
    the effectful companion to the pure {!Sse} core, in the shape of
    [oauth2]/[oauth2_eio]: it supplies the byte plumbing the core deliberately
    omits. *)

module Reader : sig
  type t
  (** An incremental SSE reader positioned in a byte stream. *)

  val of_source : _ Eio.Flow.source -> t
  (** [of_source source] reads SSE events from [source]. *)

  val next : t -> Sse.Event.t option
  (** [next t] is the next event, or [None] at end of stream. Read failures on
      the underlying source are not caught — they raise through. *)
end
