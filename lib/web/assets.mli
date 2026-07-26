(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The embedded browser assets served under [/static].

    The frontend script and stylesheet are shipped as external files (referenced
    by {!Page} through a [<link>] and a [<script src>]), never inlined, so the
    strict CSP ([script-src 'self'], no inline) holds. They carry no build
    dependency of their own: each is a string constant compiled into the binary,
    which keeps the daemon self-contained in the same posture as its static
    linking. The script is progressive enhancement only — every command works as
    a plain form POST without it — layering the live event stream, scroll
    anchoring, and composer keys on top. *)

val get : string -> (string * string) option
(** [get file] is [Some (media_type, body)] for a served static [file] (the leaf
    name under [/static], e.g. ["app.js"]), or [None] for an unknown file. The
    media type carries [charset=utf-8]. *)
