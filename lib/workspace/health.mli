(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The workspace tooling build-health verdict — the ambient glance's dune
    signal.

    A wire-safe projection of an OCaml dev loop's build health, owned in this
    pure library so every layer above the workspace can name it without linking
    a build-tool effect library: the protocol embeds {!jsont} on its query
    response lane, and a frontend renders the verdict directly. It carries only
    the verdict, never diagnostics.

    {b The fail-honest law.} Only {!Clean} and {!Failing} are affirmative
    verdicts a frontend renders as a status row. {!Disconnected} (no build watch
    is reachable) and {!Unknown} (a probe lost visibility, or the workspace's
    [workspace.tooling] is disabled) carry no verdict — a frontend omits the row
    rather than inventing one. A verdict is a point-in-time observation a
    frontend holds as a last observation and re-reads on demand; it is never
    persisted derived state. *)

type t =
  | Disconnected
      (** No build watch is reachable; there is nothing to report. *)
  | Clean  (** A reachable build watch reports an empty diagnostic set. *)
  | Failing of int
      (** A reachable build watch reports [n >= 1] outstanding diagnostics. *)
  | Unknown
      (** Visibility was lost — a probe timeout or failure — or the workspace's
          tooling is disabled. No verdict. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same verdict, the {!Failing}
    count included. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] for diagnostics. *)

val jsont : t Jsont.t
(** [jsont] is the JSON codec for a verdict — the one wire form the protocol's
    query response embeds. It round-trips every case, the {!Failing} count
    included. *)
