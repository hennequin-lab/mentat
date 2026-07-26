(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The drain-time dune build-health notice producer.

    This is [drain_notices]' one v1 source. It observes the workspace's Dune RPC
    — registry-first, {b never spawning dune} — at turn preparation and lowers
    build-health {e transitions} into {!Mentat_workspace.Notice.t}. It reads
    diagnostics from an already-running [dune build --watch] (the standard OCaml
    dev loop); it starts no build of its own. When no watch is registered the
    probe is a single cheap registry read and no notice is produced — build
    diagnostics being unavailable is not an error, so the channel stays silent
    rather than fabricating one.

    Notices are deduplicated in the producer because the port has no queue:
    {!drain} returns the empty list unless the verdict changed since the last
    drain. The last {e concrete} verdict ([Clean]/[Failing]) is the baseline;
    [Disconnected]/[Unknown] are lost visibility and neither emit nor move it,
    so a reconnect to the same failure does not re-notice. *)

type t
(** A stateful producer: a workspace Dune RPC instance plus the last-observed
    and last-concrete build-health verdicts. *)

val make :
  stdenv:Eio_unix.Stdenv.base ->
  ?env:(string -> string option) ->
  workspace:Mentat_workspace.t ->
  unit ->
  t
(** [make ~stdenv ~workspace ()] is a producer over [workspace]'s Dune RPC.
    [stdenv]'s filesystem and [env] (default {!Sys.getenv_opt}) drive XDG
    registry discovery, its network connects to a discovered endpoint, and its
    clock bounds each diagnostics request. Construction performs no IO; the
    first {!drain} polls. *)

val drain : t -> Mentat_workspace.Notice.t list
(** [drain t] polls build health once and is the transition notice, if any:

    - a fresh or changed failure is one [Error] notice whose body is the head
      Dune diagnostic ([path:line:col: message]);
    - a recovery from a known failure is one [Info] notice;
    - an unchanged verdict, a clean baseline, or lost visibility
      ([Disconnected]/[Unknown]) is [[]].

    The single 0.5s-bounded probe never spawns dune. *)

val health : t -> Mentat_ocaml_dune_rpc.Instance.Health.t
(** [health t] is the raw verdict of the last {!drain} ([Disconnected] before
    the first). Exposed for a frontend at-a-glance build indicator that must not
    run a second probe — one endpoint, one poll per turn. *)
