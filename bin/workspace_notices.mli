(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The drain-time dune build-health notice producer.

    This is one of [drain_notices]' two v1 sources, alongside
    {!Workspace_watch}. It observes the workspace's Dune RPC — registry-first,
    {b never spawning dune} — at each drain and lowers build-health
    {e transitions} into {!Mentat_workspace.Notice.t}. It reads diagnostics from
    an already-running [dune build --watch] (the standard OCaml dev loop); it
    starts no build of its own. When no watch is registered the probe is a
    single cheap registry read and no notice is produced — build diagnostics
    being unavailable is not an error, so the channel stays silent rather than
    fabricating one.

    Notices are deduplicated in the producer because the port has no queue:
    {!drain} returns the empty list unless the verdict changed since the last
    drain, and — for a recovery — has stayed changed long enough to be believed.
    The last {e concrete} verdict ([Clean]/[Failing]) is the baseline;
    [Disconnected]/[Unknown] are lost visibility and neither emit nor move it,
    so a reconnect to the same failure does not re-notice. *)

type t
(** A stateful producer: a workspace Dune RPC instance and the build-health
    verdicts read through it. *)

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
    - a recovery from a known failure is one [Info] notice, on the first clean
      verdict read at least fifteen seconds after the clean verdict that
      preceded it with no failure in between;
    - an unchanged verdict, a clean baseline, or lost visibility
      ([Disconnected]/[Unknown]) is [[]]. Lost visibility leaves the baseline
      alone but discards a pending recovery, so a gap in observation cannot
      supply the separation that confirms one.

    Recovery is confirmed against the clock and failure is not, because dune
    clears a rebuilt file's diagnostics before the rebuild republishes them: a
    drain landing in that window reads an empty set that does not mean the build
    is fixed, and two drains a tool call apart can both land there. A false
    recovery would tell the model to stop working on a build that is still
    broken, while a late one costs almost nothing. The separation is a heuristic
    and not a guarantee — a rebuild slower than it still reads as a recovery.

    The single 0.5s-bounded probe never spawns dune. A turn's total probe cost
    scales with the tool claims it settles, since each one drains. *)

val health : t -> Mentat_ocaml_dune_rpc.Instance.Health.t
(** [health t] is the raw verdict of the last {!drain} ([Disconnected] before
    the first). Exposed for a frontend at-a-glance build indicator that must not
    run a second probe — one endpoint, one poll per drain. *)
