(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** One bounded, supervised child process, run over Eio.

    {!run} spawns a single child, drains its stdout and stderr into bounded
    {!Capture} state, and resolves to exactly one {!outcome}. It is Eio-native
    and leader-only: the child is owned by an internal switch scoped to the
    call, and exit, timeout, cooperative stop, and output limit linearize
    through a single promise — exactly one terminal cause wins (a single
    linearization point). On any non-exit cause the {e direct child only} is
    terminated (SIGTERM, a bounded grace, then SIGKILL) and reaped under
    [Eio.Cancel.protect] (leader-only reap); descendants may survive. Parent
    cancellation propagates as cancellation after Eio's own switch-release
    kill-and-reap — it is never translated into a termination.

    Once the child is spawned, every terminal cause is a {!termination} carrying
    the captured bytes, never a launch error that erases them (bytes retained on
    any cause). Only a pre-spawn launch failure escapes as {!exception:Launch}.

    This library supervises exactly one child to one bounded outcome. It is not
    a process manager, a supervision tree, or a shell: sequencing, restart
    policy, and job control belong to the caller. *)

module Capture : sig
  (** Bounded output capture for one drained stream.

      A {!t} accumulates the bytes one drain fiber reads from a child's stdout
      or stderr under one {!policy}. The state is confined to a single [run]
      call; only the structural {!parts} value crosses the API, so the boundary
      decides completeness and renders any elision marker rather than the
      capture folding it into the child's bytes. *)

  (** One stream's capture policy.

      [All] keeps every byte. [Limit n] keeps at most [n] bytes and reports the
      first excess — the caller terminates the child. [Head_tail] keeps the
      first [head] and last [tail] bytes, dropping the middle without ever
      stopping the child. *)
  type policy = All | Limit of int | Head_tail of { head : int; tail : int }

  type t

  val create : policy -> t
  (** [create policy] is a fresh capture state.

      Raises [Invalid_argument] if a [Limit] or [Head_tail] bound is negative.
  *)

  val add : t -> string -> [ `Ok | `Exceeded of int ]
  (** [add t chunk] appends [chunk]. [`Exceeded limit] reports that a [Limit]
      policy overflowed; bytes up to [limit] were kept. [All] and [Head_tail]
      never exceed. *)

  (** The captured bytes, structurally. [Whole] is the single accumulated buffer
      ([All] and [Limit]); [Split] is the head and tail kept by [Head_tail] with
      the count of bytes dropped between them ([omitted] may be [0]). The
      boundary combines this with the drain's completeness to decide whether the
      stream is complete or truncated. *)
  type parts =
    | Whole of string
    | Split of { head : string; omitted : int; tail : string }

  val parts : t -> parts
  (** [parts t] is the captured bytes. It never embeds an elision marker — the
      marker is a rendering concern of the boundary's captured value. *)
end

(** The winning terminal cause of a supervised run. *)
type termination =
  | Exited of Eio.Process.exit_status
      (** the child exited on its own; both streams are drained to EOF unless a
          surviving descendant held a pipe open past the bounded grace, in which
          case that stream is reported not complete *)
  | Timed_out  (** the timeout won the race *)
  | Stopped  (** the cooperative stop predicate reported true *)
  | Output_limit of { stream : [ `Stdout | `Stderr ]; limit : int }
      (** a [Limit] stream exceeded its bound; the child was terminated *)
  | Supervision_failed of Eio.Exn.err
      (** a drain read or the stdin feeder failed with an unexpected IO error
          (not the child closing stdin); the child was terminated. The captured
          bytes so far are retained. *)

type outcome = {
  termination : termination;
  stdout : Capture.parts;
  stdout_complete : bool;
      (** [true] iff the stdout drain reached clean EOF; [false] on an exceeded
          limit, a supervision failure, or a drain still reading when the
          bounded grace expired — the boundary marks such a stream truncated *)
  stderr : Capture.parts;
  stderr_complete : bool;
  duration : Mtime.Span.t;
}

exception Launch of exn
(** Raised when the launch sequence itself — opening the null stdin, creating
    the pipes, or spawning — fails, carrying the underlying failure. Raised
    before any supervision starts; cancellation is never wrapped. *)

val terminate :
  mono:_ Eio.Time.Mono.t -> ?grace:float -> _ Eio.Process.t -> unit
(** [terminate ~mono ?grace child] signals [child] (SIGTERM, a bounded [grace]
    in seconds, then SIGKILL) and reaps it — the {e direct child only},
    leader-only — under [Eio.Cancel.protect] so a pending parent cancellation
    cannot interrupt the bounded cleanup. It is the cooperative-stop sequence
    {!run} performs on a preemptive cause, exposed for a supervised session's
    explicit kill. [grace] defaults to the same bound {!run} uses. *)

val run :
  proc_mgr:_ Eio.Process.mgr ->
  mono:_ Eio.Time.Mono.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cwd:Eio.Fs.dir_ty Eio.Path.t ->
  env:string array ->
  executable:string ->
  ?stdin:Eio.Flow.source_ty Eio.Std.r ->
  capture:Capture.policy ->
  timeout:Eio.Time.Timeout.t ->
  cancelled:(unit -> bool) option ->
  ?grace:float ->
  ?drain_grace:float ->
  ?poll:float ->
  string list ->
  outcome
(** [run ~proc_mgr ~mono ~fs ~cwd ~env ~executable ?stdin ~capture ~timeout
     ~cancelled argv] spawns [argv] and supervises it to one {!outcome}.

    [cwd] must be an opened directory capability. Omitted [stdin] is an explicit
    [/dev/null], never the parent's. A pending output-limit excess is observed
    before a concurrent exit is finalized, so an over-limit child that exits at
    once is deterministically {!Output_limit}, never a silently truncated
    {!Exited} (output-limit before exit). After the terminal cause, draining is
    given a short bounded grace — an orphaned descendant that keeps the pipes
    open cannot stall the return, and its stream is reported not complete rather
    than silently short.

    The timing bounds are all in seconds: [grace] is how long a signalled child
    may take to die before SIGKILL; [drain_grace] is how long the drains may
    keep reading after the terminal cause resolved; [poll] is the
    cooperative-stop poll period. Each defaults to a small bound. *)
