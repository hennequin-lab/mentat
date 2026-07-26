(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Real-process PTY observation for terminal-host contracts.

    This helper deliberately exposes raw output rather than an emulated screen.
    Complete UI pixels belong to [test/tui]; this module is only for host
    invariants that cannot be observed through a rendered frame, such as OSC
    bytes, terminal restoration, and a repaint caused by [SIGWINCH]. *)

type t
(** A child process attached to the slave side of a pseudo-terminal. *)

type forced_cleanup = {
  sigterm_sent : bool;
      (** [true] iff sending [SIGTERM] to the child succeeded. *)
  sigkill_sent : bool;
      (** [true] iff the child remained alive through the grace deadline and
          sending [SIGKILL] succeeded. *)
  child_reaped : bool;
      (** [true] iff cleanup consumed the child's status with [waitpid]. *)
  resources_closed : bool;
      (** [true] iff the PTY master descriptor is closed after reaping. *)
}
(** The observable result of fallback cleanup after the observer callback raises
    or returns without closing the child.

    This record is a test-only seam for process facts that disappear when
    [waitpid] consumes the child status. It deliberately reports only whether
    the helper sent each signal, reaped the child, and closed the PTY resources;
    it does not expose general process control. *)

val with_spawn :
  ?env:string array ->
  ?cwd:string ->
  ?rows:int ->
  ?cols:int ->
  ?observe_forced_cleanup:(forced_cleanup -> unit) ->
  prog:string ->
  args:string list ->
  (t -> 'a) ->
  'a
(** [with_spawn ~prog ~args f] runs the real executable [prog] in a PTY and
    calls [f] with its master-side observer. The child is always terminated and
    reaped before the function returns or the exception from [f] is reraised.

    If [f] leaves the child open, cleanup sends [SIGTERM], waits up to a bounded
    grace deadline, escalates to [SIGKILL] when the child remains alive, reaps
    it, and closes the PTY. [observe_forced_cleanup], when supplied, receives
    those facts after cleanup and before [with_spawn] returns or reraises. It is
    intended only for black-box verification of the exceptional path. *)

val raw : t -> string
(** [raw t] is every byte emitted by the child so far. *)

val wait_raw : t -> (string -> bool) -> unit
(** [wait_raw t predicate] pumps the PTY until [predicate (raw t)] holds.

    Raises [Failure] with an escaped output tail if the child exits or the
    bounded deadline expires first. *)

val wait_quiet : t -> unit
(** [wait_quiet t] pumps until the child has emitted no bytes for a short quiet
    interval. It establishes an idle baseline before an externally triggered
    event such as a resize. *)

val send : t -> string -> unit
(** [send t bytes] writes all [bytes] to the child's terminal input. *)

val resize : t -> rows:int -> cols:int -> unit
(** [resize t ~rows ~cols] changes the PTY dimensions. On POSIX this also
    delivers [SIGWINCH] to the child process group. *)

val quit : t -> unit
(** [quit t] drives the TUI's documented double-Ctrl+C exit chord and waits for
    the process to exit successfully. A nonzero or signalled exit fails with an
    escaped raw-output tail. *)
