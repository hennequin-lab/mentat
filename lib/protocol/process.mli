(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** A live view of a session's background processes, for the side pane and the
    between-turns reminder.

    A {!View.t} is a session-scoped projection of one background process
    (started by the [shell] tool with [background:true]), derived on demand from
    the driver's ephemeral registry — never persisted, never a session fact. It
    carries the model-visible handle, the human command, the liveness, and the
    process age, and nothing about the process's buffered output.

    The wire codec is deliberately absent: the read is served in-process today
    (the TUI over its own engine); serving it over the daemon transport is a
    designated daemon-campaign follow-up that adds the wire operation and codec
    then. *)

(** The liveness of a background process at read time. [Exited] and [Signaled]
    carry the exit code and signal number; [Signaled] is a child that exited on
    a signal from elsewhere, distinct from [Terminated], the process we killed.
*)
module Status : sig
  type t = Running | Exited of int | Signaled of int | Terminated

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a status for diagnostics. *)

  val equal : t -> t -> bool
end

module View : sig
  type t
  (** One background process. Invariant: [age_ms] is non-negative. *)

  val make :
    handle:string -> command:string -> status:Status.t -> age_ms:int -> t
  (** [make ~handle ~command ~status ~age_ms] is the view of one process.

      Raises [Invalid_argument] if [age_ms] is negative. *)

  val handle : t -> string
  (** [handle t] is the [bg_N] handle the model polls and kills with. *)

  val command : t -> string
  (** [command t] is the human command, for display (may be elided). *)

  val status : t -> Status.t
  (** [status t] is the process liveness. *)

  val age_ms : t -> int
  (** [age_ms t] is the process age in milliseconds, non-negative. *)

  val equal : t -> t -> bool
end
