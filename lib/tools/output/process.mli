(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Compact completion facts for command-like tools.

    Standard output, standard error, and supervision diagnostics remain solely
    in {!Mentat_tool.Output.text} or the terminal result error. *)

(** A captured process stream. *)
type stream = Stdout  (** Standard output. *) | Stderr  (** Standard error. *)

(** The way a process or evaluation pipeline terminated. *)
type termination =
  | Exited of int  (** Normal exit with its status code. *)
  | Signaled of int
      (** Termination by the platform signal identifier reported by the process
          supervisor. *)
  | Timed_out  (** The configured deadline elapsed. *)
  | Stopped  (** Cooperative cancellation stopped the process. *)
  | Output_limit of { stream : stream; limit : int }
      (** A captured stream exceeded its byte limit. *)
  | Supervision_failed  (** The command supervisor itself failed. *)

type t
(** The type for compact durable process completion facts. Durations and output
    limits are non-negative. *)

val make : termination:termination -> duration_ms:int -> t
(** [make ~termination ~duration_ms] is a process completion.

    Raises [Invalid_argument] if [duration_ms], an exit code, or an output limit
    is negative. Signal identifiers are preserved exactly because their signed
    representation is platform-dependent. *)

val termination : t -> termination
(** [termination t] is how the process completed. *)

val duration_ms : t -> int
(** [duration_ms t] is the observed duration in milliseconds. *)

val jsont : t Jsont.t
(** [jsont] maps process facts to their closed version-1 JSON shape. *)
