(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Structured sandbox errors.

    Each constructor is a distinct recoverable fact carrying its own fields, so
    a consumer branches on the constructor directly — never by parsing
    {!message} or {!to_json}. Lowering returns these in [result] values; a
    sealed sandbox reports refusals as {!Evidence.refused}.

    Two constructors are minted by the effect twin, not by this library:
    {!Unavailable} is the opaque outcome of the twin's platform probe, injected
    into {!Mentat_sandbox.confined}; {!Stale_policy} is the twin's
    obligation-discharge failure, spoken here because {!Evidence.Refused}
    carries it and the twin's evidence must stay in this vocabulary.

    {b Copy states confinement facts, never product remedies.} {!message} names
    only what the sandbox observed — never a CLI flag, verb, or mode name. A
    read-only seal's {!Escalation_denied}, for instance, states {e that} a
    read-only sandbox admits no escalation, never
    {e "rerun with [--sandbox workspace-write]"}; the recovery hint that names a
    flag is a consumer decoration added at the frontend that renders the
    refusal, exactly as the run-start gate's {!Requirement.Rejection} is
    decorated with a mode and a command downstream. The library owns the
    confinement fact; the frontend owns the product remedy. Keeping the flag
    hint out of these strings is deliberate: it is what lets one message serve
    every frontend without leaking a specific product's vocabulary. *)

type t =
  | Unavailable of string
      (** enforcement is unavailable on this host; the payload is the twin's
          probe diagnostic. Fatal for a confined run — fail closed. *)
  | Cwd_outside_scope of Lpath.Abs.t
      (** the working directory is outside the confined readable roots.
          Retryable with a different working directory. *)
  | Escalation_denied
      (** the sealed posture promises no mutation: a read-only sandbox admits no
          approval-shaped exception. *)
  | Escalation_irrelevant
      (** escalation asks for what is already true on an unconfined or
          declared-external route. *)
  | Empty_program
      (** the program of a lowered argv is the empty string; OS [execve] cannot
          launch it. *)
  | Nul_in_argv of { index : int }
      (** token [index] of the lowered argv contains a NUL byte, which OS argv
          cannot represent. Index [0] is the program. *)
  | Stale_policy of { path : Lpath.Abs.t }
      (** the resolved root or protected path [path] disappeared or changed kind
          after sealing. Transient — re-resolve and re-seal. Minted by the
          effect twin when discharging an obligation fails. *)

val message : t -> string
(** [message t] is the human-readable diagnostic. It is not a stable matching
    key; branch on the constructor instead. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same error, fields included.
*)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats {!message}. *)

val to_json : t -> Jsont.json
(** [to_json t] is the canonical JSON projection, built from the constructor: an
    object with ["kind"] (["unavailable"], ["cwd_outside_scope"],
    ["escalation_denied"], ["escalation_irrelevant"], ["empty_program"],
    ["nul_in_argv"], or ["stale_policy"]) and ["message"], plus the
    constructor's structured fields — ["path"] for {!Cwd_outside_scope} and
    {!Stale_policy}, ["index"] for {!Nul_in_argv}. It is encode-only — one
    spelling for every product JSON surface so the class cannot drift between
    contracts. *)
