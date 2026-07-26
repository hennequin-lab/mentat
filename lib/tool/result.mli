(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Terminal tool results.

    A result records the single terminal state of a tool run and optionally
    carries typed output. Output stays typed until a call maps it through the
    tool's {!Output.encoder}; {!jsont} then stores the erased result, and
    {!to_model} lowers it to the model-visible transcript answer.

    Represent expected tool-domain failures with {!failed} and cooperative stops
    with {!interrupted}. Use exceptions only for programming errors or failures
    the engine intentionally handles outside the result protocol — an unexpected
    callback exception settles the call as ambiguous, not as a result. *)

(** {1:results Results} *)

type failure =
  [ `Invalid_input
  | `Permission_denied
  | `Not_found
  | `Stale
  | `Unavailable
  | `Timed_out
  | `Failed ]
(** The type for stable categories of expected tool-domain failures.

    These categories are intentionally broad: put human-facing detail in the
    failure message and machine-readable host detail in failure metadata.

    [`Invalid_input] here is a callback-domain failure after dispatch succeeded;
    provider JSON that fails to decode is {!Mentat_tool.Call.Decode_error}
    instead. *)

(** The type for a result's terminal status, without its output. *)
type status =
  | Completed  (** The tool completed successfully, always with output. *)
  | Failed of { kind : failure; message : string; metadata : Jsont.json option }
      (** An expected domain failure. [message] is non-empty human-readable
          detail; [metadata], when present, is already-encoded host data. *)
  | Interrupted of { reason : string; cancelled : bool }
      (** A cooperative stop before completion. [reason] is non-empty;
          [cancelled] is [true] iff the stop answered a cancellation request. *)

type 'a t
(** The type for one terminal tool result carrying typed output ['a].

    A completed result always carries output; a failed or interrupted result may
    carry partial output. These invariants are enforced by construction — no
    value represents "completed without output". *)

val completed : output:'a -> unit -> 'a t
(** [completed ~output ()] is a successful result carrying [output]. *)

val failed : ?output:'a -> ?metadata:Jsont.json -> failure -> string -> 'a t
(** [failed ?output ?metadata kind message] is a failed result.

    [output], when present, is partial typed output still useful to the model.
    [metadata], when present, is already-encoded host data. [message] is
    non-empty detail carried in the result's {!status}.

    Raises [Invalid_argument] if [message] is empty. *)

val interrupted : ?output:'a -> reason:string -> cancelled:bool -> unit -> 'a t
(** [interrupted ?output ~reason ~cancelled ()] is an interrupted result.

    [output], when present, is partial typed output produced before the stop.
    Use [cancelled:false] for host stops that are not caller cancellation.

    Raises [Invalid_argument] if [reason] is empty. *)

val cancelled : ?output:'a -> unit -> 'a t
(** [cancelled ?output ()] is a tool call stopped by caller cancellation:
    {!interrupted} with reason ["tool call cancelled"] and [cancelled:true].
    [output], when present, is partial typed output produced before the stop. *)

(** {1:queries Queries} *)

val status : _ t -> status
(** [status t] is [t]'s terminal status, without output. *)

val output : 'a t -> 'a option
(** [output t] is [Some output] if [t] carries typed output and [None]
    otherwise. A {!type-status.Completed} result always answers [Some]. *)

(** {1:transforming Transforming and erasing} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f t] applies [f] to [t]'s typed output, preserving its status. A call
    uses it to erase a typed callback output through the tool's
    {!Output.encoder}. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal eq a b] is [true] iff [a] and [b] have the same status and their
    outputs are equal under [eq]. Failure metadata is compared as JSON values.
*)

val jsont : 'a Jsont.t -> 'a t Jsont.t
(** [jsont value] maps a result carrying output described by [value] to and from
    durable JSON.

    This is the known-result codec the session stores. Each status is a distinct
    case object, so a result carrying members from another status is a decode
    error. *)

val to_model :
  call:Mentat_llm.Tool.Call.t -> Output.t t -> Mentat_llm.Tool.Result.t
(** [to_model ~call t] lowers an erased result to the model-visible transcript
    answer for the pending [call] it responds to.

    A {!type-status.Completed} result becomes its output text; a
    {!type-status.Failed} or {!type-status.Interrupted} result becomes an error
    answer carrying the message and any partial output text. The lowering is
    pure and deterministic.

    {b Note (durable contract).} The session stores the erased result through
    {!jsont} and derives the transcript answer through this function at replay,
    so its output is part of the durable contract: changing it changes replayed
    history and requires a session-document version bump. *)
