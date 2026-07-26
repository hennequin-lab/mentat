(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Decoded tool calls.

    A call binds one complete model call and its decoded input to a live
    boundary: the exact source call, the codec-canonical input, the cached
    permission requests computed from that input, and the callback closing over
    it. The same abstraction serves both boundaries a staged tool has — the
    {!Stage.Prepare} stage and the {!Stage.Run} stage — so permission facts
    always derive from the exact typed value the next callback consumes.

    Name dispatch — mapping a name to a tool and detecting unknown or duplicate
    names — belongs to the agent's catalog, not here. This module only guards
    the definition it is handed against the call it decodes, so the
    {!Decode_error.t} it mints is either a name mismatch between that definition
    and the call, or invalid input. {!decode} decodes provider JSON once; {!run}
    invokes exactly the current callback with a caller-supplied cancellation
    source; {!resume} rebuilds a run-stage call from a durable {!Prepared.t}
    after a process-exiting permission wait, revalidating it. *)

type t
(** The type for a decoded call bound to one hidden typed value.

    The decoded value and the tool's callbacks are hidden: an observer can read
    the exact source call, tool declaration, canonical input, stage, and cached
    requests, but cannot alter the value between permission planning and
    execution. *)

(** {1:outcomes Outcomes} *)

(** The type for the outcome of running a call. *)
type outcome =
  | Finished of Output.t Result.t
      (** The callback returned a terminal result, with output erased. *)
  | Prepared of Prepared.t
      (** A {!Stage.Prepare}-stage callback produced a durable plan. *)

(** {1:errors Errors} *)

module Decode_error : sig
  (** Failures binding a model call to an executable definition. *)

  type t =
    | Name_mismatch of { declaration : string; call : string }
        (** [Name_mismatch { declaration; call }] means the model invoked
            [call], but this executable definition declares [declaration]. *)
    | Invalid_input of { tool : string; diagnostic : string }
        (** [Invalid_input { tool; diagnostic }] means [tool]'s input contract
            rejected the provider JSON. [diagnostic] is a human-readable decoder
            message, not a stable format. *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] formats {!message} on [ppf]. *)
end

module Resume_error : sig
  (** Failures revalidating a prepared value before it authorizes execution.

      Every case rejects resume before any callback runs, so drift never reaches
      the run callback. *)

  type t =
    | Not_staged  (** the call is ordinary, or already a run-stage call *)
    | Tool_mismatch  (** the prepared value belongs to a different tool *)
    | Input_mismatch
        (** the prepared value was prepared from different provider input *)
    | Invalid_prepared of string
        (** the prepared payload does not decode with the tool's codec *)
    | Prepared_drift  (** the prepared payload decodes but is not canonical *)
    | Permission_drift
        (** the final requests recomputed from the prepared plan changed *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] formats {!message} on [ppf]. *)
end

(** {1:decoding Decoding} *)

val decode :
  Definition.t -> Mentat_llm.Tool.Call.t -> (t, Decode_error.t) result
(** [decode definition source] checks that [source]'s name matches [definition],
    decodes its input with the definition's input contract, and binds it to a
    live boundary, computing this boundary's permission requests once. The exact
    [source] is retained unchanged.

    An ordinary tool yields a {!Stage.Direct} call; a staged tool yields a
    {!Stage.Prepare} call. Returns [Error (Name_mismatch _)] if the model call
    names another tool, or [Error (Invalid_input _)] if its input is rejected. A
    permission planner that raises propagates the exception. Raises
    [Invalid_argument] if the tool's input codec cannot re-encode the value it
    just decoded (needed for {!input}) — a construction defect in the tool. *)

(** {1:queries Queries} *)

val name : t -> string
(** [name t] is the invoked tool's name. *)

val source : t -> Mentat_llm.Tool.Call.t
(** [source t] is the exact model call passed to {!decode}, including its
    original input and optional provider signature. *)

val input : t -> Jsont.json
(** [input t] is the canonical re-encoding of the decoded provider input.

    It is the single owner of the shape the session stores as a claim's input:
    the same canonicalization {!Prepared.t} applies internally. A staged call
    keeps this provider input across resume. *)

val stage : t -> Stage.t
(** [stage t] is the boundary [t] sits at: {!Stage.Direct}, {!Stage.Prepare}, or
    {!Stage.Run}. *)

val permissions : t -> Mentat_permission.Request.t list
(** [permissions t] is [t]'s cached permission requests: the requests guarding
    an ordinary or resumed call, or the preliminary requests guarding a prepare
    call. It is a pure observer and never re-invokes an author function. *)

(** {1:running Running and resuming} *)

val run : t -> cancelled:(unit -> bool) -> outcome
(** [run t ~cancelled] invokes [t]'s current callback and erases a terminal
    typed output through the tool's encoder. [cancelled] is the cooperative
    cancellation source the callback polls. A prepare call returns {!Finished}
    or {!Prepared}; a direct or resumed call returns {!Finished}. It decides no
    permissions and invokes no other boundary. A prepared plan the tool's codec
    cannot serialize settles as a {!Result.failed} result — preparation has
    already completed, so an ambiguous settlement would be dishonest — while an
    exception raised by the [run], [prepare], or [describe] callback is a
    definition defect that propagates. *)

val resume : t -> Prepared.t -> (t, Resume_error.t) result
(** [resume t prepared] rebuilds the run-stage call that executes [prepared]'s
    plan, after the agent has already re-validated the tool's declaration.

    It requires [t] to be a {!Stage.Prepare} call for the same tool and provider
    input as [prepared], reconstructs the plan from [prepared]'s payload through
    the tool's codec (never re-running preparation), and requires the final
    requests recomputed from the plan to still equal [prepared]'s. Any failure
    is a {!Resume_error.t}; on success the returned call has stage {!Stage.Run}.
*)
