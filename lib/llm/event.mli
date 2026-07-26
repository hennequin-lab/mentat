(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Live, non-terminal provider progress.

    Events are display and early-execution signals only. A {!Client.t} delivers
    them in order through its [on_event] callback while producing a response.

    {b Terminal authority.} Events carry no durable authority: the terminal
    {!Response.t} is the transcript fact. A consumer never appends an event to a
    transcript and reconciles any early action taken on a live [Tool_call]
    against the terminal response, which remains authoritative. *)

module Tool_input : sig
  (** Live partial tool-call input deltas.

      These deltas are for display and early-execution heuristics only; the
      complete {!Tool.Call.t} in a later event or the terminal response is the
      durable call value. *)

  type t
  (** The type for live partial tool-call input progress. *)

  val make :
    key:string ->
    ?call_id:string ->
    ?name:string ->
    input_delta:string ->
    unit ->
    t
  (** [make ~key ?call_id ?name ~input_delta ()] is live partial tool input for
      stream-local output [key].

      Raises [Invalid_argument] if [key] or [input_delta] is empty, or [call_id]
      or [name] is empty when present. *)

  val key : t -> string
  (** [key t] is [t]'s stream-local output key. *)

  val call_id : t -> string option
  (** [call_id t] is the associated provider call id, if known. *)

  val name : t -> string option
  (** [name t] is the associated tool name, if known. *)

  val input_delta : t -> string
  (** [input_delta t] is [t]'s live input text delta. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same payload. *)
end

module Retry : sig
  (** Live retry announcements.

      An adapter that owns a retry policy announces each upcoming attempt so a
      frontend can show the wait instead of an apparently hung call. The
      announcement is display-only: the adapter still performs the retry and the
      terminal result remains the authority on the call's outcome. *)

  type t
  (** The type for one upcoming retry announcement. *)

  val make :
    attempt:int -> limit:int -> delay:float -> reason:string -> unit -> t
  (** [make ~attempt ~limit ~delay ~reason ()] announces retry [attempt] of at
      most [limit], starting [delay] seconds after the announcement. [reason] is
      a short lowercase display phrase for the failure being retried, such as
      ["provider overloaded"].

      Raises [Invalid_argument] if [attempt < 1], [limit < attempt], [delay] is
      negative or not finite, or [reason] is empty. *)

  val attempt : t -> int
  (** [attempt t] is the 1-based ordinal of the upcoming retry. *)

  val limit : t -> int
  (** [limit t] is the largest retry ordinal the adapter's policy allows for
      this failure class. *)

  val delay : t -> float
  (** [delay t] is the wait in seconds before the attempt starts. *)

  val reason : t -> string
  (** [reason t] is the short display phrase for the failure being retried. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same payload. *)
end

type t = private
  | Text_delta of string
  | Reasoning_summary_delta of string
  | Tool_input_delta of Tool_input.t
  | Tool_call of Tool.Call.t
  | Usage of Usage.t
  | Retry of Retry.t
      (** The type for live non-terminal provider events.

          String deltas are non-empty. *)

val text_delta : string -> t
(** [text_delta s] is visible assistant text delta [s].

    Raises [Invalid_argument] if [s] is empty. *)

val reasoning_summary_delta : string -> t
(** [reasoning_summary_delta s] is reasoning-summary delta [s].

    Raises [Invalid_argument] if [s] is empty. *)

val tool_input_delta : Tool_input.t -> t
(** [tool_input_delta delta] is live partial tool input [delta]. *)

val tool_call : Tool.Call.t -> t
(** [tool_call call] is a live complete tool call.

    It supports early execution for hosts that can do so safely and reconcile
    with the terminal response, which remains the durable transcript authority.
*)

val usage : Usage.t -> t
(** [usage usage] is a live usage snapshot. *)

val retry : Retry.t -> t
(** [retry retry] announces an upcoming provider-call retry. *)
