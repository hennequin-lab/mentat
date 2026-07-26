(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Checked model-visible transcripts.

    A transcript is the ordered message sequence replayable to a provider. It
    owns only model-visible ordering grammar:

    - tool results answer the immediately preceding assistant message with tool
      calls;
    - each result names one pending call;
    - each pending call is answered at most once;
    - ordinary messages cannot appear while calls are still pending.

    Durable item ids, summaries, compaction, sessions, permissions, and storage
    live outside this module.

    Build transcripts incrementally with {!add}, for example
    [Transcript.add (Message.user_text s) t], or load persisted messages with
    {!of_list}. Request construction additionally requires the transcript to be
    non-empty and {!is_ready}. *)

type t
(** The type for checked transcripts. *)

module Error : sig
  (** Transcript grammar violations. *)

  type t =
    | Tool_result_without_call of Tool.Result.t
        (** A tool-result message appeared without pending assistant tool calls.
        *)
    | Unknown_tool_result of { call_id : string }
        (** A tool result answered a call id that is not pending. *)
    | Duplicate_tool_result of { call_id : string }
        (** A tool result answered a call id that was already answered. *)
    | Tool_result_name_mismatch of {
        call_id : string;
        expected : string;
        actual : string;
      }  (** A tool result used the pending call id with the wrong tool name. *)
    | Duplicate_tool_call of { call_id : string }
        (** An assistant message contained duplicate tool-call ids. *)
    | Pending_tool_results of Tool.Call.t list
        (** Ordinary messages or requests were attempted while tool calls still
            need results. *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] formats [e] for diagnostics. *)
end

type state =
  | Ready
  | Awaiting_tool_results of Tool.Call.t * Tool.Call.t list
      (** The type for request readiness.

          [Awaiting_tool_results (call, calls)] means [call :: calls] must be
          answered before an ordinary message or request may be added. *)

val empty : t
(** [empty] is the empty transcript. *)

val is_empty : t -> bool
(** [is_empty t] is [true] iff [t] contains no messages. *)

val length : t -> int
(** [length t] is the number of messages in [t]. *)

val of_list : Message.t list -> (t, Error.t) result
(** [of_list messages] checks [messages] as a transcript.

    The empty list is valid here; {!Request.make} separately rejects empty
    transcripts. *)

val of_list_exn : Message.t list -> t
(** [of_list_exn messages] is [of_list messages].

    Raises [Invalid_argument] if [messages] is not a valid transcript. *)

val add : Message.t -> t -> (t, Error.t) result
(** [add msg t] appends [msg] to [t] if the result remains a valid transcript.

    Tool-result messages may be appended while calls are still pending (see
    {!pending}). Any other message must wait until all pending calls are
    answered. Assistant messages that contain tool calls start a new pending
    set; call ids must be unique within that assistant message. *)

val add_exn : Message.t -> t -> t
(** [add_exn msg t] is [add msg t].

    Raises [Invalid_argument] if [msg] cannot be appended to [t]. *)

val add_response : Response.t -> t -> (t, Error.t) result
(** [add_response response t] appends {!Response.message}[ response] to [t]. *)

val messages : t -> Message.t list
(** [messages t] is [t]'s ordered message list, in provider order. *)

val pending : t -> Tool.Call.t list
(** [pending t] is the unanswered tool calls in their original assistant output
    order. *)

val state : t -> state
(** [state t] is [Ready] iff {!pending}[ t] is empty. *)

val is_ready : t -> bool
(** [is_ready t] is [true] iff [t] can be used to construct a request. *)

val require_ready : t -> (unit, Error.t) result
(** [require_ready t] is [Ok ()] if [t] is request-ready and
    [Error (Error.Pending_tool_results calls)] otherwise. *)

val last_assistant : t -> Message.Assistant.t option
(** [last_assistant t] is the most recent assistant message in [t], if any.

    It is not filtered on emptiness: an assistant turn that only requested tool
    calls, or {!Message.Assistant.empty}, is still returned. *)

val last_assistant_text : ?sep:string -> t -> string option
(** [last_assistant_text ?sep t] is the concatenated visible text of the most
    recent assistant message that produced any visible text, joining parts with
    [sep] (default [""]). Assistant turns that emitted only tool calls, or
    {!Message.Assistant.empty}, are skipped.

    This is the "final answer" projection; without it, consumers re-walk
    {!messages} in reverse to find the last non-empty assistant text. The joined
    text is returned untrimmed — the caller trims if it wants, matching
    {!Response.text}. *)

val jsont : t Jsont.t
(** [jsont] maps transcripts to and from JSON objects.

    Decoding checks message ordering and tool-call/result grammar with
    {!of_list}. *)
