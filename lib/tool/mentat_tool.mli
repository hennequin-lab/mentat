(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The typed, effect-neutral waist between provider JSON and a live OCaml
    callback.

    A tool author uses this library to turn a typed OCaml callback into an
    executable {!t} that the agent can decode provider JSON into, permission
    check, and run. The waist owns exactly: typed input decoding, permission
    {e requests} computed from the exact decoded value a callback consumes,
    output erasure, the terminal {!Result.t} algebra, and one durable staged
    intermediate ({!Prepared.t}) with its resume rule. It imports neither the
    session nor the engine.

    {b Authoring a tool.} There are two constructors. {!make} is the ordinary
    one-callback tool; {!make_staged} is the one case where an authorized
    observation must produce a typed plan before final permissions can be known.
    An author supplies an input contract ({!Input.t}), an output encoder
    ({!Output.encoder}), an optional permission function, and a [run] callback
    that receives a cancellation source:

    {[
    let echo =
      Mentat_tool.make ~name:"echo"
        ~description:"Echo the message back to the model." ~input:Echo.input
        ~output:Echo.encode ~permissions:Echo.requests
        ~run:(fun ~cancelled input -> Echo.run ~cancelled input)
        ()
    ]}

    A tool's identity is the model-visible {!Mentat_llm.Tool.t} its constructor
    validates and stores; {!declaration} projects it, and the agent compares it
    on resume.

    {b Where permissions come from.} A tool declares permission {e requests} —
    what it intends to touch — as a pure function of the {e decoded input value}
    its callback will consume, never an impact promise, repeatability flag, or
    write class. {!Call.decode} evaluates that function exactly once, from the
    same value later handed to [run], so the requests that guard a call and the
    value it runs on can never drift apart. Whether a request is granted is a
    policy decision made elsewhere ([mentat.permission], recorded by the
    session); this library only computes and caches the requests.

    {b What a callback may and may not do.} A callback returns exactly one
    terminal {!Result.t} — {!Result.completed}, {!Result.failed}, or
    {!Result.interrupted} — carrying typed output that the boundary erases
    through the tool's {!Output.encoder}. It may poll its [cancelled] argument
    at useful points and stop cooperatively. It may not decide permissions, emit
    progress, or return workspace evidence: tool output is model-visible text
    plus optional structured JSON, and nothing a fact owner may trust as a
    receipt.

    {b Deliberately absent, with its owner.} There is no name-keyed dispatch
    catalog — the agent owns it and hands one {!t} plus the complete model call
    to {!Call.decode}; no streaming or progress channel — an ephemeral engine
    and frontend concern; no mutation evidence, receipt, or revert —
    [mentat.edit] and [mentat.mutation]; no permission policy or answers —
    [mentat.permission] and the session; and no claims, settlement, or ambiguity
    — [mentat.session]. *)

(** {1:contracts Inputs, outputs, results} *)

module Input = Input
(** Typed JSON input contracts. *)

module Output = Output
(** Typed output encoders and erased model-visible outputs. *)

module Result = Result
(** The terminal result algebra and its durable and model-visible lowerings. *)

module Codec = Codec
(** The decode-side [Invalid_argument] to [Jsont] error bridge shared by tool
    input and output codecs. *)

(** {1:lifecycle Execution lifecycle} *)

module Stage = Stage
(** The shared [Direct | Prepare | Run] execution-stage vocabulary. *)

(** {1:tools Tool definitions} *)

type t
(** The type for an executable tool definition.

    A value is a pure, immutable definition. Creating a tool starts no work,
    requests no permissions, and touches no files; its identity is validated by
    {!Mentat_llm.Tool.make}. *)

val make :
  name:string ->
  description:string ->
  input:'input Input.t ->
  output:'output Output.encoder ->
  ?permissions:('input -> Mentat_permission.Request.t list) ->
  run:(cancelled:(unit -> bool) -> 'input -> 'output Result.t) ->
  unit ->
  t
(** [make ~name ~description ~input ~output ?permissions ~run ()] is the
    ordinary one-callback tool.

    [input] decodes provider JSON to the typed value [run] consumes and [output]
    erases the value [run] returns. [permissions] computes the permission
    requests that guard [run] from the {e same} decoded value later passed to
    it; it defaults to no requests. [run] receives a [cancelled] source to poll
    and returns exactly one terminal result; represent expected failures with
    {!Result.failed}.

    The identity is built with {!Mentat_llm.Tool.make}, which raises
    [Invalid_argument] if [name] is not a valid model tool name (a non-empty
    ASCII identifier of at most 64 characters, its first character a letter or
    ['_'] and the rest also digits or ['-']) or [description] is empty. *)

val make_staged :
  name:string ->
  description:string ->
  input:'input Input.t ->
  prepared:'prepared Jsont.t ->
  describe:('prepared -> string) ->
  output:'output Output.encoder ->
  ?prepare_permissions:('input -> Mentat_permission.Request.t list) ->
  prepare:
    (cancelled:(unit -> bool) ->
    'input ->
    [ `Finished of 'output Result.t | `Prepared of 'prepared ]) ->
  ?permissions:('prepared -> Mentat_permission.Request.t list) ->
  run:(cancelled:(unit -> bool) -> 'prepared -> 'output Result.t) ->
  unit ->
  t
(** [make_staged ...] is a tool whose final permissions can be known only after
    an authorized observation produces a typed plan.

    [prepare_permissions] guards [prepare] from the decoded input; [prepare]
    returns [`Finished result] to end without a second permission decision, or
    [`Prepared plan] to bind a typed plan serialized by [prepared] and described
    by [describe]. [describe] must depend only on the plan value, never on the
    workspace. [permissions] then computes the final requests from that plan,
    and [run] consumes the {e same} reconstructed plan after those requests are
    allowed. Both permission functions default to no requests.

    Validation is as in {!make}. A plan [prepared] cannot serialize settles the
    call as a {!Result.failed} result — preparation has already completed. An
    exception from [describe], [prepare], [run], or a permission planner is a
    definition defect that propagates, and stored-value resume drift is the
    recoverable {!Call.Resume_error.t}. *)

val declaration : t -> Mentat_llm.Tool.t
(** [declaration t] is [t]'s model-visible declaration and durable identity: the
    value sent to the model and sealed in the session's turn contract. *)

(** {1:staging Staging} *)

(** The one durable staged payload, for {!make_staged}. Only the query surface
    is exposed, so no public constructor mints one from a live plan outside a
    staged {!Call.run} and none reads the resume-only payload. A stored envelope
    decodes via {!Prepared.jsont} (the session reloads it) and becomes authority
    only by passing {!Call.resume}'s revalidation. *)
module Prepared : sig
  type t
  (** The type for a durable prepared payload. *)

  val tool : t -> string
  (** [tool t] is the name of the staged tool that produced [t]. *)

  val description : t -> string
  (** [description t] is the presentation description of [t]'s plan. *)

  val requests : t -> Mentat_permission.Request.t list
  (** [requests t] is the final permission requests [t]'s plan needs. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same tool, canonical input,
      canonical plan payload, final requests, and description. JSON member order
      is significant. *)

  val jsont : t Jsont.t
  (** [jsont] maps prepared values to and from their version-1 JSON envelope.
      The provider input and plan payload are already canonical; decoding
      rejects an unknown envelope version. *)
end

(** {1:running Calls} *)

module Call : sig
  (** Decoded tool calls: retain a complete model call, decode its input,
      inspect the declaration and permissions, run the current callback, and
      resume a staged call. *)

  type tool := t
  (** The enclosing tool definition; used only as the argument of {!decode} and
      not re-exposed as a distinct type. *)

  type t
  (** The type for a decoded call bound to one hidden typed value.

      The decoded value and callbacks are hidden: an observer reads the exact
      source call, tool declaration, canonical input, stage, and cached
      requests, but cannot alter the value between permission planning and
      execution. *)

  (** The type for the outcome of running a call. *)
  type outcome =
    | Finished of Output.t Result.t
        (** The callback returned a terminal result, with output erased. *)
    | Prepared of Prepared.t
        (** A {!Stage.Prepare}-stage callback produced a durable plan. *)

  module Decode_error : sig
    (** Failures binding a model call to an executable definition. *)

    type t =
      | Name_mismatch of { declaration : string; call : string }
          (** [Name_mismatch { declaration; call }] means the model invoked
              [call], but this executable definition declares [declaration]. *)
      | Invalid_input of { tool : string; diagnostic : string }
          (** [Invalid_input { tool; diagnostic }] means [tool]'s input contract
              rejected the provider JSON. [diagnostic] is a human-readable
              decoder message, not a stable format. *)

    val message : t -> string
    (** [message e] is a human-readable diagnostic. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf e] formats {!message} on [ppf]. *)
  end

  module Resume_error : sig
    (** Failures revalidating a prepared value before it authorizes execution.

        Every case rejects resume before any callback runs, so drift never
        reaches the run callback. *)

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

  val decode : tool -> Mentat_llm.Tool.Call.t -> (t, Decode_error.t) result
  (** [decode definition source] checks that [source]'s name matches
      [definition], decodes its input with the definition's input contract, and
      binds it to a live boundary, computing this boundary's permission requests
      once. The exact [source] is retained unchanged.

      An ordinary tool yields a {!Stage.Direct} call; a staged tool yields a
      {!Stage.Prepare} call. Returns [Error (Name_mismatch _)] if the model call
      names another tool, or [Error (Invalid_input _)] if its input is rejected.
      A permission planner that raises propagates the exception. Raises
      [Invalid_argument] if the tool's input codec cannot re-encode the value it
      just decoded (needed for {!input}) — a construction defect in the tool. *)

  val name : t -> string
  (** [name t] is the invoked tool's name. *)

  val source : t -> Mentat_llm.Tool.Call.t
  (** [source t] is the exact model call passed to {!decode}, including its
      original input and optional provider signature. *)

  val input : t -> Jsont.json
  (** [input t] is the canonical re-encoding of the decoded provider input, the
      single owner of the shape the session stores as a claim's input. A staged
      call keeps this provider input across resume. *)

  val stage : t -> Stage.t
  (** [stage t] is the boundary [t] sits at: {!Stage.Direct}, {!Stage.Prepare},
      or {!Stage.Run}. *)

  val permissions : t -> Mentat_permission.Request.t list
  (** [permissions t] is [t]'s cached permission requests: the requests guarding
      an ordinary or resumed call, or the preliminary requests guarding a
      prepare call. It is a pure observer and never re-invokes an author
      function. *)

  val run : t -> cancelled:(unit -> bool) -> outcome
  (** [run t ~cancelled] invokes [t]'s current callback and erases a terminal
      typed output through the tool's encoder. [cancelled] is the cooperative
      cancellation source the callback polls. A prepare call returns {!Finished}
      or {!Prepared}; a direct or resumed call returns {!Finished}. It decides
      no permissions and invokes no other boundary. A prepared plan the tool's
      codec cannot serialize settles as a {!Result.failed} result — preparation
      has already completed, so an ambiguous settlement would be dishonest —
      while an exception raised by the [run], [prepare], or [describe] callback
      is a definition defect that propagates. *)

  val resume : t -> Prepared.t -> (t, Resume_error.t) result
  (** [resume t prepared] rebuilds the run-stage call that executes [prepared]'s
      plan, after the agent has re-validated the tool's declaration.

      It requires [t] to be a {!Stage.Prepare} call for the same tool and
      provider input as [prepared], reconstructs the plan from [prepared]'s
      payload through the tool's codec (never re-running preparation), and
      requires the final requests recomputed from the plan to still equal
      [prepared]'s. Any failure is a {!Resume_error.t} and rejects before the
      callback runs; on success the returned call has stage {!Stage.Run}. *)
end
