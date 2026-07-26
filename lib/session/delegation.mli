(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Subagent delegation edges.

    A delegation is the single immutable edge recording that the owning session
    spawned a child. It carries the child session id, source turn and call, the
    task, an optional short {!description} label, and an optional {!Role.t}
    selecting the child's specialized subagent prompt. The role also fixes the
    child's authority — selected at attachment rather than represented by an
    access field here: a roleless (generic) delegate is a write-capable peer of
    a top-level session (the same catalog, sealed workspace capability, and
    permission policy), while an {!Role.Explore}, {!Role.Review}, or
    {!Role.Verify} role is a read-only specialist. Workspace capability identity
    is a separate host seam and is not described by this value. It does {e not}
    store a parent id: the owning journal is the parent. There is no update or
    settlement fact — the edge never changes. *)

type session_id = Id.t
(** [session_id] is the enclosing session {!Id.t}. Named here because {!Id}
    below shadows the module with the delegation's own id type. *)

(** {1:ids Identifiers} *)

module Id : sig
  type t
  (** The type for a delegation identifier. It is content-derived from the
      source turn and call ({!id}); a caller cannot supply an unrelated id.

      Invariant: an identifier's stable textual form is non-empty. *)

  val of_string : string -> t
  (** [of_string s] is [s] as a delegation id.

      Raises [Invalid_argument] if [s] is empty. *)

  val to_string : t -> string
  (** [to_string id] is [id]'s stable string representation. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same delegation id. *)

  val compare : t -> t -> int
  (** [compare a b] orders ids by their stable string representations,
      compatibly with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an id for diagnostics. The output is not stable storage
      syntax. *)

  val jsont : t Jsont.t
  (** [jsont] maps delegation ids to JSON strings, validating the non-empty
      invariant of {!of_string} on decode. *)
end

(** {1:roles Roles} *)

module Role : sig
  (** A delegated child's role: the specialized subagent prompt it runs under.

      The set is closed. A spawn may omit the role, leaving the child a generic
      delegate with no role prompt; a present role selects exactly one
      specialized subagent prompt, re-applied at the child's every turn. *)

  type t =
    | Explore  (** A fast, read-only search specialist. *)
    | Review  (** A read-only reviewer that reports findings by severity. *)
    | Verify  (** An adversarial verifier that runs checks to break a change. *)

  val to_string : t -> string
  (** [to_string t] is [t]'s stable lowercase tag: [explore], [review], or
      [verify]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same role. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a role for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps roles to their string tags and rejects any unknown tag on
      decode, so an invalid role is a decode failure rather than a silent
      default. *)
end

(** {1:delegations Delegations} *)

type t
(** The type for an immutable delegation edge. Invariant: [source_call] and
    [task] are non-empty, and [description], when present, is non-empty. *)

val make :
  child:session_id ->
  source_turn:Turn.Id.t ->
  source_call:string ->
  ?description:string ->
  ?role:Role.t ->
  task:Mentat_llm.Content.t list ->
  unit ->
  t
(** [make ~child ~source_turn ~source_call ?description ?role ~task ()] is a
    delegation edge. Its id is derived from [source_turn] and [source_call],
    independent of [description], [role], and [task].

    [description] is an optional short human-authored label naming the child's
    mission (a few words), distinct from the full [task]. A [description] that
    is empty or whitespace-only is treated as absent. It is optional and
    backward compatible: an edge recorded without one decodes to [None].

    [role], when present, is the child's specialized subagent {!Role.t}; absent
    means a generic delegate. Like [description] it is optional and backward
    compatible: an edge recorded without one decodes to [None].

    [task] must be text-only: a media block is rejected, since a delegation
    task's media does not join the media relocation and versioning passes.

    Raises [Invalid_argument] if [source_call] or [task] is empty, or [task]
    carries media content. *)

val id : t -> Id.t
(** [id t] is [t]'s content-derived delegation id:
    [H("mentat.session.delegation.v1", source_turn, source_call)]. *)

val child : t -> session_id
(** [child t] is the spawned child session id. *)

val source_turn : t -> Turn.Id.t
(** [source_turn t] is the parent turn that spawned [t]. *)

val source_call : t -> string
(** [source_call t] is the model tool call id that spawned [t]. *)

val task : t -> Mentat_llm.Content.t list
(** [task t] is the child's task content. *)

val description : t -> string option
(** [description t] is [t]'s optional short label, or [None] when the edge was
    recorded without one. *)

val role : t -> Role.t option
(** [role t] is [t]'s optional delegated {!Role.t}, or [None] when the edge was
    recorded without one (a generic delegate). *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same edge. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a delegation for diagnostics. The output is not stable storage
    syntax. *)

val jsont : t Jsont.t
(** [jsont] maps delegations to JSON values, validating local invariants on
    decode. *)
