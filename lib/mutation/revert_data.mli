(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Revert vocabulary — selection, evidence, plan, and lifecycle data.

    The data half of the public [Revert] module: {!Revert} re-exports these
    submodules by alias and adds the operations that consume {!State}. The
    top-level values under {!section-internal} are library-internal seams for
    those operations; because {!Revert} aliases only the submodules, they are
    not reachable through the public facade. *)

module Id : sig
  type t
  (** The type for stable revert identifiers — a command identity supplied by
      the command/session boundary, never derived from session, scope, or
      ordinal.

      Invariant: an identifier's stable textual form is non-empty. *)

  val of_string : string -> t
  (** [of_string s] is [s] as a revert id.

      Raises [Invalid_argument] if [s] is empty. *)

  val to_string : t -> string
  (** [to_string id] is [id]'s stable string representation. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same revert id. *)

  val compare : t -> t -> int
  (** [compare a b] orders ids by their stable string representations,
      compatibly with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an id for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps revert ids to JSON strings, validating the non-empty
      invariant of {!of_string} on decode. *)
end

(** {1:selection Selection} *)

module Selection : sig
  (** The type for a canonical, non-empty selection over recorded history.
      [turns [a; b]], [turns [b; a]], and [turns [a; a; b]] construct equal
      values: the smart constructors deduplicate and sort, so equality and
      identity never depend on caller list discipline. The arms are exposed
      [private] so replay folds can match them; introduction is through the
      smart constructors and decode only. *)
  type t = private
    | All
    | Turns of Mentat_session.Turn.Id.t list
    | Changes of Change.Id.t list
    | Paths of Mentat_workspace.Path.t list

  val all : t
  (** [all] selects the whole session. *)

  val turns : Mentat_session.Turn.Id.t list -> t
  (** [turns ids] selects the changes recorded by the named turns.

      Raises [Invalid_argument] if [ids] is empty — like every other constructor
      in this library, an unusable value raises rather than returning a typed
      refusal; {!jsont} maps the raise to a decode error. *)

  val changes : Change.Id.t list -> t
  (** [changes ids] selects the named change rows.

      Raises [Invalid_argument] if [ids] is empty. *)

  val paths : Mentat_workspace.Path.t list -> t
  (** [paths ps] selects the changes that touched the named paths.

      Raises [Invalid_argument] if [ps] is empty. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same selection. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a selection for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps selections to JSON objects by a per-arm tag, rejecting
      unknown tags and non-canonical or empty payloads. *)
end

(** {1:evidence Evidence} *)

module Evidence : sig
  type t
  (** The type for a closed record of already-performed reads and
      already-resolved blobs, assembled by the engine before preparation: the
      current observed state of each candidate target path, and the bytes of
      each restore image. Passing evidence in keeps [Revert.prepare] pure — the
      planner performs no IO and is deterministic in its arguments. *)

  val make :
    current:(Mentat_workspace.Path.t * Mentat_edit.Observed.t) list ->
    blobs:(Mentat_digest.Content_ref.t * string) list ->
    t
  (** [make ~current ~blobs] is revert evidence.

      Raises [Invalid_argument] if [current] repeats a path, or if a blob's
      bytes do not match the reference they are supplied for — the contract is
      already-resolved content, and a mismatch is a caller bug, not a
      preparation problem. *)
end

(** {1:preparation Preparation data} *)

module Override : sig
  type t
  (** The type for explicit user acknowledgment that reverting the named
      non-contiguous paths erases unrecorded edits. Built from a presented
      {!Problem.Needs_override} refusal, so consent is path-specific, never
      blanket. Persisted in the started fact for audit. *)

  val accept_unrecorded_loss : Mentat_workspace.Path.t list -> t
  (** [accept_unrecorded_loss paths] consents to unrecorded-edit loss on exactly
      [paths].

      Raises [Invalid_argument] if [paths] is empty. *)

  val jsont : t Jsont.t
  (** [jsont] maps overrides to JSON objects, validating a canonical non-empty
      path list on decode. *)
end

module Problem : sig
  (** Structured preparation refusals. Any problem refuses the whole
      preparation; refusal changes no file and mints no fact. *)

  (** The type for one preparation problem. *)
  type t =
    | Nothing_to_revert
        (** The selection resolved to no revertable work; no empty plan is
            minted. *)
    | Stale of {
        path : Mentat_workspace.Path.t;
        expected : Image.t;  (** The recorded net-after image. *)
        actual : Image.t;  (** The image observed at plan time. *)
      }  (** The current read does not match the recorded net-after. *)
    | Unreadable of { path : Mentat_workspace.Path.t }
        (** The current read observed a non-text target
            ([Mentat_edit.Observed.Other]). *)
    | Missing_read of Mentat_workspace.Path.t
        (** The evidence carries no current read for a resolved target. *)
    | Missing_blob of {
        path : Mentat_workspace.Path.t;
        content : Mentat_digest.Content_ref.t;
      }
        (** A restore image's bytes are absent — because blobs are never garbage
            collected, a referenced image's bytes are always present, so absence
            is integrity corruption, reported here where bytes are actually
            resolved and never as a {!Revertability} answer. *)
    | Needs_override of { path : Mentat_workspace.Path.t }
        (** The path's history is non-contiguous and no covering override was
            supplied. *)
    | Unmatched_override of { path : Mentat_workspace.Path.t }
        (** The override names a path whose resolved history is not
            non-contiguous: the consent does not correspond to a presented risk,
            so the whole preparation is refused. *)
    | Superseded of { path : Mentat_workspace.Path.t; by : Change.Id.t }
        (** The selection's net-after for [path] is not the ledger's recorded
            head: a later recorded change outside the selection would be
            silently undone. Refused only when the merge gate is off or the
            merge itself conflicts; with the gate on a clean merge applies
            instead. *)
    | Conflict of { path : Mentat_workspace.Path.t; merge : Textdiff.Merge.t }
        (** Reverting [path] three-way conflicts with a later change: the merge
            is carried whole so a surface can render its git-style markers
            ({!Textdiff.Merge.render_markers}, folded into {!message}) for
            user-initiated resolution as an ordinary edit. There is no codec —
            the conflict crosses only as the rendered refusal string. *)
    | Unresolved_revert of Id.t
        (** A started revert has no settlement; no new plan is resolved against
            a history in an unknown intermediate state. *)

  val message : t -> string
  (** [message t] is a human-readable diagnostic. Presentation only; no caller
      matches on it. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats {!message} output. *)
end

module Target : sig
  type t = private {
    path : Mentat_workspace.Path.t;
    expected : Image.t;
        (** The current image validated at plan time — the [Mentat_edit]
            precondition, frozen. For an overridden non-contiguous target this
            is the plan-time read, not any recorded image. *)
    restore : Image.t;  (** The net-before image to restore. *)
    sources : Change.Id.t list;  (** Contributing change rows, ledger order. *)
  }
  (** The type for one frozen revert transition. Invariant: [expected] and
      [restore] differ and [sources] is non-empty. *)
end

module Started : sig
  type t = private {
    id : Id.t;
    selection : Selection.t;  (** What the user asked, for audit. *)
    targets : Target.t list;
        (** The frozen transitions, with expected current images, in plan order.
            Non-empty, with unique paths. *)
    override : Override.t option;
        (** The persisted consent, when one was required — naming exactly the
            plan's non-contiguous target paths. Invariant: every override path
            is a target path; preparation narrows the caller's consent to the
            targets that survived into the plan, so consent for a dropped path
            is never frozen. *)
  }
  (** The type for the durable started fact — persisted {e before} any revert
      filesystem IO, constructed from the plan alone. Settlement and crash
      recovery consume this frozen record and never re-resolve. *)

  val sources : t -> Change.Id.t list
  (** [sources t] is the resolved source change ids, in order. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same started fact. *)

  val jsont : t Jsont.t
  (** [jsont] maps started facts to JSON objects, re-validating the stated
      invariants on decode. *)
end

module Plan : sig
  type t
  (** The type for a ready revert plan. A plan exists only when every target is
      ready: any problem refuses the whole preparation, so there is no
      partially-ready plan. A plan is exactly its two halves: the frozen durable
      record and the lowered edit — the record's own fields (id, selection,
      targets, override, sources) are read through {!started}. *)

  val started : t -> Started.t
  (** [started t] is the frozen durable record — what {!Event.revert_started}
      persists and what settlement and crash recovery consume. *)

  val edit : t -> Mentat_edit.t
  (** [edit t] is the lowered all-or-nothing edit plan. Its text preconditions
      are the frozen expected images, so [Mentat_edit.apply] revalidates every
      target under the write lock immediately before mutating it. *)

  val merge_blobs : t -> (Mentat_digest.Content_ref.t * string) list
  (** [merge_blobs t] is the fresh restore-image bytes a clean merge produced —
      the merged images, keyed by their content reference. Unlike every other
      restore image, these are not durable from a prior event, so the store puts
      them before the started event lands and verifies only the restore refs not
      among them. Empty for a plan with no merge target. *)
end

(** {1:lifecycle Lifecycle data} *)

module Edit_failure : sig
  (** The persisted edit-error form — a structured, Mutation-owned subset of
      {!Mentat_edit.Error.t}: constructor identity and primary path survive;
      payloads without durable codecs do not. Flattening to a message string is
      forbidden — no caller may parse prose. If [mentat.edit] adds an error
      codec upstream, the subset widens to carry the full value; the kinds
      remain the stable branching surface either way. *)

  (** The type for the surviving constructor identity. *)
  type kind =
    | Invalid_text
    | Duplicate_path
    | State_mismatch
    | Conflict
    | Too_large
    | Invalid_target
    | Workspace
    | Out_of_workspace
    | Read_only_path
    | Protected_path
    | Io

  type t
  (** The type for a persisted edit failure. *)

  val of_error : Mentat_edit.Error.t -> t
  (** [of_error e] is the durable projection of [e]: its constructor identity,
      its primary path, and its message captured at construction. *)

  val kind : t -> kind
  (** [kind t] is [t]'s constructor identity. *)

  val path : t -> Mentat_workspace.Path.t option
  (** [path t] is the target path most directly associated with the failure, if
      any. *)

  val message : t -> string
  (** [message t] is the diagnostic captured at construction. Presentation only;
      no caller matches on it. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same failure. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a failure for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps failures to JSON objects, rejecting unknown kinds. *)
end

module Settled : sig
  (** The durable settlement fact: per-path outcomes plus the apply evidence.
      Because a revert's restoration rows live inside the settled fact, the
      audit and the change history agree by construction: a confirmed path has a
      matching restoration row in the same event; an ambiguous or not-attempted
      path has none. The disposition constrains the outcomes: a recovered
      settlement is all-ambiguous, a clean apply is all-confirmed, a preflight
      failure is all-not-attempted, and a commit failure is a confirmed prefix,
      exactly one ambiguous outcome at the stopping target, and a not-attempted
      suffix. *)

  (** The type for one target's settlement outcome. There is no failed arm:
      upstream proves no stopping write had no effect, so an uncertain stopping
      write is {!Ambiguous} — a vocabulary that admitted an unmintable
      "proven-failed" history was an impossible decodable state and is gone. *)
  type outcome =
    | Confirmed of Change.Id.t
        (** The write confirmed; the id names the revert-generated restoration
            row in this event. Invariant: it derives from the revert id and this
            outcome's own path, so an outcome can only name its own path's row.
        *)
    | Ambiguous  (** The write may have run; no confirmation exists. *)
    | Not_attempted
        (** Application stopped before reaching this target; it is untouched — a
            distinct audit fact, never conflated with failure. *)

  module Failure : sig
    (** The type for the stopping phase, mirroring
        {!Mentat_edit.Apply_error.Phase} structurally. *)
    type phase = Preflight | Commit of { target : Mentat_workspace.Path.t }

    type t = private { phase : phase; error : Edit_failure.t }
    (** The type for the plan-level stopping failure. *)
  end

  (** The type for how the settlement was produced. *)
  type disposition =
    | Applied of { failure : Failure.t option }
        (** The apply returned; [failure] is present iff it errored. *)
    | Recovered
        (** Minted by crash recovery from the frozen started record; no apply
            evidence exists. *)

  type t = private {
    revert : Id.t;
    outcomes : (Mentat_workspace.Path.t * outcome) list;
        (** Exactly the started targets, in plan order — coverage the replay
            fold enforces against the started fact
            ({!State.Error.Settlement_mismatch}). *)
    changes : Change.t list;
        (** The restoration rows for {!Confirmed} paths: [before] is the frozen
            expected image, [after] the restored image; ids derive from the
            revert id and path. *)
    disposition : disposition;
  }
  (** The type for a durable revert settlement. *)

  val revert : t -> Id.t
  (** [revert t] is the id of the revert [t] settles. *)

  val changes : t -> Change.t list
  (** [changes t] is [t]'s restoration rows — the confirmed-path transitions the
      revert recorded, whose ids the undo flow selects to un-revert (widen,
      narrow, or cancel) back to the arm-time baseline. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same settlement. *)

  val jsont : t Jsont.t
  (** [jsont] maps settlements to JSON objects, re-validating restoration row
      and confirmed outcome id derivation and disposition/outcome consistency on
      decode. *)
end

(** {1:internal Internal}

    Checked constructors and evidence eliminators for {!Revert}'s operations.
    Each constructor raises [Invalid_argument] exactly when its result type's
    stated invariant would be violated; none of these values is re-exported by
    {!Revert}.

    The top-level placement is the hiding mechanism, not an accident: because
    {!Revert} aliases only the submodules, values defined here stay out of the
    public facade. Relocating them into their submodules would re-export them
    through those aliases as public introduction forms and forfeit the
    smart-constructor discipline. *)

val target :
  path:Mentat_workspace.Path.t ->
  expected:Image.t ->
  restore:Image.t ->
  sources:Change.Id.t list ->
  Target.t
(** [target] is {!Target.t}'s validated constructor. *)

val started :
  id:Id.t ->
  selection:Selection.t ->
  targets:Target.t list ->
  override:Override.t option ->
  Started.t
(** [started] is {!Started.t}'s validated constructor: targets non-empty with
    unique paths, and every override path a target path. *)

val settled :
  revert:Id.t ->
  outcomes:(Mentat_workspace.Path.t * Settled.outcome) list ->
  changes:Change.t list ->
  disposition:Settled.disposition ->
  Settled.t
(** [settled] is {!Settled.t}'s validated constructor: outcomes non-empty with
    unique paths, restoration row and confirmed outcome ids re-derived from the
    revert id and their own paths, confirmed outcomes and rows in
    correspondence, and the disposition consistent with the outcomes (a
    recovered settlement is all-ambiguous, a clean apply is all-confirmed, a
    preflight failure is all-not-attempted, a commit failure is a confirmed
    prefix, one ambiguous outcome at the stopping target, and a not-attempted
    suffix). *)

val stopping_failure :
  phase:Settled.Failure.phase -> error:Edit_failure.t -> Settled.Failure.t
(** [stopping_failure ~phase ~error] is the plan-level stopping failure. *)

val plan :
  started:Started.t ->
  edit:Mentat_edit.t ->
  merge_blobs:(Mentat_digest.Content_ref.t * string) list ->
  Plan.t
(** [plan ~started ~edit ~merge_blobs] pairs the frozen record with its lowered
    edit and the fresh merged restore bytes (empty unless a merge target was
    minted). *)

val evidence_read :
  Evidence.t -> Mentat_workspace.Path.t -> Mentat_edit.Observed.t option
(** [evidence_read t path] is the recorded current read for [path], if any. *)

val evidence_find_blob :
  Evidence.t -> Mentat_digest.Content_ref.t -> string option
(** [evidence_find_blob t content] is the bytes [t] resolves for [content], if
    any. *)

val override_paths : Override.t -> Mentat_workspace.Path.t list
(** [override_paths t] is the consented paths, canonical. *)
