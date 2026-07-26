(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Request-scoped workspace instructions.

    A context is a snapshot of everything the host tells the model about the
    workspace — the system prompt, the working directory, and the discovered
    [AGENTS.md] instructions — together with the facts of where every byte came
    from and why anything was left out. One {!load} reads the filesystem; every
    other operation is a pure view of the snapshot, so an instruction inspector,
    the execution prelude, and JSON output cannot disagree.

    Requested policy — enablement, compatibility, the byte budget — lives on the
    resolved {!Mentat_config.Resolved.t} the executable hands in, with the trust
    outcome and the resolved directories. The snapshot carries only discovered
    and built facts. Snapshots are never persisted: every request recomputes
    from disk. *)

(** {1:sources Sources} *)

module Source : sig
  (** Instruction sources and their statuses.

      The candidate/status vocabulary here is shared with {!Skills.Skill}: a
      discovery surface reports a {!kind}, a {!state_string}, an optional
      machine {!reason_string}, and an optional free-text {!detail_string},
      rather than reinventing one. *)

  type kind =
    | Global  (** [AGENTS.md] in the user config directory. *)
    | Project  (** [AGENTS.md] under the workspace root. *)
    | Local_override  (** [AGENTS.override.md] under the workspace root. *)
    | Compatibility  (** [CLAUDE.md] under the workspace root. *)

  type content = {
    bytes : int;  (** Size of the original file bytes. *)
    digest : string;  (** Digest of the original file bytes. *)
    included_bytes : int;  (** Size of the projected text. *)
    included_digest : string;  (** Digest of the projected text. *)
    omitted_bytes : int;  (** Bytes omitted by the budget; [0] if none. *)
    utf8_repaired : bool;  (** Invalid UTF-8 was replaced with U+FFFD. *)
  }
  (** The type for read facts. Only active sources have content facts. *)

  type status =
    | Active of content  (** The source contributes projected text. *)
    | Shadowed of { by : Lpath.Abs.t }
        (** A higher-precedence candidate in the same directory won. *)
    | Disabled of [ `Instructions | `Project_instructions | `Compatibility ]
        (** Enablement excluded the source before any content read. *)
    | Not_activated
        (** A nested [AGENTS.md] below the run cwd; reported by the {!load}
            [nested_scan] audit only, never read or projected. *)
    | Skipped of
        [ `Not_file
        | `Outside_workspace
        | `Unreadable of string
        | `Empty
        | `Budget_exhausted ]
        (** Observation or projection excluded the source. *)

  type t
  (** The type for discovered instruction sources. A source's identity is its
      normalized lexical absolute path; no path is ever canonicalized. *)

  val path : t -> Lpath.Abs.t
  (** [path source] is [source]'s normalized absolute path. *)

  val display_path : t -> string
  (** [display_path source] is [source]'s display text: root-relative for
      project-side sources, absolute for global sources. *)

  val kind : t -> kind
  (** [kind source] is [source]'s kind. *)

  val status : t -> status
  (** [status source] is what happened to [source]. *)

  val kind_string : kind -> string
  (** [kind_string kind] is the stable kind spelling: ["global"], ["project"],
      ["local_override"], or ["compatibility"]. *)

  val state_string : status -> string
  (** [state_string status] is the stable state spelling: ["active"],
      ["shadowed"], ["disabled"], ["not_activated"], or ["skipped"]. *)

  val reason_string : status -> string option
  (** [reason_string status] is the stable machine reason for an inactive
      status, for example ["shadowed_by_override"] or
      ["compatibility_disabled"]; it is [None] for {!constructor:Active}. *)

  val detail_string : status -> string option
  (** [detail_string status] is the free-text detail a status carries — the
      shadowing winner's path, or an unreadable candidate's message — and [None]
      otherwise. *)

  val jsont : t Jsont.t
  (** [jsont] maps a source to the stable support JSON shape: always [path],
      [display_path], [kind], and [state]; a [reason] and [detail] when the
      status carries them; and, for an {!constructor:Active} source, the content
      facts [bytes], [included_bytes], [digest], [included_digest],
      [omitted_bytes], and [utf8_repaired]. It is a diagnostic and status
      projection; encoding then decoding round-trips a source. *)
end

(** {1:loading Loading} *)

module Environment : sig
  type t
  (** Environment facts rendered into the workspace fragment: the date, the
      resolved model with its knowledge cutoff, and the platform. They are
      sourced at the composition root and passed to {!load}, so a load never
      reads a clock and stays deterministic. The git flag is not carried here —
      {!load} derives it from the [.git] marker it already detects. *)

  val none : t
  (** [none] carries no facts; the workspace fragment then names only the
      current directory and the git flag. *)

  val make :
    ?date:string ->
    ?model:string ->
    ?model_cutoff:string ->
    ?platform:string ->
    unit ->
    t
  (** [make ?date ?model ?model_cutoff ?platform ()] carries the facts to
      render. Each omitted field is left out of the block. [model_cutoff] is the
      model's knowledge cutoff, rendered as a trailing clause on the model line;
      it is ignored when [model] is absent. *)
end

type t
(** The type for loaded workspace context.

    Values are snapshots. Later file changes do not affect an existing value. *)

val load :
  environment:Environment.t ->
  stdenv:Eio_unix.Stdenv.base ->
  nested_scan:bool ->
  config:Mentat_config.Resolved.t ->
  trusted:bool ->
  root:Lpath.Abs.t ->
  cwd:Lpath.Abs.t ->
  user_config_file:Lpath.Abs.t ->
  (t, Mentat_diagnostic.t) result
(** [load ~environment ~stdenv ~nested_scan ~config ~trusted ~root ~cwd
     ~user_config_file] reads the filesystem once and snapshots the workspace
    context. Pass {!Environment.none} for [environment] to render only the
    directory and the git flag.

    Each directory from [root] down to [cwd] contributes at most one active
    instruction file, chosen from [AGENTS.override.md], then [AGENTS.md], then
    [CLAUDE.md], by the [instructions.*] enablement fields of [config]. Project
    candidates are discovered only when [trusted]; otherwise their paths are not
    statted, walked, or read. Trusted candidates are observed through
    workspace-contained filesystem checks: a symlinked candidate is followed and
    must resolve inside [root]. Project instruction text is read only for active
    candidates, against the configured [instructions.project_max_bytes] budget.
    Global instructions come from [AGENTS.md] in [user_config_file]'s directory
    and are not budgeted.

    [nested_scan] (default [false]) additionally records [AGENTS.md] files in
    directories strictly below [cwd] as {!Source.Not_activated} audit sources
    when [trusted]. The scan never reads file contents and never affects the
    projection; it skips VCS metadata directories, does not follow directory
    symlinks, and stops at a fixed visited-directory cap, recording
    {!nested_scan}[ = `Capped].

    Content-level problems are never errors; they are source statuses. The only
    error is a {!Mentat_diagnostic.t} when [cwd] does not lie within [root]. *)

(** {1:facts Discovered facts} *)

val cwd : t -> Lpath.Abs.t
(** [cwd t] is the resolved run directory the snapshot was taken for. *)

val root : t -> Lpath.Abs.t
(** [root t] is the workspace root the snapshot was taken for. *)

val root_marker : t -> string option
(** [root_marker t] is [Some ".git"] when [root] holds a [.git] marker and
    [None] otherwise. *)

val budget_used : t -> int
(** [budget_used t] is the number of original project instruction bytes consumed
    from the configured budget. *)

val nested_scan : t -> [ `Off | `Complete | `Capped ]
(** [nested_scan t] is the nested-scan outcome: [`Off] when not requested,
    [`Complete] when the walk finished, [`Capped] when it stopped at the
    visited-directory cap. *)

val sources : t -> Source.t list
(** [sources t] are the discovered sources in deterministic order: global, then
    per directory from root to cwd in candidate precedence order, then
    nested-scan results in lexicographic order. *)

(** {1:projection Projection} *)

val fragments : t -> Mentat_llm.Message.t list
(** [fragments t] are the exact model-visible context messages for the next
    normal request, in provider order: a {!Mentat_llm.Message.system} carrying
    the system prompt, a {!Mentat_llm.Message.developer} carrying the workspace
    block, and — when any instructions are active — a
    {!Mentat_llm.Message.user_text} wrapping the instruction blocks. The list
    contains only prelude-admissible message kinds. *)

val prelude :
  t ->
  developer:Mentat_llm.Message.t list ->
  skills:Skills.t ->
  Mentat_llm.Message.t list
(** [prelude t ~developer ~skills] is the exact model-visible prelude fragment
    sequence for the next request, in provider order: the {!fragments} of [t],
    then the host [developer] instructions the caller slots for the turn — a
    mode prompt or a delegated role's subagent prompt, and empty for a plain
    Build turn — then the {!Skills.catalog_fragment} of [skills] when one is
    active. It is the one assembly the run's sealed prelude and the offline
    prompt inspector both project, so the two cannot disagree on fragment order
    or membership. *)

val fragments_json : t -> Jsont.json list
(** [fragments_json t] is the model-visible context projection as JSON objects
    with [role], [sources], and [text], for debug and inspection output. *)

val rendered_digest : t -> string
(** [rendered_digest t] is the projection identity: a digest over a
    length-prefixed encoding of each fragment's role and text, spelled
    [sha256:<hex>:<length>]. Two contexts project the same bytes exactly when
    their rendered digests are equal.

    The length prefix makes the encoding injective over the (role, text)
    fragment sequence, so digest equality implies byte equality: it must not be
    reduced to plain concatenation, which would confuse fragment boundaries
    (["ab"] then ["c"] versus ["a"] then ["bc"]).

    The identity intentionally covers the resolved run directory, which is
    model-visible in the workspace and instruction fragments: it is a
    per-location, per-session projection identity, not a portable content hash.
    Two checkouts of the same files under different absolute paths therefore
    differ. *)

(** {1:warnings Warnings} *)

val warnings : t -> string list
(** [warnings t] are human-readable diagnostics derived from {!sources} and
    {!nested_scan}: unreadable candidates, UTF-8 repairs, budget truncation and
    exhaustion, a disabled-only compatibility file, and a capped nested scan.
    Warnings never change exit codes and are not separate state. *)
