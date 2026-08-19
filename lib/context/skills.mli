(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Skill discovery, catalog rendering, and the on-demand [skill] tool.

    A skill is named, reusable guidance for one kind of task — a [SKILL.md] file
    whose YAML frontmatter carries a [description] and whose body is the
    guidance sent to the model on demand. {!load} takes one filesystem snapshot:
    every candidate discovered across every root, each classified into a
    {!Skill.status}, together with the {!Catalog.t} that lists the active skills
    under a byte budget.

    The snapshot feeds three model surfaces, all injected by the executable:
    - {!catalog_fragment}, a developer message naming the available skills, sewn
      into the turn prelude so it can be regenerated freely without disturbing
      the sealed tool declarations;
    - {!tool}, the boot-fixed [skill] tool that loads one skill's full guidance
      (or one of its resource files) on demand, refreshing its view of the
      filesystem per call;
    - {!injections}, the guidance the CLI [--skill] flag pins into a run.

    {b Trust and confinement.} Project roots ([.mentat], [.agents], [.claude])
    are discovered only when the workspace is trusted; a configured search root
    inside an untrusted workspace is dropped rather than promoted to a project
    skill. Resource reads are confined to the loaded skill's own directory and
    bounded, following symlinks only while they stay in-root.

    This module owns discovery, classification, catalog math, and the tool's
    handler. It does not own trust adjudication, path canonicalization of the
    workspace root, or prelude assembly: those are the executable's, which
    passes the results in. *)

(** {1:skills Skills} *)

module Skill : sig
  (** One discovered skill: its name, provenance, and classified status. *)

  module Name : sig
    (** Validated skill names.

        A name is the on-disk directory name (or built-in key) a skill is loaded
        by. The grammar is pinned so a name is safe as a single path component
        and stable on the wire. *)

    type t
    (** A skill name: 1 to 64 bytes of lowercase ASCII letters, digits, and
        hyphens, its first byte a letter or digit. *)

    val of_string : string -> (t, string) result
    (** [of_string s] is [Ok name] if [s] satisfies the grammar and
        [Error message] otherwise, with [message] naming the grammar. *)

    val to_string : t -> string
    (** [to_string name] is [name]'s bytes. *)

    val equal : t -> t -> bool
    (** [equal a b] is byte equality. *)

    val compare : t -> t -> int
    (** [compare a b] is the total byte order, consistent with {!equal}. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf name] prints [name]'s bytes. *)
  end

  (** Where a skill was discovered. Discovery visits kinds in this order, and
      the first {!Active} candidate for a name wins; see {!load}. *)
  type kind =
    | Project  (** A [.mentat/skills] skill; discovered only when trusted. *)
    | Compat_agents  (** A [.agents/skills] compatibility skill. *)
    | Compat_claude  (** A [.claude/skills] compatibility skill. *)
    | User  (** A skill under the account config home. *)
    | Path  (** A skill under a configured [skills.paths] search root. *)
    | Builtin  (** A skill shipped with the executable. *)

  type content = {
    description : string;
        (** The frontmatter [description], the catalog blurb. *)
    display_name : string option;
        (** The frontmatter [name], a human display label, if present. *)
    text : string;
        (** The raw [SKILL.md] bytes: provenance for display, and the identity
            behind {!field-bytes} and {!field-digest}. *)
    body : string;  (** The frontmatter-stripped guidance sent to the model. *)
    bytes : int;  (** [String.length text]. *)
    digest : string;  (** The content digest of {!field-text}. *)
    resources : string list;
        (** The skill directory's other files, sorted, loadable through
            {!tool}'s [resource] field. Empty for built-ins. *)
    ignored_keys : string list;
        (** Frontmatter keys other than [name] and [description], preserved so
            {!warnings} can report them. *)
  }
  (** The facts of an active skill. *)

  type invalid =
    [ `Description_missing  (** No [description] in the frontmatter. *)
    | `Description_too_long  (** The [description] exceeds 1024 bytes. *)
    | `Invalid_frontmatter of string
      (** The frontmatter did not parse, or violated a metadata rule; carries
          the reason. *)
    | `Unreadable of string
      (** The [SKILL.md] could not be read; carries the reason. *) ]
  (** Why a candidate is not usable. *)

  (** A candidate's classification in the snapshot. *)
  type status =
    | Active of content  (** Usable: the winning candidate for its name. *)
    | Shadowed of { by : string }
        (** A valid candidate a higher-precedence root already claimed; [by] is
            the winner's origin. *)
    | Disabled of [ `Project_skills | `Compat | `Builtin | `Config ]
        (** Suppressed by configuration: project skills off, compatibility roots
            off, built-ins off, or this name in [skills.disabled]. *)
    | Invalid of invalid  (** Discovered but not loadable. *)

  type t
  (** One discovered skill. *)

  val name : t -> Name.t
  (** [name t] is the skill's name. *)

  val kind : t -> kind
  (** [kind t] is where the skill was discovered. *)

  val status : t -> status
  (** [status t] is the skill's classification. *)

  val origin : t -> string
  (** [origin t] is the display path the candidate was discovered at. *)

  val kind_string : kind -> string
  (** [kind_string k] is [k]'s stable wire spelling: ["project"],
      ["compat_agents"], ["compat_claude"], ["user"], ["path"], or ["builtin"].
  *)

  val state_string : status -> string
  (** [state_string s] is [s]'s stable wire spelling: ["active"], ["shadowed"],
      ["disabled"], or ["invalid"]. *)

  val reason_string : status -> string option
  (** [reason_string s] is a stable machine reason for a non-active [s]
      (["shadowed"], ["project_skills_disabled"], ["compat_disabled"],
      ["builtin_disabled"], ["config_disabled"], ["description_missing"],
      ["description_too_long"], ["invalid_frontmatter"], ["unreadable"]), and
      [None] for {!Active}. *)

  val detail_string : status -> string option
  (** [detail_string s] is the free-text detail of [s] when it carries one — the
      shadowing winner's origin, or an invalid candidate's message — and [None]
      otherwise. *)

  val jsont : t Jsont.t
  (** [jsont] maps a skill to the stable support JSON shape: always [name],
      [kind], [origin], and [state]; a [reason] and [detail] when the status
      carries them; and, for an {!Active} skill, [description], [bytes],
      [digest], [resources], [ignored_keys], and an optional [display_name].

      The projection omits {!field-text} and {!field-body}, so a value decoded
      through [jsont] carries those two fields empty. It is a diagnostic and
      status projection, not a reload path for skill content. *)
end

(** {1:catalog Catalog} *)

module Catalog : sig
  (** The rendered listing of active skills.

      The catalog is a newline-joined list of ["- name: description"] entries
      for the active skills, fitted under a byte budget. Built-in descriptions
      are never trimmed and no name is ever dropped; when the budget is tight,
      non-built-in descriptions share the remaining bytes and longer ones are
      cut at a UTF-8 boundary with a trailing ellipsis, and when even that does
      not fit, non-built-in descriptions drop entirely ({!names_only}). *)

  type t
  (** A rendered catalog and the facts of how it was fitted. *)

  val text : t -> string
  (** [text t] is the rendered listing. *)

  val bytes : t -> int
  (** [bytes t] is [String.length (text t)]. *)

  val digest : t -> string
  (** [digest t] is the content digest of {!text}. *)

  val trimmed : t -> Skill.Name.t list
  (** [trimmed t] are the skills whose descriptions were cut to fit, in listing
      order. Empty when nothing was cut or the catalog fell back to
      {!names_only}. *)

  val names_only : t -> bool
  (** [names_only t] is [true] iff the budget forced non-built-in descriptions
      to drop entirely. *)

  val jsont : t Jsont.t
  (** [jsont] maps a catalog to the stable support JSON shape: [bytes],
      [digest], [trimmed], and [names_only]. It omits {!text}; a value decoded
      through [jsont] carries {!text} empty and is a diagnostic projection only.
  *)
end

(** {1:snapshot The snapshot} *)

type t
(** One filesystem snapshot of skill discovery: every candidate and the rendered
    {!Catalog.t}. A value is inert; it does not re-observe the filesystem. *)

val load :
  stdenv:Eio_unix.Stdenv.base ->
  builtins:(string * string) list ->
  config:Mentat_config.Resolved.t ->
  trusted:bool ->
  root:Lpath.Abs.t ->
  cwd:Lpath.Abs.t ->
  user_config_file:Lpath.Abs.t ->
  t
(** [load ~stdenv ~builtins ~config ~trusted ~root ~cwd ~user_config_file] is
    one discovery snapshot.

    [builtins] are the shipped [(name, SKILL.md)] pairs (from
    [Mentat_prompts.Skills.all]). [config] supplies the [skills.*] fields.
    [trusted] gates the project roots. [root] is the project root the [.mentat],
    [.agents], and [.claude] roots sit under; [cwd] anchors relative
    [skills.paths] entries; [user_config_file]'s directory is the account config
    home whose [skills] subdirectory holds {!Skill.User} skills.

    Discovery visits, in order: [root/.mentat/skills] ({!Skill.Project}),
    [root/.agents/skills] ({!Skill.Compat_agents}), [root/.claude/skills]
    ({!Skill.Compat_claude}) — the three only when [trusted] — then the config
    home's [skills] ({!Skill.User}), each [skills.paths] root ({!Skill.Path}),
    and the [builtins] ({!Skill.Builtin}). Under each filesystem root, a
    directory holding a [SKILL.md] file is a candidate named by that directory
    — its subdirectories are the skill's resources, never further skills — and
    a directory without one is a grouping folder whose subdirectories are
    searched in turn, so a shared pack may organize skills in nested folders.
    The search takes candidates in path order, skips dot-directories, follows
    symbolic links only while they resolve in-root, and is depth-bounded, so a
    link cycle terminates. Within the visit order the first
    {!Skill.Active} candidate for a name wins and later candidates of the name
    become {!Skill.Shadowed}; a name in [skills.disabled] is marked
    {!Skill.Disabled} before shadowing, so no lower-precedence candidate takes
    its place. A configured search root inside an untrusted [root] is dropped.

    When [skills.enabled] is [false] the snapshot is empty and {!enabled} is
    [false]. *)

val enabled : t -> bool
(** [enabled t] is [false] iff [skills.enabled] was [false] at {!load}. *)

val skills : t -> Skill.t list
(** [skills t] are all discovered candidates in discovery order. *)

val catalog : t -> Catalog.t
(** [catalog t] is the rendered listing of [t]'s active skills. *)

val find_active : t -> Skill.Name.t -> (Skill.t * Skill.content) option
(** [find_active t name] is the {!Skill.Active} skill named [name] and its
    content, or [None] if no active skill has that name. *)

val warnings : t -> string list
(** [warnings t] are the human-readable discovery warnings: trust restriction,
    invalid or ignored-key skills, and an over-budget catalog. *)

(** {1:surfaces Model surfaces} *)

val catalog_fragment : t -> Mentat_llm.Message.t option
(** [catalog_fragment t] is the developer message that documents the [skill]
    tool and lists the available skills, or [None] when the surface is disabled
    or no skill is active. It carries the skill guidance followed by
    ["\n\nAvailable skills:\n"] and {!Catalog.text}. The executable sews it into
    the turn prelude, where regenerating it as the catalog changes never
    disturbs the boot-fixed tool declarations. *)

val tool :
  stdenv:Eio_unix.Stdenv.base -> discover:(unit -> t) -> Mentat_tool.t option
(** [tool ~stdenv ~discover] is the [skill] tool, or [None] when the surface is
    disabled or no skill is active in [discover ()] at boot.

    The declaration is static — a fixed description that points the model at the
    catalog already in its context, carrying no catalog itself — so it stays a
    stable member of the boot-fixed tool catalog that turn contracts seal and
    resume compares. The handler, by contrast, calls [discover ()] afresh on
    every invocation, so a skill added or edited mid-session is served from the
    current filesystem. Given [{ name; resource }], it serves the active skill's
    guidance (bounded at 256 KiB) — or, with [resource], a file read confined to
    the skill's own directory and bounded at 128 KiB — cutting an over-cap body
    at a UTF-8 boundary and flagging it with a visible marker, and fails an
    unknown name with the list of active names. *)

val injections : t -> names:string list -> (string list, string) result
(** [injections t ~names] is the pinned-skill guidance for [names], in order, or
    [Error message] on the first unknown or malformed name. Each element frames
    one skill's guidance as user-invoked, for the CLI [--skill] flag. The
    guidance is bounded like {!tool}'s: an over-cap body is cut at a UTF-8
    boundary and flagged with a visible marker. *)
