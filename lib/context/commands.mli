(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** User-invoked prompt-template commands.

    A command is a markdown file whose optional YAML frontmatter carries a
    [description] and an [argument-hint] and whose body is a prompt template.
    Commands are user-invoked as [/name args]: invoking one expands its body,
    substituting the arguments, into an ordinary user turn. They never enter the
    model's context and are never model-invoked — that is {!Skills}.

    {!load} takes one filesystem snapshot: every candidate discovered across the
    project and user roots, each classified into a {!Command.status}. Every
    other operation is a pure view. Snapshots are never persisted.

    {b Trust and confinement.} The three project roots ([.mentat], [.agents],
    [.claude]) are discovered only when the workspace is trusted, so an
    untrusted checkout cannot introduce a [/deploy] that expands to a
    prompt-injection payload the user runs unawares. File reads follow symbolic
    links only while they stay in-root. {!expand}'s [@file] embedding obeys the
    same rule: it resolves only under a trusted workspace and through the same
    realpath-contained, byte-bounded read the model's read tool uses.

    This module owns discovery, classification, the pure [$]-substitution, and
    the [@file] resolution [expand] layers on top of it. It does not own trust
    adjudication, workspace-root canonicalization, or turn submission: those are
    the executable's, which passes the results in. *)

(** {1:names Names} *)

module Name : sig
  (** Validated command names.

      A name is the [:]-joined path a command is discovered and invoked by:
      [git/commit.md] under a commands root yields [git:commit]. Each segment is
      a well-formed name component, so a name is safe as a set of path
      components and stable on the wire. *)

  type t
  (** A command name: one or more [:]-joined segments, each 1 to 64 bytes of
      lowercase ASCII letters, digits, and hyphens with a leading letter or
      digit. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is [Ok name] if [s] is a non-empty [:]-joined sequence of
      well-formed segments, and [Error message] otherwise, with [message] naming
      the grammar. *)

  val to_string : t -> string
  (** [to_string name] is [name]'s [:]-joined bytes. *)

  val equal : t -> t -> bool
  (** [equal a b] is byte equality of {!to_string}. *)

  val compare : t -> t -> int
  (** [compare a b] is the total byte order of {!to_string}, consistent with
      {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf name] prints [name]'s bytes. *)
end

(** {1:commands Commands} *)

module Command : sig
  (** One discovered command: its name, provenance, and classified status. *)

  (** Where a command was discovered. Discovery visits kinds in this order, and
      the first {!Active} candidate for a name wins; see {!load}. *)
  type kind =
    | Project
        (** A [.mentat/commands] command; discovered only when trusted. *)
    | Compat_agents  (** A [.agents/commands] compatibility command. *)
    | Compat_claude  (** A [.claude/commands] compatibility command. *)
    | User  (** A command under the account config home. *)

  type content = {
    description : string option;
        (** The frontmatter [description], shown in completion. *)
    argument_hint : string option;
        (** The frontmatter [argument-hint], shown after the name in completion.
        *)
    body : string;
        (** The frontmatter-stripped template, non-blank; the argument for
            {!substitute}. *)
    text : string;  (** The raw file bytes: provenance for display. *)
    bytes : int;  (** [String.length text]. *)
    digest : string;  (** The content digest of {!field-text}. *)
    ignored_keys : string list;
        (** Frontmatter keys other than [description] and [argument-hint],
            preserved so {!warnings} can report them. *)
  }
  (** The facts of an active command. *)

  type invalid =
    [ `Invalid_name of string
      (** A path segment is not a well-formed name component; carries the
          [:]-joined raw name. *)
    | `Empty_body  (** The body is empty or blank after frontmatter. *)
    | `Invalid_frontmatter of string
      (** The frontmatter did not parse, or violated a metadata rule; carries
          the reason. *)
    | `Unreadable of string
      (** The file could not be read; carries the reason. *) ]
  (** Why a candidate is not usable. *)

  (** A candidate's classification in the snapshot. *)
  type status =
    | Active of content  (** Usable: the winning candidate for its name. *)
    | Shadowed of { by : string }
        (** A valid candidate a higher-precedence root already claimed; [by] is
            the winner's origin. *)
    | Disabled of [ `Project | `Compat | `Config ]
        (** Suppressed by configuration: project commands off, compatibility
            roots off, or this name in [commands.disabled]. *)
    | Invalid of invalid  (** Discovered but not loadable. *)

  type t
  (** One discovered command. *)

  val name : t -> string
  (** [name t] is the command's [:]-joined display name derived from its path. A
      candidate whose status is not {!Invalid} carrying [`Invalid_name] has a
      name that parses through {!Name.of_string}; an ill-formed candidate keeps
      its raw name here for display. *)

  val kind : t -> kind
  (** [kind t] is where the command was discovered. *)

  val status : t -> status
  (** [status t] is the command's classification. *)

  val origin : t -> string
  (** [origin t] is the display path the candidate was discovered at. *)

  val kind_string : kind -> string
  (** [kind_string k] is [k]'s stable wire spelling: ["project"],
      ["compat_agents"], ["compat_claude"], or ["user"]. *)

  val state_string : status -> string
  (** [state_string s] is [s]'s stable wire spelling: ["active"], ["shadowed"],
      ["disabled"], or ["invalid"]. *)

  val reason_string : status -> string option
  (** [reason_string s] is a stable machine reason for a non-active [s]
      (["shadowed"], ["project_disabled"], ["compat_disabled"],
      ["config_disabled"], ["invalid_name"], ["empty_body"],
      ["invalid_frontmatter"], ["unreadable"]), and [None] for {!Active}. *)

  val detail_string : status -> string option
  (** [detail_string s] is the free-text detail of [s] when it carries one — the
      shadowing winner's origin, the ill-formed raw name, or an invalid
      candidate's message — and [None] otherwise. *)

  val jsont : t Jsont.t
  (** [jsont] maps a command to the stable support JSON shape: always [name],
      [kind], [origin], and [state]; a [reason] and [detail] when the status
      carries them; and, for an {!Active} command, [bytes], [digest],
      [ignored_keys], and an optional [description] and [argument_hint].

      The projection omits {!field-text} and {!field-body}, so a value decoded
      through [jsont] carries those two fields empty. It is a diagnostic and
      status projection, not a reload path for command content. *)
end

(** {1:snapshot The snapshot} *)

type t
(** One filesystem snapshot of command discovery. Discovery is complete after
    {!load}; the snapshot re-observes the filesystem only through {!expand},
    which resolves a command body's [@file] references against the workspace
    captured at load. *)

val load :
  stdenv:Eio_unix.Stdenv.base ->
  config:Mentat_config.Resolved.t ->
  trusted:bool ->
  root:Lpath.Abs.t ->
  user_config_file:Lpath.Abs.t ->
  t
(** [load ~stdenv ~config ~trusted ~root ~user_config_file] is one discovery
    snapshot.

    [config] supplies the [commands.*] fields. [trusted] gates the project
    roots. [root] is the project root the [.mentat], [.agents], and [.claude]
    roots sit under; [user_config_file]'s directory is the account config home
    whose [commands] subdirectory holds {!Command.User} commands.

    Unlike [Skills.load], [load] takes no [cwd]: [cwd] only anchors relative
    [skills.paths] roots, and the [commands.paths] equivalent is deferred, so no
    command root is relative. It returns to the signature if extra command roots
    land.

    Discovery visits, in order: [root/.mentat/commands] ({!Command.Project}),
    [root/.agents/commands] ({!Command.Compat_agents}), [root/.claude/commands]
    ({!Command.Compat_claude}) — the three only when [trusted] — then the config
    home's [commands] ({!Command.User}). Each root is walked recursively: a file
    at [<a>/<b>/<c>.md] yields the name [a:b:c], and an ill-formed path segment
    classifies the candidate {!Command.Invalid} ([`Invalid_name]) rather than
    renaming it. Within the discovery order the first {!Command.Active}
    candidate for a name wins and later candidates of the name become
    {!Command.Shadowed}; a name in [commands.disabled] is marked
    {!Command.Disabled} before shadowing, so no lower-precedence candidate takes
    its place.

    When [commands.enabled] is [false] the snapshot is empty and {!enabled} is
    [false]. *)

val enabled : t -> bool
(** [enabled t] is [false] iff [commands.enabled] was [false] at {!load}. *)

val commands : t -> Command.t list
(** [commands t] are all discovered candidates in discovery order. *)

val find_active : t -> Name.t -> Command.content option
(** [find_active t name] is the {!Command.Active} command named [name]'s
    content, or [None] if no active command has that name. *)

val warnings : t -> string list
(** [warnings t] are the human-readable discovery warnings: trust restriction,
    invalid commands, and ignored or unsupported frontmatter keys. *)

(** {1:expansion Expansion} *)

val substitute : body:string -> arguments:string -> string
(** [substitute ~body ~arguments] is [body] with its [$]-tokens expanded, in a
    single left-to-right pass.

    [arguments] is the whole argument string, its surrounding horizontal
    whitespace already trimmed by the caller. The positional tokens are the
    maximal runs of bytes in [arguments] that are neither an ASCII space nor an
    ASCII tab; every other byte, a newline included, is part of a token.

    At each [$]: if the following bytes are exactly [ARGUMENTS] (checked first,
    byte-exact and case-sensitive, no word boundary) the whole [arguments]
    string is substituted; if the following byte is a single digit [1]–[9] the
    corresponding positional token is substituted, or the empty string when it
    is absent; otherwise the [$] is emitted verbatim. So [$HOME], [$0], a lone
    [$], and [$$] survive unchanged, [$10] is [$1] then a literal [0], and
    [$ARGUMENTSX] is the argument string then a literal [X]. Substituted values
    are inserted literally and never re-scanned. [@] is an ordinary byte here:
    embedding a file is {!expand}'s effectful [@]-resolution, not this pure
    pass. *)

type file_error = {
  path : string;  (** The [@]-reference path exactly as written in the body. *)
  reason : string;
      (** Why it did not resolve: a stable, one-line diagnostic (workspace not
          trusted, path escapes the workspace, no such file, not a regular file,
          or an underlying read failure). *)
}
(** Why an [@path] file reference failed to resolve. *)

val expand :
  t ->
  name:Name.t ->
  arguments:string ->
  ( Mentat_llm.Content.t list,
    [ `Unknown of string | `File_unresolved of file_error ] )
  result
(** [expand t ~name ~arguments] is the expansion of the active command [name] as
    user-turn content, ready for [Command.prompt]'s [input].

    Expansion is a single left-to-right walk over the active command's body that
    layers two sigils over the {!substitute} grammar: a [$]-token splices the
    (inert) argument text, and an [@]-token embeds a file. [@] followed by a
    maximal run of non-whitespace bytes is a file reference whose run is a
    workspace-relative path; an [@] at end of input or before whitespace is a
    literal byte, exactly as a bare [$] is. Neither spliced argument text nor
    embedded file bytes are re-scanned, so an argument or a file that itself
    contains [$1] or [@secret] is inert.

    An [@path] resolves through the same realpath-contained, byte-bounded read
    the model's read tool uses, and only when the workspace is trusted. Invalid
    UTF-8 is repaired with U+FFFD; a file over 128 KiB is embedded cut at a
    UTF-8 boundary and flagged with a visible marker, as the skill resource read
    bounds its file (see {!Skills}). A reference that resolves outside the
    workspace, names no file or a non-file, exceeds the 4 MiB read ceiling or is
    otherwise unreadable, or is requested in an untrusted workspace is a hard
    failure: [Error (`File_unresolved _)] with no content, so the caller sends
    no turn.

    Otherwise it is [Ok [ Content.text expansion ]] for the active command
    [name], except that an expansion empty or blank for the given arguments is
    [Ok []] — the caller's prompt constructor rejects that as an empty prompt (a
    [$1] body invoked with no argument, say), never submitting an empty user
    turn. It is [Error (`Unknown n)] when no active command has that name. *)
