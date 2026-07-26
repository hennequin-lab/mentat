(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Recursive workspace file discovery by path glob.

    [Glob] walks an existing non-symlink directory through
    {!Mentat_workspace_io.File.read_dir} and returns regular files whose paths
    match a validated glob. It never spawns a process and never observes the
    filesystem outside the injected workspace capability.

    Pattern syntax follows ripgrep's globset contract. [*] consumes zero or more
    non-[/] bytes and [?] consumes one. A run of exactly two stars is recursive
    only as a complete [**] path component or as a [**/] prefix; embedded [**]
    and longer star runs have ordinary component-local [*] behavior. Character
    classes support ranges and [!]/[^] complement syntax; unlike [*] and [?], a
    class may explicitly contain and match [/]. A backslash quotes the next
    byte. Braces may be nested and expand alternatives; empty alternatives are
    ignored when a non-empty alternative exists, while an entirely empty brace
    contributes the empty string.

    A pattern containing a slash is matched against the complete path relative
    to its bound workspace root, matching ripgrep even when traversal is
    restricted by [path]; a pattern without a slash is matched against each
    file's basename. A leading [!] makes the one tool pattern an exclusion:
    [!*.ml] returns non-[ml] files and prunes directories named with the same
    match, [!] returns no files, and [!!] excludes the literal basename [!].
    Ordinary hidden files and directories participate in matching. Protected VCS
    metadata directories ([.git], [.svn], [.hg], [.bzr], [.jj], and [.sl]) and
    symlinked children are never traversed; an explicit symlink root is refused
    rather than followed.

    Standard per-directory [.gitignore], [.ignore], and [.rgignore] files are
    applied in that order while walking, including comments, escaping, trailing
    space rules, directory-only patterns, and ordered negation. Later rules win;
    child-directory rules follow inherited parent rules, and [.rgignore] has the
    final say within one directory. As with Git, an excluded directory is
    pruned, so a descendant cannot be re-included unless an earlier rule first
    makes its parent traversable. A nested requested root inherits rules read
    only along its bound workspace-root ancestor chain; an ignored requested
    root returns no files. Ambient user-global Git excludes are not consulted,
    and protected VCS metadata is not opened to read [.git/info/exclude]. An
    ignore file above the 16 MiB observation bound is a loud invalid-input
    failure rather than silently changing the match set.

    Results are sorted by workspace-root-relative path by default, or by
    descending modification time with path as a stable tie-breaker, then paged
    with a one-based [offset] and bounded [limit]. [total] is always exact.
    [root] and every returned path are provider-resolvable addresses: [.] or a
    path relative to the logical current directory for that directory and its
    descendants, and a canonical absolute path for an ancestor, sibling, or
    different admitted root. Thus equal relative names in two roots remain
    distinguishable, and resolving an emitted address recovers the path it
    denotes even when the logical current directory is nested.

    A continuation preserves a valid relative [path] spelling from the request,
    including [..] segments; an absolute request becomes the root's canonical
    absolute address. It repeats the pattern, ordering, and page size and
    advances [offset] by the number returned; no continuation is emitted after
    the final page or for an offset beyond the result set.

    The durable JSON projection contains only the exact number of matching
    files. Patterns, paths, paging state, ordering, and continuations remain in
    the authoritative model-facing text. A partial page marks
    {!Mentat_tool.Output.truncated} because that text omits matching paths.

    For example, a glob with twelve matches produces this semantic projection:

    {[
      {"version":1,"shape":"files","total":12}
    ]} *)

val name : string
(** [name] is ["glob"]. *)

val default_limit : int
(** [default_limit] is the returned-path budget used when input omits [limit].
*)

val max_limit : int
(** [max_limit] is the greatest accepted explicit returned-path budget. *)

module Enumeration : sig
  (** Typed file enumeration shared by tools that compose glob discovery with
      another operation. This seam avoids treating a model-facing tool call or
      its presentation output as an internal protocol. *)

  type error =
    [ `Cancelled
    | `File_error of Mentat_workspace_io.File_error.t
    | `Invalid_pattern of string ]
  (** The type for cancellation, workspace observation failures, and invalid
      glob syntax. *)

  val paths :
    Mentat_workspace_io.t ->
    cancelled:(unit -> bool) ->
    root:Mentat_workspace.Path.t ->
    pattern:string ->
    (Mentat_workspace.Path.t list, error) result
  (** [paths workspace_io ~cancelled ~root ~pattern] returns every matching
      regular file beneath the already-resolved directory [root], ordered by
      workspace-root-relative path. Ignore files and protected VCS metadata
      follow the same rules as the public tool.

      The caller must first observe [root] with
      {!Mentat_workspace_io.File.lstat} and establish that it is a non-symlink
      directory. This internal composition seam does not repeat root-kind
      validation; the model-facing tool performs that validation before entering
      the shared walker. *)
end

val vcs_metadata_dirs : string list
(** [vcs_metadata_dirs] is the top-level VCS metadata directory names ([.git],
    [.svn], [.hg], [.bzr], [.jj], [.sl]) that every scanner in this library
    treats as never-content: the glob walk prunes them, [read_file]'s directory
    listing hides them, and [search_text] excludes them. Single-homed here so
    the set cannot drift between the scanners. *)

module Ignore : sig
  (** Root-anchored ignore-rule evaluation for scanners that prune outside a
      glob walk, such as the filesystem watcher. Rules use the same parser as
      the walker's per-directory ignore files, so syntax — comments, escaping,
      trailing-space rules, directory-only patterns, and ordered negation —
      cannot drift between the two. This seam performs no I/O: the caller reads
      the workspace root's ignore files and supplies their contents. *)

  type rules
  (** Ordered rules anchored at the workspace root. Later rules win. *)

  val empty : rules
  (** [empty] is the rule set that prunes nothing. *)

  val parse : string -> rules
  (** [parse contents] is the rules in one root-level ignore file's [contents],
      in file order. Malformed pattern lines are dropped, as in the walker. *)

  val join : rules -> rules -> rules
  (** [join earlier later] is [earlier]'s rules followed by [later]'s, so
      [later] has the final say — the [.gitignore], [.ignore], [.rgignore]
      precedence when joined in that order. *)

  val prunes : rules -> Lpath.Rel.t -> bool
  (** [prunes rules path] is whether the root-relative [path] is excluded under
      subtree-pruning semantics: every candidate is treated as a directory, so
      directory-only rules apply, and a regular file named like a directory-only
      pattern is conservatively pruned. The root [.] is never pruned. *)
end

val make : Mentat_workspace_io.t -> Mentat_tool.t
(** [make workspace_io] is the immutable [glob] tool definition over
    [workspace_io]. Constructing it performs no I/O.

    The input is a strict JSON object with:

    - required non-empty string [pattern];
    - optional non-empty string [path], resolved relative to the capability's
      logical workspace current directory and defaulting to [.];
    - optional one-based integer [offset], defaulting to [1];
    - optional integer [limit] in [1] through {!max_limit}, defaulting to
      {!default_limit};
    - optional string [sort], either ["path"] (the default) or ["modified"].

    Integer members must also lie in JSON's safe integer range, as published by
    the schema. Unknown or duplicate members, malformed patterns, invalid roots,
    file roots, and symlink roots fail as [`Invalid_input]. Missing roots fail
    as [`Not_found]; filesystem observation failures fail as [`Failed]. Decoded
    calls request exact read access to the resolved root; invalid or outside
    roots produce no permission request and fail before filesystem access.

    The run polls cancellation before observation and between directory,
    ignore-file, and entry reads. Cancellation returns a cancelled interruption
    and no misleading partial result. *)
