(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Merlin-backed semantic OCaml reference lookup.

    [Find_references] provides the stable ["ocaml_find_references"] model tool.
    It asks Merlin for occurrences of the entity at a concrete source position.
    The query is identity-based: it respects shadowing, opens, includes, labels,
    constructors, and generated bindings instead of treating equal source text
    as the same entity. There is deliberately no textual search fallback.

    {1:input Input contract}

    Provider input is a strict JSON object with:

    - required ["path"], a non-empty, NUL-free relative or workspace-contained
      absolute OCaml source-file address;
    - required ["line"], a one-based source line;
    - required ["column"], a zero-based {e byte} column within that line;
    - optional ["scope"], one of ["buffer"], ["project"], or ["renaming"],
      defaulting to ["project"];
    - optional ["include_stale"], defaulting to [false];
    - optional ["offset"], the one-based first eligible occurrence, defaulting
      to [1];
    - optional ["limit"] in \[[1];[max_limit]\], defaulting to {!default_limit}.

    ["buffer"] restricts Merlin to the current source. ["project"] and
    ["renaming"] consult project occurrence indexes; ["renaming"] uses Merlin's
    rename-oriented occurrence scope but does not modify any file. Merlin
    exposes a stale bit on individual occurrences but no aggregate index version
    or freshness. Consequently project and renaming results report
    ["index_status":"unknown"], while buffer results report ["not_applicable"].

    Stale filtering precedes pagination. With ["include_stale":false], stale
    locations are excluded and counted in ["stale_skipped"]. Canonically equal
    locations are deduplicated before filtering; if duplicate reports disagree
    on freshness, a fresh report dominates. ["offset"] then indexes this
    filtered, deduplicated, path-sorted set. A partial page's text includes a
    [next: ocaml_find_references {...}] continuation with the same canonical
    query and an advanced offset; passing that JSON object back as the next
    provider input preserves scope, stale policy, and limit. An offset beyond
    the final location completes with an empty page.

    A continuation is a new semantic lookup, not a snapshot token. Merlin may
    observe a different project index or source tree between pages; insertions,
    removals, or freshness changes before the next offset can therefore move a
    location across the page boundary. Restart from offset [1] when one coherent
    post-build result set is required.

    For example:

    {[
     {
       "path": "lib/parser.ml",
       "line": 42,
       "column": 17,
       "scope": "project",
       "include_stale": false,
       "limit": 100
     }
    ]}

    Every numeric member must be an exact integer in JSON's safe integer range.
    Fractional values, numeric strings, infinities, unsafe integers, unknown
    members, duplicate members, empty paths, NUL, and out-of-range pagination
    are rejected during call decoding, before permission planning or I/O.
    Columns are bytes rather than Unicode-scalar or grapheme indexes, matching
    OCaml and Merlin source coordinates.

    {1:source Source and command route}

    The requested path must resolve to a regular file. The tool loads its
    complete contents through {!Mentat_workspace_io.File.load}, bounded by
    {!max_source_bytes}, and supplies those exact bytes to Merlin on standard
    input. It never reads through a host filesystem API, invokes a build or
    clean, discovers a program, or inspects the ambient environment.

    Merlin runs from the root that owns the queried source, including an
    auxiliary read-only root. The capability supplies the canonical physical
    source spelling for [-filename]. A logical workspace cwd nested below that
    root does not alter the child cwd or project context. Existing Merlin and
    Dune metadata remains subject to the sealed command policy; the tool neither
    creates missing index data nor retries through an unconfined route.

    Each execution that reaches the semantic backend makes exactly one
    [ocamlmerlin single occurrences] invocation. Its fixed arguments are
    [-identifier-at line:column], the selected [-scope], and the physical
    [-filename]. The immutable boot-resolved [program] prefix precedes that
    argv. The invocation goes exclusively through
    {!Mentat_workspace_io.Command.run} via the shared Merlin transport, with
    source stdin, a 30-second timeout, a one-MiB bound on each captured stream,
    and cooperative cancellation. No shell, raw process manager, [Unix], [Sys],
    environment, or sandbox-lowering handle is used.

    {1:locations Location normalization and ordering}

    A buffer-scope Merlin occurrence contains start/end positions and a stale
    bit. Project and renaming occurrences additionally contain a file. An empty
    file and Merlin's ["*buffer*"] sentinel denote the queried source. Relative
    files resolve from the source-owning root, not from the process ambient
    directory or the logical workspace cwd. Absolute files are lexically
    normalized. Every result must belong to an admitted workspace root; an
    outside path fails the whole call rather than leaking an unresolvable or
    unauthorized location.

    Occurrence objects, their position objects, and member types are validated
    exactly. Missing, duplicate, unknown, malformed, unsafe-integer, reversed,
    or NUL-bearing protocol data is [`Failed]; no valid prefix is returned.
    Locations are normalized, ordered by canonical address and range, and
    deduplicated by that identity before stale filtering and pagination.

    A workspace location at or below the capability's logical current directory
    is ["."] or relative to it. A sibling, ancestor, or auxiliary-root location
    is rendered with its canonical capability absolute address. Query,
    reference, and continuation paths use this same rule, so equal root-relative
    spellings in two roots cannot collide and every stored path can be passed to
    a later provider call.

    {1:output Durable output}

    A completed call retains the established text shape: a query header, scope,
    returned/reported counts, page status, index status, backend, and one
    [path:start-end] line per reference. Stale entries carry a [" stale"]
    suffix. A partial page adds a machine-copyable
    [next: ocaml_find_references {...}] line. Filtering stale locations adds the
    established Dune index-rebuild note.

    Completed calls also carry the closed version-1
    {!Mentat_tools_output.Ocaml.References} JSON projection: the total number of
    eligible deduplicated occurrences before pagination and the number of
    distinct canonical files containing them. Every page of the same unchanged
    result set therefore carries the same semantic counts even though its text
    lists a different window. Query parameters, locations, stale and duplicate
    counts, continuation, index status, and backend remain solely in the
    authoritative text. The erased output truncation bit is true exactly when a
    continuation exists.

    Text and semantic JSON are durable session data. Replay consumes the compact
    projection directly and does not parse text. The output contains no source
    bytes, argv, build data, capability, mutation fact, or edit receipt. The
    source-read bound, one-MiB per-stream command capture, and {!max_limit} page
    bound keep every retained call result finite.

    {1:permissions Errors and cancellation}

    Permission planning is pure over decoded input. A lexically resolvable path
    produces one request containing its read access and the shared
    ["command.confinement"] custom fact. The fact's subject is the stable
    {!Mentat_permission.Access.Command.execution_to_string} projection computed
    once from the immutable workspace capability when {!make} constructs the
    tool. Fixed Merlin argv and source bytes are implementation details and are
    not exposed as model-authored shell or argv permissions. An unresolvable
    path produces no speculative request and is rejected during execution.

    A missing source is [`Not_found]. An unresolved or non-regular source and a
    source over {!max_source_bytes} are [`Invalid_input]. Filesystem I/O,
    malformed Merlin envelopes or occurrence records, outside-workspace result
    paths, non-zero exit, signal, output overflow, incomplete capture, or
    supervision failure are [`Failed]. A missing or refused Merlin launch is
    [`Unavailable], and a 30-second timeout is [`Timed_out]. Merlin query
    failures remain [`Failed] because they are semantic backend diagnostics, not
    textual evidence suitable for a fallback.

    Cancellation is checked before source observation, after the complete read,
    while Merlin runs, and before parsing or lowering its response. Once
    observed, the call is interrupted with reason ["tool call cancelled"] and
    [cancelled=true], with no completed or partial output. *)

val name : string
(** [name] is ["ocaml_find_references"]. *)

val default_limit : int
(** [default_limit] is [200]. *)

val max_limit : int
(** [max_limit] is [1000], the largest accepted reference page. *)

val max_source_bytes : int
(** [max_source_bytes] is the eight-MiB complete source-read bound. *)

val make :
  Mentat_workspace_io.t ->
  clock:_ Eio.Time.Mono.t ->
  program:string list ->
  Mentat_tool.t
(** [make workspace_io ~clock ~program] is the immutable semantic-reference tool
    definition. It closes the workspace capability, monotonic clock, and
    boot-resolved Merlin program prefix for the definition's lifetime, and
    projects command confinement once. Construction performs no filesystem or
    process operation and does not warm, discover, or validate the executable.

    [program] is an argv prefix, for example [["ocamlmerlin"]] or a resolved
    wrapper prefix. The composition root owns resolving it before construction.

    Raises [Invalid_argument] if [program] is empty. *)
