(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Merlin-backed OCaml definition lookup at source positions.

    [Find_definitions] provides the stable ["ocaml_find_definitions"] model
    tool. It locates the definition, declaration, or inferred type definition of
    the entity at a concrete source position. Merlin performs semantic
    resolution, so results respect opens, includes, aliases, labels,
    constructors, and shadowing rather than guessing from textual matches.

    {1:input Input contract}

    Provider input is a strict JSON object with:

    - required ["path"], a non-empty, NUL-free relative or workspace-contained
      absolute OCaml source-file address;
    - required ["line"], a one-based source line;
    - required ["column"], a zero-based {e byte} column within that line;
    - optional ["identifier"], a non-empty, NUL-free Merlin locate prefix;
    - optional ["kind"], one of ["definition"], ["declaration"], or
      ["type-definition"], defaulting to ["definition"].

    ["definition"] asks Merlin to prefer an implementation and ["declaration"]
    asks it to prefer an interface. ["type-definition"] asks for the definition
    of the type inferred at the cursor and cannot be combined with
    ["identifier"]. Omitting ["identifier"] asks Merlin to reconstruct the
    identifier under the cursor. For example:

    {[
     {
       "path": "lib/parser.ml",
       "line": 42,
       "column": 17,
       "kind": "declaration"
     }
    ]}

    Every numeric member must be an exact integer in JSON's safe integer range.
    Fractional values, numeric strings, infinities, unsafe integers, unknown or
    repeated members, invalid bounds, empty strings, NUL, and the incompatible
    ["identifier"]/["type-definition"] pair are rejected during call decoding,
    before permissions or I/O. Columns are bytes rather than Unicode scalar or
    grapheme indexes, matching OCaml compiler and Merlin positions.

    {1:source Source and command semantics}

    The requested path must resolve to a regular file. The tool reads its
    complete contents through {!Mentat_workspace_io.File.load}, with the hard
    {!max_source_bytes} bound, and supplies those exact bytes to Merlin on
    standard input. It never reads the source through a host filesystem API,
    invokes a build or clean, discovers a program, or inspects the ambient
    environment.

    Merlin runs from the root that owns the source path, including when that is
    a read-only auxiliary root rather than the primary root. The capability
    supplies the canonical physical source spelling for [-filename]. A logical
    workspace cwd nested below that root does not change Merlin's project cwd.
    Merlin and Dune determine which existing project metadata is visible under
    the sealed command policy; this tool neither generates metadata nor retries
    through an unconfined route.

    Each call makes exactly one child invocation. ["definition"] and
    ["declaration"] use [ocamlmerlin single locate] with
    [-look-for implementation] or [-look-for interface]; ["type-definition"]
    uses [ocamlmerlin single locate-type]. The immutable boot-resolved [program]
    prefix precedes that argv. The invocation goes exclusively through
    {!Mentat_workspace_io.Command.run} via the shared Merlin transport, with
    source stdin, a 30-second timeout, a one-MiB bound on each captured stream,
    and cooperative cancellation. No shell, raw process manager, [Unix], [Sys],
    environment, or sandbox-lowering handle is used.

    {1:targets Target semantics and normalization}

    A successful Merlin object names one source position and optionally a file.
    An omitted file or Merlin's ["*buffer*"] sentinel denotes the queried
    source. Relative target files are resolved against the source-owning root;
    absolute targets are lexically normalized. Targets admitted by the workspace
    are classified as workspace locations; all others remain external absolute
    locations. Merlin's ["Already at definition point"] sentinel returns the
    query source and cursor and marks the project index as not applicable.

    Target objects and their nested position objects are protocol-checked for
    exact member sets, duplicate members, safe integers, valid coordinates,
    non-empty paths, and NUL. Malformed or ambiguous protocol values fail the
    call; they never become partial findings. Targets are normalized, sorted,
    and deduplicated by class, canonical address, and position before output.
    The current Merlin command yields at most one target, but that invariant
    keeps the durable target-list order independent of backend ordering.

    Workspace addresses at or below the capability's logical current directory
    are ["."] or relative to it. A workspace sibling, ancestor, or auxiliary
    target is rendered with its canonical capability absolute address. External
    targets are normalized absolute addresses. Every workspace address in the
    result is therefore provider-resolvable, and identical root-relative names
    in different roots cannot collide.

    {1:output Durable output}

    A completed call preserves the established text contract:

    {v
    OCaml definitions: 1
    - lib/parser.ml:7:4-7:4
    index_status: unknown
    v}

    Workspace targets render as point ranges; external targets render as
    [path:line:column]. A target returned by [locate] or [locate-type] has
    [index_status: unknown], because Merlin's command may consult project data
    whose freshness its CLI does not expose. A cursor already at its definition
    has [index_status: not_applicable]. Lookup misses are [`Not_found] failures,
    not successful empty lists.

    Completed calls also carry the compact
    {!Mentat_tools_output.Ocaml.Definition} JSON projection: the canonical
    provider-facing path and one-based line. The path may identify a workspace
    or external target; JSON contains no query, column, range, backend, index
    status, source bytes, command bytes, build data, capability, or mutation
    evidence.

    Text and semantic JSON are durable session data. The finite source-read and
    per-stream process bounds also bound all backend-derived data entering a
    result; this tool performs no unbounded output query.

    {1:permissions Errors and cancellation}

    Permission planning is pure over the decoded input. A lexically resolvable
    path produces one request containing its read access and the shared
    ["command.confinement"] custom fact. The fact's subject is the stable
    {!Mentat_permission.Access.Command.execution_to_string} projection computed
    once from the immutable workspace capability when {!make} constructs the
    tool. Fixed Merlin argv and source bytes are implementation details and are
    not exposed as model-authored shell or argv permissions. An unresolvable
    path produces no speculative request and is rejected by execution.

    A missing source is [`Not_found]. An unresolved or non-regular source and a
    source over {!max_source_bytes} are [`Invalid_input]. Filesystem I/O,
    malformed Merlin output, an invalid target path, non-zero exit, signal,
    output overflow, incomplete capture, or supervision failure are [`Failed]. A
    missing or refused Merlin launch is [`Unavailable], timeout is [`Timed_out],
    and Merlin's not-found, invalid-context, builtin, and unavailable-source
    messages are [`Not_found]. Process-supplied diagnostics are repaired to
    valid UTF-8, stripped of ANSI control sequences, trimmed, and bounded before
    becoming model-visible.

    Cancellation is checked before source observation, after the complete read,
    while Merlin runs, and before lowering its response. Once observed, the
    result is interrupted with reason ["tool call cancelled"] and
    [cancelled=true], with no completed or partial output. *)

val name : string
(** [name] is ["ocaml_find_definitions"]. *)

val max_source_bytes : int
(** [max_source_bytes] is the eight-MiB complete source-read bound. *)

val make :
  Mentat_workspace_io.t ->
  clock:_ Eio.Time.Mono.t ->
  program:string list ->
  Mentat_tool.t
(** [make workspace_io ~clock ~program] is the immutable definition-lookup tool
    definition. It closes the workspace capability, monotonic clock, and
    boot-resolved Merlin program prefix for the definition's lifetime, and
    projects command confinement once. Construction performs no filesystem or
    process operation and does not warm, discover, or validate the executable.

    [program] is an argv prefix, for example [["ocamlmerlin"]] or a resolved
    wrapper prefix. The composition root owns resolving it before construction.

    Raises [Invalid_argument] if [program] is empty. *)
