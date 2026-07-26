(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Merlin-backed types at source positions.

    [Type_at] provides the stable ["ocaml_type_at"] model tool. It reports the
    inferred type at a concrete source position, optionally followed by outer
    enclosing types and the entity's odoc documentation. Position queries avoid
    the ambiguity of names under opens, shadowing, labels, constructors, and
    generated code.

    {1:input Input contract}

    Provider input is a strict JSON object with:

    - required ["path"], a non-empty, NUL-free relative or workspace-contained
      absolute source-file address;
    - required ["line"], a one-based source line;
    - required ["column"], a zero-based {e byte} column within that line;
    - optional ["max_enclosings"] in \[[1];[max_enclosings]\], defaulting to
      {!default_max_enclosings};
    - optional ["verbosity"] in \[[0];[max_verbosity]\], defaulting to [0];
    - optional ["documentation"], defaulting to [false].

    Every numeric member must be an exact integer in JSON's safe integer range.
    Fractional values, numeric strings, infinities, unsafe integers, unknown or
    duplicate members (including repeated required or optional names), an empty
    path, NUL, and values outside the documented bounds are rejected during call
    decoding, before permissions or I/O. Columns are byte offsets, not
    Unicode-scalar or grapheme offsets, matching OCaml compiler and Merlin
    locations.

    {1:source Source, project, and process semantics}

    The requested path must resolve to a regular file. The tool loads its
    complete contents through {!Mentat_workspace_io.File.load}, with the hard
    {!max_source_bytes} bound, and sends those exact bytes to Merlin on standard
    input. It never reads a build artifact, invokes a build or clean, discovers
    a program, inspects the ambient environment, or accesses a path outside the
    workspace capability.

    Each query runs from the root that owns the source path—not necessarily the
    primary workspace root. The capability supplies Merlin's physical
    [-filename] through {!Mentat_workspace_io.to_abs}; this is the only physical
    spelling exposed to the child. Consequently auxiliary roots and a workspace
    whose logical current directory is nested below its root retain the correct
    project context. Merlin and Dune decide what project metadata is visible
    under the sealed command policy; this tool does not build missing metadata
    or fall back to unconfined discovery.

    Every child invocation is the boot-resolved program prefix followed by
    [single], the Merlin command, and fixed arguments. It goes exclusively
    through {!Mentat_workspace_io.Command.run}, with a 30-second timeout,
    one-MiB capture bound per stream, cooperative cancellation, source stdin,
    and printer width 80. No raw process manager, shell, [Unix], [Sys], direct
    environment, or sandbox-lowering handle is retained here. Program warming
    and dev-tool resolution belong to the composition root's boot window.

    {1:stack Enclosing stack and query cost}

    The first [type-enclosing] invocation uses Merlin index [0]. It prints the
    innermost type and reveals the complete innermost-first range stack.
    Adjacent entries with equal ranges are deduplicated, keeping the first and
    preserving every survivor's original Merlin index. The requested limit is
    applied after this deduplication.

    Frame zero needs no second query. Every later returned frame costs exactly
    one additional [type-enclosing] invocation targeted at its preserved
    original index; the type string and range must agree with the primary stack.
    Thus a result with [n] frames uses exactly [n] type invocations. Requesting
    documentation adds exactly one [document] invocation after all frames.
    [verbosity = 0] deliberately omits Merlin's [-verbosity] flag, preserving
    Merlin's default; positive values pass the flag. Printer width is fixed at
    80 for deterministic type wrapping.

    Type text is Merlin's own printer output. Each frame is bounded to
    {!max_type_bytes} at the longest valid UTF-8 prefix. A successful response
    with no frames is [`Not_found]. A successful Merlin envelope has exactly one
    [class] and one [value] member; additional diagnostic members are ignored.
    Every frame has exactly [start], [end], [type], and [tail]. Each position
    has exactly [line] and [col], both exact JSON-safe integers satisfying
    OCaml's one-based-line and non-negative-byte-column invariants. [tail] is
    ["no"], ["position"], or ["call"]. Every frame returned to the caller
    resolves to a non-empty printed type string. Duplicate or missing protocol
    members, malformed coordinates or ranges, an absent or empty printed type, a
    changed targeted range, or an invalid response envelope is [`Failed] rather
    than partial evidence.

    {1:documentation Documentation}

    With ["documentation":true], one [document] query uses the same source, cwd,
    filename, and position. A returned string is available documentation unless
    it is one of Merlin's documented absence sentinels:
    ["No documentation available"], ["Not a valid identifier"], text starting
    ["didn't manage to find"] or ["Not in environment"], a builtin no-doc
    suffix, or a [" but could not be found"] suffix. A sentinel is retained as
    the unavailable reason.

    Documentation text and sentinel reasons are each bounded to
    {!max_documentation_bytes} at a valid UTF-8 boundary. A documentation query
    failure or non-string value is non-fatal and becomes a stable unavailable
    slot; cancellation is the sole exception and interrupts the complete tool
    call. No completed result can therefore imply that a requested documentation
    lookup was silently skipped.

    {1:output Durable output}

    Completed calls carry authoritative text and the compact
    {!Mentat_tools_output.Ocaml.Type_at} JSON projection: the bounded first
    type-expression line and the number of additional frames. JSON contains no
    query, source position, range, full frame, documentation, or backend data.

    A model-visible path at or below the capability's logical current directory
    is ["."] or relative to it. Any sibling, ancestor, or auxiliary-root path is
    its canonical capability absolute address. Query and frame paths use the
    same rule, so equal root-relative names cannot collide.

    Text preserves the established type-at presentation: a path-and-position
    header, one line per frame, an optional documentation line, then the
    backend. The semantic JSON, text, and output truncation bit are durable
    session data.

    {1:permissions Errors and cancellation}

    Permission planning is pure over decoded input. A resolvable path produces
    one request containing its read access and the shared
    ["command.confinement"] custom fact. The fact's subject is the stable
    {!Mentat_permission.Access.Command.execution_to_string} projection computed
    once from the immutable workspace capability when {!make} constructs the
    tool. The fixed Merlin argv and source are implementation details and are
    never represented as model-authored shell or argv permission text. An
    unresolvable path produces no speculative request and then fails execution.

    A missing source or empty type stack is [`Not_found]. An unresolved or
    non-regular source and a source over the complete-read bound are
    [`Invalid_input]. Filesystem I/O and malformed protocol are [`Failed]. A
    missing or refused Merlin launch is [`Unavailable], timeout is [`Timed_out],
    and non-zero exit, signal, supervision failure, either stream exceeding its
    independent one-MiB bound, or stdout or stderr remaining incomplete after
    child exit is [`Failed]. Process-supplied diagnostics are repaired to valid
    UTF-8, stripped of ANSI CSI and OSC sequences, and detail-bounded before
    becoming model-visible.

    Cancellation is checked before source observation, after the complete read,
    between every targeted frame query, and before documentation; the command
    boundary also polls it while each child runs. Once observed, the result is a
    cancelled interruption with no completed output. *)

val name : string
(** [name] is ["ocaml_type_at"]. *)

val default_max_enclosings : int
(** [default_max_enclosings] is [1]. *)

val max_enclosings : int
(** [max_enclosings] is [8], the largest accepted enclosing-frame request. *)

val max_verbosity : int
(** [max_verbosity] is [3], the largest accepted Merlin expansion depth. *)

val max_source_bytes : int
(** [max_source_bytes] is the eight-MiB complete source-read bound. *)

val max_type_bytes : int
(** [max_type_bytes] is the 4-KiB bound for each printed type. *)

val max_documentation_bytes : int
(** [max_documentation_bytes] is the 8-KiB bound for documentation text and
    unavailable sentinel reasons. *)

val make :
  Mentat_workspace_io.t ->
  clock:_ Eio.Time.Mono.t ->
  program:string list ->
  Mentat_tool.t
(** [make workspace_io ~clock ~program] is the immutable type-at tool
    definition. It closes the workspace capability, monotonic clock, and
    boot-resolved Merlin program prefix for the definition's lifetime, and
    projects command confinement once. Construction performs no filesystem or
    process operation and does not warm, discover, or validate the executable.

    [program] is an argv prefix, for example [["ocamlmerlin"]] or a resolved
    wrapper prefix. The composition root owns resolving it before construction.

    Raises [Invalid_argument] if [program] is empty. *)
