(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Staged, project-wide semantic OCaml rename.

    [Rename] provides the stable ["ocaml_rename"] model tool. It asks Merlin for
    every renaming occurrence of the entity at one source position, validates
    all returned spans against complete current sources, and applies one
    stale-safe multi-file edit. Equal spelling is never treated as semantic
    identity: there is no textual-search fallback.

    Provider input is a strict JSON object containing:

    - ["path"], a required non-empty, NUL-free relative or workspace-contained
      absolute source address;
    - ["line"], a required one-based source line;
    - ["column"], a required zero-based {e byte} column;
    - ["new_name"], a required non-empty, NUL-free replacement token;
    - ["dry_run"], optional and [false] by default;
    - ["max_occurrences"], optional, in \[[1];[max_occurrences]\], and
      {!default_max_occurrences} by default.

    Numeric members must be exact integers in JSON's safe-integer range.
    Fractions, numeric strings, infinities, unsafe integers, duplicate or
    unknown members, malformed positions, and the string violations above are
    rejected during call decoding, before permission planning or I/O. Columns
    are byte offsets, matching OCaml and Merlin source coordinates.

    The cursor may point anywhere within an ASCII OCaml identifier or directly
    after it. It must select an ordinary value, constructor, or module
    identifier. The replacement must contain only OCaml identifier characters,
    must not be a keyword, and must preserve the old token's lowercase-value or
    uppercase-constructor/module class. Operators, unsupported identifier forms,
    unchanged names, and cross-class renames are rejected. The source must be a
    regular [.ml] or [.mli] file admitted by the workspace capability and no
    larger than {!max_file_bytes}.

    {1:planning Merlin and source validation}

    Preparation reads the complete query source through
    {!Mentat_workspace_io.File} and makes exactly one
    [ocamlmerlin single occurrences] request. The immutable boot-resolved
    [program] prefix is followed by [single], [occurrences],
    [-identifier-at line:column], [-scope renaming], and [-filename] with the
    canonical physical query path. The query runs from the root owning the
    source, including an admitted auxiliary root; the logical workspace cwd and
    ambient process cwd do not change that project context.

    The shared Merlin transport is the only launch route. It calls
    {!Mentat_workspace_io.Command.run} with the exact source on standard input,
    a 30-second timeout, a one-MiB limit on each captured stream, and
    cooperative cancellation. The tool performs no shell invocation, environment
    lookup, executable discovery, build, clean, or index refresh.

    A successful transport value must be an array of exact occurrence objects.
    Each object has exactly [file], [start], [end], and [stale]; positions have
    exactly safe-integer [line] and [col] members. Missing, duplicate, unknown,
    ill-typed, reversed, or NUL-bearing protocol data fails the whole call.
    Empty filenames and Merlin's ["*buffer*"] sentinel denote the query source.
    Relative filenames resolve from the source-owning root. Every normalized
    path must belong to an admitted workspace root; an outside path is refused
    rather than emitted or edited.

    Any stale report refuses the complete rename before deduplication, so a
    fresh duplicate cannot conceal stale index evidence. Remaining locations are
    sorted by durable workspace path and range, equal locations are
    deduplicated, and the requested cap is applied to that semantic set. An
    empty set is a failure, not a successful no-op. Every distinct target and
    its spans consequently have deterministic strict order.

    Targets are observed through {!Mentat_workspace_io.Edit.observe}, which uses
    the native writer's no-follow traversal and one-MiB complete-text bound. The
    compiler parser must accept the original and rewritten complete source.
    Every span must hold exactly the old identifier at identifier boundaries;
    ranges outside the source or overlapping another occurrence are refused.
    Record-field puns and labelled or optional argument sites are detected from
    the parsed tree and refused because one token can denote two namespaces
    there.

    {1:staging Staging and mutation ownership}

    This is the library's one {!Mentat_tool.make_staged} definition. Its initial
    permission planner is pure over decoded input. A resolvable query produces
    one request containing read access to the complete owning root and the
    ["command.confinement"] fact projected once from the immutable workspace
    capability when {!make} constructs the definition. The root scope is
    necessary because exact project targets are unknowable before Merlin runs.

    Every validated target becomes part of a serializable private plan. The
    closed version-1 codec retains its durable {!Mentat_workspace.Path.t},
    provider-resolvable display address, strictly ordered occurrence spans, and
    before and after {!Mentat_digest.Content_ref.t} identities. It retains no
    source bytes, live {!Mentat_edit.t}, process, or filesystem handle. Decoding
    rejects an unsupported or non-integral version, invalid name pair, unsafe
    coordinates, empty or unordered targets or spans, duplicate paths or spans,
    unchanged content identities, identities beyond {!max_file_bytes}, an empty
    address or NUL, and a total above {!max_occurrences}. The staged waist
    additionally rejects non-canonical payload, description, input, or
    recomputed-permission drift on resume.

    A dry run completes directly from the same fully validated plan. It creates
    no {!Mentat_tool.Prepared.t}, asks for no final write authority, invokes no
    edit application, and records no mutation evidence. A non-dry prepare
    returns the durable plan. Its final permission function produces one
    {!Mentat_permission.Request.make} with exactly one [Modify] item per target
    and no other target. Item display names the target, but the request contains
    no {!Mentat_permission.Request.Change} manufactured from observed source.

    On approval or resume, execution never reruns Merlin. It re-observes every
    target, requires the complete bytes to match the prepared before identity,
    revalidates spans and syntax while rebuilding the after bytes, and requires
    those bytes to match the prepared after identity. It constructs one
    full-file rewrite per target, concatenates them, and calls
    {!Mentat_workspace_io.Edit.apply} exactly once. The edit boundary then
    rebinds and revalidates every complete before state again under its shared
    lock, closing the race between execution observation and commit.

    Native writes independently refuse read-only roots, protected metadata,
    symlink traversal, and paths rebound outside the workspace. Multi-file
    application is not transactional: an I/O failure may commit a prefix.
    Claim-scoped mutation facts are the sole owner of confirmed or uncertain
    changes, diffs, and revertability; tool output never acts as a receipt.

    {1:output Durable output}

    Authoritative text reports the old and new names, applied/previewed state,
    positive aggregate occurrence and file counts, backend, and one bounded line
    per target. Compact JSON is the closed version-1
    {!Mentat_tools_output.Ocaml.Rename} projection: disposition, occurrence
    count, and file count. Its constructor and JSON decoder reject zero or
    negative counts and a file count greater than occurrences. Names, paths,
    spans, identities, diffs, receipts, and mutation evidence are not duplicated
    in semantic JSON. The TUI derives its concise wording from those aggregate
    facts for live and replayed results.

    {1:errors Errors and cancellation}

    Invalid cursor/name combinations, caps, target kinds, boundaries, puns,
    overlaps, and simulated-size excesses are [`Invalid_input]. Missing sources
    or targets are [`Not_found]. A stale Merlin report, prepared content
    mismatch, or edit precondition conflict is [`Stale]. Refused or missing
    process launch is [`Unavailable]; timeout is [`Timed_out]. Malformed backend
    output, out-of-scope locations, semantic Merlin failure, source or
    after-rewrite parse failure, and native I/O failure are [`Failed]. Process
    and parser details are repaired to UTF-8, stripped of terminal control
    sequences, and bounded before becoming model-visible.

    Cancellation is checked before and after source observation, while Merlin
    runs, while targets are prepared, before returning a plan, and while the
    edit is reconstructed, and immediately before application. Once the single
    protected application begins it is allowed to finish, preserving
    commit-and-attribution ownership. *)

val name : string
(** [name] is ["ocaml_rename"]. *)

val default_max_occurrences : int
(** [default_max_occurrences] is [200], the default per-call rename bound. *)

val max_occurrences : int
(** [max_occurrences] is [1000], the largest accepted per-call rename bound. *)

val max_file_bytes : int
(** [max_file_bytes] is one MiB, the complete-source bound for each target file.
*)

val make :
  Mentat_workspace_io.t ->
  clock:_ Eio.Time.Mono.t ->
  program:string list ->
  Mentat_tool.t
(** [make workspace_io ~clock ~program] is the immutable staged rename tool. It
    closes the workspace capability, monotonic clock, and boot-resolved Merlin
    argv prefix. Construction performs no filesystem or process operation and
    does not validate or discover the executable.

    [program] is an argv prefix such as [["ocamlmerlin"]]; the composition root
    owns resolving it before construction. The constructor rejects an empty
    prefix. At execution, the command capability validates every argv token,
    including empty or NUL-bearing tokens, before launch.

    Raises [Invalid_argument] if [program] is empty. *)
