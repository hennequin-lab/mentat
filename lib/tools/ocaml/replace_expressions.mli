(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Verified structural replacement over workspace OCaml implementations.

    [Replace_expressions] provides the stable ["ocaml_replace_expressions"]
    model tool. It finds syntactic expression patterns in workspace [.ml] files,
    renders a replacement template from the exact source bytes captured at each
    site, and accepts a file rewrite only after the complete edited file
    reparses and every replacement has the requested structure. It needs neither
    build artifacts nor a child process.

    {1:language Pattern and template language}

    [pattern] is one complete expression in {!Mentat_ocaml_grep.Pattern} syntax.
    The pattern language has these structural forms:

    - [__] matches any expression independently at each occurrence;
    - [__1], [__2], and other numbered names are unification metavariables, so
      repeated occurrences must match structurally equal expressions;
    - [f ?arg:PRESENT] and [f ?arg:MISSING] constrain optional arguments;
    - [match] clauses and record fields match as sets rather than by source
      order.

    A wildcard replaces an expression, not arbitrary OCaml grammar. For example,
    [match __ with Some x -> __ | None -> __] is complete, while
    [match __ with Some x] is invalid.

    [template] is one complete OCaml expression. Its numbered metavariables must
    be a subset of those bound by [pattern]. Every hole is replaced with the
    exact source bytes captured at that match, preserving formatting, comments,
    attributes, and literal spellings inside the fragment. [__], [PRESENT], and
    [MISSING] are pattern-only and are rejected in templates.

    Bytes outside matched ranges are preserved exactly, including existing LF or
    CRLF line endings. A leading UTF-8 BOM is passed to the OCaml parser
    unchanged. The parser rejects it, so the file is reported as a syntax-error
    skip and remains byte-identical.

    A template may not place a hole in the scope of a value binder introduced by
    that template. This rejects capture-prone forms under [let], [let rec],
    [fun] bodies, optional-parameter defaults following a value binder,
    [function]/[match]/[try] cases and guards, [for], binding operators, and
    local definitions. A hole in the right-hand side of a non-recursive [let],
    or in the first function parameter's default, is outside the introduced
    binder and is accepted.

    Rendering first splices fragments verbatim, then retries with the minimum
    parentheses needed to preserve the template AST. Non-atomic fragments,
    signed constants, and fragments carrying attributes are widened when
    necessary. The complete file is then reparsed. A replacement whose exact
    final range is absent or strip-differs from the template with captured ASTs
    substituted is retried with outer parentheses. If widening still cannot
    prevent precedence capture, token adjacency, or boundary reassociation, that
    whole file is skipped rather than approximated.

    {1:input Input contract}

    Provider input is a strict JSON object with:

    - required non-empty, NUL-free strings ["pattern"] and ["template"];
    - optional ["paths"], a non-empty array of non-empty, NUL-free relative or
      workspace-contained absolute paths. Omission means the capability's
      logical current directory;
    - optional ["max_sites"], a safe JSON integer in \[[1];[max_max_sites]\],
      defaulting to {!default_max_sites};
    - optional ["dry_run"], a boolean defaulting to [false].

    Duplicate or unknown members, fractional or unsafe JSON numbers, numeric
    strings, an explicitly empty paths array, and invalid bounds are rejected
    before permission planning or filesystem access. Pattern and template
    parsing is intentionally a run-time validation step and fails
    [`Invalid_input].

    A minimal request is:

    {[
      {"pattern":"List.rev __1 @ __2",
       "template":"List.rev_append __1 __2"}
    ]}

    {1:roots Roots, enumeration, and coverage}

    Each root must resolve to an existing non-symlink regular file or directory.
    An explicit regular file is considered regardless of suffix. A directory
    recursively contributes [**/*.ml] through {!Fs.Glob.Enumeration.paths},
    including its VCS exclusions and [.gitignore], [.ignore], and [.rgignore]
    semantics. No process is launched.

    Resolved duplicate roots and candidate files are considered once, keeping
    the first requested root spelling. Directory pages and the complete
    multi-root candidate stream are deterministic. The typed enumeration seam
    returns workspace paths directly; presentation output is not used as an
    internal protocol.

    Every model-visible path is a provider-resolvable unambiguous address. A
    path at or below the logical current directory is ["."] or relative to that
    directory. A sibling, ancestor, or path in another admitted root is
    represented by its canonical absolute address. Equal root-relative names in
    the primary and auxiliary roots therefore cannot collide.

    Candidate files are observed completely up to {!max_source_bytes}. Binary,
    invalid UTF-8, oversized, unreadable, syntactically invalid, unrenderable,
    and rewrite-unparsable files appear as path-and-reason skip summaries.
    Skipping one file does not suppress valid rewrites in other files. A valid
    file with no match produces no per-file output entry.

    [max_sites] bounds the total matched sites across every readable and
    parseable candidate, before rendering. Exceeding it fails the whole call and
    guarantees that this invocation did not request a write. It is not a
    returned-output page size.

    {1:application Preview and application}

    [dry_run = true] performs the complete discovery, render, whole-file parse,
    and structural verification pipeline but never calls
    {!Mentat_workspace_io.Edit.apply}. With [dry_run = false], all non-empty
    verified file rewrites are concatenated into one {!Mentat_edit.t} and passed
    to [Edit.apply] exactly once. An empty net plan also performs no apply.

    The edit plan carries every complete observed before-state, so a predictable
    conflict detected during the capability's preflight is [`Stale] and writes
    nothing. Application is serialized by the capability; each individual
    replacement commit is atomic and preserves permission bits. The capability
    is not a filesystem transaction and does not promise crash durability or
    rollback after an unforeseen later commit failure. Authoritative applied
    transitions, including any such partial outcome, belong to the surrounding
    claim scope—not this tool's output.

    Search observation accepts source files through eight MiB. Native edit
    application has an independent one-MiB complete-read boundary. Therefore a
    dry run can validate a matched file between one and eight MiB, while an
    applying call containing it is refused by [Edit.apply] before a predictable
    commit. This asymmetry is intentional and does not reduce search coverage.

    {1:output Output}

    A completed call returns human-readable text and compact semantic JSON with
    its applied/previewed disposition, changed-file count, aggregate line counts
    when known, and skipped-file count. Paths and per-file details remain solely
    in model-facing text.

    Output deliberately omits patterns, templates, roots, match counts, source
    ranges, captured fragments, before/after bytes, excerpts, diffs, content
    identities, edit results, receipts, and mutation evidence. Replay therefore
    cannot turn a tool result into write authority. Rich mutation rendering and
    revert data come only from the engine-owned mutation fact recorded from
    [Edit.apply].

    {1:permissions Errors and cancellation}

    Permission planning is pure over the decoded request. A preview requests one
    deduplicated read access per lexically resolvable root. An applying call
    requests one deduplicated modify item per resolvable root, with change
    evidence derived only from [pattern] and [template]. Invalid roots produce
    no speculative request and then fail during execution. The tool requests no
    command, clock, or network capability.

    Missing roots fail [`Not_found]. Outside-workspace, symlink, special-file,
    malformed pattern/template, and policy refusals fail [`Invalid_input].
    Enumeration and ordinary filesystem faults fail [`Failed]. A concurrent
    content change at apply time fails [`Stale]. Per-file read, parse, and
    safe-render failures are successful-result skip entries as described above.

    Cancellation is polled before validation, throughout root enumeration,
    between candidate searches and site renders, during whole-file verification,
    and immediately before application. Once observed it returns a cancelled
    interruption and does not publish a completed partial summary. Once the
    single protected [Edit.apply] begins, that capability boundary runs to its
    terminal result. *)

val name : string
(** [name] is ["ocaml_replace_expressions"]. *)

val default_max_sites : int
(** [default_max_sites] is the default global match bound, [200]. *)

val max_max_sites : int
(** [max_max_sites] is the greatest accepted explicit global match bound,
    [1_000]. *)

val max_source_bytes : int
(** [max_source_bytes] is the eight-MiB complete observation bound per candidate
    source. *)

val make : Mentat_workspace_io.t -> Mentat_tool.t
(** [make workspace_io] is the immutable structural-replacement tool backed by
    [workspace_io]. Construction performs no filesystem access. The returned
    definition composes typed {!Fs.Glob.Enumeration.paths} for directory
    discovery and otherwise uses only the capability's file and edit routes. *)
