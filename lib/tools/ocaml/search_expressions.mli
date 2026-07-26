(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Syntactic structural search over workspace OCaml sources.

    [Search_expressions] provides the stable ["ocaml_search_expressions"] model
    tool. It matches one OCaml expression pattern against the parse trees of
    workspace [.ml] files through {!Mentat_ocaml_grep}. Matching is syntactic:
    identifiers are compared as written, not as resolved. The search therefore
    needs neither build artifacts nor a child process and works on unbuilt and
    mid-refactor code.

    {1:patterns Pattern language}

    A pattern is one complete OCaml expression in {!Mentat_ocaml_grep.Pattern}
    syntax. Wildcards replace expressions but do not relax the surrounding OCaml
    grammar:

    - [__] matches any expression independently at each occurrence;
    - [__1], [__2], and other numbered wildcards are unification metavariables,
      so repeated occurrences must match the same expression;
    - [f ?arg:PRESENT] and [f ?arg:MISSING] constrain whether an optional
      argument occurs;
    - clauses of [match] expressions and fields of record expressions match as
      sets rather than by source order.

    For example, [List.map (fun __1 -> __) __2] finds calls whose first argument
    is a one-argument function. A match pattern must still contain complete
    clauses, such as [match __ with Some x -> __ | None -> __];
    [match __ with Some x] is invalid because [__] cannot stand for the missing
    arrow and expression.

    Patterns are decoded as non-empty, NUL-free strings and parsed when the tool
    runs. A syntax error is an [`Invalid_input] result whose diagnostic explains
    the complete-expression rule. An expression that parses but uses unsupported
    pattern syntax is also [`Invalid_input].

    {1:input Input contract}

    Provider input is a strict JSON object with these members:

    - ["pattern"] is the required pattern string;
    - ["paths"] is an optional non-empty array of non-empty, NUL-free path
      strings. Each path may be workspace-relative or an absolute path inside an
      admitted workspace root. Omission means the logical workspace current
      directory, not necessarily the primary root;
    - ["offset"] is an optional safe JSON integer greater than or equal to [1].
      It is the one-based first finding and defaults to [1];
    - ["limit"] is an optional safe JSON integer from [1] through {!max_limit}.
      It defaults to {!default_limit}.

    Unknown or repeated members, fractional numbers, numeric strings, integers
    outside JSON's exactly represented range, an explicitly empty ["paths"]
    array, and invalid bounds are rejected by call decoding before permissions
    or execution. A minimal request is:

    {[
     {"pattern":"List.map __ __"}
    ]}

    A paged, scoped request is:

    {[
     {
         "pattern":"match __ with Some x -> __ | None -> __",
         "paths":["lib","bin/main.ml"],
         "offset":101,
         "limit":100
       }
    ]}

    {1:roots Search roots and enumeration}

    Every requested root is resolved against the workspace's fixed logical
    current directory and must exist as a non-symlink regular file or directory.
    A regular-file root is searched even when its name does not end in [.ml]. A
    directory root recursively contributes files matching [**/*.ml], honors
    [.gitignore], [.ignore], and [.rgignore] rules, and excludes VCS metadata
    directories. Directory enumeration uses the same semantics as {!Fs.Glob} and
    never launches a process.

    Resolved duplicate roots are searched once, preserving the first requested
    position. Files are path-sorted within each directory root, then
    deduplicated across roots while preserving the first root's position. The
    final finding stream is sorted globally by workspace path and source range,
    so paging is deterministic.

    The implementation composes {!Fs.Glob.Enumeration.paths} directly. The typed
    seam returns workspace paths rather than invoking the public tool and
    decoding presentation JSON, while preserving Glob's ignore and VCS
    semantics. Enumeration adds no process or command capability.

    {1:coverage Coverage and findings}

    A candidate source is loaded completely with a hard {!max_source_bytes}
    bound. Binary files, invalid UTF-8, oversized files, files that disappear or
    become unreadable, and syntactically invalid OCaml implementations
    contribute a structured ["skipped"] entry instead of disappearing from
    coverage. A readable, valid implementation increments ["searched_files"]
    even when it has no matches. Thus [searched_files + skipped_count] is the
    number of enumerated candidates.

    Every root, finding, and skipped entry has a provider-resolvable durable
    address. A path at or below the logical workspace current directory is ["."]
    or relative to that directory; a sibling, ancestor, or path in another
    admitted root is its canonical absolute address. This keeps primary and
    auxiliary files with the same root-relative name distinct without retaining
    a workspace capability in output.

    Each finding contains that address and exact one-based start and end lines
    and zero-based byte columns from {!Mentat_ocaml.Location}. It also contains
    every complete source line spanned by that location. A trailing carriage
    return is removed. A source line longer than 2,000 bytes is cut at the
    longest valid UTF-8 prefix no longer than that bound and carries
    ["truncated":true]. This tool deliberately has no line-anchor rendering
    mode; its output carries no anchors.

    All matches are collected before finding pagination. ["total_results"] is
    therefore the complete match count, ["returned_results"] is the page length,
    and a page is partial exactly when another finding follows it. A partial
    page returns a canonical next input with each resolved root's first accepted
    provider spelling, the same pattern and limit, and
    [offset + returned_results]. Retaining that spelling preserves logical-cwd
    and auxiliary-root identity when the continuation is decoded again. An
    offset beyond the end returns a complete empty page.

    {1:output Output}

    Completed calls carry authoritative text plus compact semantic JSON with
    only the exact match count and distinct matching-file count. Patterns,
    roots, locations, source lines, skipped-file evidence, pagination, and
    continuations remain solely in the model-facing text.

    The text projection starts with:

    {v
    ocaml_search_expressions pattern="PATTERN" results=R/T offset=O limit=L status=STATUS searched_files=N
    v}

    It then renders either [No matches] or each location followed by its
    numbered source lines, then any [skipped:] coverage section, and finally a
    copy-pasteable [next: ocaml_search_expressions JSON] line when partial. JSON
    string encoding is used for the pattern in the header. A bounded line ends
    in [[truncated]].

    Text, compact JSON, and the output truncation bit are durable session data.

    {1:permissions Errors and cancellation}

    {!make} requests one read permission for each lexically resolvable input
    root. It requests no command, network, clock, or write capability. Invalid
    paths that cannot be resolved contribute no speculative permission request
    and are rejected by execution.

    Missing roots produce [`Not_found]. A root outside workspace scope, a
    symlink root, an unsupported root kind, or an invalid pattern produces
    [`Invalid_input]. Filesystem faults while resolving or enumerating roots
    produce [`Failed]. Per-file read and parse failures are successful-result
    coverage evidence, not whole-search failure.

    Cancellation is checked before pattern parsing, during recursive Glob
    enumeration, and between candidate files. Once observed it returns an
    interrupted result with reason ["tool call cancelled"] and [cancelled=true];
    it never publishes a completed partial search. *)

val name : string
(** Stable model tool name, ["ocaml_search_expressions"]. *)

val default_limit : int
(** Default maximum number of findings in one page, [100]. *)

val max_limit : int
(** Maximum accepted explicit finding limit, [1_000]. *)

val max_source_bytes : int
(** Maximum complete source size searched, eight MiB. Larger sources are
    reported as skipped with reason ["too_large"]. *)

val make : Mentat_workspace_io.t -> Mentat_tool.t
(** [make workspace_io] is the immutable structural-search tool definition
    backed by [workspace_io]. Construction performs no I/O. Running a decoded
    call uses only the workspace file route, including typed
    {!Fs.Glob.Enumeration.paths} composition; it never uses the command route.
*)
