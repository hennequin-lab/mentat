(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Fresh one-shot Dune project descriptions.

    [Dune_describe] provides the stable ["ocaml_dune_describe"] model tool. It
    asks Dune for the complete workspace component graph and described tests,
    normalizes both through {!Mentat_ocaml.Project}, and returns a bounded
    human-readable project summary plus compact durable counts.

    {1:input Input contract}

    Provider input is exactly an empty JSON object. Unknown or repeated members
    and non-object values are rejected during call decoding, before permission
    planning or effects. The project root is not provider-selectable: it is the
    root that owns the workspace capability's fixed logical current directory.

    {1:execution Dune and project semantics}

    Every call performs two sequential one-shot invocations from that owning
    root:

    {v
    program @ [ "describe"; "workspace"; "--root"; "."; "--with-deps" ]
    program @ [ "describe"; "tests"; "--root"; "." ]
    v}

    The workspace query retains Dune's recursive default and includes external
    dependencies. The tests query augments the same normalized project rather
    than replacing its components. Both outputs are decoded by
    {!Mentat_ocaml_dune_describe.of_outputs}; compilation units, implementation
    and interface paths, component requirements, test ownership, enabled state,
    packages, source locations, and build context therefore retain the shared
    Dune adapter's validation and normalization semantics.

    [program] is supplied by the host to {!make} and is normally [["dune"]] or a
    boot-resolved wrapper prefix. Both children run exclusively through
    {!Mentat_workspace_io.Command.run} with full capture and an independent
    {!command_timeout_seconds} timeout. This module never invokes a shell,
    discovers an executable, reads an ambient environment, holds a process
    manager, lowers a sandbox, or opens a filesystem path directly. The
    capability supplies its private environment and binds the logical root to
    its already-opened physical root.

    Each call is fresh. The tool neither uses Dune RPC nor retains a boot
    snapshot, project value, watch endpoint, or drift flag across calls. It does
    not build, clean, start, stop, or bypass a Dune watch. A process already
    holding Dune's build lock can therefore make a describe command fail.

    {1:paths Path and provenance semantics}

    Decoding reconstructs a logical single-root workspace using the current
    root's stable key, canonical capability address, and logical cwd. Relative
    Dune paths bind beneath that root. Absolute workspace paths are imported
    only when the shared adapter can prove they belong to it. Paths outside the
    root are omitted from optional component and compilation-unit fields, while
    dependency identities remain normalized. Equal root-relative names cannot be
    mistaken for another capability root because the command and decoder are
    both rooted in the current path's owning root.

    Model-visible paths use {!Mentat_workspace.Path.display}, preserving the
    established Dune-root-relative spellings such as ["."] and ["lib"]. The
    compact JSON intentionally carries no paths; selected project provenance
    remains in authoritative text, while the complete normalized value exists
    only while the call is being encoded.

    {1:output Durable output}

    Completed text preserves the established summary:

    {v
    OCaml Dune project
    root: .
    build_context: _build/default
    components: 3 local=2 external=1
    tests: 1

    local components:
    - local library parser (id=...)

    tests:
    - parser_test: test/parser_test.exe in test (enabled)
    v}

    At most {!max_display_items} local components and tests are rendered in
    their normalized order. A larger group ends with its exact omitted count.
    External components contribute to the aggregate line but are not listed.
    This intentional summary has no continuation; exceeding either display cap
    sets the output truncation bit. The complete summary is repaired to valid
    UTF-8 and shortened at a valid boundary to {!max_output_bytes}; reaching the
    byte bound also sets the truncation bit.

    Durable JSON is the closed version-1 {!Mentat_tools_output.Ocaml.Project}
    projection: total normalized component count and described-test count. It
    contains no component name, path, dependency, compilation unit, source
    location, build context, command transcript, sandbox value, capability,
    process handle, mutation fact, edit receipt, checkpoint, or freshness
    evidence. Text and semantic JSON are the only data retained for replay.

    {1:bounds Bounds, failures, and cancellation}

    Each command has its own 30-second wall-clock bound. Because describe is a
    one-shot command whose output must parse whole, both streams use complete
    {!Mentat_workspace_io.Command.All} capture rather than a byte cap;
    successful parsing never consumes a truncated stream. Process and parser
    diagnostics are repaired to valid UTF-8, stripped of ANSI control sequences,
    trimmed, and bounded by {!max_detail_bytes} before becoming model-visible.

    A missing executable, sandbox refusal, or pre-supervision I/O failure is
    [`Unavailable], except invalid argv or an unknown logical cwd root, which is
    [`Invalid_input]. Timeout is [`Timed_out]. Non-zero exit, signal, output
    overflow, incomplete capture, supervision failure, or malformed Dune output
    is [`Failed]. A workspace-context projection failure is also [`Failed]. No
    failure carries a partial project output.

    Cancellation is checked before root projection, between commands, after the
    second command, and after decoding. It is also polled while each child runs.
    Once observed, the result is interrupted with reason ["tool call cancelled"]
    and [cancelled=true], with no output. Parent-fiber cancellation remains Eio
    cancellation after command cleanup and is not converted to a tool result.

    {1:permissions Permissions}

    Permission planning is pure over the empty decoded input. It produces one
    request containing read access to the current owning root and the shared
    ["command.confinement"] custom fact. The fact's subject is the stable
    {!Mentat_permission.Access.Command.execution_to_string} projection computed
    once from the immutable workspace capability when {!make} constructs the
    tool. Fixed Dune argv is an implementation detail and is not exposed as
    model-authored command permission. If the current root cannot be projected,
    planning emits no speculative request and execution fails before launch. *)

val name : string
(** [name] is ["ocaml_dune_describe"]. *)

val command_timeout_seconds : float
(** [command_timeout_seconds] is the hard per-command timeout, [30]. *)

val max_detail_bytes : int
(** [max_detail_bytes] is the maximum byte length of a repaired model-visible
    process or parser diagnostic, [4096]. *)

val max_display_items : int
(** [max_display_items] is the maximum number of local components and the
    maximum number of tests listed in completed text, [20] for each group. *)

val max_output_bytes : int
(** [max_output_bytes] is the 64-KiB maximum for completed authoritative text.
    Shortening preserves a valid UTF-8 prefix. *)

val make :
  Mentat_workspace_io.t ->
  clock:_ Eio.Time.Mono.t ->
  program:string list ->
  Mentat_tool.t
(** [make workspace_io ~clock ~program] is the immutable Dune-description tool
    definition. It closes the workspace capability, monotonic clock, and
    boot-resolved Dune program prefix for the definition's lifetime, and
    projects command confinement once. Construction starts no process, reads no
    file, and performs no project discovery.

    [program] is an argv prefix whose first token replaces the ["dune"] token in
    {!Mentat_ocaml_dune_describe.workspace_args} and
    {!Mentat_ocaml_dune_describe.tests_args}; additional tokens are preserved
    before the describe arguments.

    Raises [Invalid_argument] if [program] is empty or contains an empty or
    NUL-bearing token. *)
