(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Apply a complete Codex-style patch to workspace text files.

    [Apply_patch] accepts one strict JSON member, [patch], whose value is a
    complete document delimited by [*** Begin Patch] and [*** End Patch]. The
    document may add, update, delete, and move files. Paths use normalized
    root-relative syntax and are interpreted from the root containing the
    workspace's fixed current directory, not from that directory itself. Path
    header suffixes are raw after surrounding whitespace is trimmed: spaces need
    no quoting, and quotation marks have no escaping role. Absolute paths,
    escaping paths, empty documents, unknown or duplicate JSON members, and
    malformed hunks are rejected during input decoding.

    For example:

    {v
    {"patch":"*** Begin Patch\n*** Update File: lib/x.ml\n@@\n-let x = 1\n+let x = 2\n*** End Patch"}
    v}

    {1 Planning semantics}

    Operations are evaluated in document order against a virtual file map. An
    update therefore observes an earlier add or update in the same document; a
    delete followed by an add is a replacement; and a move chain retains its
    original source. Adds and move destinations require a missing target.
    Updates and deletes require text. A contradictory sequence reports both
    operation indices and kinds. Any pair of patch paths in an ancestor/child
    relationship is rejected before observation because a file transition and a
    nested transition cannot be applied coherently in one plan.

    Existing files are observed only through
    {!Mentat_workspace_io.Edit.observe}. The one-MiB complete-file bound is
    enforced by that native boundary and by this module for simulated final
    contents. Non-text targets, invalid UTF-8, likely-binary text, protected
    metadata, read-only roots, symlinked write paths, and out-of-workspace paths
    fail with the structured tool category selected from the underlying
    {!Mentat_edit.Error.t}. Missing update context reports the patch engine's
    exact chunk diagnostic.

    Updates preserve an existing UTF-8 byte-order mark. They select CRLF only
    for a target containing at least one CRLF and no bare LF; otherwise they
    select LF. Patch matching is then exact on the normalized LF view, and the
    selected target line ending is restored in the result. An add uses the
    parser-produced contents directly: patch-document CRLF delimiters become LF
    when add lines are parsed and joined, and no target line-ending style exists
    to inherit.

    {1 Permissions and effects}

    Permission planning is pure and syntactic. One request contains, in patch
    order, create access for an add, delete access for a delete, modify access
    for an ordinary update, and delete-source plus create-destination accesses
    for a move. Add contents and update hunk lines provide input-only
    {!Mentat_permission.Request.Change} values. Delete contents are deliberately
    unknown and are never read to manufacture permission evidence.

    The complete final virtual state is lowered to at most one stale-safe
    {!Mentat_edit} transition per path. A non-empty net plan is applied exactly
    once through {!Mentat_workspace_io.Edit.apply}; operations that cancel to an
    empty net plan succeed without minting empty mutation evidence. The boundary
    creates missing parent directories, re-observes every before state under the
    write lock, and rejects a concurrent preflight change as stale. Planning
    performs no writes. A later commit-time I/O failure may leave the confirmed
    prefix applied; the failed tool result carries no mutation data, while
    {!Mentat_workspace_io.Claim_scope} records that prefix and the uncertain
    target. The run callback polls cancellation before and throughout planning
    and immediately before application; once application begins, it is allowed
    to finish under the capability's commit-and-attribution boundary.

    A successful result retains no file contents, diff, edit result, receipt, or
    created-directory list. Its durable {!Mentat_tools_output.Update.t} contains
    only the applied disposition, net changed-file count, aggregate
    input-derived added and removed line counts, and zero skipped files. A total
    count is unknown if any contributing net entry has an unknown count. An
    observed move source never contributes observed byte or line counts. A
    create contributes the exact logical-line count of its final input-derived
    contents and zero removals; a delete contributes zero additions and unknown
    removals; and a delete-then-add replacement contributes the final
    input-derived line count with unknown removals. Hunk counts become unknown
    if bounded input-evidence rendering omits any lines. The durable projection
    contains no paths, operations, move sources, or observed complete-file diff.

    Net entries are ordered by first path observation, sequential operations
    collapse to their final semantic effect, and a move chain is one move from
    the original source to the final destination.

    Human text preserves the established compact path summary:

    {v
    Success. Updated the following files:
    modify lib/x.ml
    v}

    The corresponding durable projection has this shape:

    {v
    {"version":1,"disposition":"applied","files":1,
     "additions":1,"deletions":1,"skipped":0}
    v}

    The authoritative {!Mentat_edit.Result.t} returned by application is
    deliberately discarded by this tool and remains owned by
    {!Mentat_workspace_io.Claim_scope}. *)

val name : string
(** [name] is ["apply_patch"]. *)

val max_file_bytes : int
(** [max_file_bytes] is the one-MiB bound for every complete observed or
    simulated file. *)

val make : Mentat_workspace_io.t -> Mentat_tool.t
(** [make workspace_io] is the immutable [apply_patch] definition over
    [workspace_io]. Constructing it performs no filesystem I/O.

    Malformed documents, contradictory sequential operations, missing context,
    unsafe targets, and invalid final contents fail as [`Invalid_input]. A
    missing update or delete source fails as [`Not_found]. An observed text file
    whose contents change after planning fails as [`Stale]. A create or move
    destination that appears after it was observed missing fails as
    [`Invalid_input], preserving the edit layer's missing-target state-mismatch
    classification. Native I/O faults fail as [`Failed], and a cancellation
    observed before protected application returns a cancelled interruption. *)
