(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Model-facing complete-file text writer.

    [Write_file] creates or atomically replaces one UTF-8 regular file through
    {!Mentat_workspace_io.Edit.apply}. The strict input object contains required
    string members [path] and [contents], plus an optional [if_identity] from a
    complete {!Read_file} result:

    - without [if_identity], the target must be missing;
    - with a matching [if_identity], an existing text file is replaced;
    - with [if_identity] and a missing target, the file is created;
    - with a stale [if_identity], no write is attempted.

    Missing parent directories are created inside the workspace. Replacement
    preserves an existing UTF-8 BOM when [contents] omits it, and the native
    write boundary preserves an existing file's permission bits. Atomic
    replacement publishes a fresh directory entry, so any other hard links keep
    their prior contents. Invalid UTF-8, likely-binary input, existing non-text
    targets, write-side symlinks, protected metadata, read-only roots, paths
    outside the workspace, and files over {!max_file_bytes} are refused.

    Decoded calls without [if_identity] request create access for the resolved
    path. Because the preserved create-or-replace contract cannot know the
    target's runtime state at decode, calls with [if_identity] atomically
    request both create and modify access. Each attached
    {!Mentat_permission.Request.Change} is derived only from the decoded
    [contents]; permission planning performs no file I/O.

    Successful output JSON contains only whether the write changed the file and,
    when it did, the logical line count of the complete written contents. The
    human summary remains authoritative for model use. Logical lines are newline
    bytes plus one for non-empty input without a final newline: empty input has
    zero lines, ["a\n"] has one, ["a\r\nb"] has two, and CRLF is therefore one
    line break rather than two.

    For example, the provider input

    {[
     {"path":"notes/todo.txt","contents":"first\nsecond\n"}
    ]}

    produces a durable summary of the following shape:

    {[
      {"version":1,"shape":"wrote","lines":2}
    ]}

    The output retains no file contents, identity, edit result, receipt, or
    mutation evidence: {!Mentat_workspace_io.Claim_scope} owns the authoritative
    result recorded by {!Mentat_workspace_io.Edit.apply}. *)

val name : string
(** [name] is ["write_file"]. *)

val max_file_bytes : int
(** [max_file_bytes] is the 1 MiB bound on supplied contents, complete
    replacement reads, and final contents after UTF-8 BOM preservation. *)

val make : Mentat_workspace_io.t -> Mentat_tool.t
(** [make workspace_io] is the immutable [write_file] definition over
    [workspace_io]. Constructing it performs no I/O.

    Unknown or duplicate input members are rejected. [path] must be non-empty,
    [contents] must be valid UTF-8 text, and [if_identity], when present, must
    be a valid {!Mentat_digest.Content_ref} token. A path that cannot be
    resolved produces no permission request and fails before filesystem access.

    Decode failures, invalid or unsafe paths, non-text targets, size refusals,
    and protected or read-only targets fail as [`Invalid_input]. A mismatched
    identity or a target changed during application fails as [`Stale]. Native
    filesystem faults fail as [`Failed]. Cancellation returns an interrupted,
    cancelled result. Diagnostics for an intermediate symlink or non-directory
    name the offending path component.

    The run callback polls cancellation once before resolving or observing the
    target. Supplied contents above {!max_file_bytes} are refused before target
    observation; a preserved BOM that would make the final contents exceed the
    bound is refused after observation and before application. Replacement
    observes through the native write boundary's no-follow seam; a later
    application revalidates the observed contents, so symlink or content races
    cannot turn an unchanged fast path into a write or identity oracle. A
    cancellation at the initial poll returns a cancelled interruption. Once
    native application begins, {!Mentat_workspace_io.Edit.apply} completes under
    its protected commit-and-attribution boundary. *)
