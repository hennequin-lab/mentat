(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Content-addressed replacement in one existing workspace text file.

    [Edit_file] is the small, targeted editor. It replaces exact UTF-8 text in
    an existing regular file through {!Mentat_workspace_io.Edit.apply}. Use
    [write_file] to create or replace a complete file, and [apply_patch] for a
    broad structural or multi-file edit.

    {1 Input contract}

    The model input is a strict JSON object with these members:

    - [path], a required non-empty workspace-relative or workspace-contained
      absolute path;
    - [old_string], required non-empty UTF-8 text copied from the target without
      line-number prefixes, anchors, or diff markers;
    - [new_string], required UTF-8 replacement text, which may be empty but must
      differ from [old_string];
    - [occurrence], optionally ["once"] (the default) or ["all"];
    - [if_identity], optionally the token from a complete [read_file]
      observation.

    Both text members must be valid, non-binary UTF-8. Unknown or duplicate
    members, malformed identities, and other schema violations are rejected
    before the run callback. For example, a minimal call has the shape:

    {v
    {"path":"lib/x.ml","old_string":"let x = 1\n","new_string":"let x = 2\n"}
    v}

    Input decoding and permission planning perform no filesystem I/O.

    {1 Matching and freshness}

    Matching is byte-exact after the newline and BOM rules below. Occurrences
    are counted from left to right and do not overlap. The default
    [occurrence = "once"] requires exactly one match. No match fails as
    [`Not_found]; more than one fails as [`Stale]. ["all"] replaces every
    non-overlapping match and still requires at least one.

    A supplied [if_identity] is checked against the complete current file before
    matching. A mismatch fails as [`Stale]. After a stale or ambiguous result,
    callers should re-read the file and retry with current, wider surrounding
    text. [occurrence = "all"] is appropriate only when every match is intended.

    {1 Newlines and byte-order marks}

    The legacy newline rule selects CRLF only when the target contains at least
    one CRLF pair and no bare LF; every other target selects LF. CR, CRLF, and
    LF sequences in both input strings are normalized to that selected style
    before matching and replacement. The target bytes themselves are not
    normalized. Consequently, in a mixed-ending target an [old_string] that
    spans a CRLF may not match after LF normalization; the caller must choose
    context that remains exact or rewrite the complete file.

    When the target starts with a UTF-8 BOM, one leading target BOM is excluded
    from matching and at most one leading BOM is stripped from each input
    string. Restoration prepends the target BOM only when the simulated content
    does not already start with one, so an input-provided leading BOM is not
    duplicated. Further leading BOM sequences remain ordinary content. A
    BOM-only [old_string] is rejected for such a target because it would become
    an empty pattern. When the target has no BOM, input strings are matched as
    supplied apart from newline normalization; callers should not introduce or
    remove a BOM in replacement text.

    {1 Effects, errors, and output}

    The target and simulated final contents are bounded by {!max_file_bytes}.
    Observation rejects invalid UTF-8, likely-binary or non-regular targets. The
    no-follow write boundary also refuses final and intermediate symlinks,
    protected workspace metadata, read-only roots, and paths outside the
    workspace. A missing target is [`Not_found]; malformed or non-text targets
    and oversized results are [`Invalid_input]. An identity or matching conflict
    and a target changed between observation and application are [`Stale]. I/O
    and confinement failures retain their structured tool failure category.

    A changed edit preserves an existing file's permission bits. If newline or
    BOM normalization makes the replacement byte-identical to the current file,
    the call succeeds as [unchanged] without invoking
    {!Mentat_workspace_io.Edit.apply}.

    Decoded calls request exactly one modify access for the resolved path. Its
    {!Mentat_permission.Request.Change} is the decoded [old_string] to
    [new_string] change, computed with [Change_evidence.modify] and without
    observing the target. An unresolved path produces no permission request.

    Successful output preserves the human [modify] or [unchanged] summary line.
    Its durable {!Mentat_tools_output.Update.t} carries only aggregate
    presentation facts: applied disposition, one changed file for a modification
    or zero for an unchanged result, the input-derived line counts, and zero
    skipped files. An omitted input diff makes its corresponding count unknown.
    The durable projection contains no path, file bytes, identity, edit result,
    receipt, or mutation evidence. {!Mentat_workspace_io.Claim_scope} owns the
    authoritative result returned by {!Mentat_workspace_io.Edit.apply}; the tool
    deliberately discards that result after successful application. *)

val name : string
(** [name] is ["edit_file"]. *)

val max_file_bytes : int
(** [max_file_bytes] is the 1 MiB bound on complete target and final contents.
    The native observation and application boundary independently enforces the
    same bound. *)

val make : Mentat_workspace_io.t -> Mentat_tool.t
(** [make workspace_io] is the immutable [edit_file] definition over
    [workspace_io]. Constructing it performs no I/O.

    The run callback polls cancellation before resolving or observing the target
    and again immediately before a changed edit is applied. An initial
    cancellation returns a cancelled interruption without filesystem access. A
    cancellation detected after observation prevents a changed edit; an
    unchanged result needs no application.

    Observation uses the native write boundary's no-follow seam. Application
    revalidates the observed contents under the edit lock, so a concurrent
    target change fails [`Stale] rather than being overwritten. Once application
    begins, {!Mentat_workspace_io.Edit.apply} completes under its protected
    commit-and-attribution boundary. *)
