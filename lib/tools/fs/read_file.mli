(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Model-facing workspace text reader.

    [Read_file] reads a UTF-8 regular file or lists one directory through
    {!Mentat_workspace_io.File}; it performs no filesystem access by any other
    route. File text is rendered with one-based line numbers. [offset] and
    [limit] select lines, and [max_bytes] bounds returned UTF-8 file bytes. A
    byte cap that splits a well-formed UTF-8 scalar omits only that incomplete
    suffix; malformed selected text is rejected rather than repaired. Rendering
    preserves at most 2,000 bytes from each line, shortened to a valid UTF-8
    prefix and followed by the marker [" [line truncated]"] when necessary. A
    ranged page whose line limit leaves more content carries a progress-making
    [next:] call; byte-capped partial lines do not, because advancing by a line
    would skip their undisplayed suffix. Directories are VCS-metadata-filtered,
    sorted by kind and name, and paged by the same [offset]/[limit] input
    members. A contained symlink naming the requested file or directory is
    followed; child symlinks in a listing are classified without following them.
    Names that {!Mentat_workspace_io.File.read_dir} cannot represent as
    workspace paths are omitted. Explicit budgets cannot exceed
    {!max_file_bytes} and {!max_directory_limit}; continuations preserve the
    effective budgets.

    These are hard output bounds: a file result contains at most
    [max_file_bytes] selected content bytes before line-number rendering, and a
    directory page contains at most [max_directory_limit] entries. They do not
    bound total filesystem observation. A directory is still enumerated before
    paging, and reaching a requested line offset may scan earlier file bytes.

    Complete file reads include a content identity over the exact file bytes;
    display-only CR stripping and per-line truncation do not affect it. When
    conditional reads are enabled at construction, [if_identity] returns an
    unchanged result if it matches that identity; it is rejected in ranged,
    byte-capped, and directory requests. The default declaration excludes
    [if_identity], preserving the production model surface. [max_bytes] is
    accepted but ignored for a directory request.

    The durable JSON projection contains only the result shape and returned
    file-line or directory-entry count. File contents, directory entries, paths,
    identities, continuations, and paging detail remain solely in the
    authoritative model-facing text. No output contains a receipt, mutation
    evidence, fingerprint, or edit anchor.

    Output truncation is [true] whenever model-visible text omits data because
    of [max_bytes], the per-line display bound, or directory pagination. An
    explicitly selected file range is not itself truncation when every selected
    byte is displayed. *)

val name : string
(** [name] is ["read_file"]. *)

val max_file_bytes : int
(** [max_file_bytes] is [1_048_576] (1 MiB), the greatest accepted explicit
    [max_bytes] budget. It is a bound on selected file-content bytes, not on
    line-number and header rendering overhead or on bytes scanned before a
    requested line range. *)

val default_max_bytes : int
(** [default_max_bytes] is [65_536] (64 KiB), the returned-file byte budget used
    when the input omits [max_bytes]. *)

val max_directory_limit : int
(** [max_directory_limit] is [1_000], the greatest accepted explicit [limit].
    Since the target kind is observed only after decoding, the same maximum
    applies to a file-line limit and a directory-entry limit. It bounds returned
    entries, not the number enumerated before paging. *)

val default_directory_limit : int
(** [default_directory_limit] is [200], the entry budget used for a directory
    request whose input omits [limit]. *)

val make :
  ?conditional_read:bool ->
  ?image_max_bytes:int ->
  ?image_max_dimension:int ->
  Mentat_workspace_io.t ->
  Mentat_tool.t
(** [make ?conditional_read ?image_max_bytes ?image_max_dimension workspace_io]
    is the immutable [read_file] tool definition over [workspace_io].
    Constructing it performs no I/O.

    [image_max_bytes] and [image_max_dimension] cap an image the read returns as
    media: the executable resolves them from the [image.max_bytes] and
    [image.max_dimension] configuration the attach path honors, and they default
    to those fields' defaults. A read has no process-spawn boundary, so it
    cannot downscale an over-cap image; it rejects it with an instructive
    message.

    The input is a strict JSON object with:

    - required string [path], resolved relative to the capability's logical
      workspace current directory or as a workspace-contained absolute path;
    - optional one-based integer [offset], defaulting to [1];
    - optional integer [limit] from [1] through {!max_directory_limit};
    - optional integer [max_bytes] from [0] through {!max_file_bytes}, applying
      only to files;
    - optional string [if_identity] when [conditional_read = true].

    [offset] is capped at the lesser of JSON's maximum safe integer
    (9,007,199,254,740,991) and [Int.max_int]. The schema publishes the smaller
    product maxima above for [limit] and [max_bytes]. Inputs above either
    product maximum fail during decode, before permission planning or filesystem
    I/O.

    Member names must be unique. [conditional_read] defaults to [false], in
    which case [if_identity] is an unknown input member. With
    [conditional_read = true], [if_identity] must be a valid
    {!Mentat_digest.Content_ref} token and cannot be combined with [offset],
    [limit], or [max_bytes].

    Decoded calls request read access to the resolved path. Invalid or outside
    paths produce no permission request and fail without filesystem access.
    Missing paths fail as [`Not_found]; malformed paths, escaping symlinks,
    files detected as likely binary, malformed selected UTF-8 text, special
    files, and [if_identity] on directories fail as [`Invalid_input]; filesystem
    I/O failures fail as [`Failed]. The run polls cancellation before
    observation and while scanning bytes or classifying directory entries,
    returning a cancelled interruption when requested.

    A continuation is constructed through the same validated input contract: a
    file continuation preserves its effective [limit] and explicit [max_bytes],
    and a directory continuation preserves its effective [limit]. *)
