(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Model-facing workspace text search.

    [Search_text] searches UTF-8 text through a confined [rg] child launched
    only by {!Mentat_workspace_io.Command}. Search roots are resolved by the
    workspace capability before the command starts; final symlink roots, missing
    paths, special files, and paths outside the workspace are refused. Execution
    requires [rg] on the capability's private [PATH], disables user ripgrep
    configuration, and passes query values as exact argument-vector tokens
    rather than shell-parsing them.

    The tool preserves three result modes: matching file paths, per-file
    matching-line counts, and matching lines with bounded context. Results are
    ordered by workspace path and line number and support one-based
    [offset]/[limit] pagination. Overlapping roots do not duplicate a logical
    matching file or line. Binary and non-UTF-8 files are named as skipped
    evidence instead of producing unsafe line text. Protected VCS metadata is
    excluded even when a query glob attempts to include it; ordinary dotfiles
    remain searchable and standard ignore files retain ripgrep's behavior.

    Durable output JSON contains only the mode-appropriate total: matching
    files, matching lines, or matches and their distinct file count. Queries,
    paths, source lines, skipped-file evidence, pages, and continuations remain
    solely in authoritative model-facing text. *)

val name : string
(** [name] is ["search_text"]. *)

val default_limit : int
(** [default_limit] is the result-entry budget used when [limit] is absent.
    Context lines do not consume this budget. *)

val max_limit : int
(** [max_limit] is the largest accepted explicit result-entry budget. *)

val max_context_lines : int
(** [max_context_lines] is the largest accepted symmetric context size. *)

val make : Mentat_workspace_io.t -> clock:_ Eio.Time.Mono.t -> Mentat_tool.t
(** [make workspace_io ~clock] is the immutable [search_text] tool definition.
    Constructing it starts no process and performs no filesystem access.

    The input is a strict JSON object with:

    - required non-empty string [pattern], interpreted as a Rust regular
      expression by ripgrep;
    - optional non-empty [paths] array of non-empty workspace-relative or
      workspace-contained absolute path strings, defaulting to the workspace
      current directory;
    - optional non-empty string [glob];
    - optional [mode], one of ["files"], ["count"], or ["matches"], defaulting
      to ["files"];
    - optional boolean [case_insensitive], defaulting to [false];
    - optional integer [context_lines] in \[[0];[5]\], valid only in ["matches"]
      mode and defaulting to [0];
    - optional one-based integer [offset], defaulting to [1];
    - optional integer [limit] in \[[1];[1000]\], defaulting to
      {!default_limit}.

    Numeric members must be JSON numbers representing exact integers in JSON's
    safe integer range; numeric strings and fractional values are rejected.
    Unknown members are rejected. Regular-expression and glob syntax errors are
    reported by the callback as [`Invalid_input].

    A decoded call requests read access once for each distinct root that
    resolves lexically. A malformed or outside root contributes no request and
    the run fails before spawning. The command is bounded to sixty seconds and
    each captured stream to 16 MiB. A returned source line is a valid UTF-8
    prefix of at most 2,000 bytes and is marked when truncated. Model-visible
    stderr diagnostics are additionally capped at 64 KiB and repaired to valid
    UTF-8. Cancellation is polled before each root observation, by the command
    supervisor, and while parsing its bounded output; a cooperative stop returns
    a cancelled interruption. Missing roots fail as [`Not_found], invalid root
    kinds and escaping paths as [`Invalid_input], timeout as [`Timed_out], and
    launch, supervision, or malformed command output failures as [`Failed]. *)
