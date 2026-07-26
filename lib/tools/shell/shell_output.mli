(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Model-facing incremental reader for a background [shell] command.

    [shell_output] takes a handle returned by a background [shell] call and
    returns the {e new} output since the model's last read of that handle — a
    monotonic per-handle cursor the registry owns, so the model never threads a
    cursor. The first read returns everything buffered.

    {b A read waits.} It returns the instant the process writes, exits, or the
    engine's cooperative stop fires, and otherwise when its wait budget expires.
    An immediate read is what makes a poll loop cheap, and a cheap poll loop is
    what a model spends its turn on; a read that costs time is one the model
    only makes when it needs the output.

    {1 Input contract}

    The strict input object has these members:

    - [handle], required non-empty, a handle such as [bg_1];
    - [filter], an optional non-empty Perl-compatible regular expression; only
      rendered output lines matching it are returned;
    - [wait_ms], an optional wait budget from {!min_wait_ms} to {!max_wait_ms},
      defaulting to {!default_wait_ms}. It is declared in the schema and
      enforced: a value outside the range is rejected, never clamped, so the
      model never reasons about a deadline the read did not have.

    String members may not contain NUL. Unknown and duplicate members are
    rejected.

    {1 Output and rendering}

    A read requests no permission — it reads an already-authorized process's
    buffer. Each stream is rendered from raw bytes: UTF-8 repaired (invalid
    sequences become [U+FFFD]), ANSI escapes stripped, then, when a [filter] is
    given, reduced to matching lines. A stream with more than {!max_read_bytes}
    of new bytes returns its {e tail} — the most recent bytes — and counts the
    skipped older bytes as dropped; the model polls again for more. Bytes the
    ring already rolled off the head below the cursor are likewise counted,
    never dropped silently. A multibyte UTF-8 sequence split across a cap or
    roll-off boundary renders as two [U+FFFD] — an accepted cosmetic cost of
    byte-exact cursors.

    The authoritative text carries the handle, the live status (running / exited
    N / signaled N / terminated), a rolled-off note when bytes were dropped, and
    the new stdout and stderr. An empty read of a still-running process names
    the budget it waited, because that is the fact that distinguishes it from
    the immediate empty read it used to be. The compact JSON is
    [{ "handle", "status", "new_bytes", "dropped" }].

    {1 Failures}

    An unknown handle — never started in this session, or minted by a prior
    engine that did not survive a restart — fails [`Not_found] naming the
    handle, without waiting. A malformed [filter] or an out-of-range [wait_ms]
    fails [`Invalid_input]. A stop observed during the wait ends it: the read is
    [`Interrupted] rather than an empty answer, unless it had already taken
    bytes off the cursor, which are reported instead of discarded. *)

val name : string
(** [name] is ["shell_output"]. *)

val max_read_bytes : int
(** [max_read_bytes] is [65536], the most new bytes one read returns per stream.
    A stream with more new bytes returns its tail and counts the skipped older
    bytes as dropped. *)

val default_wait_ms : int
(** [default_wait_ms] is [5000], the wait budget of a read that does not ask for
    one. *)

val min_wait_ms : int
(** [min_wait_ms] is [5000], the smallest accepted budget. There is no shorter
    read: the floor is the point. *)

val max_wait_ms : int
(** [max_wait_ms] is [300000], the largest accepted budget. *)

val make : Registry.t -> Mentat_tool.t
(** [make registry] is the immutable [shell_output] tool reading background
    process output from [registry]. Constructing it observes no path, spawns
    nothing, and projects no permission. *)
