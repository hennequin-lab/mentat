(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Model-facing terminator for a background [shell] command.

    [shell_kill] takes a handle returned by a background [shell] call, signals
    the process group it leads and then the process itself (SIGTERM, a bounded
    grace, then SIGKILL), drains its final tail, and returns the final
    status.

    {1 Input contract}

    The strict input object has one member: [handle], required non-empty, a
    handle such as [bg_1]. It may not contain NUL; unknown and duplicate members
    are rejected.

    {1 Output and failures}

    A kill requests no permission — it signals an already-authorized process. It
    is idempotent: killing an already-settled handle drains nothing more and
    returns its recorded status ([Completed]). The authoritative text carries
    the handle, the final status, and the final tail of stdout and stderr
    (capped at {!max_tail_bytes} per stream, UTF-8 repaired, ANSI stripped). The
    compact JSON is [{ "handle", "status" }].

    An unknown handle — never started in this session, or minted by a prior
    engine that did not survive a restart — fails [`Not_found] naming the
    handle.

    The result states what the kill reached, narrowed to the case: a signalled
    process reports that its group went with it and names the two survivors the
    group signal misses — a worker that left the group, and one that ignores
    SIGTERM and outlives the process. A handle that had already settled reports
    that nothing was signalled at all. *)

val name : string
(** [name] is ["shell_kill"]. *)

val max_tail_bytes : int
(** [max_tail_bytes] is [65536], the most bytes of the final tail returned per
    stream. Older bytes are noted as rolled off, never dropped silently. *)

val make : Registry.t -> Mentat_tool.t
(** [make registry] is the immutable [shell_kill] tool terminating background
    processes in [registry]. Constructing it observes no path, spawns nothing,
    and projects no permission. *)
