(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The per-session background-process registry behind the [shell] family's
    [run_in_background], [shell_output], and [shell_kill] surfaces.

    A registry names background {!Mentat_workspace_io.Command.Session} handles
    ([bg_1], [bg_2], …), enforces the {!max_concurrent} bound, and stores each
    live session with the reader's incremental cursor. It is bound at creation
    to a switch — the driver's nested per-session switch — under which every
    process spawns: releasing that switch kills and reaps all of them
    leader-only, so a session's background processes die when the session
    closes.

    The registry is {e ephemeral runtime state}: it holds no durable facts and
    is never persisted. A resumed session starts with an empty registry, so a
    handle minted by a prior engine {!read} or {!kill}s to [None] — the honest
    "not running in this session" answer. Handle strings are the only durable
    trace, and they live in the tools' model-visible output, not in a fact.

    Not thread-safe by design: the engine drives one tool callback at a time
    under its serialized drain, and every registry operation runs in that single
    cooperative Eio domain. *)

type t
(** A per-session background-process registry. *)

val max_concurrent : int
(** [8] — the most background processes one session may have {e running} at
    once. Exited and killed handles stay queryable but do not count against the
    bound, so a slot frees as soon as its process settles. *)

val create : sw:Eio.Switch.t -> t
(** [create ~sw] is an empty registry whose processes spawn under [sw] and die
    leader-only on its release. The driver owns [sw] (its nested per-session
    switch, held across turns and released in [Driver.close]'s teardown); the
    registry never releases it. *)

(** {1 Start} *)

type started = {
  handle : string;  (** the minted [bg_N] handle *)
  pid : int;  (** the direct child's process id *)
}

(** Why a background start did not spawn a process. *)
type start_error =
  | At_capacity of { running : string list }
      (** {!max_concurrent} processes are already running; [running] names them,
          oldest first. No process was started and no handle was minted. *)
  | Refused of Mentat_workspace_io.Command.Error.t
      (** The sandboxed spawn refused before any child ran — an unknown cwd
          root, a sandbox lowering refusal, a missing program, or launch I/O.
          Same pre-spawn taxonomy as {!Mentat_workspace_io.Command.run}. *)

val start :
  t ->
  Mentat_workspace_io.t ->
  label:string ->
  ?cwd:Mentat_workspace.Path.t ->
  string list ->
  (started, start_error) result
(** [start t workspace_io ~label ?cwd argv] mints the next [bg_N] handle, spawns
    [argv] as a supervised background child under the registry's switch (via
    {!Mentat_workspace_io.Command.start_session} over [workspace_io], the
    tool-side spawn over the sealed capability), and registers it. [label] is
    the human command retained for a future glance. It fails [At_capacity] when
    {!max_concurrent} processes are already running — checked before any spawn,
    so no handle is minted — and [Refused] on a pre-spawn refusal. *)

(** {1 Poll and kill}

    Both return the primitive's incremental
    {!Mentat_workspace_io.Command.Session.chunk}: the {e raw} bytes appended to
    each stream since the previous {!read} or {!kill} of this handle, the count
    rolled off the ring head, the live status, and the process age. The registry
    owns the cursor and advances it; the chunk's own cursor field is inert to
    the caller. UTF-8 repair, ANSI stripping, any per-read cap, and line
    filtering are the calling tool's concern, never the registry's. *)

val read :
  t -> handle:string -> Mentat_workspace_io.Command.Session.chunk option
(** [read t ~handle] is the output appended since the last {!read} or {!kill} of
    [handle], advancing the stored cursor, with the live status. It is [None]
    when [handle] names no process in this session — never started, or minted by
    a prior engine (the honest resume answer). *)

val kill :
  t -> handle:string -> Mentat_workspace_io.Command.Session.chunk option
(** [kill t ~handle] signals the process (SIGTERM→SIGKILL, leader-only), drains
    its final tail, advances the cursor, and returns the final chunk.
    Idempotent: killing an already-settled handle drains no more and returns its
    recorded status. [None] for an unknown handle, as {!read}. *)

(** {1 Glance} *)

(** A registered process for the between-turns reminder and the side pane. Its
    fields live in a submodule so they do not collide with {!started}. *)
module View : sig
  type t = {
    handle : string;
    command : string;  (** the [label] given to {!start}, for display *)
    status : Mentat_workspace_io.Command.Session.status;
    since : Mtime.Span.t;  (** the process age, monotonic *)
  }
end

val running : t -> View.t list
(** [running t] is the processes currently
    {!Mentat_workspace_io.Command.Session.Running}, in mint order (oldest handle
    first) — the datum for the engine-authored between-turns reminder and the
    side pane's running section. A settled process drops out on its next
    observation. *)
