(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The mentat daemon: lifecycle, discovery, instancing, and attach.

    The composition-root machinery behind [mentat serve] and [--attach], in one
    place (multi-consumer: [cli_serve], [cli_run], [cli_tui], [cli_session]). It
    owns the {b path policy} for the per-user daemon (the socket directory under
    [/tmp] keyed by the store, and the [daemon.json]/[daemon.lock]/[daemon.log]
    home under the data home), the foreground {b serve body} (claim → bind →
    write discovery → an instance registry serving a session-routing composite
    driver → signalled teardown), and the client-side {b find-or-spawn} that
    attaches to — or launches — the daemon.

    It sequences {!Composition.stage_shared} once, then one
    {!Composition.instance} per workspace over it (the per-user vs per-workspace
    split); [mentat.server] provides the wire, [composition] the instances, and
    this module the registry and process lifecycle. No dependency cycle:
    [cli_* → daemon → composition]; [composition] never names [daemon]. *)

val serve :
  socket_override:string option ->
  spawned:bool ->
  web:bool ->
  web_port:int option ->
  Exit_status.t
(** [serve ~socket_override ~spawned ~web ~web_port] runs the foreground daemon
    and blocks until a signal stops it. It stages the shared per-user state,
    takes the [daemon.lock] claim (returning a {!Exit_status.Runtime_error}
    "already running" when it is held — the serialisation that collapses racing
    starts to one), binds the unix socket (default [/tmp/mentat-<uid>-<key>],
    overridden by [socket_override] = the [--socket] flag), writes [daemon.json]
    atomically, and serves. The registry get-or-boots a workspace instance per
    connection and hands the wire a composite driver whose session-cone calls
    route by session id to the owning instance, evicting idle instances by the
    three-zeros rule.

    [web] additionally binds a loopback listener serving the [mentat.web]
    browser frontend behind [Mentat_server.Web]'s shared edge (the cookie
    exchange, the [Origin]/[Host] checks, the strict CSP), over the daemon's own
    working-directory workspace. [web_port] pins the loopback port ([None] takes
    an ephemeral one); the URL to open — carrying the bootstrap token — is
    printed to stdout. A web frontend whose client cannot assemble is a loud
    warning that skips the browser listener, leaving the wire serving.

    [spawned] is the hidden [--spawned] flag: when set the daemon calls [setsid]
    at startup so it outlives the terminal. A first SIGTERM/SIGINT stops
    accepting, clears the discovery file, shuts every instance down
    durable-first, and exits 0.

    The test-only environment variable [MENTAT_DAEMON_MAX_IDLE] (seconds) stops
    the daemon after that many continuous seconds with zero bound connections,
    so a background daemon a blackbox test spawns cannot outlive the test. *)

val stop : unit -> Exit_status.t
(** [stop ()] stops the running per-user daemon: it reads [daemon.json], and —
    if the claim is held (the daemon is live) — sends the recorded pid a SIGTERM
    and waits, bounded, for the claim to release. A free claim with a stale
    [daemon.json] is unlinked. No daemon running is a clean success-shaped no-op
    (idempotent stop); a wedged daemon that never releases is a
    {!Exit_status.Runtime_error} naming the pid. Uses no store — it resolves the
    directories from the environment and speaks only the claim and the signal.
*)

val is_running : User_dirs.t -> bool
(** [is_running dirs] is whether a per-user daemon is live for [dirs]: its
    discovery file is present {b and} its claim is held. A present file whose
    claim is free is a dead daemon (a stale file), so [false]. The offline
    session verbs consult it to add the "re-run with --attach" hint to a [Busy];
    it takes and immediately releases the claim, so it is safe to call from a
    non-daemon process. *)

val find_or_spawn :
  Composition.t -> (Mentat_client.Driver.t, Exit_status.t) result
(** [find_or_spawn t] returns a driver attached to the per-user daemon for [t]'s
    workspace, spawning the daemon if none is reachable. It reads [daemon.json];
    a {b live} daemon (its socket answers a handshake) whose recorded binary or
    config home differs from [t]'s is a loud {!Exit_status.Runtime_error} naming
    [mentat serve --stop] (never auto-killed); a matching live daemon is
    attached. A stale file (free claim, dead socket) or an absent file spawns
    [mentat serve --spawned] detached — stdio to [daemon.log], the current
    environment inherited so the daemon opens the same store — then polls
    discovery on a fixed cadence (re-read + re-connect every ~50 ms up to a
    bounded budget, then one full retry) until it answers. The returned driver
    is handed to {!Mentat_client.make}; everything downstream is
    transport-neutral. *)
