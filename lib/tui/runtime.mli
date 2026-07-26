(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Production driver for the terminal application.

    [Runtime] is the sole impure boundary of the next TUI. It owns the Matrix
    terminal lifetime, the Mosaic loop, session and child feed fibers, and the
    interpretation of commands emitted by the internal application reducer.
    Session effects travel only through an injected {!Mentat_client.t};
    machine-local persistence and optional operating-system effects travel
    through {!Local.t}. The composition root itself never crosses this boundary.
*)

(** {1:local Executable-local effects} *)

module Local : sig
  type t
  (** The executable-local effects available to one TUI run.

      Prompt history is always present and best-effort. File enumeration,
      browser opening, local shell execution, and the external-editor escape are
      advertised to the internal application reducer exactly when their callback
      is supplied, so it cannot issue an effect the runtime did not receive.
      Clipboard copy remains an unconditional best-effort Mosaic command.
      Session auto-titling is not a reducer effect: the runtime itself invokes
      [auto_title] once, for a fresh session, just before its first prompt is
      submitted. *)

  val make :
    load_prompt_history:(unit -> string) ->
    append_prompt_history:(string -> unit) ->
    ?enumerate_files:(unit -> (Lpath.Rel.t list, Mentat_diagnostic.t) result) ->
    ?open_url:(Uri.t -> (unit, Mentat_diagnostic.t) result) ->
    ?run_local_shell:
      (cancelled:(unit -> bool) ->
      command:string ->
      (Mentat_tool.Output.t Mentat_tool.Result.t, Mentat_diagnostic.t) result) ->
    ?edit_in_editor:(text:string -> (string, Mentat_diagnostic.t) result) ->
    ?notify:(title:string -> body:string -> unit) ->
    ?auto_title:(session:Mentat_session.Id.t -> prompt:string -> unit) ->
    ?attach_image_path:(Lpath.Rel.t -> Mentat_workspace.Path.t) ->
    ?clipboard_image:(unit -> (string * string) option) ->
    ?attribute_session:(Mentat_session.Id.t option -> unit) ->
    unit ->
    t
  (** [make ... ()] is the local effect set supplied by the executable.

      Clipboard copy is owned directly by Mosaic and is therefore not a local
      callback. Persistence crosses this boundary as encoded bytes: the runtime
      owns the history codec and passes one history record without its trailing
      newline to [append_prompt_history]. Callbacks must contain their own IO
      failures and return an empty value, a structured error, or no-op as
      appropriate. [enumerate_files] returns the workspace's bounded,
      ignore-pruned recursive regular-file list in root-relative path order —
      the completable universe for one open [@] token; it and browser opening
      keep recoverable failures structured. [run_local_shell] receives the exact
      command and a cooperative [cancelled] predicate; it returns the same
      erased typed tool result used by durable shell calls, without exposing an
      executor to the TUI. [edit_in_editor] receives the fully-expanded
      plain-text draft and returns the edited buffer or a structured error; it
      owns temp-file creation, [$VISUAL]/[$EDITOR] resolution and spawn, and
      read-back, and does no terminal-mode work — the runtime brackets it with
      terminal suspend and resume. Callbacks may block; the runtime invokes them
      on Mosaic's effect fibers, never in the update or render path.
      [auto_title], when supplied, is invoked once per fresh session with that
      session's first prompt, inline on the admission fiber just before the
      prompt is submitted (submission would otherwise hold the session guard the
      rename needs); it owns generating and persisting the title, bounds its own
      latency, and contains its own failures, since the runtime never observes
      its result. [attach_image_path], when supplied, advertises image
      attachment and resolves a workspace-relative image path to the workspace
      path the client attach flow reads; [clipboard_image] probes the OS
      clipboard out-of-band for an image and returns its [(media_type, bytes)]
      or [None]. Both are executable-side workspace and spawn effects the pure
      App never performs.

      [attribute_session], when supplied, receives the active session's identity
      each time it changes — every attachment the runtime establishes, whether
      the session was opened fresh, resumed, forked, or rewound into, and [None]
      when the user detaches. Like [auto_title] it is not a reducer effect and
      advertises no capability: the runtime invokes it itself, from the one
      place it owns the active-session reference. It exists so machine-local
      diagnostics can attribute their lines and any crash report to the session
      that was live, which the executable cannot observe on its own, since a
      session opened inside the terminal never crosses the launch boundary. It
      is best-effort and its result is never observed, so it must contain its
      own failures. *)
end

(** {1:result Run result} *)

type outcome = { last_session : Mentat_session.Id.t option }
(** The outcome of one terminal lifetime. [last_session] is the session active
    at exit, or [None] when the user left before opening a session. *)

type error =
  | No_tty  (** A launch failure detected before terminal state is changed. *)

val error_message : error -> string
(** [error_message error] is the user-facing diagnostic for [error]. *)

val goodbye : palette:Theme.Palette.t -> color:bool -> outcome -> string
(** [goodbye ~color outcome] is the restored-terminal farewell for [outcome]. It
    includes a continuation command exactly when [outcome.last_session] is
    present. *)

(** {1:running Running} *)

val run :
  stdenv:Eio_unix.Stdenv.base ->
  client:Mentat_client.t ->
  startup:Startup.t ->
  local:Local.t ->
  reduced_motion:bool ->
  show_reasoning:bool ->
  overlay:Command.Overlay.t ->
  notify_policy:App.notify_policy ->
  palette:Theme.Palette.t ->
  theme_name:string ->
  themes:Theme.Preset.t list ->
  theme_auto:(Theme.Palette.t * Theme.Palette.t) option ->
  image_max_count:int ->
  mouse:bool ->
  ?matrix:Matrix.app ->
  ?probe:(Mosaic.Probe.t -> unit) ->
  unit ->
  (outcome, error) result
(** [run ~stdenv ~client ~startup ~local ~reduced_motion ~show_reasoning
     ~overlay ~notify_policy ()] runs the complete TUI until the user quits.

    [reduced_motion] is fixed for the terminal lifetime and suppresses the Home
    animation when [true]. [show_reasoning] controls whether reasoning progress
    is initially rendered. [overlay] is the launch-fixed keybinding overlay,
    resolved from the user's keybindings file over the registry defaults
    ({!Command.Overlay}). [mouse] is launch-fixed: when [true] the default
    backend captures the mouse in SGR button mode (press, release, drag, wheel);
    when [false] no capture is requested and the terminal keeps native selection
    and scroll. The executable must resolve every launch policy before crossing
    this boundary; [run] does not read configuration or the process environment
    for them.

    Read-only reducer requests run on effect fibers through the injected
    client's typed query seam. Their completions preserve the reducer's exact
    request identity; provider-owned model-readiness snapshots cross without a
    Runtime projection or executable-local fallback.

    [matrix] and [probe] are advanced driver seams for complete-screen
    integration tests. A supplied [matrix] is driven directly under Mosaic's
    normal stop semantics and bypasses the interactive-terminal check; otherwise
    [run] requires supported stdin and stdout terminals and creates an
    alternate-screen Matrix backend over [stdenv]. A supplied [probe] receives
    Mosaic's live probe before the loop starts. Its checks close over runtime
    state and must only be queried while [run] is active.

    The runtime observes a fresh session before admitting its first prompt,
    replays resumed and forked sessions from the beginning, and continuously
    drains every committed fact and progress update. A replacement records its
    initiating request as the desire before blocking, while the admitted feed
    remains live until the candidate has followed and completed attach-time
    recovery inspection. A stale candidate closes silently; a failed current
    candidate leaves the admitted session usable. Every delivered main-feed
    message carries its exact session and initiating request, so an already
    queued callback cannot mutate the reducer after replacement. Detach retires
    both the desire and the admitted feed.

    An unexpected feed closure or invalid position is retried at most once; both
    outcomes consume the same reconnect allowance and use the same origin rule.
    Recovery resumes strictly after the last committed position, or from
    [`Beginning] only when the feed has not delivered a committed fact. A
    populated reducer therefore never rewinds and cannot duplicate facts it has
    already accepted. A second closure or position failure is terminal. The
    client feed contains pull exceptions into that same terminal error channel.
    Terminal feed loss preserves the accepted transcript and session identity,
    but clears the reducer's observation identity and refuses prompts until an
    explicit resume succeeds.

    Prompt-history attribution is captured before asynchronous effects begin: a
    first prompt is bound to the exact synchronously minted session, while later
    drafts carry the reducer's explicit session choice. Runtime-owned feed and
    login cleanup is cancellation-protected and exception-contained; login start
    and pull exceptions are returned through the structured authentication
    failure path.

    At most one executable-local shell callback is current. A replacement
    cooperatively cancels its predecessor, cancellation is correlated by the
    opaque reducer request, and a completion is delivered only while its exact
    request remains current. Callback-domain failures remain typed tool results;
    callback-boundary failures use the structured capability-failure path.

    Returns [Error No_tty] without changing terminal state when the default
    backend cannot be used. Client and capability failures are folded into the
    application as structured messages and do not terminate the Mosaic loop. *)
