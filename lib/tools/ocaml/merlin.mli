(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Internal one-shot transport for Merlin-backed tools.

    [Merlin] owns the common [ocamlmerlin single] process boundary. Callers
    provide the boot-resolved, non-empty program prefix and decode only the
    command-specific value returned by {!run}. Every invocation goes through
    {!Mentat_workspace_io.Command.run}; this module never discovers a binary,
    reads an environment, opens a filesystem path, or holds a process manager.

    The transport sends the complete source buffer on standard input, captures
    each output stream up to {!max_output_bytes}, and imposes
    {!timeout_seconds}. Successful stdout must be complete JSON with a Merlin
    envelope whose [class] and [value] members occur exactly once. Modern Merlin
    diagnostic members are accepted and ignored. A [return] envelope yields its
    value; [failure], [error], and [exception] are typed query failures.

    Model-visible process diagnostics are repaired to UTF-8, stripped of ANSI
    CSI and OSC sequences, trimmed, and bounded by {!max_detail_bytes}. The
    source, argv, and raw captured bytes never enter a tool result. *)

val timeout_seconds : float
(** [timeout_seconds] is the hard per-query timeout, [30]. *)

val max_output_bytes : int
(** [max_output_bytes] is the hard capture limit for each stream, one MiB. *)

val max_detail_bytes : int
(** [max_detail_bytes] is the maximum byte length of a repaired diagnostic,
    [4096]. *)

(** The type for one transport failure. *)
type error =
  | Cancelled  (** Cooperative cancellation stopped the child. *)
  | Unavailable of string
      (** The capability refused the launch or could not start the child. *)
  | Timed_out  (** The child exceeded {!timeout_seconds}. *)
  | Signaled of int  (** The child was terminated by the given signal. *)
  | Exited of { code : int; detail : string }
      (** The child exited non-zero. [detail] is bounded stderr, or bounded
          stdout when stderr is empty. *)
  | Output_exceeded of {
      stream : Mentat_workspace_io.Command.stream;
      limit : int;
    }  (** The named stream exceeded its capture [limit]. *)
  | Supervision_failed of string
      (** Process supervision failed after spawn. The string is a bounded
          diagnostic. *)
  | Incomplete_output of string
      (** A child exited successfully but a captured stream was incomplete. The
          string names the stream. *)
  | Query_failure of { class_ : string; detail : string }
      (** Merlin returned [failure], [error], or [exception]. [detail] is its
          bounded value. *)
  | Malformed of string
      (** Stdout was not a valid Merlin envelope, or the envelope's value did
          not satisfy the transport protocol. *)

val error_message : error -> string
(** [error_message error] is a valid UTF-8, ANSI-free diagnostic. Its
    process-supplied detail is bounded by {!max_detail_bytes}; callers branch on
    [error], not this rendering. *)

val run :
  Mentat_workspace_io.t ->
  clock:_ Eio.Time.Mono.t ->
  program:string list ->
  cwd:Mentat_workspace.Path.t ->
  command:string ->
  args:string list ->
  source:string ->
  cancelled:(unit -> bool) ->
  (Jsont.json, error) result
(** [run workspace_io ~clock ~program ~cwd ~command ~args ~source ~cancelled]
    invokes [program @ ["single"; command] @ args] from [cwd], with [source] as
    its standard input. It returns the value of a strict [return] envelope.

    [cancelled] is polled by the capability while the child runs. The child has
    a 30-second timeout and a one-MiB limit on each captured stream. A stopped
    child is [Error Cancelled]; a pre-spawn refusal is [Unavailable]; timeout,
    signal, non-zero exit, output overflow, supervision failure, incomplete
    output, and malformed protocol remain distinct errors.

    Raises [Invalid_argument] if [program] is empty. Empty [command] and NUL
    bytes are rejected by the capability's inseparable argv validation. *)
