(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Isolated temporary projects for complete-screen TUI tests.

    A project owns a workspace root and a sibling scratch tree containing its
    home and XDG directories. Values remain valid only during the callback
    passed to {!with_temp} or {!with_git_fixture}. The same environment fixture
    drives the in-process renderer through {!with_process_env} and real PTY
    children through {!env_array}. *)

(** {1:projects Projects} *)

type t
(** The type for a temporary test project. *)

val root : t -> string
(** [root project] is the absolute workspace root. *)

val path : t -> string -> string
(** [path project local] resolves [local] below the workspace root. *)

val scratch : t -> string -> string
(** [scratch project local] resolves [local] below the project-private scratch
    tree, which is outside the workspace. *)

val data : t -> string -> string
(** [data project local] resolves [local] below Mentat's test XDG data
    directory. *)

val state : t -> string -> string
(** [state project local] resolves [local] below Mentat's test XDG state
    directory. *)

val exists : t -> string -> bool
(** [exists project local] is [true] iff the workspace path [local] exists. *)

(** {1:files Files} *)

val write : t -> string -> string -> unit
(** [write project local contents] replaces the workspace file [local] with
    [contents], creating parent directories as necessary. *)

val read : t -> string -> string
(** [read project local] is the complete contents of the workspace file [local].
*)

val write_scratch : t -> string -> string -> unit
(** [write_scratch project local contents] replaces the scratch file [local]
    with [contents], creating parent directories as necessary. *)

val read_scratch : t -> string -> string
(** [read_scratch project local] is the complete contents of the scratch file
    [local]. *)

val write_path : string -> string -> unit
(** [write_path path contents] replaces the test-owned file at [path] with
    [contents], creating parent directories as necessary. *)

val read_path : string -> string
(** [read_path path] is the complete contents of the file at [path]. *)

(** {1:environment Environment} *)

type binding = string * string
(** The type for one process-environment assignment: a variable name and its
    value. *)

val bindings :
  ?openai_base_url:string ->
  ?unset:string list ->
  ?extra:binding list ->
  t ->
  binding list
(** [bindings project] is the deterministic process configuration for [project].
    It creates private home and XDG directories and assigns them, pins terminal
    rendering, reduced motion, the test model, the unrestricted sandbox, and
    disabled workspace tooling.

    When [openai_base_url] is present, the configuration also assigns the fake
    OpenAI credential and base URL. Later [extra] assignments replace defaults
    and OpenAI assignments with the same name. Names in [unset] are omitted
    regardless of other assignments. *)

val with_process_env : ?unset:string list -> binding list -> (unit -> 'a) -> 'a
(** [with_process_env bindings f] calls [f] with [bindings] installed in the
    current process environment. Later assignments with the same name win. Names
    in [unset], Dune RPC variables, and unassigned Mentat configuration or
    provider-credential variables known to the harness are absent during the
    call.

    The previous values of every variable controlled by this function are
    restored before returning, including when assignment or [f] raises.
    Variables changed independently by [f] are outside that restoration set.
    Because the process environment is global, calls must not overlap. *)

val env_array :
  ?openai_base_url:string ->
  ?unset:string list ->
  ?extra:binding list ->
  t ->
  string array
(** [env_array project] is the isolated environment passed to a child process.
    It contains {!bindings}, inherits unrelated variables from the current
    process, and excludes names in [unset], Dune RPC variables, and unassigned
    Mentat configuration or provider-credential variables known to the harness.
*)

(** {1:git Git fixtures} *)

val git : t -> string list -> unit
(** [git project args] runs Git with [args] in the workspace and a pinned test
    identity.

    Raises [Failure] with the command status if Git exits unsuccessfully or is
    terminated by a signal. *)

val git_baseline : t -> unit
(** [git_baseline project] initializes a Git repository and commits the current
    workspace as its baseline. *)

(** {1:lifecycle Lifecycle} *)

val with_temp : string -> (t -> 'a) -> 'a
(** [with_temp name f] calls [f] with a fresh project and removes the workspace
    and scratch trees before returning, including when [f] raises. The variable
    part of the generated path is a stable, executable-qualified token. It has
    the same width as [name], or eight characters when [name] is shorter. This
    keeps both complete and layout-truncated terminal paths deterministic
    without censoring the truncation under visual review, while allowing
    separate visual test executables to reuse a local fixture name.

    Raises [Invalid_argument] if [name] is empty or contains a path separator.
    Raises [Failure] if another instance of the same executable and fixture
    identity already owns the deterministic root, or if an earlier owner was not
    cleaned up. *)

val with_git_fixture : string -> (t -> 'a) -> 'a
(** [with_git_fixture name f] creates a temporary project containing a small
    committed baseline and calls [f project]. *)

val with_external_dune_watch : t -> (unit -> 'a) -> 'a
(** [with_external_dune_watch project f] runs [dune build --watch] for [project]
    while [f] executes, then stops and reaps the process, including when [f]
    raises. *)

(** {1:runtime Runtime fixtures} *)

val resolve_env_path : string -> string
(** [resolve_env_path name] is the absolute path stored in environment variable
    [name]. Relative values are resolved against the current directory.

    Raises [Failure] if [name] is absent or the resolved path does not exist. *)

val wait_for_file : string -> unit
(** [wait_for_file path] waits until [path] exists and is non-empty.

    Raises [Failure] if the file does not become non-empty within ten seconds.
*)
