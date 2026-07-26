(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** One-shot [dune describe] project normaliser.

    This library turns the output of [dune describe] into the backend-neutral
    {!Mentat_ocaml.Project.t}. It supplies the argv a caller executes and
    decodes the bytes that caller collects; it spawns no process itself and
    links no effect library.

    It is separate from [Mentat_ocaml_dune_rpc] because the two Dune surfaces
    have nothing in common but the vendor: this one parses a plain s-expression
    stream from a command's stdout, while the RPC client speaks the
    length-prefixed protocol over a socket. Keeping them apart is what lets
    [mentat_tools] normalise a project description without linking [dune-rpc],
    [csexp], and [xdg]. *)

module Error = Error
(** Structured errors returned by the normaliser. *)

type prepare =
  cwd:Lpath.Abs.t ->
  argv:string list ->
  (string list * string array, string) result
(** A host-supplied process preparation boundary. [Ok (argv, env)] is the exact
    invocation and environment to execute; [cwd] is the working directory the
    adapter will pass to the process. [Error message] refuses the spawn. The
    preparation boundary owns the child environment rather than receiving an
    ambient candidate from the adapter. *)

val workspace_args : ?with_deps:bool -> ?recursive:bool -> unit -> string list
(** [workspace_args ()] is the [dune describe workspace] argv used by the
    adapter. [with_deps] defaults to [true]. [recursive] defaults to [true].

    The returned list includes [dune] and is suitable for permission checks and
    process execution. *)

val tests_args : ?context:string -> unit -> string list
(** [tests_args ()] is the [dune describe tests] argv used by the adapter.
    [context] selects a Dune build context when supplied. The returned list
    includes [dune]. *)

val of_workspace_output :
  workspace:Mentat_workspace.t ->
  string ->
  (Mentat_ocaml.Project.t, Error.t) result
(** [of_workspace_output ~workspace output] decodes
    [dune describe workspace --lang 0.1 --with-deps] output.

    Component dependency information is {!Mentat_ocaml.Project.Deps.Unknown}
    when Dune did not emit dependency fields and
    {!Mentat_ocaml.Project.Deps.Known} when it did.

    Repeated record fields and repeated singleton [root] or [build_context]
    items are rejected as ambiguous protocol data.

    Errors are {!Error.Parse_error}, {!Error.Path_error},
    {!Error.Duplicate_library_uid}, {!Error.Unknown_library_uid}, or
    {!Error.Invalid_state} depending on the malformed input. *)

val of_tests_output :
  workspace:Mentat_workspace.t ->
  Mentat_ocaml.Project.t ->
  string ->
  (Mentat_ocaml.Project.t, Error.t) result
(** [of_tests_output ~workspace project output] adds [dune describe tests]
    entries to [project].

    Existing project components are preserved. A test is linked to the first
    normalized component whose source directory equals the test's source
    directory, and remains unlinked when no component matches. Repeated record
    fields are rejected as ambiguous protocol data. *)

val of_outputs :
  workspace:Mentat_workspace.t ->
  workspace_output:string ->
  tests_output:string ->
  (Mentat_ocaml.Project.t, Error.t) result
(** [of_outputs] decodes and merges workspace and test describe outputs. *)
