(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Trusted operation facts for permission review.

    An access is an inert description of one operation the host intends to run
    on behalf of the agent. Constructing an access does not perform the
    operation, and authorizing an access does not grant a runtime capability.

    Access values are trusted claims. They should be produced by the host code
    that already decoded and normalized the operation it will run, not by the
    model text that requested it.

    Workspace paths are supplied as {!Mentat_workspace.Path.t} values. Command
    and network values are compared by the strings supplied by the host. This
    module does not resolve symlinks, normalize Unicode, fold case, parse shell
    text, resolve DNS, or infer default network ports.

    Access constructors return private variants so callers can inspect facts by
    pattern matching while construction still enforces invariants. Constructors
    raise [Invalid_argument] when their arguments cannot satisfy the documented
    invariants. The JSON codec reports the same invalid states as decode errors.

    {b Warning.} Permission is not sandboxing. Filesystem confinement, process
    execution, network enforcement, path normalization, and shell parsing belong
    to the host interpreter. *)

(** {1:access_facts Access facts} *)

type kind = [ `Read | `Write | `Command | `Network | `Custom ]
(** The type for coarse access kinds.

    [`Custom] is for caller-defined accesses that do not fit a built-in kind. *)

type path_op = [ `Read | `Create | `Modify | `Delete ]
(** The type for filesystem operations. [`Read] has kind [`Read]; the other
    operations have kind [`Write]. *)

type network_protocol =
  [ `Http | `Https | `Ssh | `Tcp | `Udp | `Other of string ]
(** The type for caller-normalized network protocols.

    Constructors and JSON decoders accept [`Other p] only when [p] is non-empty.
*)

(** {1:path_scopes Path scopes} *)

module Path_scope : sig
  (** Classified path scopes. A workspace scope carries the unified durable
      {!Mentat_workspace.Path.t} identity and is never resolved to an absolute
      filesystem path by this library.

      [Outside_workspace] means the caller proved the path is outside the
      workspace. [Unknown] means the caller did not classify the path.

      Scope constructors return private variants so callers can inspect scopes
      without bypassing validation. *)

  type t = private
    | Workspace of Mentat_workspace.Path.t
        (** A path proven by the caller to be inside a workspace. *)
    | Outside_workspace of Lpath.Abs.t
        (** A path proven by the caller to be outside the workspace. *)
    | Unknown of string  (** A path whose workspace relation is unknown. *)

  val workspace : Mentat_workspace.Path.t -> t
  (** [workspace path] is a scope for a path proven inside a workspace.

      The scope is derived from the path's workspace root and root-relative
      syntax. Display text belongs to request-item metadata. *)

  val outside_workspace : Lpath.Abs.t -> t
  (** [outside_workspace path] is a scope proven outside a workspace. *)

  val unknown : string -> t
  (** [unknown path] is a scope whose workspace relation is unknown.

      Raises [Invalid_argument] if [path] is empty. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] contain the same path scope. *)

  val to_string : t -> string
  (** [to_string scope] is human-readable path text. Workspace paths include
      their root key; the result is not stable serialization. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf scope] formats [scope] for diagnostics. *)
end

(** {1:commands Commands} *)

module Command : sig
  (** Command execution facts.

      [Shell] preserves ambiguous shell text. [Argv] is a structured process
      invocation with no shell interpretation. [Code] is language source
      evaluated as a process. All are command facts; policy decides how to match
      them.

      {b Confinement is a permission-owned mirror, not a sandbox dependency.}
      This inert claim is populated by the tool bridge that projects a
      {e sealed} sandbox route into it. The mirror is deliberate for two
      reasons. It keeps [permission] free of any type shared with [sandbox]:
      neither library names the other's confinement representation. And it is a
      coarse summary — the read, write, and network scopes below — not the
      sealed policy: projecting to this shape keeps a sandbox's absolute host
      roots out of the durable permission facts a decision is journaled into.
      Permission grants no runtime capability from it. *)

  module Confinement : sig
    (** The filesystem and network authority enforced for a command. *)

    type read = Project | All  (** The command's readable filesystem scope. *)

    type write =
      | Read_only
      | Workspace  (** The command's writable filesystem scope. *)

    type network = Restricted | Enabled  (** The command's network scope. *)

    type t = { read : read; write : write; network : network }
    (** The type for an enforced command boundary. *)
  end

  type execution =
    | Enforced of Confinement.t
    | External
    | Direct
        (** The runtime route the command fact describes.

            [Enforced confinement] is a trusted claim that the exact operation
            executes through the sealed sandbox with [confinement]. [External]
            records a user-selected boundary that is not verified here. [Direct]
            makes no confinement claim. *)

  type t = private
    | Shell of { text : string; cwd : Path_scope.t; execution : execution }
    | Argv of {
        program : string;
        args : string list;
        cwd : Path_scope.t;
        execution : execution;
      }
    | Code of {
        language : string;
        source : string;
        cwd : Path_scope.t;
        execution : execution;
      }

  val shell : cwd:Path_scope.t -> execution:execution -> string -> t
  (** [shell ~cwd ~execution text] is shell command execution of [text] in [cwd]
      through [execution].

      Raises [Invalid_argument] if [text] is empty. *)

  val argv :
    cwd:Path_scope.t ->
    execution:execution ->
    program:string ->
    string list ->
    t
  (** [argv ~cwd ~execution ~program args] is execution of [program] with [args]
      in [cwd] through [execution].

      [args] does not include [program]. Empty arguments are valid process
      arguments.

      Raises [Invalid_argument] if [program] is empty. *)

  val code :
    cwd:Path_scope.t -> execution:execution -> language:string -> string -> t
  (** [code ~cwd ~execution ~language source] is evaluation of [source] as
      [language] code in [cwd] through [execution].

      Raises [Invalid_argument] if [language] or [source] is empty. *)

  val execution : t -> execution
  (** [execution command] is the command's claimed runtime route. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff both commands contain the same facts. *)

  val execution_to_string : execution -> string
  (** [execution_to_string execution] is [execution]'s stable configuration and
      diagnostic spelling. *)

  val execution_jsont : execution Jsont.t
  (** [execution_jsont] maps execution identities to their canonical tagged JSON
      object. Enforced identities include every confinement dimension. *)

  val stable_text : t -> string
  (** [stable_text t] is the canonical digest-input byte string for command [t],
      embedded verbatim in {!Access.stable_text}. It includes the execution
      route ({!execution_to_string}), so two commands that differ only in
      claimed route have distinct identity. Same stability contract as
      {!Access.stable_text}: length-framed, byte-stable for the current schema,
      and one-way — never decoded. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. The output is not stable storage
      syntax. *)
end

(** {1:access_values Access values} *)

(** The type for access facts.

    Private constructors expose the trusted facts for inspection. Build values
    with the constructors below; do not treat the variants as proof that the
    underlying operation has been normalized. A command's execution field is a
    host-produced claim about the route that operation will use; the variant
    alone grants no capability.

    Invariant: path [root_key], outside-workspace path, unknown path, command
    text, command programs, code languages and sources, unknown [cwd] text,
    network protocol names, hosts, custom access names, and subjects are
    non-empty when present. Command execution route participates in access
    identity. Ports are in \[[1];[65535]\]. Construction does not resolve
    symlinks, normalize Unicode, fold case, parse shell text, resolve DNS, or
    infer default network ports — normalization is the host's job. *)
type t = private
  | Path of { op : path_op; scope : Path_scope.t }  (** Filesystem access. *)
  | Command of Command.t  (** Command execution. *)
  | Network of { protocol : network_protocol; host : string; port : int option }
      (** Network access. *)
  | Custom of { name : string; subject : string option }
      (** Caller-defined access. *)

(** {1:constructing Constructing access facts} *)

val path : op:path_op -> Mentat_workspace.Path.t -> t
(** [path ~op workspace_path] is filesystem access [op] to [workspace_path].

    The access identity uses the path's workspace root and root-relative syntax.
    Display text belongs to {!Request.Item.t} metadata. *)

val path_scope : op:path_op -> Path_scope.t -> t
(** [path_scope ~op scope] is filesystem access [op] to [scope].

    This is the explicit form for callers that already classified a path scope
    or need caller-supplied workspace key material. *)

val outside_workspace_path : op:path_op -> Lpath.Abs.t -> t
(** [outside_workspace_path ~op path] is filesystem access [op] to [path]
    outside the workspace.

    Use this only after the host has classified [path] as outside the active
    workspace. *)

val unknown_path : op:path_op -> string -> t
(** [unknown_path ~op path] is filesystem access [op] to [path] whose workspace
    relation is unknown.

    Use this when the host can describe the target text but has not resolved it
    against a workspace.

    Raises [Invalid_argument] if [path] is empty. *)

val command : Command.t -> t
(** [command command] is command execution access. *)

val shell : cwd:Path_scope.t -> execution:Command.execution -> string -> t
(** [shell ~cwd ~execution text] is command execution access for shell command
    [text]. It is [command (Command.shell ~cwd ~execution text)] and exists so
    producers need not name the {!Command} sub-fact.

    Raises [Invalid_argument] if [text] is empty. *)

val argv :
  cwd:Path_scope.t ->
  execution:Command.execution ->
  program:string ->
  string list ->
  t
(** [argv ~cwd ~execution ~program args] is command execution access for
    [program] run with [args] in [cwd] through [execution]. It is
    [command (Command.argv ~cwd ~execution ~program args)]; [args] does not
    include [program].

    Raises [Invalid_argument] if [program] is empty. *)

val code :
  cwd:Path_scope.t ->
  execution:Command.execution ->
  language:string ->
  string ->
  t
(** [code ~cwd ~execution ~language source] is command execution access for
    evaluating [source] as [language] code. It is
    [command (Command.code ~cwd ~execution ~language source)].

    Raises [Invalid_argument] if [language] or [source] is empty. *)

val network : protocol:network_protocol -> ?port:int -> host:string -> unit -> t
(** [network ~protocol ?port ~host ()] is network access to [host].

    The host is responsible for normalizing host names and addresses, including
    case, trailing dots, IDNA, IPv6 literal spelling, DNS aliases, and default
    ports.

    Raises [Invalid_argument] if [host] is empty, if [protocol] is [`Other ""],
    or if [port] is outside \[[1];[65535]\]. *)

val custom : ?subject:string -> string -> t
(** [custom ?subject name] is a caller-defined custom access.

    Raises [Invalid_argument] if [name] is empty or if [subject] is empty when
    present. *)

(** {1:inspecting Inspecting access facts} *)

val kind : t -> kind
(** [kind a] is [a]'s coarse access kind. *)

val stable_text : t -> string
(** [stable_text access] is a canonical byte string derived from [access]'s
    permission identity, suitable as digest {e input}. {!equal} accesses have
    byte-identical [stable_text]: it distinguishes exactly what {!equal}
    distinguishes, the command execution route included.

    {b Stability contract.} The grammar is length-framed and tagged, so distinct
    accesses never collide on a byte string, and it is held fixed for the
    current permission identity schema: a given value encodes to the same bytes
    across library versions, so an id a consumer digests over it stays stable.
    Changing the framing, field order, or tags is therefore an identity break
    that forces every consumer to bump its digest domain version. It is one-way
    — not for display, user input, or decoding, and has no inverse. *)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] contain the same access identity. *)

val compare : t -> t -> int
(** [compare a b] orders accesses by their {!stable_text}, so the order stays
    compatible with {!equal} by construction. *)

module Set : Set.S with type elt = t
(** Sets of accesses. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats an access for diagnostics. The output is not stable storage
    syntax. *)

(** {1:json JSON} *)

val jsont : t Jsont.t
(** [jsont] maps accesses to JSON objects tagged by a ["type"] member ([path] /
    [command] / [network] / [custom]).

    A workspace path scope composes {!Mentat_workspace.Path.jsont} under
    ["workspace_path"]. Outside-workspace and unknown scopes carry a ["path"]
    string instead.

    Access JSON is unversioned because accesses are always embedded in versioned
    {!Request} and {!Policy} values. Decoding is fail-closed: an unknown object
    member, and any JSON that would reconstruct a constructor-invalid access (an
    empty required string, a port outside \[[1];[65535]\], an empty [`Other]
    protocol), is a decode error — decoding re-runs the same construction
    invariants, so a decoded access is exactly as checked as a constructed one.
*)
