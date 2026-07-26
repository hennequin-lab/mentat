(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The pure half of the command-confinement subsystem.

    This library is the vocabulary a resolver uses to describe
    {e what a spawned process may touch} ({!Policy}), to seal that description
    into an inert route value the spawn boundary lowers through ({!type:t}), and
    to record {e what actually guarded a command} ({!Evidence}). It probes
    nothing, launches nothing, canonicalizes no path, and logs nothing.

    {b The flow.} A resolver builds a {!Policy.t} from a product posture and
    host-resolved roots, probes the platform for an available {!Backend.t}, and
    injects the probe outcome into {!val:confined} — an [Ok backend] or the
    [Error] naming why none is usable. The sealed {!type:t} fixes the
    enforcement {!Evidence}, the {!type:escalation} stance, and the durable
    {!Identity} once, before any command runs; {!val:direct} and
    {!val:external_} are the two unconfined routes, sealed with class-constant
    evidence and identity. Per command, the effect twin discharges
    {!obligations} and calls {!lower_argv}, which returns the full lowered argv
    for the route — enforcing prefix plus command for a confined route, the
    command verbatim otherwise. {!admits} gates the whole run against a
    {!Requirement} before credentials load.

    {b What is pure here, and what the twin owes.} Every value here is a pure
    function of its inputs: the route constructors invoke nothing, profile
    generation is deterministic, and {!lower_argv} makes only a lexical
    containment decision. The effects live in the single effect twin,
    [mentat.workspace_io] — the one sealed process-spawn boundary. The twin
    owes, in order: probing the platform for an available {!Backend.t};
    resolving and [realpath]-canonicalizing roots; minting the per-run scratch
    directory ({!Policy.scratch}); discharging every {!Obligation} — the
    existence re-checks this library names but cannot perform — before each
    launch; and launching the lowered argv. A public {!lower_argv} beside a
    public {!obligations} is safe only because that one boundary discharges the
    obligations; this library documents the precondition it cannot enforce.

    {b Process values live at the launch boundary.} The exact child environment,
    the argv value vocabulary, and the launch plan are process concerns owned by
    the launch boundary, not confinement concerns: this library returns a
    lowered [string list] and nothing else. Argv {e validation} (a non-empty
    program, no NUL byte) is checked at every lowering here, minting
    {!Error.Empty_program} and {!Error.Nul_in_argv}, but its enforcement home is
    the single launch boundary, through which every command passes. Likewise the
    exact-environment law — ambient secrets and agent sockets stripped on every
    route, {e including an escalated command} — is enforced where the
    environment is built and passed to the child; the sandbox-side fact is that
    {!lower_escalated_argv} never emits an enforcing prefix: escalation escapes
    the {e profile}, never the {e environment}.

    {b Product posture lives above this library.} Sandbox modes, read presets,
    and the status and effective-configuration displays are product vocabulary
    that [mentat.config] (field types) and the executable (display) compose over
    the {!Policy} algebra exported here. A preset that resolves roots is impure
    and belongs to the effect twin; one that does not collapses to
    {!Policy.make}, so it adds no independent value. This library exports the
    confinement algebra, not the modes a product builds from it.

    {b Three distinct controls.} Trust decides {e activation} (whether Mentat
    runs at all), permission decides {e consent} (whether Mentat may attempt an
    operation), and the sandbox — this library — {e confines} process authority
    and records what guarded a command. The three share no type and no owner: a
    sealed sandbox is not a permission fact, and permission lives in
    [mentat.permission]. The only coupling is a projection in [mentat.tools]
    that reads a sealed route into a permission [Access.Command].

    {b Identity versus evidence.} Two digests describe an enforced seal, and the
    distinction is load-bearing. {!Evidence} keeps the {e real} profile digest —
    including the per-run scratch path — as the run-start audit record.
    {!Identity} is the durable, secret-free, {e scratch-invariant} fingerprint
    the session turn contract seals and resume revalidates: it normalizes the
    per-run scratch out, so a freshly minted scratch directory does not trip
    resume. It {e does} change when the confinement changes — or when a Mentat
    binary upgrade alters the generated profile text — so a pending invocation
    never runs under an old approval once the lowering shifts; resume
    re-approves under the new profile, fail-closed. Because generation is pure
    and deterministic, both digests are reproducible and profiles are
    golden-testable on any platform, through {!lower_argv}. *)

module Error = Error
(** Structured sandbox errors. *)

module Policy = Policy
(** Pure command confinement policies. *)

module Backend = Backend
(** The closed set of built-in enforcing platforms, as pure data. *)

module Requirement = Requirement
(** How strong a run demands its confinement to be. *)

module Evidence : sig
  (** Sandbox enforcement evidence.

      Evidence states what actually guarded a spawned command. It is owned by
      the sandbox library — not by any tool — because tool output, JSONL events,
      and status rendering all speak it.

      [Not_requested] means the host chose unconfined execution, including a
      user-approved per-command escalation. [Enforced] records the backend and
      the digest of the generated enforcement profile. [Refused] means a
      restricted sandbox was requested but could not be enforced, so the command
      was not spawned. [Declared_external] reports the user's declared external
      boundary; it is never upgraded to enforced.

      The facade deliberately omits the underlying [not_requested], [enforced],
      and [declared_external] introduction functions. {!direct}, {!confined},
      {!external_}, and an escalated route own those claims because they also
      own the route or generated profile the evidence describes. Arbitrary
      callers may inspect evidence but cannot claim that a command ran under a
      backend. {!refused} remains public because the effect twin must record an
      obligation failure discovered after sealing. *)

  (** The type for sandbox enforcement evidence.

      Constructors are private: callers may distinguish the four outcomes, but
      only sandbox-owned routes mint unconfined, enforced, and external claims.
      The public {!refused} constructor is the sole exception, for reporting a
      failed obligation discharge without spawning the command. *)
  type t = private
    | Not_requested
        (** Mentat applied no command confinement. This covers a direct route
            and a separately approved escalation; it is not evidence of safety.
        *)
    | Enforced of {
        backend : Backend.t;  (** The backend that guarded the command. *)
        profile : Mentat_digest.t;
            (** The digest of the generated profile used for this run, including
                its real per-run scratch path. *)
      }
        (** The command ran through the named backend and generated profile.
            Only a successfully sealed confined route mints this audit claim. *)
    | Refused of Error.t
        (** Confinement was required but could not be established, so the
            command was not spawned. The error remains structured. *)
    | Declared_external
        (** Confinement belongs to a user-declared boundary outside Mentat.
            Mentat does not verify that boundary and never upgrades this to
            {!Enforced}. *)

  val refused : Error.t -> t
  (** [refused error] reports that a requested restricted sandbox refused to run
      a command. Use it when lowering returns [Error error] or when the twin's
      obligation discharge fails ({!Error.Stale_policy}). The error remains
      structured; human-readable messages are diagnostics, not matching keys. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same evidence. *)

  val to_json : t -> Jsont.json
  (** [to_json t] is the canonical JSON projection: an object whose ["kind"]
      member discriminates the variant. [Enforced] adds ["backend"] and
      ["profile_hash"] (lowercase hexadecimal). [Refused] adds ["reason"] (the
      error message, for display) and ["error"] (the structured
      {!Error.to_json}, whose ["kind"] member is the programmatic matching key).
      [Not_requested] and [Declared_external] carry only ["kind"]. Never branch
      on the presentation-only ["reason"] string.

      It is encode-only — evidence flows into JSON and is never parsed back —
      and every product JSON surface (model-visible tool output, JSONL events)
      uses this one spelling so evidence cannot drift between contracts. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats evidence for diagnostics. The output is not stable storage
      syntax. *)
end

module Identity = Identity
(** Durable, scratch-invariant confinement identities. *)

type t
(** A sealed command sandbox: the route commands are lowered through, the
    evidence they report, and whether escalation is available. Pure: it starts
    no process and touches no filesystem. *)

(** The sealed per-command escalation stance. It is fixed from the policy's
    writable roots, independent of the enforcement outcome: a confined seal
    [Refused] for backend unavailability but with writable roots is still
    [Available] — {!lower_argv} refuses every command, yet
    {!lower_escalated_argv} returns an unconfined argv, gated by its separate
    permission approval. *)
type escalation =
  | Available
      (** workspace-write shaped: one command may run unconfined after a
          separate permission approval *)
  | Denied of Error.t
      (** read-only shaped: the no-mutation promise admits no approval-shaped
          exception *)
  | Ignored
      (** unconfined or declared-external: escalation asks for what is already
          true *)

module Obligation : sig
  (** A filesystem existence check the effect twin discharges before launch. *)

  (** What kind of entry an obligation requires. *)
  type kind =
    | Exists
        (** some entry must exist, checked with [lstat] — which confirms
            existence without following a swapped symlink to its target, and
            does {e not} detect a kind change such as a directory replaced by a
            symlink *)
    | Directory
        (** the path must be an existing directory ([stat] + [S_DIR]) *)

  type t

  val path : t -> Lpath.Abs.t
  (** [path t] is the canonical absolute path to check. *)

  val kind : t -> kind
  (** [kind t] is the entry kind the twin must confirm. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats the obligation for diagnostics. *)
end

val confined : backend:(Backend.t, Error.t) result -> Policy.t -> t
(** [confined ~backend policy] seals a confined route. [backend] carries the
    resolver's probe outcome as data — never a profile — so the enforcement
    profile can only be generated from {e this} policy, and evidence,
    obligations, and identity always describe the policy commands run under.
    [backend] is required: a forgotten probe outcome is a type error, not a
    weaker seal. [Ok backend] lowers the profile with that backend's generator;
    [Error e] (typically {!Error.Unavailable}) refuses every command —
    fail-closed. Pure and total.

    {b Law (evidence fixed at seal).} {!evidence} is [Enforced] for [Ok],
    [Refused] for [Error] — and never changes across {!lower_argv} calls. *)

val direct : t
(** [direct] is the intentionally unconfined route (for example
    danger-full-access): Mentat applies no filesystem or network confinement.
    Evidence is [Not_requested]; identity is class-constant; escalation is
    [Ignored]; there are no obligations. *)

val external_ : t
(** [external_] is the route whose confinement a boundary outside Mentat owns
    and Mentat does not verify. Evidence is [Declared_external]; identity is
    class-constant; escalation is [Ignored]; there are no obligations. *)

val policy : t -> Policy.t option
(** [policy t] is the sealed confinement policy — [Some] for a confined route
    (enforced or refused), [None] for {!direct} and {!external_}, which have no
    confinement to describe. *)

val evidence : t -> Evidence.t
(** [evidence t] is the sealed posture: the evidence every command from [t]
    reports, fixed at seal time. Not per-command. *)

val escalated_evidence : t -> Evidence.t
(** [escalated_evidence t] is the evidence an escalated command reports: always
    [Not_requested] — an approved escalation runs unconfined, and its record
    must say so rather than claim the sealed enforcement. *)

val escalation : t -> escalation
(** [escalation t] is the sealed escalation stance. *)

val identity : t -> Identity.t
(** [identity t] is the durable confinement identity. *)

val obligations : t -> Obligation.t list
(** [obligations t] are the existence checks the twin must discharge before
    launching any lowered command: each resolved readable and protected path
    ([Exists]) and each writable root and the private scratch ([Directory]).
    Deduplicated by path keeping the strongest kind, in canonical path order.
    [[]] for direct, external, and refused routes. A failing check is reported
    as [Evidence.refused (Stale_policy _)] naming the exact path, and nothing
    launches. *)

val lower_argv :
  t -> cwd:Lpath.Abs.t -> string list -> (string list, Error.t) result
(** [lower_argv t ~cwd argv] is the complete pure lowering decision for one
    command: the full argv the launch boundary must execute. It starts no
    process.

    {b Precondition (the twin upholds):} [cwd] is the twin's realpath-resolved
    working directory and the twin has discharged {!obligations}.

    Every route first validates [argv] — an empty list or empty first token is
    {!Error.Empty_program}; a NUL byte in any token is {!Error.Nul_in_argv} (OS
    argv cannot represent either). Then, for a confined route, it re-checks
    lexical containment of [cwd] in the read roots (else
    [Error (Cwd_outside_scope _)]) and returns the sealed enforcing prefix — the
    backend executable, its profile arguments, and for Bubblewrap a
    [--chdir cwd --] segment — followed by [argv] verbatim: a prefix cannot
    rewrite, reorder, or drop the command it wraps. A direct or external route
    returns [argv] verbatim — no confinement is applied. A refused route is
    [Error]; the result reports {!Evidence.refused}. *)

val lower_escalated_argv :
  t -> cwd:Lpath.Abs.t -> string list -> (string list, Error.t) result
(** [lower_escalated_argv t ~cwd argv] lowers one command with filesystem and
    network confinement dropped — {b only} when {!escalation} is [Available]. It
    validates [argv] exactly as {!lower_argv}, then returns it verbatim:
    {b an escalated lowering never emits an enforcing prefix}. Unlike
    {!lower_argv} it applies no read-root cwd containment, so a cwd that
    {!lower_argv} would reject as out of scope is accepted — that is the point
    of escalation. [Denied] and [Ignored] stances are structured errors
    ({!Error.Escalation_denied}, {!Error.Escalation_irrelevant}) and lower
    nothing.

    {b Law (escalation escapes the profile, never the environment).} The escaped
    confinement is exactly the enforcing prefix this function omits. The exact
    child environment is owned and enforced by the launch boundary, which passes
    an escalated command the {e same} stripped environment as a confined one:
    ambient secrets and agent sockets stay withheld even with the profile
    dropped.

    {b Law (escalated evidence).} An escalated command reports
    {!escalated_evidence} — [Not_requested] — never the sealed [Enforced]
    posture. *)

val admits : Requirement.t -> t -> (unit, Requirement.Rejection.t) result
(** [admits req sandbox] is [Ok ()] iff [sandbox]'s sealed {!evidence} meets
    [req]. It reads {!evidence} (the sealed posture), so the gate cannot be fed
    per-command evidence.

    {b Law.}
    - [Off] always admits;
    - [Enforced_or_external] rejects only [Refused] (as [Unenforceable]); it
      admits [Enforced], [Declared_external], and [Not_requested];
    - [Enforced] rejects [Refused] (as [Unenforceable]) and [Declared_external]
      (as [External_not_enforced]); it admits [Enforced] and [Not_requested].

    {b Note.} [Not_requested] — the intentionally unconfined {!direct} route,
    e.g. danger-full-access — is admitted even under [Enforced]: the requirement
    gates whether a {e requested} confinement can enforce, not whether an
    explicit unconfined mode was chosen. Passing the gate does not run refused
    confined commands unconfined: each is still refused at {!lower_argv}.

    {b Startup fail-closed.} [admits] reads only the sealed {!evidence} —
    backend availability — so on its own it would let a run whose resolved roots
    are already stale at seal time proceed to load credentials. The executable
    closes that gap by composition: after sealing it runs [admits] {e and} the
    twin discharges {!obligations} once, before any credential or session
    effect. Each later launch re-discharges {!obligations}; the startup
    discharge is the early check, not a substitute for it. *)
