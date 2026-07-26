(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** What confined a command, observed once at the spawn boundary.

    These are facts, not prose. The boundary is the only place that holds the
    sealed policy, the evidence, and the child's bytes at the same moment, so it
    is the only place that can state them — and every spawn site gets them,
    rather than the one tool that happened to grow a scan. Rendering belongs
    above: a caller turns these into a message its own audience can act on. *)

type refusal =
  | Filesystem  (** the output matches a refused read or write *)
  | Network  (** the output matches a blocked network request *)

type t
(** What guarded one command, and whether its output looks like that guard
    refusing something. *)

val reads_scoped : t -> bool
(** [reads_scoped t] is [true] iff reads were confined to admitted roots rather
    than left open. It decides which standing grant is worth naming: an unscoped
    route discards [sandbox.readable_roots] entirely, so advising it would send
    the reader to a field that cannot change the outcome. *)

val reads_attributable : t -> bool
(** [reads_attributable t] is [false] when a denied {e read} cannot be
    distinguished from a missing file on this backend and read scope — under
    bubblewrap a scoped root is a tmpfs, so an unbound path is simply absent and
    the child reports [ENOENT]. No scan can recover the difference, so a caller
    that would otherwise say "this was refused" must say "this may be outside
    the read scope" instead. Refused {e writes} stay attributable on both
    backends. *)

val escalatable : t -> bool
(** [escalatable t] is [true] iff this route admits a per-command escalation. A
    caller must not advise retrying with an escalation on a route that will
    refuse it — most spawn sites, and every read-only one. *)

val refusal : t -> refusal option
(** [refusal t] is the signature match, if any. It is a lenient affix scan over
    the child's own bytes: a hint, never proof.

    A network-shaped failure is reported only where the posture actually
    restricts the network; elsewhere the host was genuinely unreachable and the
    answer is [None], never {!Filesystem} — a blocked-connection message
    contains wording the filesystem signatures would otherwise claim. *)

val observe :
  sandbox:Mentat_sandbox.t ->
  evidence:Mentat_sandbox.Evidence.t ->
  transcript:string ->
  t option
(** [observe ~sandbox ~evidence ~transcript] is what guarded this command, or
    [None] when nothing did — only an enforced route produces an observation,
    which is what keeps an unconfined or escalated command from reporting a
    confinement it did not have. *)
