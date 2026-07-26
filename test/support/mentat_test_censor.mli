(** The single normalization policy shared by cram goldens (via the
    [mentat_cram censor] CLI) and TUI [%expect] frames (via [Screen.normalize]).
    Defeating nondeterminism here — once — is what lets whole-frame goldens
    scale, and keeps the two visual layers speaking one marker vocabulary so a
    session id, a port, or a digest reads byte-identically in a cram golden and
    a TUI frame.

    Two-layer determinism: nondeterminism is killed at {e construction} first
    (explicit [--id], [MENTAT_NOW], a pinned model/budget/sandbox), and only
    what construction cannot reach — temp paths, content digests, OS-assigned
    ports, pids — is caught here at the {e scrub} layer. A seam that pins beats
    a scrub that hides, so timestamps are pinned by [MENTAT_NOW] and never
    scrubbed by default. *)

type marker =
  | Fixed of string
      (** Every match is replaced by this exact marker text. The pattern may
          fold a constant prefix into the match (e.g. [127.0.0.1:PORT]); the
          marker re-types that prefix so only the volatile tail is normalized.
      *)
  | Unique of string
      (** Distinct matches are numbered in first-seen order, stable within one
          {!apply} call. A lone match becomes the bare marker [m]; two or more
          distinct matches become [m1], [m2], … — so two racing pids read
          [$PID1] and [$PID2], while a single digest reads [$DIGEST]. *)

type rule = { re : Re.t; marker : marker }
(** An uncompiled pattern paired with how its matches are normalized. {!apply}
    compiles the pattern; rules run in list order, each transforming the whole
    string before the next sees it, so ordering is load-bearing. *)

val default : rule list
(** The static default pipeline, in application order:

    - [[0-9a-f]\{12,\}] → [Unique "$DIGEST"] — content digests (32/64-hex),
      [profile_hash], and the 12-hex content-derived permission-rule ids.
    - [127\.0\.0\.1:[0-9]\{2,5\}] → [Fixed "127.0.0.1:$PORT"] — fake-server
      ports in base URLs and transport messages.
    - [\bpid [0-9]+] → [Unique "pid $PID"] — the concurrency-fence message
      [busy … pid NNNN]; [Unique] so two racing pids read [$PID1]/[$PID2]. (Re
      has no lookbehind, so the [pid ] prefix is folded into the match and
      re-typed by the marker — the observable output is [pid $PID].)

    {!apply} prepends a cwd rule ahead of this list (the temp root is stripped
    {e first}, so a path-embedded hex string is not later mistaken for a
    digest).

    Deliberately {b not} scrubbed: timestamps, ages, token counts, session ids,
    turn ids, and model/provider names — each is pinned by a construction seam,
    and the golden then proves the seam works. *)

val times : rule list
(** Opt-in only (the [censor --times] flag): RFC3339 → [$TS], 13-digit unix-ms →
    [$NOW]. {b Not} in {!default} — time is pinned by [MENTAT_NOW], not
    scrubbed, so a broken clock surfaces as a golden diff instead of being
    hidden. Because {!apply} runs {!default} before [~extra], a bare 13-digit
    unix-ms is already folded by the digest rule; the RFC3339 rule (which the
    digest rule cannot match) is the reliable half. *)

val apply : ?extra:rule list -> ?cwd:string -> string -> string
(** [apply s] runs the cwd rule, then {!default}, then [~extra] over [s].

    [~cwd] defaults to {!Sys.getcwd}. All known forms of the working directory —
    the given/effective [cwd], its canonical form as reported by [Sys.getcwd],
    and [$PWD] from the environment — are folded to [$TESTCASE_ROOT] as literal
    (regex-quoted) replacements, longest first. Folding every form is what keeps
    the macOS [/private/var/…] vs [/var/…] symlink pair from leaking. *)
