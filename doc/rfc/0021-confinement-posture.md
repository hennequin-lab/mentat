# RFC 0021: Confinement posture — inherit the environment, order the policy, close the denial

- Status: `discussion`. (Lifecycle: `ideation → discussion → published →
  committed | abandoned`. `committed` means the document describes how the
  system works, not what we intend.)
- Audience: Mentat maintainers; the workspace-io (RFC 0009) and sandbox
  (RFC 0002) authors
- Derives from: RFC 0002 (the sealed route, Identity, escalation-as-route),
  RFC 0009 (the one spawn boundary, L1/L2), RFC 0000 D4
- Amends: **RFC 0009 non-regression 9** (scratch as `HOME`/`TMPDIR`) and
  **RFC 0009 L8** (scratch lifetime and disjointness) — both repealed with the
  scratch itself, §2. **RFC 0009 L6** (existence-filtered carveouts, strict
  `--ro-bind` never `--ro-bind-try`) — replaced by materialization, §4.
  **RFC 0002 L8**'s scratch-invariance clause lapses: with nothing per-run in
  the policy, the durable identity is the profile digest and needs no
  normalization. L8 is otherwise not amended; §9 records what its wording
  invites a reader to miss.
- Provenance: a 12-question source audit, four blind designs, five adversarial
  lenses including a codex-divergence audit, a fold verification, and a
  four-agent campaign on the scratch. §9 records what each changed. Four passes
  refuted rulings an earlier draft had adopted.
- **Implemented:** slices 1–3 have landed (`bd2210dc`, `1373b585`, `e4c66c5e`,
  `046c6b15`, `9cef8515`, `bc27c44d`). This document records the design as
  built; §11 marks what remains. Where implementation disagreed with the draft,
  the draft was corrected — those points are marked *(on inspection)*.

## Summary

Mentat predicts every directory a confined command may touch, in eight
derivations, four of them functions of the launcher's shell. Codex predicts
almost nothing and inherits the environment untouched. The divergence has never
been justified, and it has produced three bugs in a row.

Three rulings, in descending order of what they buy:

1. **Inherit the environment; stop rewriting it.** Delete the `HOME`/`TMPDIR`
   redirect and the carried-directory apparatus. The redirect is the only
   component that can point a child at a directory the policy does not
   authorize, which is exactly how the child's view and the policy's view came
   to disagree. This deletes the mechanism rather than repairing its output.
2. **One ordered policy, not four parallel lists.** `(path, access)` resolved
   most-specific-wins, `Deny > Write > Read`, as the reference does. It subsumes
   `protected_paths` and fixes a live bug: `Policy.make` collapses descendants,
   so `sandbox.writable_roots += ~/.cache/dune/toolchains` is silently dropped
   and the escape hatch the product advertises does not work.
3. **Admission cannot close; denial closes trivially.** Reads and writes stay
   best-effort *blast radius*. The deny set is small, fixed, complete, and wins.

Two safety bugs found during the campaign are fixed here because they are the
same subject (§3, §11 slice 1).

## Motivation

**A build was sent to a cache it was forbidden to write.** `HOME` is redirected
to the scratch, so `derive.ml` carries `OPAMROOT` and dune's XDG directories
back — read-only, until `0e04ba4d`. A dune build with git-pinned sources must
flock `$XDG_CACHE_HOME/dune/rev-store.lock` unconditionally, warm store included
(`_ref/dune/src/dune_pkg/rev_store.ml:367-370`, `:677-720`). The environment
said "your cache is here"; the policy said "you may not write there."

**On Linux, out-of-scope writes silently succeed.** Under `reads = Only` — the
default — the root is `--tmpfs /` (`lib/sandbox/bubblewrap.ml:26-28`) and
nothing remounts it read-only, so a write to any *unbound* path lands in an
ephemeral mount and **exits 0**. The model believes it worked; the bytes are
gone at teardown. A denial the model can see beats a success it cannot verify.

**There is no `/tmp`.** Neither readable nor writable on either backend
(`derive.ml:294-337`). A literal `/tmp/...` path fails on macOS and vanishes on
Linux. It went unnoticed because `TMPDIR` points at the scratch — the redirect
concealing a missing writable root.

Three symptoms, one cause: authority is derived from an environment the same
component rewrites. Doing nothing grows the derivation one entry per discovered
omission, forever, each omission either a broken build or a silent one.

## 1. Guide-level: what a user sees

Almost nothing changes, and that is the intent. `sandbox explain` grows a deny
block naming the three directories that hold Mentat's own record:

```
$ mentat sandbox explain
write-root=/Users/t/Workspace/project
write-root=/tmp
write-root=/private/var/folders/.../T
write-root=/Users/t/.cache/dune
read-root=/Users/t/.opam
read-root=/Users/t/.config/dune
protected=/Users/t/.cache/dune/db
protected=/Users/t/.cache/dune/toolchains
protected=/Users/t/Workspace/project/.git
deny=/Users/t/.config/mentat
deny=/Users/t/.local/share/mentat
deny=/Users/t/.local/state/mentat
```

`$HOME` is no longer rewritten, so every tool resolves its real cache — cargo,
npm, pip, go and the rest stop paying the silent cold-start tax they pay today.
A `$HOME`-relative *write* outside the writable set now fails visibly instead of
succeeding into a scratch that is deleted at teardown.

When a confined command is refused, the first such refusal in a session raises
one durable workspace notice (§4) rather than an advisory the model may or may
not act on.

## 2. Inherit the environment

`Child_env` keeps its allow-list — ambient secrets and agent sockets stay
stripped, which is where Mentat beats the reference. What goes is the rewrite:
`HOME`, `TEMP`, `TMP`, `TMPDIR` are inherited, and with them the carry —
`access`, `home_relative_dirs`, `carried_user_dirs`, `carried_read_roots`,
`carried_carveouts`, `carried_subpath`, `carried_bindings`, `carried_writable`,
the `carried_dirs` field, and the merge in `child_env.ml`.

**The carry's read roots must be replaced, not merely deleted — and replaced at
the base.** Today `carried_read_roots` folds `~/.opam` and `~/.config/dune` into
`readable` (`derive.ml:707-708`). Deleting the carry drops both, losing
`~/.opam/repo`, `~/.opam/download-cache`, and dune's user config. They are
re-added as ordinary derived read entries — **at the base directory the variable
names, never at a version- or switch-specific descendant.**

That is not a style preference. `root_paths` collapses descendants
(`derive.ml:144-152`), so admitting `~/.opam` today absorbs `OPAM_SWITCH_PREFIX`,
the switch `bin` on `PATH`, its `lib`/`share` siblings, `OCAMLLIB` and
`CAML_LD_LIBRARY_PATH` — which is **why `opam switch set` does not currently
move the sealed Identity**. Re-adding `~/.opam/repo` instead of `~/.opam` would
destroy an invariant nobody wrote down, and every switch change would start
breaking suspended turns. L7 states it.

**Nothing is rewritten, including the temp family, and the per-run scratch is
deleted with it** *(on inspection: an earlier draft kept `TMPDIR`→scratch)*.
Once the environment is inherited the child never learns the scratch path, so a
writable root nobody names is authority granted for nothing. Deleting it also
removes the minting, the identity-verified teardown, the disjointness guard,
`Policy.scratch` and its fold into the read set, the obligation, and the whole
scratch-normalization mechanism behind the durable identity — which existed
only because one field was per-run. `Identity` is now the profile digest itself.

The decisive argument is the redirect's own: `/tmp` was ungranted for the life
of the product and nobody noticed, *because nothing ever pointed at it*. A
redirect does not only risk disagreeing with the policy; it hides whether the
policy was ever right. Six agents were surveyed and none mints a per-run
directory and aims the child at it; among the three that sandbox, the omission
is deliberate in each.

**What the redirect bought.** `$TMPDIR` working is not a property of the
redirect; it is the missing `/tmp` grant the redirect concealed. `$HOME`
read-confidentiality is real only under `read=all`, redundant under the default
posture, and disclaimed by N1. **Graceful degradation is genuine and we are
giving it up**: a tool writing `~/.foo_history` succeeds today — into a
directory deleted at teardown, so a tool that writes and reads back gets a
silently wrong answer with no signal. After this it is a visible refusal.

**What it costs everyone off the carried list.** Three variables are carried;
cargo, npm, pip, go, gradle, uv and pre-commit are not, so each gets an empty
home every run: permanently cold, no denial, no advisory. A UX regression
against the reference that nobody had counted.

**One consequence worth stating.** Today Mentat's directories are partly
unreachable *because* `HOME` points elsewhere — the deny set of §3 is
belt-and-braces over an accident. Inherit `HOME` and the deny set becomes the
only thing between a confined command and Mentat's record. That is the strongest
argument for shipping §3, and it is why §11 orders slice 3 with or before slice 2.

## 3. One ordered policy

```ocaml
module Access : sig type t = Deny | Read | Write end   (* Deny > Write > Read *)

val make :
  scratch:Lpath.Abs.t ->
  entries:(Lpath.Abs.t * Access.t) list ->
  reads_default:[ `All | `Denied ] ->
  network:Network.t -> t
```

Most-specific-wins by path depth, ties broken by access order — the reference's
rule (`_ref/codex/codex-rs/protocol/src/permissions.rs:655`, `:1400`).
`writable_roots` and `protected_paths` become entries. Three consequences:

- **The escape hatch starts working.** `~/.cache/dune/toolchains` added to
  `sandbox.writable_roots` is a deeper entry that wins over the shallower
  carveout. Today the descendant collapse at `policy.ml:41-49` drops it.
- **A carveout is an access, not a list.** `db` and `toolchains` are `Read`
  entries — readable, not writable. They are *not* `Deny`: denying reads would
  kill cache *restore*, a larger regression than the write risk.
- **`Deny` beats every present and future derivation** without knowing what they
  are. That is what makes admission's incompleteness survivable.

**Default writable set:** the primary workspace root, `/tmp`, `$TMPDIR`,
configured roots, and `$XDG_CACHE_HOME/dune` — *not* all of `~/.cache`, which
exposes `~/.cache/ms-playwright`, `~/.cache/puppeteer`, `~/.cache/pre-commit`
and `~/.cache/uv`, all holding binaries and hook scripts the user later executes
unsandboxed.

**`db` and `toolchains` stay unwritable.** An earlier draft made `db` writable on
the argument that entries are keyed by action digest. False: dune restores a hit
by hardlinking `files/v4/<file_digest>` into `_build` **without re-digesting**
(`_ref/dune/src/dune_cache/shared.ml:304-309`), and the reproducibility check
re-runs the rule rather than verifying bytes, defaulting to `Skip`. Write access
to `db` is write access to the user's next unsandboxed build, shared across
projects in hardlink mode.

**The escalation stance is stated, not inferred** *(on inspection)*.
`Seal.confined` read it off the shape of the writable list — empty meant
read-only, therefore no approval-shaped exception. That held only while a
no-mutation route was granted nothing, and it comes apart the moment one is
granted somewhere to put a temporary file, which it needs. `confined` now takes
`~mutates`; the product supplies it from the mode and the pure library still
never learns what a mode is. A read-only route is granted the platform scratch
space, not the workspace, and its escalation stays `Denied`.

Dune's cache is a third writable class, separate from both: persistent user
state rather than scratch, so a no-mutation route does not get it while a build
route still takes its revision-store lock.

**The deny set is mode-invariant.** `mentat_workspace_io.ml:431-437` returns
`([], [])` for `Read_only`, zeroing writable and protected. Deny entries are
constructed outside that match, or read-only mode ships with no deny set — the
exact leak this RFC exists to close.

**The deny set is three whole directories**, from `bin/user_dirs.ml:30-44`:
`config_home`, `data_home`, `state_home`. Not configurable. An earlier draft
carved the config home down to three files, to keep user-level skills'
resources reachable — *on inspection that was unnecessary*: those resources are
read by Mentat in-process, not by a confined child. Denying whole directories
also avoids needing bubblewrap's file-deny path at all.

## 4. Lowering, and the two safety fixes

**Bubblewrap.** The `reads = Only` root stays `--tmpfs /` — project-scoped read
is the divergence we keep (§8) and `--ro-bind / /` would silently abandon it on
Linux — but gains `--remount-ro /` so an unbound write fails EROFS instead of
succeeding into the tmpfs. **`--remount-ro /` must be emitted last, after
`--proc` and `--dev`**, or bwrap cannot create them. Verified in a container:
writable roots stay writable, read roots stay readable, unbound paths stay
unreadable, and an unbound write is denied `Read-only file system`.

A `Deny` entry lowers as `--perms 000 --tmpfs <p> --remount-ro <p>` for a
directory and `--perms 000 --ro-bind-data /dev/null <p>` for a file. Also
verified: `--tmpfs` alone masks the read but *accepts* the write; the
`--remount-ro` is load-bearing, and the host file survives untouched.

**Seatbelt.** One section appended last — SBPL is last-match-wins, so it beats
the base policy's `(allow file-read* … (literal "/"))` and `Policy.All`'s
blanket allow. Parameterized like every other root; emitted only when non-empty.
The claim that last-match-wins holds is pinned by a black-box test, not asserted.

**Owned paths are materialized, not existence-filtered.** This repeals RFC 0009
L6. `--tmpfs DEST` needs no source, but bwrap must create the mount point inside
the new root, so an absent denied path aborts every spawn; and a `Read` carveout
still lowers to a strict `--ro-bind`, which L6 handled by dropping it. So every
path Mentat owns is created at resolution by `mkdir(0700)`; on `EEXIST` the path
is `lstat`ed and a **symlink, non-directory, or foreign-uid entry fails the
resolution closed** — the chain at `lib/server/mentat_server.ml:120-131`.

**The materialized set, exhaustively:** the three denied directories,
`$XDG_CACHE_HOME/dune/db`, `$XDG_CACHE_HOME/dune/toolchains`, and
`<root>/.mentat`. **`<root>/.git` is never materialized** — you cannot create a
repository and its absence is semantically empty — so it keeps L6's filter as
the one documented exception. `.mentat` is the other half of
`protected_meta_names` (`lib/workspace/mentat_workspace.ml:119`) and is
materialized because it sits under the primary writable root.

The **lexical** path enters the policy; `realpath` is not consulted. Today the
two carveout resolvers disagree — `.git` uses `lstat` and keeps the lexical
path, the cache carveouts use `stat` and keep the physical one
(`derive.ml:536-542` vs `:162-169`) — so an agent can plant
`ln -s ~/.cache/evil ~/.cache/dune/toolchains`, satisfy the `mkdir -p`, and make
the exclusion name a decoy while the real path stays writable, within one run.

**A denied path may not contain a writable root** *(on inspection: the
symmetric check was wrong)*. The resolver refuses —
`Resolve_error.Denied_overlaps_writable` — when a denied path contains or
equals a writable root, because deny lowers last and would mask the root
itself, leaving the agent unable to tell an emptied workspace from a deleted
one. A denial *inside* a writable root is admitted and enforced: that is a store
kept in the workspace, and masking that subtree is the point. The symmetric
check broke every blackbox test, whose harness deliberately puts Mentat's home
inside the test workspace.

**An empty write set must emit no socket rule.** `unix_socket_policy` filtered
its allow by the writable roots and emitted it unguarded; an empty predicate
list is an SBPL allow with no filter, i.e. unconditional outbound under a
restricted network. It was unreachable only because both generators prepended
the scratch, and a read-only posture reaches it now.

## 5. What a denial says, and to whom

Detection moves out of the shell tool into `run_route`
(`mentat_workspace_io.ml:1368`) — the only point holding the policy, the
evidence, the termination and the bytes at once. Seven of eight spawn sites have
no advisory today, including `ocaml_eval` and `dune_describe`, which spawn dune
and are the cold-cache case exactly.

```ocaml
type confinement =
  | Restricted of stance                (* structural; a closed enumeration *)
  | Refused_write of stance             (* signature matched *)
  | Reads_unattributable of stance      (* Linux + Only: cannot distinguish *)
```

One sum, not a record of three observations: attributability has no per-command
referent, being a function of `(backend, reads)`, both sealed at resolution. The
payload is a closed `stance`, never a rendered string and never a root *count* —
counts would make the pinned expect tests machine-dependent.

**Rendered only on signal.** `Restricted` is always computable but is attached
to a failing result only when a signature matched or the backend is
unattributable, or every `grep` returning exit 1 would carry confinement prose
into the model's context.

**Cold start is a workspace precondition, not a command failure.** The first
confined command in a session whose report indicates a filesystem refusal
produces one durable `Workspace_notice` — self-deduplicating, drained
transactionally, injected into the turn's continuation *and* rendered in the
transcript (`lib/workspace/notice.mli`, `lib/tui/turn.ml:794`):

```
this workspace has not been built on this machine

A confined command was refused filesystem access. Mentat cannot fetch
git-pinned sources or download a toolchain: those need the network and a
writable cache. Run your build once in your own terminal, then continue.
```

No modal, no retry, no second execution, no `Tool.Call.t` — so the git bridge,
`Tool_boot` and background sessions participate, which a decision-based loop
structurally cannot. It also dissolves the worst corner: a cold escalated build
exceeds `max_timeout_ms = 600_000` (`lib/tools/shell/shell.ml:9`) against a CI
first-uncached-run budget of 90 minutes (`.github/workflows/ci.yml`), and a
backgrounded command cannot be escalated — so escalation was never a working
answer for cold start.

**Per-command recovery stays escalation, model-mediated.** The model retries
with `escalate=true`; the existing `Decision.Request.Permission` arm fires, now
quoting the recorded prior denial.

## 6. Laws

- **L1 (admission is blast radius, not confidentiality).** The read set narrows
  reach; it admits whatever its directories contain. *Prevents:* treating a
  missing read root as a security incident, or an admitted one as a promise.
- **L2 (denial is closed, mode-invariant, and wins).** A `Deny` entry is
  unreachable regardless of which entry would otherwise admit it, on every
  backend and in every mode. *Prevents:* a future derivation, or a mode that
  zeroes the other lists, re-exposing Mentat's record.
- **L3 (the environment is inherited, never rewritten).** No component hands a
  child a path the policy has not authorized, and no variable is substituted —
  not `HOME`, not the temp family. *Prevents:* the class this RFC exists for,
  and the second-order one where a redirect conceals whether a grant was ever
  correct.
- **L4 (one detector).** Denial classification is computed once, at the spawn
  boundary. *Prevents:* seven of eight sites lacking a diagnosis.
- **L5 (an owned path is materialized under a symlink guard).** Created
  `mkdir(0700)`, `lstat`-checked on `EEXIST`, entered lexically; `<root>/.git`
  is the one exception and keeps RFC 0009 L6's filter. *Prevents:* both the
  existence-filter inversion and the symlink pivot.
- **L6 (recovery is a route, never a policy).** A per-command exception selects
  a different lowering; it never mutates the policy, re-seals, or re-mints
  Identity. *Prevents:* killing the in-flight turn.
- **L7 (an ambient-derived root is admitted at its variable's base).** Never at
  a version- or switch-specific descendant. *Prevents:* a toolchain switch
  moving the Identity and breaking every suspended turn — the property that
  holds today only by accident of descendant collapse.

## 7. Threat model

**Adversary:** the model's tool-call stream — prompt-injected, jailbroken, or
mistaken. It issues arbitrary tool calls within an approved permission policy.
Not a local attacker with a shell, not a malicious Mentat binary, not the user.

**Wants:** exfiltrate credentials and unrelated source; **persist** — arrange
execution outside the sandbox via hooks, rc files, or a substituted toolchain
binary; damage files outside the workspace; defeat the record by rewriting the
session store, which carries the accepted permission policy and the identity
resume revalidates against.

**Promises.** **P1** write containment outside the resolved `Write` set.
**P2** record integrity and confidentiality: no **confined** command reads or
writes Mentat's three directories. **P3** network default-off. **P4**
attributability. **P5** read narrowing, best-effort, *and identical on both
backends*.

**Non-promises.** **N1** confidentiality of anything under an admitted read
root. **N2** anything about an escalated command. **N3** persistence inside the
project. **N4** anything about a consented operation. **N5** any kernel-level
guarantee. **N6** resource exhaustion, or egress under `network=enabled`.

## 8. Drawbacks

- **Escalation bypasses the deny set.** `lower_escalated_argv` emits no
  enforcing prefix (`lib/sandbox/mentat_sandbox.mli:296-300`, RFC 0002 L6), so
  an escalated command runs with §3's denies gone. P2 is scoped to *confined*
  commands for exactly this reason. Slice 6 narrows this — a scoped grant stays
  sandboxed and refuses any entry within a `Deny` — but it does not close it:
  total `escalate:true` remains, and must, for the commands scoped grants cannot
  express.
- **Identity drift is currently unreadable and non-terminal.** When the check at
  `mentat_agent_step.ml:421-432` fails it returns a *retryable* failed tool
  result whose message renders class and backend only (`identity.ml:28-33`), so
  both sides print the same string. The contract's identity is fixed for the
  turn, so every subsequent executable tool call fails identically while verbs
  keep working — the model retries to the step limit against a message that
  reads like a no-op. `mentat_sandbox.mli:77-78` promises resume "re-approves
  under the new profile"; nothing re-approves. Slice 5 fixes both.
- **We give up graceful degradation.** A `$HOME`-relative write that silently
  succeeded now fails. Mitigated by `/tmp` and `$TMPDIR`; a tool that *aborts*
  without a writable home now fails where it previously limped.
- **The ordered policy is a real refactor** of `policy.ml` and both generators.
- **Materialization creates directories the user did not ask for.**
- **Three paths get a confidentiality guarantee the read roots do not.**
  Deliberate — Mentat's record is the one thing it can be complete about.

## 9. Divergence from the reference

Codex is the baseline; the burden of proof is on divergence.

**Project-scoped reads (kept).** Measured: 37 read roots, 9 under `$HOME`, every
credential store already denied — ssh, aws, gh, npmrc, gitconfig, shell history,
and Mentat's own `auth.json`. Codex's full-disk read would newly expose all of
them, and codex ships **no** default deny for its own `~/.codex/auth.json`. This
is the one divergence with measurement behind it, which is why §4 keeps
`--tmpfs /` rather than adopting `--ro-bind / /`.

**A stripped child environment (kept).** Codex inherits a `core` set; we strip
ambient secrets and agent sockets. Strictly stronger, no UX cost.

**Deny defaults (kept).** Codex has the more general facility — `ReadDenyMatcher`
with globs — and ships no defaults. For this threat, defaults beat generality.

**Adopted:** the inherited environment (§2), the ordered policy (§3), `/tmp` and
`$TMPDIR` writable, and the full deny lowering including `--perms` and the file
case (§4).

**Not resolved here.** Codex's `with_additional_permissions` grants *specific
paths* for one command while staying sandboxed; our escalation is
all-or-nothing, so the recovery we steer users toward is "no sandbox at all" to
write one lock file. Strictly worse, and the direct cause of the §8 bypass. Out
of scope only because it belongs to the permission stack. §12 Q3.

**On RFC 0002 L8, and an accounting correction.** L8 says the digest changes
when a read root is added and that "the environment is excluded structurally:
the library never sees one". Both are true — the library is pure — and L8 also
lists a generator change as a mover, so Identity is deliberately a **lowering**
fingerprint, not an intent fingerprint. An added read root is squarely within
its meaning. What the closing clause invites a reader to miss is that the
*derivation* couples the identity to thirteen ambient variables on macOS, twelve
on Linux (`TMPDIR` reaches the policy only via `darwin_user_dirs`, empty on
Linux). Three places in the tree have already missed it, including a pinned test
comment asserting the false reading (`test/unit/test_sandbox.ml:1262-1265`).

**An earlier draft of this section claimed deleting the carry removes two or
three of those variables. That was wrong in both directions.** It removes
**none**: §12 Q4 rules that the replacement roots honour `OPAMROOT`,
`XDG_CACHE_HOME` and `XDG_CONFIG_HOME`, so the resolver still reads the same
variables to place the same directories — only the export to the child goes
away. And §3 **adds five** that reach the policy nowhere today —
`XDG_DATA_HOME`, `XDG_STATE_HOME`, and the three `MENTAT_*_HOME` overrides —
because the deny set derives from them. **This RFC increases the identity's
environment coupling from thirteen to eighteen.** That is the honest number, and
L7 is what keeps the increase from being felt: coupling to a *base* directory
that does not move when a toolchain switches costs nothing at resume.

## 10. Rationale and alternatives

**Codex-style full-disk read.** Rejected on measurement, above.

**Staged detect-then-retry**, where the shell tool runs confined, classifies,
and raises an approval to re-run escalated. Fully designed, and rejected on
three grounds: it **runs the command twice**, wrong for `rm -rf x && curl …` and
against RFC 0000 D1's at-most-once guarantee; the trigger is a heuristic, and a
modal fired on an affix match trains click-through on the one dialog that must
never be reflexive; and `Decision.Requested.make` requires a `Tool.Call.t`, so
the git bridge, `Tool_boot` and background sessions cannot raise one — a
decision-based loop is unimplementable for exactly the sites a boundary-level
loop exists to serve. §5's notice has none of these properties, which is why it
is the cold-start answer. **Kill criterion:** if the model converts a fired
report into an `escalate=true` retry less than half the time, build the staged
version — it also needs `?denied:` on `make_staged`, since a staged tool with an
effectful prepare cannot honestly settle as denied with the effect erased.

**Dropping `"share"` from the PATH sibling list** (`derive.ml:447`) — a
one-token diff that removes `~/.local/share` and hence today's single reachable
path into Mentat's store. Rejected as the *whole* answer: it is one derivation of
several, `~/.local/state` is reachable independently, and after §2 inherits
`HOME` the store is reachable by more routes, not fewer. It is a reasonable
belt-and-braces addition alongside §3, not instead of it.

**Also rejected:** reusing `protected_paths` for deny (dropped by the membership
filter at `policy.ml:59`); a configurable deny facility; denial classification
inside `lib/sandbox` (RFC 0002 alt 6, pinned by the dependency-law test); a
`~recover:bool` on `Command.run` (the deleted route enum returning).

## 11. Cost, slices, kill criteria

An earlier draft's `+160/−40` was wrong by roughly 4× and priced tests at zero.
Corrected, production plus test:

| slice | status | buys |
|---|---|---|
| 1 · `--remount-ro /`, `/tmp` + `$TMPDIR` writable | **landed** | stops silent data loss; restores `/tmp` |
| 2 · inherit the environment, delete the carry and the scratch | **landed**, −411 net | closes the bug class |
| 3 · deny set, materialization, symlink guard | **landed** | P2 |
| 4 · ordered policy | **landed** | escape hatch; unifies the rungs |
| 5 · boundary report | **landed** | seven sites gain a diagnosis |
| 5b · workspace precondition notice | ~40 / ~30 | **not built** |
| 6 · scoped grants | **landed**, ~330 / ~180 | recovery stops meaning "no sandbox" |

Slice 6 came in at roughly half the estimate, and as one slice rather than two:
the read and write halves differ only in the `Access.t` a caller names, so
splitting them would have been staging without a seam. It is also much smaller
than priced because `Policy.grant` needed no resolution rule of its own — a
widened clause takes part in the ordinary law — and because the widened value is
an ordinary sealed route, so lowering, evidence and obligation discharge all
applied unchanged. The one place it needed a rule is the one the law would have
answered silently: a grant at or beneath a denied path is refused rather than
admitted-and-lost.

Slices 1–3 came in under the estimate because the environment work was mostly
deletion. Slice 4 is the last piece that moves the identity digest, so it wants
to land with them rather than after 0.1.0.

**Sequencing.** Slice 1 ships alone and first. **Slice 3 lands with or before
slice 2** — §2 removes the accident that partly protects Mentat's store, so
inheriting `HOME` without the deny set is a net regression. Slice 4 is the least
urgent: it changes no containment outcome. The identity digest moves in 2, 3 and
4, and each move forces a drift wave on suspended sessions; that wave is **free
before 0.1.0 and expensive after** (`CHANGES.md:1`), so the identity-affecting
work should land together, now.

**Success:** no new entry is added to the admission lists for two release cycles
because a user hit a missing root.

**Kill criteria.** *Environment adoption:* if within one release cycle a
first-class toolchain (dune, opam, git) fails because it cannot write under
`$HOME` and one writable entry does not fix it, the redirect earns its place
back, with the carry. *Deny set:* if it needs a fourth path within two cycles,
the not-a-general-facility ruling is wrong.

## 12. Questions the campaign resolved

**Q1 — does an opam switch break suspended turns? No, today, and L7 keeps it
that way.** The premise is false on a standard layout: `~/.opam` is admitted as
a base, and descendant collapse absorbs every switch-relative root, so
`opam switch set` is identity-neutral. It *does* fire for a switch prefix
outside `~/.opam`, and far more often for non-opam launcher changes — `nvm use`,
rbenv and pyenv shims, a `direnv` entry, `$TMPDIR` differing between a GUI and
an ssh login. The digest moving is correct: Identity is a lowering fingerprint
by explicit design. **The response to it moving is the bug** (§8), and slice 5
fixes it. Normalizing ambient roots out of the digest is rejected — it would
leave `$TMPDIR` and `~/.cache` coupled anyway, and it would let an already-
approved pending call execute under a *wider* read set than its approval, with
no signal.

**Q2 — read-only mode: neither the catalog nor the ruling was wrong, and the
coupling that made it look like a fork is gone.** Removing `shell_tools` cuts
what read-only subagents actually do; permitting escalation contradicts RFC
0002's no-mutation promise. The question only had force because the stance was
inferred from an empty writable list, so granting read-only a temp directory
would have flipped it. With `~mutates` stated (§3) the two are independent: a
read-only route gets scratch space and keeps `Denied`. What remains is the
silent dead end — the report tells the model to retry with `escalate=true` on a
route where `route` will answer `Escalation_denied`. One branch in §5's report
emits the non-escalatable text instead, and it generalises to every
non-escalatable spawn site, which is most of them.

**Q3 — yes, escalation should be scoped, as slice 6.** A grant is a **route**, a
third lowering function beside `lower_argv` and `lower_escalated_argv`, so L6
holds verbatim and the turn contract never moves for a widening that expires
with the command. Paths are named as ordinary `Access.Path` values, one per
entry — mentat's grants are exact-`Access.t`, so a remembered approval cannot
silently widen and we need no analogue of codex's intersection pass. **One
deliberate divergence from `policy_transforms.rs`:** a grant entry within or
equal to a `Deny` entry is *refused*, not appended, because codex ships no
default deny set and §3 is one.

**Q4 — honour `$XDG_CACHE_HOME`; do not derive the cache root from `$HOME`.**
Forced by L3: the child now inherits the variable, so deriving a different
directory recreates the policy/child disagreement this RFC exists to remove.
The cost is the accounting in §9, and L7 makes it painless.

## 12b. Still open

**During implementation.** The `Reads_unattributable` wording. Whether denied
paths should be masked at both the lexical and canonical spelling where `$HOME`
is itself a symlink (Silverblue's `/home → /var/home`).

**Out of scope.** Whether Mentat's own writes to its store should be confined.

## 13. Future possibilities

A structural refusal carrying the specific path; a private `DUNE_CACHE_ROOT`
under the scratch if confined builds prove to lose materially to unpublished
cache entries.

*The anti-ratchet rule: something appearing here is never a reason to accept
this or a later RFC.*
