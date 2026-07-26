# Command sandbox

The command sandbox decides what an approved tool or spawned process may
access. It does not approve the operation itself; that is
[permission policy](permissions.md).

This page covers modes, filesystem read scope, writable and protected paths,
network policy, child environments, and enforcement backends. For how the
three boundaries fit together, see [Security](security.md).

The sandbox applies to the `shell` tool, fixed-command search helpers, OCaml
tools that spawn Dune, Merlin, ocamlfind, or a toplevel, and automatic trusted
project integrations. The host resolves one posture, gates it before credential
or session effects, seals it against a platform backend, and hands command
executors the sealed spawn capability. Shell results additionally carry
evidence saying whether confinement was enforced, refused, not requested, or
declared external.

The spawn plan also owns the canonical working directory. Confined cwd must be
inside the resolved readable roots; Bubblewrap enters it after mounting the
policy, and direct process runners fork into that same directory. An invalid,
missing, or out-of-scope cwd refuses before a child starts.

Shell command facts distinguish Mentat-enforced, externally confined, and direct
execution routes. Enforced identity includes its exact read, write, and network
posture. Sandbox refusal produces no route, no permission prompt, and no child.
Project-read, restricted-network execution and explicitly selected external
boundaries receive product credit; read-all, network-enabled, and direct routes
remain reviewable. Fixed host tools do not expose their implementation argv as
command facts. Model-authored evaluator source is itself a command fact, with
its language, source, cwd, and confinement in exact permission identity. Shell
escalation is a `direct` command fact plus a separate custom access, so an
enforced command grant cannot approve dropping confinement.

## Modes

| Mode | Command behavior |
| --- | --- |
| `read-only` | Reads follow `sandbox.read`; writes are limited to a private run scratch directory and network is denied. Build retains its interaction verbs, native reads/search, confined `shell`, `ocaml_eval`, enabled web fetch, and applicable non-editing OCaml tools. It omits exactly `write_file`, `edit_file`, `apply_patch`, `ocaml_ast_edit`, `ocaml_replace_expressions`, and `ocaml_rename`. Shell escalation is unavailable. |
| `workspace-write` | Reads follow `sandbox.read`. Writes are allowed only under resolved writable roots, with protected carve-outs. Network is restricted by default. |
| `danger-full-access` | Commands run without Mentat filesystem or network confinement. They still receive the exact host-constructed child environment. |
| `external-sandbox` | Mentat records that an external boundary owns confinement. Commands are not wrapped, but still receive the exact host-constructed child environment. |

The mode precedence is the `--sandbox` flag, then `sandbox.mode`, then the
built-in `workspace-write` default.

## Filesystem read scope

`sandbox.read` selects what confined commands may read:

| Value | Read behavior |
| --- | --- |
| `project` | Default. Reads are limited to the workspace, `sandbox.readable_roots`, executable search roots, OCaml toolchain roots, and the platform runtime files required to launch commands. |
| `all` | Reads may reach the host filesystem wherever ordinary filesystem permissions allow. |

Configured readable roots must be absolute or `~`-relative. They must already
exist, resolve physically, and may not name the filesystem root or the user's
home directory. The resolver reports an invalid root before the run starts;
there is no silent fallback to broader reads.

Ambient toolchain variables — `OPAM_SWITCH_PREFIX`, `OCAML_TOPLEVEL_PATH`,
`OCAMLLIB`, and similar — are recovered best-effort so a command resolves the
same toolchain it would from a login shell, and are treated more leniently than
configured roots. A value that names no usable directory — an unexpanded
placeholder such as the `%{toplevel}%` that `dune exec` leaks, a stale path, or a
file where a directory is expected — is skipped with a logged warning rather than
refused, so a launcher artifact the user never set cannot brick a run. The one
exception is a toolchain value that resolves to a broad root, which still fails
closed (below) because silently widening reads is never acceptable.

Project scope resolves physical roots once and shows their origin in
`mentat sandbox explain`. The active OPAM switch is admitted as a whole because
OCaml executables need its libraries, stublibs, findlib metadata, and sibling
tools. A linked Git worktree's `gitdir` and `commondir` are parsed without
executing Git and admitted read-only. Platform runtime roots expose some
machine facts to commands; project scope is a bounded confidentiality boundary,
not a claim that command output contains only repository text.

Broad roots fail closed: `/`, the user's home directory, and an ancestor of the
workspace cannot enter a project-scoped allowlist indirectly through config,
`PATH`, or OCaml toolchain variables. Readable roots may be files or
directories; writable roots must be directories. Requested roots must still
exist when a command starts, or the sandbox reports a stale-policy refusal.

Selecting `sandbox.read=all` makes the confined modes not confidentiality
boundaries. A confined command can then read files outside the workspace and
return their contents in tool output. Exact environment reconstruction withholds
ambient credentials, and restricted network reduces command-side exfiltration,
but neither prevents disclosure to the model. The default `project` scope, or an
external isolation boundary, preserves host-file confidentiality. If
read-anywhere is deliberate — for example with a local model — use the ordered
opt-in in [Permission rules](permission-rules.md#prompt-free-confined-shell-for-a-local-model).

Native file tools have a narrower boundary: they accept workspace paths, check
realpath containment when dereferencing them, refuse symlink escapes, and do
not expose arbitrary host-file reads.

## Writable and protected paths

`workspace-write` makes these roots writable:

- every workspace root;
- a private mode-`0700` home and temporary directory owned by the run;
- absolute or `~`-relative paths in `sandbox.writable_roots`.

Existing paths are realpath-canonicalized before the policy is generated, so
the described path and the backend-enforced path agree across symlinks such as
macOS `/tmp`.

The following remain read-only even when nested under a writable root:

- existing workspace `.git` and `.mentat` entries;
- linked-worktree Git metadata outside the workspace;
- the user config, credential, and trust-store directories;
- the project config directory;
- the session store root.

Native mutation tools share the `.git` and `.mentat` protection. They also
validate workspace containment independently of the command sandbox.

Machine-global toolchain state — the OPAM root and the XDG cache, config, state,
and data directories — is admitted read-only under the project read scope so
tools resolve their real locations even though the child `$HOME` is the private
scratch. It is never writable: command writes stay confined to the workspace and
that scratch. A confined `dune build` therefore reads but cannot populate the
shared `~/.cache/dune` cache; dune detects the unwritable cache directory, warns,
disables the cache, and builds normally into the workspace `_build`. The result
is a graceful loss of cross-project cache acceleration, not a build error.
Granting write access to the shared cache is deliberately left to the explicit
`sandbox.writable_roots` opt-in rather than the default: the cache is a
machine-global content-addressed store shared by every project, so a confined
command writing it could influence unrelated projects' later builds — the exact
out-of-workspace effect the write boundary exists to prevent.

## Network policy

`sandbox.network=restricted` is the default for `read-only` and
`workspace-write`. Linux Bubblewrap creates a separate network namespace;
macOS Seatbelt omits network permission from its profile. Set
`sandbox.network=enabled` or `MENTAT_SANDBOX_NETWORK=enabled` to permit network
for confined shell commands.

This setting does not authorize a command under the permission policy and does
not control provider calls or web tools. Web fetching has separate enablement,
private-network checks, URL policy, and permission facts.

## Exact child environments

Every spawned route — confined, direct, externally sandboxed, and approved
escalation — receives one exact environment constructed when the run resolves its
sandbox. Tools cannot add per-call overlays and no route inherits the ambient
process environment.

The child environment contains:

- `PATH`, validated as absolute non-empty entries;
- private run-owned `HOME`, `TMPDIR`, `TMP`, and `TEMP`;
- deterministic non-interactive pager, terminal, and color settings;
- valid locale and OCaml toolchain path variables from a fixed allowlist.

Optional inherited values that are malformed are omitted. Values are never
included in sandbox diagnostics. After repository activation, an existing
canonical workspace-local `_opam/bin` leads `PATH`; a restricted repository
cannot contribute executable roots.

## Enforcement requirements and backends

`sandbox.require` controls the run-start gate:

| Value | Gate behavior |
| --- | --- |
| `enforced` | Default. Confined modes require a working Mentat backend; an external declaration is not sufficient. |
| `enforced-or-external` | Accepts either a working Mentat backend or `external-sandbox`. |
| `off` | Does not fail the run at startup. A confined mode with no backend still refuses each shell command rather than running it unconfined. |

`--require-sandbox` forces `enforced-or-external` for one invocation.

Mentat selects `/usr/bin/bwrap` on Linux and `/usr/bin/sandbox-exec` on macOS.
The Linux path is fixed: a `bwrap` found elsewhere on `PATH` is not selected.
Bubblewrap is unavailable on WSL1 and is probed with a minimal isolated process
before use. Other platforms have no built-in enforcing backend. A restricted
run fails closed when its applicable requirement is not met. See
[Installation](installation.md) for the host prerequisite matrix.

## Per-command escalation

In a `workspace-write` sandbox, the model can request `escalate:true` on one
shell call. The request adds a separate `shell.escalate` access whose subject
is the exact command text. Reaching execution means both the ordinary command
access and the escalation access were allowed by policy or reviewer.

An approved escalation:

- runs that one command without filesystem or network confinement;
- retains the policy's exact child environment;
- records `not_requested` sandbox evidence;
- does not broaden to another command, even after an exact-conversation answer.

Read-only mode refuses escalation without opening a permission review. In
`danger-full-access` and `external-sandbox`, escalation is ignored because the
requested lack of Mentat confinement is already the current posture.
