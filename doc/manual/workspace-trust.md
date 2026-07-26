# Workspace config and trust

Workspace trust decides whether repository configuration, instructions,
skills, executable tools, and project processes may activate at all. It does
not approve an operation or widen the selected sandbox; those are
[permission policy](permissions.md) and the [command sandbox](sandbox.md).

For how the three boundaries fit together, see [Security](security.md).

Workspace trust is persistent consent to activate repository-controlled inputs
and processes. The decision is stored user-side for the canonical project root
and has three states:

- `unknown`: no decision has been stored;
- `untrusted`: the user explicitly chose restricted operation;
- `trusted`: repository config, instructions, skills, and project processes may
  activate after reload.

Unknown and untrusted workspaces have identical runtime capabilities. Mentat
does not open project config, scan project instructions or skills, offer the
generic shell or evaluator, start project notices, or run automatic
Dune/Merlin/Git discovery. Native source inspection, search, and structural
edits remain available according to workflow, permission, and sandbox policy,
as do user-owned config, instructions, and skills. Directly reading or editing
`.mentat/config.json` is also allowed; its values do not become effective until
the workspace is trusted and Mentat reloads. Files edited while restricted may
therefore execute after later activation.

In an interactive TUI, an unknown workspace gets a preflight before the normal
app, session creation, or project process startup. Continuing restricted is
selected by default and remembers `untrusted`; trusting activates only after a
clean host reload and remembers `trusted`; exiting stores nothing. The selected
sandbox continues to bound filesystem and network access. Headless commands
never prompt or infer consent: they continue restricted and explain how to run
`mentat trust`. Permission bypass does not bypass workspace trust.

Activation makes repository execution eligible; workflow still owns its
lifetime. Build engages configured Dune/Merlin producers when a turn binds.
Plan and Review start none, and switching away from Build stops the current
project watcher, clears its captured project snapshot, and resets Merlin
resolution before the new runner is installed.

Once trusted, project config (`.mentat/config.json`) and project-local config
(`.mentat/config.local.json`) are reduced to this shared allowlist:

- `model`, `small_model`, and `reasoning`;
- `run.max_steps`;
- `permission.unattended`;
- `workspace.tooling`;
- `tools.editor`.

Trusted automatic Dune, Merlin, notice, and mutation integrations are
product-owned startup behavior, not model tool calls, so they do not create
permission prompts. Their subprocesses still use the run's sealed sandbox and
degrade rather than retrying unsandboxed.

Workspace `run.max_steps` may tighten a value selected outside the workspace
but cannot widen it. Workspace `permission.rules` are always stripped. Every
other supported key outside the allowlist — including permission mode, sandbox
posture, shell program, provider endpoints, web enablement, private-network
access, all web resource limits, and user instruction/skill switches — is
ignored. The web limits are user-owned host policy: a repository may neither
widen nor tighten them. Invalid, unreadable, or oversized workspace config
degrades rather than failing host startup.
`mentat config show --origins` reports every ignored, clamped, or degraded
input.

The allowlist still applies after trust. Trust is consent to consume named
project inputs, not a grant of arbitrary authority: permission mode, sandbox
posture, shell and Merlin programs, provider endpoints, web enablement,
private-network access, web resource limits, and instruction/skill switches
remain user-owned.
Project prose and skills may influence the model once enabled, but operations
proposed as a result still pass through permission and confinement.

`mentat trust DIR` and `mentat untrust DIR` record canonical workspace paths in
the user-side `trust.json` store. Mentat resolves a symlink in `DIR`, but records
that exact canonical directory rather than searching its ancestors for a
repository root. `untrust` stores an explicit refusal instead of deleting the
entry. Config and state directories use mode `0700`, and the trust and lock
files use mode `0600`.

The trust grant is deliberately narrow. It does not silently enable future
hooks, plugins, MCP servers, credential helpers, environment mutation, or
project-selected executables. Those capabilities require their own explicit,
content-bound approval design.
