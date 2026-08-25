# Configuration

Mentat configuration is JSON, resolved from layered sources. `mentat config
show --origins` prints the effective configuration and where each value came
from; `mentat config path` prints the user file path by default and accepts a
layer flag for either workspace file.

## Files and precedence

Values are resolved in increasing precedence:

1. User config: `~/.config/mentat/config.json` (or
   `$XDG_CONFIG_HOME/mentat/config.json`).
2. Project config: `.mentat/config.json` — shared, checked into the project.
3. Project-local config: `.mentat/config.local.json` — personal, gitignored.
4. Extra config file named by the `MENTAT_CONFIG` environment variable.
5. `MENTAT_*` environment overrides.
6. Runtime overrides, such as run flags (`--model`, `--sandbox`, ...).

Mentat canonicalizes the requested cwd and uses that exact directory as both the
workspace root and the discovery root. It does not search parent directories for
`.git`; project config therefore resolves only from `<cwd>/.mentat/`.
Initializing either project config file maintains an exact
`config.local.json` entry in `.mentat/.gitignore` while preserving other lines.
Do not ignore the whole `.mentat/` directory if shared config, skills, or
commands are committed.

Storage roots are independent of these config layers and cannot be redirected
by project files:

- `MENTAT_CONFIG_HOME`: user-authored config plus auth and trust stores;
- `MENTAT_DATA_HOME`: durable sessions, workflow artifacts, and workspace state;
- `MENTAT_STATE_HOME`: machine-local prompt history, logs, and crash reports.

On Unix, data and state fall back through `XDG_DATA_HOME`/`XDG_STATE_HOME` to
`~/.local/share/mentat`/`~/.local/state/mentat`.

Project layers activate only when the canonical workspace root is trusted. In an
unknown or explicitly untrusted workspace, Mentat does not open either file and
`mentat config show --origins` reports that project configuration is disabled.
Once trusted, the files are still reduced to a narrow allowlist: permission
rules and authority-bearing keys are ignored, and budget values may tighten but
not widen values selected outside the workspace. Every dropped input is named on
stderr by `config show`, by `config get` for the key it affects, and at the
start of a headless run, so a key that does nothing says so. See
[Workspace trust](workspace-trust.md) for the complete boundary.

## Commands

```sh
mentat config path                 # print the selected config path
mentat config show [--json] [--origins]
mentat config validate [--strict] [PATH]
mentat config get KEY
mentat config set KEY VALUE [--project | --project-local]
mentat config unset KEY [--project | --project-local]
mentat config init                 # scaffold a config file
```

Editing commands write the user config by default; `--project` targets
`.mentat/config.json` and `--project-local` targets `.mentat/config.local.json`.
These explicit file operations remain available before trust, but the values
activate only after `mentat trust` records the workspace root.

`config validate` reports two kinds of finding. Errors — a member of a supported
field with the wrong shape or an unusable value — fail the check. Warnings name
keys that parse but have no effect: a member no field spells (a typo, or a
setting from another tool), and, when the file is a workspace layer, a key
outside the shared allowlist. Unknown members are preserved across edits rather
than rejected, so they warn by default; `--strict` fails on them instead, which
is the form to run in CI.

## Keys

Keys supported by `get`, `set`, and `unset`:

| Key | Meaning |
| --- | --- |
| `model` | Main model selector, e.g. `openai/gpt-5.6-sol`. |
| `small_model` | Auxiliary model for cheap side calls such as automatic session titles; an unset, unknown, or unavailable selector falls back to the main model. |
| `reasoning` | Default reasoning effort: `none`, `minimal`, `low`, `medium`, `high`, `xhigh`, or `max`. |
| `tui.thinking` | Whether the TUI shows thinking summaries (default `true`). |
| `tui.mouse` | Whether the TUI captures terminal mouse events (default `true`; a non-empty `MENTAT_DISABLE_MOUSE` flips the default). |
| `tui.theme` | TUI color theme: a built-in name, a user theme file basename, or `auto`; defaults to `mentat-dark`. |
| `tui.theme_dark` | Theme `tui.theme=auto` selects on a dark terminal (default `mentat-dark`). |
| `tui.theme_light` | Theme `tui.theme=auto` selects on a light terminal (default `mentat-light`). |
| `tui.diff_layout` | Review diff layout: `auto` (default; side by side once the pane is wide enough, else stacked), `unified`, or `split`. |
| `notify.enabled` | Enable TUI completion and decision notifications (default `true`). |
| `notify.channel` | Notification channel: `off`, `bell`, `osc9`, `osc777`, `auto` (default), or `command`. |
| `notify.when` | Notify `unfocused` (default) or `always`. |
| `notify.command` | JSON argv prefix for the `command` notification channel. |
| `notify.on` | JSON event list containing `turn-done`, `decision`, or both (both by default). |
| `tools.editor` | File-editing tool family: `auto` (default), `apply-patch`, or `string-replace`. |
| `providers.ID.base_url` | API root override for provider `ID`. |
| `run.max_steps` | Positive model-response limit per turn (env `MENTAT_MAX_STEPS`). |
| `run.subagent_max_concurrent` | Maximum concurrently running subagents across a session tree (default `4`). |
| `run.subagent_max_depth` | Maximum spawn depth; direct children are depth 1 (default `2`). |
| `run.subagent_max_exchanges` | Per-run limit on model-originated parent/child exchanges (default `8`). |
| `compaction.auto` | Whether context is compacted automatically when it grows large (default `true`). |
| `revert.merge` | Whether reverting a superseded selection three-way merges rather than refusing (default `true`). |
| `permission.unattended` | Headless review policy: `block` (default) or `deny`. |
| `sandbox.mode` | Build sandbox: `read-only`, `workspace-write` (default), `danger-full-access`, or `external-sandbox`. |
| `sandbox.require` | Enforcement gate: `enforced` (default), `enforced-or-external`, or `off`. |
| `sandbox.read` | Confined filesystem read scope: `project` (default) or `all`. |
| `sandbox.readable_roots` | Additional absolute or `~`-relative readable roots for `sandbox.read=project`. |
| `sandbox.writable_roots` | Additional absolute or `~`-relative writable roots for `workspace-write`. |
| `sandbox.network` | Confined shell-command network posture: `restricted` (default) or `enabled`. |
| `sandbox.env_inherit` | Child-environment inheritance: `allowlist` (default) or `all` — inherit every ambient variable that survives the built-in secret/agent floor. |
| `sandbox.env_exclude` | Case-insensitive `*` globs stripped from the inheritable child-environment sets, on top of the floor. |
| `sandbox.env_include_only` | When non-empty, only inheritable variables matching these globs reach the child (the structural core always does). |
| `shell` | Shell program used for shell commands (defaults to `SHELL`, or `COMSPEC` on Windows). |
| `workspace.tooling` | Whether project-scoped OCaml/Dune tools enter fresh turn catalogs: `auto` (default), `on`, or `off`. |
| `instructions.global` | Load the global `AGENTS.md` from the config home. |
| `instructions.project` | Load project instruction files. |
| `instructions.claude_md` | Load `CLAUDE.md` compatibility files. |
| `instructions.project_max_bytes` | Byte budget for project instruction text. |
| `commands.enabled` | Enable user-invoked custom commands (default `true`). |
| `commands.project` | Discover custom commands from workspace roots (default `true`). |
| `commands.compat` | Include `.agents` and `.claude` command roots (default `true`). |
| `commands.disabled` | JSON array of command names disabled in every root. |
| `image.max_bytes` | Maximum decoded image size after best-effort downscaling (default 5 MiB). |
| `image.max_dimension` | Maximum pixels on either image side (default `8000`). |
| `image.max_count` | Maximum images in one input (default `20`). |

The full configuration surface is larger; `mentat config show --json` prints
every effective key but omits unset optional keys and withholds credentials: an
API key reads `[REDACTED]`, and a provider `base_url` keeps the endpoint it
names while losing any `user:password@` it carries. `config get KEY` still
returns the selected value verbatim.
Additional groups accepted by `get`, `set`, and `unset` include:

- `notices.*` — host notice producers: `fswatch`, `cr_comments`,
  `dune_diagnostics`, `dune_build`.
- `skills.*` — skill discovery: `enabled`, `builtin`, `project`, `compat`,
  `disabled`, `paths`, `catalog_max_bytes`.
- `ocaml.*` — OCaml toolchain: `merlin_program` (the Merlin executable and its
  arguments).
- `web.*` — web fetch and search: `enabled`, `allow_private_network`,
  `fetch_max_bytes`, `output_max_chars`, `timeout_ms`, `max_timeout_ms`,
  `search_provider` (`exa` by default), `exa_api_key`, and `parallel_api_key`.
  The four resource limits are user-owned host policy; project and
  project-local config cannot set them. The two search-provider keys can also
  come from `MENTAT_EXA_API_KEY` and `MENTAT_PARALLEL_API_KEY`.

See [Instructions and skills](instructions-and-skills.md) for instruction-file
precedence, skill discovery, context budgets, and per-run overrides. See
[Custom commands](custom-commands.md) for command authoring and expansion.

`permission.rules` is the structured exception: edit it directly in user or
extra config as `{ "version": 1, "items": [...] }`, inspect it with
`mentat permission list`, and remove individual writable rules with
`mentat permission remove`. See
[Permission rules](permission-rules.md) for the matcher JSON, source behavior,
and evaluation order.

Only `model`, `small_model`, `reasoning`, `run.max_steps`,
`permission.unattended`, `workspace.tooling`, and `tools.editor` may come from
project or project-local config. Every other key above is user-owned host
policy; put it in user config or an extra config file.

## Notifications

`notify.*` controls operating-system/terminal notifications from the TUI. It is
separate from `notices.*`, which controls workspace observations shown inside a
conversation.

The default `auto` channel emits a terminal bell and OSC 9 notification. The
`command` channel runs `notify.command` with the notification title and body
appended as its final two arguments; an empty command disables that channel.
Command failures are ignored and their output is not written into the TUI.
Unknown entries in `notify.on` are ignored with a warning.

## Themes

`tui.theme` names the palette the TUI starts in. Thirteen themes are built in:
`mentat-dark` (the default), `mentat-light`, `solarized`, `solarized-light`,
`gruvbox`, `gruvbox-light`, `catppuccin`, `catppuccin-light`, `tokyonight`,
`tokyonight-light`, `nord`, `one-dark`, and `one-light`. `/theme` browses them
and writes the selection back to user config.

`tui.theme=auto` follows the terminal's own light/dark color scheme, switching
between `tui.theme_dark` (default `mentat-dark`, and the theme used until the
terminal answers) and `tui.theme_light` (default `mentat-light`). Both keys are
ignored unless `tui.theme` is `auto`.

Any name that is not built in loads `<config-home>/themes/NAME.json`, limited to
1 MiB, when the TUI starts. A theme is a partial JSON object whose values are
`#rrggbb`, named ANSI colors, or `default`. An optional `extends` key names the
built-in theme to overlay; without it a same-named built-in is the base, else
`mentat-dark`:

```json
{
  "extends": "nord",
  "accent": "#00e5ff",
  "muted": "bright-black",
  "selection_bg": "default"
}
```

The color roles are `accent`, `mode_plan`, `mode_review`, `muted`, `faint`,
`rule`, `success`, `warning`, `error`, `history`, `chip_fg`, `user_bg`,
`overlay`, `code_keyword`, `code_type`, `code_string`, `code_number`,
`selection_bg`, and the diff group: `diff_added_bg`, `diff_removed_bg`,
`diff_added_sign`, `diff_removed_sign`, `diff_gutter_fg`,
`diff_added_gutter_bg`, `diff_removed_gutter_bg`, `diff_added_emphasis`, and
`diff_removed_emphasis`. Every built-in theme draws the diff backgrounds,
gutter backgrounds, and word emphases from the same green and red, tinted for a
dark or a light terminal, so an addition and a deletion read the same way
whichever theme is selected; the `+` and `-` signs follow each theme's own
`success` and `error`. A theme file may still override any of them. Omitted or
invalid roles keep the base's color, so a partial theme is always complete. A
missing, unreadable, or non-object theme falls back to its base and logs a
warning.

## Image limits

`image.max_bytes` applies to decoded image bytes before base64 transport and is
the binding size limit for attached images and images returned by `read_file`.
An oversized attachment is downscaled toward the limit when `sips`, `magick`, or
`convert` is available, then rejected if still too large. `image.max_dimension`
is a per-side decompression-bomb guard, and `image.max_count` limits the media
blocks in one input. See [Headless runs](headless.md#image-input) for `-i` usage.

## Subagent limits

`run.subagent_max_concurrent` limits live children across the whole session
tree. `run.subagent_max_depth` limits nesting, with the root session at depth 0.
`run.subagent_max_exchanges` limits model-originated messages and questions
between a parent and child during one run; user-originated replies do not consume
that exchange budget.

## Workspace tooling

In a trusted workspace, `workspace.tooling` gates the project-scoped OCaml/Dune
tools offered on each fresh turn. A turn already in progress keeps the tool set
with which it started.

| Value | Behavior |
| --- | --- |
| `auto` | Default. Enable the integration when `dune-project` or `dune-workspace` resolves inside the workspace to a regular file. An in-workspace symlink to a regular marker counts. |
| `on` | Always engage the tooling. |
| `off` | Never engage it. |

Unknown and untrusted workspaces behave as if the integration did not engage,
regardless of the configured value. Build mode still offers pure OCaml syntax
search and structural edits when project tooling is off. A read-only Build run
omits structural edits and `ocaml_rename`; when project tooling is enabled it
retains the non-editing Dune/Merlin/eval tools. The
`MENTAT_WORKSPACE_TOOLING` environment variable accepts the same `auto`, `on`,
and `off` values but cannot override workspace trust.

Trust does not override run mode. A trusted read-only Build run retains confined
`shell`, `ocaml_eval`, and non-editing Dune/Merlin tools, while omitting
`write_file`, `edit_file`, `apply_patch`, `ocaml_ast_edit`,
`ocaml_replace_expressions`, and `ocaml_rename`.

## OCaml toolchain resolution

The OCaml tools — Dune describe, Merlin queries, and the eval tool — spawn
`dune` (and friends) from the environment Mentat inherited at launch. Mentat
never runs
`opam env` itself; it resolves each program by walking a fixed ladder,
first match wins:

1. **`MENTAT_DUNE`** — an explicit executable override (generally
   `MENTAT_<PROGRAM>`: the program name uppercased, with non-alphanumeric
   runs as `_`, so Merlin's is `MENTAT_OCAMLMERLIN`). An override that is
   set but not an executable file fails the resolution outright; it never
   falls through to the rungs below.
2. **`PATH`** — the inherited search path. This is the normal case: launch
   Mentat from a shell where `command -v dune` prints a real path and
   nothing else engages.
3. **`$OPAM_SWITCH_PREFIX/bin`** — the variable `eval $(opam env)` exports
   for the active switch. This recovers sessions launched from a context
   that had the switch active but lost `PATH` (editor terminals, desktop
   launchers).
4. **`<workspace root>/_opam/bin`** — an opam local switch at the workspace
   root, considered only after that canonical workspace is trusted.

If your shell shows `dune` but Mentat reports it missing, the usual cause is
that the shell exposes it only through an alias or an interactive-only hook
that child processes do not inherit. Relaunch from a shell where
`command -v dune` prints a real path, or set `MENTAT_DUNE`.

Two surfaces show the resolution without starting a session: `mentat doctor`
carries an `ocaml toolchain` check — and beside it a `parity` check, which
resolves the workspace the way a run would and warns when the `dune` a
confined command's `PATH` finds is not the one mentat resolves, because two
dune binaries sharing one `_build` re-execute each other's work on every
alternation — and `mentat sandbox explain` a `toolchain=` line. Both print where `dune` resolves from — or, when it does not, every
permitted rung that was checked. They skip the project-local `_opam` rung until
workspace trust. With `sandbox.read=project`, Mentat admits resolved `PATH`
directories and the active OCaml switch to the read policy. An unrecognized
toolchain layout may need an explicit user-level `sandbox.readable_roots`
entry; `mentat sandbox explain` shows every readable physical root and why it
was selected.

## Filesystem Notices

`notices.fswatch=true` publishes "Workspace files changed" notices while a run
is active. The notice is a snapshot-diff summary since the previous watcher
scan, with a bounded preview of changed workspace-relative paths.

The filesystem watcher ignores any path with a `.git`, `_build`, `_opam`, or
`.mentat` path segment. Ignored directories are not scanned and do not appear in
filesystem notice batches.

`notices.fswatch=false` suppresses file-change notices, but the shared
filesystem watcher may still run when `notices.cr_comments=true`; the CR
comment observer uses the same watcher batches. Watcher startup and runtime
failures degrade to warning notices when filesystem notices are enabled instead
of failing the run.

## Providers and models

Configuration selects models and provider base URLs, but authentication,
credential precedence, readiness checks, local model downloads, and compatible
server setup are separate workflows. See
[Providers and accounts](providers.md) for the complete path.

Permissions, sandboxing, workspace config, and trust compose as described in
the [security guide](security.md).
