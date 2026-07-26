# Custom commands

Custom commands are Markdown prompt templates invoked by the user as
`/name arguments`. They expand into an ordinary user prompt. Command files are
not automatically inserted into model context, and the model cannot invoke them
as tools.

## Create a command

Create `.mentat/commands/review.md` in a trusted project:

```markdown
---
description: Review a pull request for correctness.
argument-hint: <base>
---

Review the changes against $1. Focus on correctness and missing tests.
```

Invoke it in the TUI or in a headless run:

```text
/review main
```

The model receives `Review the changes against main...`, not the raw invocation.
The TUI command palette shows `description` and `argument-hint`; a command with
an argument hint is inserted into the composer for completion instead of being
submitted immediately.

## Discovery and precedence

Mentat recursively discovers `*.md` files in this order:

1. `<workspace>/.mentat/commands`
2. `<workspace>/.agents/commands`
3. `<workspace>/.claude/commands`
4. `<config-home>/commands`

The first valid candidate for a name wins. The three project roots are not
scanned until the canonical workspace is trusted; config-home commands remain
available. Compatibility roots can be disabled without disabling
`.mentat/commands`.

A path becomes a colon-separated command name. For example,
`.mentat/commands/git/commit.md` is invoked as `/git:commit`. Every path segment
must be 1 to 64 bytes, start with a lowercase ASCII letter or digit, and contain
only lowercase letters, digits, and hyphens. Hidden entries and non-Markdown
files are skipped. An invalid candidate is reported rather than renamed.

These user configuration keys control discovery:

| Key | Default | Effect |
| --- | --- | --- |
| `commands.enabled` | `true` | Master switch; `false` produces an empty catalog. |
| `commands.project` | `true` | Gate all three project roots. |
| `commands.compat` | `true` | Gate only `.agents/commands` and `.claude/commands`. |
| `commands.disabled` | `[]` | Disable the listed names in every root before precedence is resolved. |

For example:

```sh
mentat config set commands.compat false
mentat config set commands.disabled '["deploy", "release"]'
```

Project configuration cannot use these keys to activate or reshape the command
surface; authority-bearing keys outside the trusted project allowlist are
ignored. Configure commands from the user or explicitly selected extra config.

## Template expansion

Expansion is one left-to-right pass:

| Token | Expansion |
| --- | --- |
| `$ARGUMENTS` | The complete argument string after surrounding spaces and tabs are trimmed. |
| `$1` through `$9` | Positional runs separated by spaces or tabs; a missing position becomes empty. |
| Other `$` text | Preserved literally. |
| `@path` | The text of a workspace-relative regular file, ending at the next whitespace byte. |

Arguments and embedded file bytes are inserted literally and are not scanned a
second time. An argument containing `@secret` therefore does not cause a file
read, and an embedded file containing `$1` does not substitute it. There is no
shell quoting or shell execution in expansion.

An `@path` reference is resolved from the workspace root only when the workspace
is trusted. Mentat rejects absolute paths, missing or non-regular files, and
symlink escapes; lexical `..` segments cannot climb above the workspace root.
Reads that cross the 4 MiB input ceiling also fail. Embedded text is repaired to
UTF-8 and truncated at 128 KiB with a visible marker. A failed reference aborts
expansion and sends no turn. Because the path token consumes all non-whitespace
bytes, put punctuation after whitespace rather than writing a reference such as
`@notes.txt,`.

Only `description` and `argument-hint` frontmatter affect a command. `model` and
`allowed-tools` are explicitly unsupported, and all other keys are ignored with
a warning. A command cannot select a model, grant a tool, bypass permission
review, weaken the sandbox, or activate an untrusted workspace.

## Frontend behavior

The TUI lists active custom commands after built-in commands in the slash
palette. A built-in slash name is reserved there and always wins over a custom
command with the same name. `mentat run` has no built-in slash dispatcher: it
expands a matching active custom command, otherwise it sends the slash-leading
text as a literal prompt. Avoid built-in names when a command must behave the
same in both frontends.

An unknown slash command is not an error. It is sent literally, so adding a
matching command file later intentionally changes that prompt's meaning. The
browser frontend currently submits slash-leading text literally and does not
expand custom commands.

The expanded body, including any `@file` content, becomes model-visible user
content. With a hosted model it leaves the machine under the provider policy
described in [Security](security.md#data-leaving-your-machine).

## Inspect commands offline

```sh
mentat commands list
mentat commands list --json
mentat commands show review
mentat commands show review --json
```

`list` reports active, shadowed, disabled, and invalid candidates plus discovery
warnings. `show` accepts an active name and prints its origin, digest, metadata,
and raw unexpanded template. These commands resolve config and trust and read one
filesystem snapshot; they do not create a session or contact a model provider.
