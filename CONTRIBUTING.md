# Contributing

Mentat is early and moving fast. Interfaces, configuration, and session
formats change without notice, so the most valuable contribution right now is
not a patch. It is a precise account of something that went wrong while you
were using it on real work.

## What helps most

1. **Bug reports from real sessions.** Especially anything where the agent
   damaged a file, ignored a permission boundary, burned tokens without
   converging, or produced a change that did not compile.
2. **Usage feedback.** Where it wasted your time, what you expected a command
   to do, which part of the output you could not read.
3. **Documentation fixes.** If a documented command did not behave as
   documented, that is a bug in the docs and worth reporting as one.
4. **Small, focused patches** for things already agreed in an issue.

Large refactors and new subsystems arriving unannounced are the least useful
contribution, because the architecture is still moving under them. Open an
issue first.

## Filing a good bug report

`mentat report --last --note "what you expected instead"` collects most of the
below into one file — version, platform, the `doctor` checks, your configuration
with credentials redacted, the session's state, and the diagnostics logs and
crash reports naming that session. Add `--with-session-export` to include the
conversation itself, which is the most useful attachment and the most revealing;
read it before you send it. The command shows you what it is about to collect,
asks before writing, and uploads nothing. See
[Troubleshooting](doc/manual/troubleshooting.md).

[Open an issue](https://github.com/invariant-hq/mentat/issues) with that file, or
by hand with:

- `mentat --version`, your OS, and the provider and model in use.
- What you asked for, and what happened instead.
- The exact error text, copied rather than described.
- The session id, printed as `session saved; resume with: mentat run resume
  's-...'`. Sessions are durable, so you can inspect one after the fact with
  `mentat session diff` and `mentat session export`.
- For anything sandbox- or permission-related, the output of
  `mentat sandbox status` and `mentat permission list`.

A session export is the single most useful attachment. Read it before you
attach it: it contains your prompts, and it can contain source from the
workspace.

For anything with a security impact, do not open an issue. Follow
[SECURITY.md](SECURITY.md).

## Building from source

Mentat uses Dune package management. You need Dune 3.22 or later; `dune pkg
lock` provisions the OCaml compiler and every dependency, so you do not need
opam and you do not need a switch.

```sh
git clone https://github.com/invariant-hq/mentat.git
cd mentat
dune pkg lock
dune build
```

`dune.lock/` is generated and not committed, so lock before your first build
and again whenever dependencies change. The first build compiles the toolchain
and can take a while; later builds are incremental.

Dune 3.24 or later is required: the build uses the Rocq prover. Parts of the
codebase are formally verified — the confinement kernel's semantics live in
`theories/` as Rocq definitions with machine-checked laws, and the OCaml under
`lib/*/kernel/` is extracted from them and promoted into the tree by the
build. Three things to know:

- The first build also compiles Rocq from source (15–20 minutes, once per
  toolchain change; the same applies in CI containers).
- On a fresh checkout the first build may fail with
  `Theory "Corelib" has not been found` while the prover is still being
  built; run `dune build` again. The cause is documented in `dune-workspace`.
- Never edit files under `lib/*/kernel/` — they are generated. Change the
  theory in `theories/` and rebuild; the build re-extracts, re-checks the
  proofs, and refreshes the promoted sources.

Run it from the checkout:

```sh
dune exec mentat --
```

Building from source also needs Git, a native C toolchain, `pkg-config` or a
compatible `pkgconf`, and GMP development headers. See
[Installation](doc/manual/installation.md) for the full matrix.

## Tests

```sh
dune runtest
```

This is what CI runs, on Linux and macOS. The full suite is slow; while
iterating, scope it to the directory you are working in:

```sh
dune runtest test/tools
```

## Conventions

[`AGENTS.md`](AGENTS.md) is the authority on how code in this repository is
written, and it is short. Read it before your first patch. The rules that
catch newcomers most often:

- **Never silence a warning** and never underscore an unused variable to make
  the build green. A warning means the implementation is incomplete; fix it.
- **No compatibility shims.** Redesigns delete the old concept and make old
  shapes fail loudly, unless compatibility is explicitly the task.
- **Public API contracts live in `.mli` files**, not in markdown. Errors that
  cross a boundary are structured `result` values, not exceptions and not
  strings that callers parse.
- **Commit subjects** are `<type>(<scope>): <Imperative subject>`, with a body
  that explains the causality rather than inventorying the files.
- Never run `dune clean`, and do not pass `--force` or a custom `--build-dir`.

Prose that ships — the README, the manual, release notes — follows the manual's
existing conventions: second person, present tense, active voice, sentence-case
headings, and every documented command executed before it is merged.

## Documentation layout

- `doc/manual/` — user-facing workflows and observable CLI behavior.
- `doc/architecture.md` and `doc/dev/` — contributor material.
- `doc/rfc/` — design documents, written at design time.
- `.mli` files and tests — the sources of truth for API contracts and for
  exact CLI output.

[`doc/README.md`](doc/README.md) explains the boundary between them. New pages
get an index line there.

## License

Contributions are accepted under the ISC license. See [LICENSE](LICENSE).
