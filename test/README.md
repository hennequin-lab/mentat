# Tests

The suite splits into five trees, each named by what it drives, not by
technique. A change is covered at the lowest tree that can prove it.

## The five trees

- **`unit/`** — per-library windtrap suites: one `test_<lib>.ml` per `mentat_*`
  library, testing the documented contract of that library in process. This
  tree also carries the **law-witness
  dune rules** — build-time `(rule)`s that grep a leaf library's dune `deps`
  and fail if it lists a forbidden upstream (e.g. `client_law8`,
  `provider_runtime_law1`, `tools_output_boundary`), so architectural
  dependency laws are enforced by the build itself.

- **`tools/`** — per-tool boundary goldens: windtrap expect suites that drive
  each tool through its public provider-JSON → `Call.run` → durable-JSON seam
  with no mocks. It also holds `test_tools_dependency_laws.ml`, a
  compiler-libs witness pinning the module-dependency shape of `lib/tools` and
  `lib/tui`.

- **`blackbox/`** — cram families exercising the real `mentat` binary end to
  end, one per concern under `test-cases/<family>/<family>.t`. The cram spine
  (`test-cases/dune`) applies to the whole subtree; `support/` holds the
  `mentat_cram` helper (its normalization delegates to the shared censor, below)
  and `bin/` the subprocess fixtures, all put on `PATH` by `setup.sh`.

- **`tui/`** — in-process frame goldens: one windtrap `test_<surface>.ml` per
  TUI surface, rendered through the `tui_harness` library (`harness/`) and run
  under a `mentat.tui.<surface>` suite name. Its `pty/` subtree runs the binary
  as a **real process** under a pseudo-terminal (`mentat.tui.pty.<name>`),
  including `test_terminal_host.ml` (raw-byte terminal-host) and the
  `test_cli_launch*` suites. The launch journeys are partitioned across three
  exes — `test_cli_launch` (launch/session), `test_cli_launch_trust` (trust
  gate), and `test_cli_launch_misc` (completion, title, logging, review) — so
  windtrap's per-exe serial execution runs them in parallel; their shared
  fixtures live in `cli_launch_fixture.ml`. The partition is a pure parallelism
  mechanism: every case, and its screen golden, is byte-identical to its
  single-exe form. Two trust-gate cases whose fixture path wraps across terminal
  rows stay in `test_cli_launch` (not the trust exe), because the harness
  qualifies each fixture root by the executable basename (`Project.with_temp`),
  and a renamed exe would shift that digest inside the golden.

- **`promptgen/`** — the prompt generator's own diagnostics: a golden for a
  valid builtin skill plus the build-failure output for malformed ones, with
  fixtures under `cases/` (kept out of `prompts/` so the real build never sees
  them).

## Shared substrate

`test/support/` holds `mentat_test_censor`, the one output-normalization policy
(paths, timings, volatile ids), placed under neither consumer so neither owns
it. Both the blackbox cram suite (through `mentat_cram`) and the TUI harness
(`tui/harness/screen.ml`) censor through it, so a rendered frame and a cram
transcript normalize identically.

## One file per library — and the deliberate exception

`unit/` holds exactly one `test_<lib>.ml` per library. The one exception is
`mentat_tools`: it gets **one file per tool** in `tools/`
(`test_tools_<tool>_expect.ml`), because each tool is an independent public
boundary and its goldens would otherwise collide in a single file. Its one
unit-level residue is `unit/test_tools_web_transport.ml` — the SSRF / IP-boundary
vetting for the web transport, a security decision a real DNS server cannot
exercise hermetically through the binary.

A second, mechanical exception exists for speed. windtrap runs a suite's cases
serially inside one exe, so dune can only parallelize *across* exes. A heavy
suite may therefore be split into `test_<name>_<part>.ml` files that share their
fixtures through a support module, purely to run the parts in parallel — the
split changes no case and no golden (see the `test_cli_launch*` partition under
`tui/pty/`). Suite names stay attributable (`mentat.<lib>.<part>`).

## The four fixture levels (layered, not duplicated)

Each fixture targets a different seam, so together they stack rather than
repeat:

1. **`unit/` `llm_test_server`** — an in-process transport fixture: satisfies
   the HTTP client seam without a subprocess, to test provider request/response
   codecs directly.
2. **`unit/bin/fake_llama_server`** — a subprocess fixture emulating a local
   GGUF/llama.cpp server, to test the local-provider path across a real process
   boundary.
3. **`blackbox/bin/fake_provider_server`** — an end-to-end fake LLM the real
   `mentat` binary talks to over the wire, to drive full CLI lifecycles.
4. **`blackbox/bin/fake_dune_rpc_server`** — a build-health fixture emulating
   dune RPC, to drive the doctor / build-health path.

## Gated live seams

Two suites reach real host services and are opt-in, staying hermetic in CI:

- **`MENTAT_DUNE_RPC_LIVE_TEST`** — `unit/test_ocaml_dune.ml` spawns a real
  `dune build --watch`.
- **`MENTAT_TUI_LIVE_TEST`** — `tui/pty/test_tool_permission_live.ml` drives the
  live TUI shell tool through the default sandbox.

Both skip themselves when the variable is unset.

## Running scoped tests

Never run the whole suite; run the smallest scope that covers the change.

- **A windtrap library or surface:** `dune exec test/unit/test_config.exe`,
  filtering cases with `-f <pattern>` (or a bare positional pattern).
- **A cram family:** `dune build @test/blackbox/test-cases/<family>/blackbox`
  (the whole cram suite is `@test/blackbox/blackbox`, and it rides `dune
  runtest` via `(alias_rec blackbox)`).
- **Refreshing goldens:** `dune promote <file>` after a scoped run — always
  name the file, never a bare `dune promote`.
