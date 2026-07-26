# Roadmap

## Phase 1 — Baseline (current)

Provide a harness that replaces Claude Code, Codex, opencode, or pi for its
users. Not full feature parity — the scope is defined by adoption friction:
users of those tools should feel at home. That scope is essentially complete;
the current focus is stabilization.

Release gate:

- Private beta users (onboarding now) run mentat daily for a week without
  complaining: zero data loss, zero P0s.
- All mentat development happens in mentat.

Also on the critical path:

- **Distribution and onboarding** — install path (static binaries, opam) and
  docs.

**Outcome:** first public release. A quiet one — the loud announcement waits
for Phase 2 numbers.

## Phase 2 — OCaml optimization

Prove, quantitatively and qualitatively, that mentat is better at working on
OCaml projects than generic agents.

- **Eval suite.** Today it is basically useless. Rebuild it around non-trivial
  tasks with metrics that translate to user experience. From then on, those
  numbers drive every change to mentat.
- **Benchmark methodology.** Superiority claims run the competitor agents
  through the same harness under identical conditions, and the methodology is
  published with the numbers.
- **Linter.** Separate project, integrated so the agent gets live feedback on
  its own changes — same push channel as the Dune diagnostics notices.
- **OCaml tools and skills.** Iterate on the structural tools and built-in
  skills against eval results.
- **Dune integration.** Deepen the live build-loop integration.
- **Knowledge base.** Experiment with a rich, queryable OCaml knowledge base
  the agent can consult during work.

**Outcome:** users report mentat outperforms other agents on OCaml work;
benchmarks show it; smaller and open models complete OCaml tasks that usually
require larger models. The second half is also the economics that make Phase 3
residency affordable. The loud announcement to the OCaml community lands here,
numbers in hand.

## Phase 3 — The always-live swarm

Up to here mentat is one-off: you launch it, it works, it stops. Phase 3
inverts the direction — events initiate, the agent works, the human reviews.
Mentat becomes a resident agent swarm: a process that runs all the time,
responds to events, and does independent work that makes the codebase better,
with no input from you beyond review. `dune build --watch` for code quality —
resident does not mean cloud; the default is a local daemon.

Most of the organs exist: goals with budgets, decision-parking, the workspace
watcher's event push, the CR/XCR convention, multi-agent delegation, the
review surface as the human interface. `lab/` is the prototype: a human
authors standing programs, the agent executes them continuously, gated by
measurement. Unprompted is not undirected — precision over recall, with the
Phase 2 numbers as the bar for what may reach a human.

The surfaces are viewports onto the resident agent, by interaction latency:

- **GitHub integration** — the asynchronous viewport, and resident-mentat v0:
  event-driven PR review on the exec+JSONL substrate (the exit-3/`run reply`
  contract already fits approval flows). First target: mentat reviews mentat
  PRs.
- **Editor integration** — the synchronous viewport: the agent working
  alongside you. Expose the protocol/client seam (RFC 0007/0013) as a server;
  ACP is the cheapest route to Zed/JetBrains/Neovim.
- **Web app** — the supervisory viewport: agents, review queue, budgets.
  `mentat_web` as a second frontend on the same client the TUI uses,
  validating the server seam.
- **Desktop app** — maybe, on the same seam.

The ladder: interactive → headless one-shot → scheduled → event-driven →
self-directed standing programs. Phase 3 climbs it rung by rung, staying true
to the philosophy: open-source, local by default.

**Outcome:** mentat runs continuously against a repository and does useful
work with no human input beyond review — resident deployments in OCaml shops
and on community projects.

## Phase 4 — Formal verification

The highest differentiator. Phase 1 makes the agent safe to run; Phase 4 makes
the code it writes verified. Verification raises the resident agent's autonomy
ceiling — the more the agent can prove, the less lands in review.

- Workflows where the agent reaches for formal verification whenever it can
  and it makes sense. Candidate wedges: Gospel specs with generated property
  tests as the on-ramp; translation-validation of refactors; verifying our own
  sandbox policies.
- Specialized model training on our workflows, tooling, and languages (OCaml,
  Rocq, or whatever we settle on) — the largest single investment on this
  roadmap.

**Outcome:** mentat gets traction in safety-critical contexts.

## Non-goals

- **MCP.** Deliberate: the tool catalog stays curated and first-party, not a
  plugin bus.
- **Anthropic subscription auth.** Not viable — Anthropic no longer permits
  third-party subscription use. API keys only.
- **Windows.** Not supported for now; the sandbox backends are Linux (bwrap)
  and macOS (Seatbelt).

## Exploratory, cross-phase

- **Workflows** — semantics and UI both. What does a deepsearch workflow look
  like in the TUI? A doc workflow? Candidate dev workflows: design / design
  review; benchmarking (the `lab/` program is a prose encoding today — can
  workflows capture it more precisely without becoming bloated?).
