# Deterministic TUI tests

Developer reference for the in-process TUI test suite at `test/tui`. It drives
the real application — `App.init/update/view/subscriptions`, the real
`Mentat_tui.Runtime` command interpreter, and the real Matrix input parser —
through the same event vocabulary a terminal produces, and observes the rendered
cell grid. Session effects are scripted at the `Mentat_client.Driver.t` seam:
the harness supplies a fake client driver and narrow `Runtime.Local.t`
callbacks, so launch queries, session attach/replay, persistence, and provider
turns cross the same Runtime composition boundary as the executable, without a
real engine, store, or provider wire. The application is not launched in a
terminal subprocess or pty; the Matrix backend is headless (`matrix.test`).
Executable-local callbacks (file enumeration, browser, local shell) may launch
real subprocesses, and the scheduler uses short real-clock breaths to let
genuine IO complete. Application-visible time remains virtual and
test-controlled.

This is the operational contract: how the harness stays deterministic, which
boundary belongs in which suite, and what breaks determinism. The harness itself
is `test/tui/harness/`, the library `tui_harness_next`. Its public surface is
deliberately small:

- `Key`, `Project`, and `Screen` provide shared fixtures and assertions;
- `Tui` drives the deterministic in-process application and scripts every
  client-side effect.

The real-process (PTY) coverage lives separately under `test/tui/pty/`:
`test_cli_launch.ml` drives the real `mentat` binary against
`mentat_fake_provider_server.exe` through a `pty_session` helper built on
`matrix.pty` and `matrix.vte`. Application tests stay directly under `test/tui/`.

## Choose the test boundary

| Boundary under test | Home |
| --- | --- |
| Application state, rendering, input decoding, the Runtime command interpreter, client-scripted provider turns, decisions and dialogs, session attach/replay/persistence, executable-local effects, resize/reflow | `test/tui/` |
| The real binary end-to-end: raw-mode and alternate-screen setup/restore, primary-screen goodbye output, real `SIGWINCH`, OSC title emission, CLI launch wiring, and the real engine/store/provider through the fake provider server | `test/tui/pty/` |
| CLI argument resolution, exit codes, and real OS-sandbox enforcement | Cram/black-box tests |

The in-process suite stops at the `Mentat_client.Driver.t` seam: it observes
that the application exits and returns the expected outcome, but it cannot
observe bytes written to the primary screen after the alternate screen closes,
terminal modes, signals, whether the CLI launched the application correctly, or
the behavior of the real engine, store, and provider wire. Keep at least one
real-process test in `test/tui/pty/` for those contracts, and drive them against
`mentat_fake_provider_server.exe` so the provider wire is real without reaching
a live model.

## The model

`Tui.run ~name f` boots a fresh Home UI at a pinned instant and calls `f` after
launch. Inside `f`, a test:

- **drives** with `Tui.keys` / `Tui.enter` / `Tui.paste` / `Tui.resize` (raw
  bytes through the real input parser, so decode regressions stay covered);
- **waits** with `Tui.settle` (quiescence) or `Tui.advance dt` (virtual time) —
  never a sleep, never a substring-with-deadline;
- **scripts provider work** by passing `?turns` to `Tui.run` and releasing each
  held settlement with `Tui.finish_turn`; `Turn_script.complete`/`fail` start
  and hold a turn, `Turn_script.tool`/`ambiguous_tool` add its tool lifecycle,
  `Mutation_script` supplies independent mutation evidence, and
  `Tui.next_task_board` / `Tui.notice` release scripted board replacements and
  ephemeral notices while a turn is held;
- **scripts decisions and children** with `?decision_answers`
  (`acknowledge_decision` / `resolve_decision` / `fail_decision`),
  `?decision_continuations` for post-resolution provider work, and
  `?child_sessions` released through `Tui.next_child_fact` / `Tui.drain_child`;
- **scripts account, settings, and lifecycle** with `?login_scripts`
  (`next_login_step`), `?model_selections` / `?permission_reviews`
  (`finish_model_selection` / `finish_permission_review`),
  `?configuration_results` / `?readiness*` (`finish_settings_queries`), and
  `?lifecycle_results` (`finish_lifecycle_result`);
- **asserts** a full frame via `Tui.print` at a pinned size (default 80×24).
  Substring facts are forbidden; a frame is a `[%expect]` golden, promoted with
  scoped `dune promote test/tui/<file>`.

A held script is the point of observation: work parked on a scripted turn,
decision, login, or query stays in its mid-flight state for exactly as long as
the test leaves it held, so the frame between `keys` and `finish_turn` is a
stable golden. Unknown effects — anything the harness was not scripted for —
fail loudly rather than being silently mocked.

`Tui.print` normalizes the grid through `Mentat_test_censor`: the temporary
project root is censored to a stable token, and volatile started session ids are
replaced with readable labels (`session-visual-00001`, and `session-fork-…` for
forks) that occupy the same 20 terminal columns, so canonicalization can never
move later cells and disguise a layout regression. Ages, elapsed counters,
spinner phase, and session-id stamps are deterministic functions of
test-controlled time, so they may appear verbatim in goldens.

## The virtual-clock contract

The suite is deterministic only because the virtual clock is the single source
of time and *nothing advances it behind the test's back*. Four invariants hold
that line. Each was violated at least once, and each violation reappeared as a
hang or a flake — treat them as load-bearing.

- **One-shot redraws render at the current instant, never by advancing the
  clock.** Under a real clock, `request_redraw` coalesces an event storm to
  `target_fps` by *waiting out* the frame interval — free, because wall time
  passes anyway. Under a virtual clock that "wait" would be an advance the
  harness did not ask for, and it would move the very clock `Sub.every` timers
  read. The headless `matrix.test` backend renders a one-shot immediately and
  the clock does not move.

- **A settle never steps time.** `Tui.settle` wakes the loop and re-settles when
  async work (a scripted responder dispatching) lands a message or redraw
  between the loop parking and the harness observing it; the backend renders
  that at the current instant, so no cadence-flush advance is needed. `settle`
  also does **not** quantize to the next whole second — there is no sub-second
  render drift to erase, so quantizing would only march timers a test never
  asked to fire.

- **Live animation is the only thing the backend auto-advances.** When a
  `Sub.on_tick` animation holds the render cadence (reduced motion off), the
  backend advances its virtual now to the frame deadline so the idle callback
  observes a presented frame. This is the one place virtual time moves without
  the script; it is bounded to a frame interval and only fires while the loop is
  active. Reduced motion is the harness default (`?reduced_motion` defaults to
  `true`) precisely so most tests sit in the idle regime where waiting consumes
  zero virtual time; opt back in per test with `~reduced_motion:false` when the
  animation *is* the behavior under test.

- **The epoch is small on purpose.** `epoch = 1000.` in the harness `tui.ml`,
  not a real Unix timestamp, and `Matrix_test` runs at `target_fps 10`. The
  runtime and mosaic pace off sub-second intervals; at a real-epoch magnitude
  (~1.7e9) those intervals fall below the float ULP, so `last +. interval -. now`
  and `now -. last >= interval` disagree at the boundary and a cadence check
  stalls the loop. A small base keeps the arithmetic exact. Nothing in the suite
  renders an absolute wall-clock date, so the base is free to be small.

## Settle and advance

- `settle` blocks until the backend is parked, the probe reports no pending
  Mosaic or Runtime work, no redraw is queued, and a short scheduler drain turns
  up nothing new. Work parked on a **held** script counts as settled — that held
  mid-flight state is exactly what a test observes. It carries a loud budget; it
  never hangs or advances virtual time.
- `advance dt` steps virtual time forward by `dt` in cadence-sized steps, firing
  due `Sub.every` / `Sub.on_tick` timers, then settles without quantizing.
  Elapsed counters tick exactly `dt`; ages age exactly `dt`. Do not use it as an
  async-work barrier — it changes elapsed counters and spinner state; release a
  held script with the matching `finish_*` / `next_*` helper instead.

The only real-clock sleep in the harness is a 1 ms scheduler breath while real
IO (tool subprocesses, fswatch systhreads, executable-local callbacks) is
genuinely in flight; park-waiting is condition-based, not polled.

## The environment contract

**The environment a run's app sees is a function of that run's own parameters —
never of the runs before it in the same executable.** `Project` pins bindings
with `Unix.putenv`, so a name one run sets would otherwise still be set for the
next. `Project` therefore records the bindings it pins and restores the
conditional ones the next run does not override, so a developer's own exported
keys stay out of the frames and no run inherits a state it never asked for.

The rule for reading a suspicious golden follows from this: **before chasing a
layout bug, check whether the frame is rendering a state the test never asked
for.** A run's frame should be explicable from its own `~env`, `~providers`,
`~sessions`, and scripted turns alone.

## Writing a test

- **Full frames, pinned size.** `Tui.print` prints the whole 80×24 grid,
  normalized. Assert the frame, not a line.
- **Batch scenarios that share state into one `Tui.run`** — a boot is ~60 ms.
  `test_turn` prints several frames (working, ticked, settled) from one boot.
  Don't contort unrelated scenarios together.
- **Give every `Tui.run ~name` a globally unique value across the suite.** The
  temporary project is `/tmp/mentat-tui-<name>` and is cleared at startup;
  duplicate names in concurrently running executables can delete each other's
  workspace.
- **Send Enter as its own write** (`Tui.enter`), never `"/cmd\r"` in one chunk.
- **Reduced motion and workspace tooling are off by default**; opt in per test
  with `~reduced_motion:false` or the tooling `~env` only when the animation or
  the dune footer is the behavior under test (workspace tooling on spawns a
  real `dune`, so initial readiness depends on host scheduling).
- Seed sessions, child sessions, drafts, and Git state through the `?sessions`,
  `?child_sessions`, `?draft`, and `?snapshot` fixtures, constructed from the
  `Project` passed to them so every summary and replay document is anchored to
  the rendered workspace.

Useful examples are `test_turn.ml` for a held turn and virtual time,
`test_question.ml` / `test_permission.ml` / `test_plan.ml` for decision dialogs,
`test_threads.ml` for scripted child feeds, and `test_review.ml` for Git-backed
fixtures and launch state.

## Debugging

- A hang is almost always a busy loop, not a deadlock: the mosaic loop fiber
  spins without yielding and starves the script's breath. Confirm with
  `ps -o %cpu` (≈100 %) and locate it with macOS `sample <pid>` — a virtual-time
  spin shows a tower of `read_events` / `compute_timeout` / `handle_every_subs`,
  not the render pipeline.
- A flaky golden is a determinism bug, not something to re-golden. When a frame
  varies run to run, find what advanced the clock outside `advance`: trace every
  `Matrix_test.set_now` (its magnitude tells you which timer fired) and check it
  against the four invariants above before touching the harness.
- Never run `--auto-promote` on the shared `test/tui` runtest alias. It can
  promote unrelated failing executables and enshrine a transient frame. Run one
  executable in isolation, inspect its output, then promote only its source
  path with `dune promote test/tui/<file>.ml`.
- Run the one exe directly (not `dune runtest`) for a fast tight loop, and force
  a sweep — `for i in $(seq 1 30); do ./_build/default/test/tui/<exe>; done`
  — before trusting a determinism fix. A harness change re-runs `test_home` and
  `test_turn` plus 30× sweeps of both. A change to release, decision, or
  child-feed serving also sweeps the representative dialog, tool, and thread
  tests that exercise that seam.
