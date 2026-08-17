---
description: Guides writing effective tests for OCaml code — choosing the test kind with the strongest oracle, using windtrap for unit, property, stateful, snapshot, and expect tests, dune cram tests for executables, and the coverage and mutation workflows that verify the tests themselves. Use when writing tests, adding a test suite, reviewing existing tests, setting up coverage or mutation testing, or deciding which kind of test fits a behavior. Triggers on phrases like "write tests for this", "add a test suite", "test this function", "property test this", "snapshot test", "review these tests", "check coverage", or "run mutation testing".
---

# OCaml Testing

A test suite is the executable statement of what the code is supposed
to do. When the same session writes both the code and its tests, a test
that merely re-states what the code does is worthless — it will agree
with any bug the code has. Everything here serves one principle:
**maximize oracle strength**. Write the test whose assertion is hardest
to satisfy by accident, prove every test can fail, and never let an
acceptance workflow bless behavior nobody reviewed.

Windtrap is one library for unit, property, stateful, snapshot, and
expect tests, plus coverage and mutation testing. `open Windtrap`;
`test`/`group` declare inert data; `run` executes and exits: 0 all
passed, 1 any failure, 2 nothing ran (the filter-typo case — treat it
as failure, never as success).

This file is the decision layer. The full mechanics live in windtrap's
own `doc/manual/` (a chapter each for assertions, property testing,
stateful testing, snapshots and expect, running tests, coverage,
mutation) and `doc/cookbook.md`. Read the matching chapter whenever you
need mechanics beyond what this carries.

## 1. Survey, then derive the obligations

`test/README.md` is the map of this repo's suite: five trees named by
what they drive, the shared censor, the four fixture levels, and the
gated live seams. Read it before adding a file — where a test goes is
already decided, and §3 below only summarizes the rule.

Then derive the test list from the interface, **before reading the
implementation**. The `.mli` is the spec: every exported value is an
obligation; every documented exception and `Error` case is an
obligation; every invariant or law stated in a doc comment is an
obligation. A suite conceived from the interface constrains what the
code *should* do; one written by reading the implementation merely
describes what it does. Where the `.mli` is silent on a behavior you
must test, the spec has a gap: surface it, or record the assumption
visibly in the test's name, rather than silently inventing the
contract.

## 2. Choose the strongest oracle the behavior admits

Work down this ladder and take the **first** row that fits. Each row
constrains strictly more behavior per line of test code than the rows
below it.

| The code under test is | Write | Why it is strongest here |
|---|---|---|
| A pure function with a law — codec, parser/printer, normalizer, ordering | `prop` over the law | One law constrains the whole input space; shrinking hands you the minimal counterexample |
| A stateful API — store, journal, cache, pool, anything with a lifecycle | `stateful` against a model | Checks laws over *sequences* of calls; finds interaction bugs no unit test reaches |
| A pure function where only specific points are specified | `test` + `equal` through a testable | Exact expected values, written by hand from the spec |
| An executable's observable behavior — CLI parsing, exit codes, error messages, file effects | Cram test in `test/blackbox/` | Tests the wiring no unit test reaches; doubles as CLI documentation |
| Rendered or serialized output too large to hand-write | `snapshot` / `[%expect]` | A reviewed baseline beats a hand-copied string |
| A claim about a value no equality captures | `satisfies ~msg` | Last resort — the failure at least prints the value and names the predicate |

Three rules outrank the table:

- **Expected values come from the spec, never from running the code
  under test.** An expected value captured from the implementation's
  own output is a snapshot with extra steps and none of the review
  discipline. The operational tell: write the assertion *before* first
  running the test. If you had to run the code to learn the value, it
  was a snapshot all along.
- **Normative vs descriptive.** Properties, stateful models, and
  hand-derived `equal` expectations are *normative*: when one fails,
  suspect the code. Snapshots and expect tests are *descriptive*: they
  pin current behavior, and when one fails the question is "was this
  change intended?". Every module needs a normative core.
- **Blackbox first.** Test through the public `.mli`; an internal
  helper is tested through the public path that reaches it. For an
  effectful function with a pure core, extract the pure core and test
  that — don't mock the effects around it. The `tools/` tree is the
  worked example: each tool is driven provider-JSON → `Call.run` →
  durable-JSON, with no mocks.

### The bad-test catalog

Reject these shapes on sight — in review, and in your own output.

*Tests that cannot fail:* **self-confirming** (the expected value was
captured by running the code under test); **vacuous** (`is_some` where
the value matters, "does not raise" on a function that cannot raise);
**tautological** (re-derives the answer with the implementation's own
algorithm, or tests the language).

*Tests that fail wrong:* **blind boolean** — `is_true (a = b)`,
`is_true (n > 0)`: the failure prints `expected true` and hides the
data. Weak predicates are weak oracles too: `is_true (apply Sub 10 4 >
0)` survives the `a - b → a + b` mutant; `equal int 6 (apply Sub 10 4)`
kills it. **Overfit** — asserts incidental detail: the whole help text
to check one flag, the order of an unordered collection (`slist`
exists), timestamps, absolute paths. **Coupled** — depends on another
test's side effects, shared mutable state, the wall clock, or
directory-listing order; it breaks the moment the suite is selected
differently, and `-f`, `--failed` and `--shard` all change which tests
run.

*Tests at the wrong level:* **over-mocked** (needs several fakes to
check one line); **snapshot-of-everything** (one giant baseline nobody
reads, churning until promotion becomes a reflex); **property without a
law** (no round-trip, invariant, oracle agreement, algebraic identity
or metamorphic relation — write `cases` instead).

The catalog is mechanically checkable: nearly every entry either
survives mutants (§8 finds it) or fails with a message that cannot
diagnose. When reviewing tests, run the file-scoped mutation survey
before trusting your eyes.

## 3. Where a test goes

`test/README.md` is authoritative; the rule it encodes is that suites
split along **mechanical and lifecycle** boundaries — a different
runner, a different test mechanism, a different dependency closure or
environment, a different cadence — and **never by test kind**. A
module's round-trip law, its example tests and its error-message
snapshot together form its contract and belong in the same file.
Kinds are already selectable at run time: property and stateful tests
carry automatic tags (`--exclude-tag prop`), slow tests carry `slow`
(`--quick` drops them), and `-f` filters by path.

For this repo that means: one `test_<lib>.ml` per `mentat_*` library in
`unit/`; one file per tool in `tools/`; cram families under
`blackbox/test-cases/<family>/`; one file per surface in `tui/`. The
two deliberate exceptions — `mentat_tools`' per-tool split, and the
speed-driven `test_<name>_<part>.ml` partitions — are documented in the
README with their reasons. **No test code in `lib/`**: expect tests
live in the `(inline_tests)` libraries under `test/tools/` and
`test/tui/pty/`. Keeping `lib/` clean also pays an instrumentation
dividend, since mutation skips any file that declares inline tests.

A snapshot `deps` glob is load-bearing wherever baselines exist:
they are runtime data, invisible to dune, so without

```lisp
 (deps (glob_files_rec __snapshots__/**))
```

editing a baseline does not re-trigger the test.

## 4. Unit assertions

Assert through testables — a printer plus an equality — so failures
print both values with a structural diff. Expected first, always.

```ocaml
equal (list (pair string int)) [ ("a", 1) ] (bindings t);
let id = require_some (find_user "alice") in     (* assert AND unwrap *)
equal int 1 id;
let msg = require_error (parse_port "0") in
equal string "invalid port: 0" msg
```

The vocabulary worth knowing rather than reinventing:

- `equal` / `not_equal` through witnesses: `int`, `string`, `bool`,
  `char`, `bytes`, `int32`, `int64`, `option`, `result`, `either`,
  `list`, `array`, `pair`, `triple`, `quad`, `float eps`,
  `float_rel ~rel ~abs`, `float_exact`.
- `text` — strings printed verbatim and diffed line by line. Use it for
  any multi-line string; `string`'s `%S` rendering buries the
  difference in `\n` soup.
- `slist t cmp` — lists as multisets; `contramap proj t` — compare and
  print through a projection. Together: "these events happened, in any
  order, ignoring noisy fields" in one line.
- `require_some` / `require_ok` / `require_error` / `require_match` —
  assert a shape and hand back its payload.
- Ordering: `greater int ~than:0 n` (and `greater_equal`, `less`,
  `less_equal`), where `is_true (n > 0)` reports only `false`.
- Strings: `contains ~sub` (with `~count:n` for exactly `n`
  non-overlapping occurrences) / `not_contains ~sub` /
  `in_order ~subs:[...]` for substrings that must appear in that order
  / `starts_with ~affix` / `ends_with ~affix`; lists: `mem`.
- Exceptions: `raises exn fn`, `raises_match pred fn` with the `Exn`
  helpers (`Exn.invalid_arg ~substring:"negative"`).
- Convergence: `eventually ~step probe` — probes, steps, probes again,
  returns the first `Some`. `~attempts` bounds the probes (default
  100), `~diagnose` adds state lines to the failure. **Windtrap never
  sleeps**: put the thing that advances the system (mock clock tick,
  event-loop turn, queue drain) in `~step`, never a sleep — a sleeping
  step hides a race instead of exposing it. Where only a real child
  process or PTY can make progress, `eventually` is the wrong tool and
  the loop stays timing-bound.
- Escape hatches: `fail` / `failf`, `skip ~reason ()` for unmet
  environment preconditions (this is how the `MENTAT_*_LIVE_TEST` seams
  stay hermetic).

Custom types: expose `pp` and `equal` in the tested module's `.mli`,
then `let point = Testable.make ~pp:Point.pp ~equal:Point.equal` (or
`Testable.of_module (module Point)`, `Testable.structural ~pp`).

Style: one behavior per test, named by the behavior. Add `~msg` to
assertions inside loops. Pick example inputs adversarially: the
empty/zero case, the singleton, a boundary and both its neighbors,
duplicates, extremes, non-ASCII text, inputs containing the format's
own delimiters, and every documented error input. `cases name inputs
fn` keeps the table readable and each row **individually selectable**,
so one bad row does not mask the rest — prefer it to `List.iter` over a
table inside one test.

## 5. Property tests

`prop name gen law` draws from an `'a Gen.t` (100 cases by default),
runs an ordinary assertion body on each, and shrinks failures to a
minimal counterexample — there is never a shrink function to write.
Every failure prints an exact replay command with the run's `s1:` seed
token.

```ocaml
prop "decode inverts encode" Gen.(list small_int) (fun l ->
    equal (list int) l (decode (encode l)))
```

Laws to reach for: round-trip (generate the *decoded* form), agreement
with a simpler oracle, invariants, algebraic identities, metamorphic
relations, and total-behavior claims (`parse` of arbitrary junk never
raises).

Generator discipline is where properties quietly go wrong:

- Sizes, indices and arithmetic use `small_int` or `nat` — full-range
  `int` drowns most laws in overflow noise.
- `assume cond` is for rare, cheap preconditions. Structural
  preconditions (nonempty, sorted) belong in the generator —
  `Gen.such_that`, or correct-by-construction with `let+`/`and+`:

```ocaml
let gen_nonempty = Gen.(list ~size:(int_range 1 20) small_int)
```

- After `map`/`bind` attach `Gen.with_pp` (the report tells you when it
  is missing). Write one `pp` and feed both worlds: `Testable.make ~pp`
  for assertions, `Gen.with_pp pp` for counterexamples.
- A property that never fails may never reach the interesting region.
  `classify`/`collect` report the distribution (visible under `-v`);
  `cover ~label ~at_least` fails the test when a region is
  under-sampled. Keep `at_least` 10–15 points below the achieved rate
  at `~count:100`.

**Pin every fixed counterexample.** When a property finds a bug and you
fix it, add the shrunk counterexample to `~examples` — it runs before
any generation, forever. Replay a failure by pasting the printed
`--seed s1:…` line; fix the bug before touching the generator.

## 6. Stateful tests

For anything with internal state — the session journal, the store, the
permission policy — `stateful` is the strongest test available: it
checks the API against a *model* over generated call sequences and
shrinks failures to a minimal program. `test/unit/test_session.ml`'s
"the journal as a state machine" is the worked example in this repo.

```ocaml
stateful "behaves like a list" ~model:[]
  ~scope:(fun run -> run (Bounded_queue.create capacity))
  ~pp_model:(Testable.pp (list int))
  ~invariant:(fun m q -> equal int (List.length m) (Bounded_queue.size q))
  commands
```

The model is the specification (a **persistent** value — `list`, `Map`
— never a mutable structure), not a second implementation. Bodies check
what a call *returns*; `~invariant` (run before the first call and
after every call) checks what the state *is*. What to know:

- `~pre` both filters and *selects*: a command whose `~pre` demands a
  full queue is generated exactly at capacity. But a precondition no
  state satisfies deletes the command silently — guard with `cover` in
  `~invariant`, never in the command's own body, which is exactly the
  code that never runs.
- `~next` is required; read-only calls say so with `~next:Fun.id`.
  `~pre`/`~next` must be pure and must neither assert nor discard.
- Generated arguments cannot be handles that don't exist yet: generate
  an *index* into the model's live set and let `~pre` keep the lookup
  total.
- `~scope` takes a callback, which is what puts an Eio resource under
  test at all: `~scope:(fun run -> Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> run (make_sut ~sw ~env))`. It runs once
  per case **and per shrink candidate**, so `temp_dir ()` — which is
  test-scoped — is wrong inside one; mint scratch paths in the scope
  and remove them on the way out. Same for `setenv`/`chdir`.
- `~steps` (default 20) is quadratic on the failing path; lower it
  first when the test is expensive.
- There is no `~examples` for programs: pin a fixed regression by
  copying the shrunk counterexample's steps into a plain `test`.

## 7. Snapshot, expect, and cram

Choose by where the expectation lives:

| Output | Use |
|---|---|
| Short, review-worthy, produced by printing | `[%expect]` in an `(inline_tests)` library |
| Large or generated — help text, JSON, renders | `snapshot` under `__snapshots__/` |
| Needs masking or custom comparison first | `output ()` + ordinary assertions |

- `snapshot "name" value` — identity is the **name** (stable across
  refactors), storage is `__snapshots__/<src_basename>/<name>.snap`.
  Nothing is silently created: a missing baseline fails and prints the
  acceptance command. Accept with `-u` / `WINDTRAP_UPDATE=1`.
- Stale baselines are reported after a full clean run; `--prune`
  deletes them, `--strict-snapshots` fails on them.
- `[%expect]` matches with ppx_expect's whitespace flexibility;
  `[%expect_exact]` is byte-for-byte. Corrections normally arrive
  through `dune promote <file>` — **always name the file**. Under
  dune's `inline_tests` protocol a crash in any partition withholds
  every other partition's correction; `WINDTRAP_UPDATE=1` writes
  accepted corrections into the source tree directly, per file, which
  removes that cross-file veto and nothing else (a crash is never
  promotable, and a partition carrying a non-expect failure accepts
  nothing).
- Nondeterminism must be masked *before* comparison. This repo has one
  normalization policy — `mentat_test_censor` in `test/support/` — and
  both the cram suite and the TUI harness censor through it, so a
  rendered frame and a cram transcript normalize identically. Extend
  that, don't write a second policy.

For cram: `(cram (applies_to :whole_subtree) (deps %{bin:mentat}))` or
the test runs stale code. A nonzero exit appears as a trailing `[1]`
line — asserting exit codes is half the value, and promotion must never
silently absorb one. Dune sanitizes only `$TESTCASE_ROOT`; sort `ls`
output.

**Promotion discipline — this is where bugs get blessed as expected
output.** Read every promoted or `-u`-accepted diff as a code change
you are authoring, hunk by hunk. If a diff surprises you, that is a bug
found by the suite — investigate, don't accept.

## 8. Prove every test can fail

A test nobody has seen fail is unverified. Two modes:

**Admit every test you write or change** — the last step of writing
one, not a separate audit:

```
WINDTRAP_MUTATE=admit dune exec --instrument-with ppx_windtrap.mutate \
  test/unit/test_foo.exe -- -f "<test name>"
```

`ADMITTED` names the fault the test kills. `UNJUSTIFIED` is
stop-the-line and exits 1: the test ran faults on its lines and never
failed — strengthen the assertion, or dismiss a genuine equivalent in
the source with `[@mutate off "reason"]` and a reason. `NO SITES` means
mutation had nothing to say; review by eye.

**Survey the module when auditing one** — file-scoped, seconds-fast,
and it names the tests that watched a change and stayed green:

```
WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_ONLY=lib/foo/bar.ml \
  dune exec --instrument-with ppx_windtrap.mutate test/unit/test_foo.exe
```

A **survivor** means "strengthen one of these named tests"; an
**unreached** mutant means "write a test". Survivors never fail the
build — the survey is a reading list, not a gate. **When fixing a bug,
write the failing test first** and see it fail; admission is for every
other test.

Coverage answers the complementary question — *did this line run*:

```
dune runtest --instrument-with ppx_windtrap
WINDTRAP_COVERAGE=full dune runtest --instrument-with ppx_windtrap
```

`full` renders uncovered points as source excerpts. Chase uncovered
branches in code you touched, never the percentage. Both backends are
inert stanzas until `--instrument-with` asks for them.

## 9. Run and iterate

**Never run the whole suite casually; run the smallest scope that
covers the change.**

- A windtrap library or surface: `dune exec test/unit/test_config.exe`,
  filtering with `-f <pattern>`.
- A cram family: `dune build @test/blackbox/test-cases/<family>/blackbox`.
- Refreshing goldens: `dune promote <file>` after a scoped run — always
  name the file, never a bare `dune promote`.

Under `dune runtest` the `WINDTRAP_*` environment mirrors *are* the
CLI. The daily loop: `-f`/`-e` filter by path substring, `--tag`/
`--exclude-tag` by tag, `--failed` reruns the last run's failures,
`-x`/`--bail N` stop early, `-l` previews a selection, `--quick` drops
`slow`-tagged tests, `-s`/`--stream` disables capture for debugging a
hang.

Structure and resources: `cases name inputs fn` declares one selectable
test per input (`?name` derives names from values); `subtest` labels
sub-cases inside one body. `bracket ~setup ~teardown` scopes a per-test
resource with teardown on every outcome; `scoped` adapts callback-style
resources (`Eio_main.run`, `with_open_text`); `fixture` shares one
expensive resource across the run. `temp_dir ()`/`temp_file ()` are
runner-cleaned scratch paths; `setenv name value_opt` and `chdir dir`
bind the environment and working directory for one test and the runner
puts both back on every outcome (`setenv name None` really unbinds, so
the missing-variable path is testable). `xfail ~reason` is the honest
holding state for a known bug: the reproduction keeps running and goes
loudly red the day a change cures it. `ftest`/`fgroup` focus while
debugging — remove before committing; CI refuses them.

When a run fails, triage before editing: read the failure block to its
end — it already carries the diff, the counterexample or program, the
captured-output tail, and the replay command. Never touch the
generator, the baseline, or the assertion while the failure is still
unexplained.

## 10. The suite is a contract

Under pressure to go green, the reaches are: weakening an assertion,
hardcoding an expected value, special-casing test inputs in the
implementation, editing or deleting the failing test, skipping it, or
blessing bad output through promotion. Every one is visible in a diff,
and none may happen silently.

- **Never weaken an assertion or edit an expected value to match
  observed behavior** unless you can name the intended behavior change
  that justifies it — and then say so in the commit message.
- **Never delete or skip a failing test to get green.** `xfail
  ~reason:"issue #42"` keeps the reproduction running.
- **Never special-case test inputs in implementation code.**
- **Promotion and `-u` are assertion authorship**, held to the same
  standard as writing the assertion by hand.
- **A skip is a deliberate environmental statement**, never a disguise
  for a failure.
- **When you conclude the test is wrong, stop.** Changing a normative
  test is a contract change: name the spec source that contradicts it
  and surface the case, instead of editing and moving on.
- Report what the run actually said: exit 2 is a broken filter, not a
  pass; a red teardown alongside a green body is still a finding.

## Checklist

- [ ] `test/README.md` consulted; the new file is in the tree its
      subject belongs to, with no new suite split by test kind
- [ ] Obligations derived from the `.mli` before reading the
      implementation; spec gaps surfaced, not silently filled
- [ ] Every behavior at the strongest oracle its shape admits (§2);
      every module has a normative core; no bad-test-catalog offender
- [ ] Expected values derived from the spec; example inputs
      adversarial; tables use `cases`, not `List.iter`
- [ ] Properties encode real laws; structural preconditions live in
      generators; shrunk counterexamples pinned in `~examples`
- [ ] Stateful models are persistent values; read-only commands say
      `~next:Fun.id`; rare states covered via `cover` in `~invariant`
- [ ] Snapshots named, masked through `mentat_test_censor`, and
      declared in the stanza's `deps`; every promoted diff read
- [ ] Cram stanzas declare `(deps %{bin:mentat})`; exit codes asserted
- [ ] Every new test seen failing — failing-first for bugfixes,
      `WINDTRAP_MUTATE=admit` otherwise — with no `UNJUSTIFIED` left
- [ ] Scoped runs only; no `ftest`/`fgroup` left in; exit 2 treated as
      failure
- [ ] No §10 violation: nothing weakened, deleted, skipped, or
      blind-promoted without a stated justification
