DOCTOR toolchain + project checks, present branches. The toolchain probe resolves
[dune] through the [Mentat_ocaml_toolchain] ladder; pointing the [MENTAT_DUNE]
override at an executable stub makes the resolved path host-independent and exercises
the found branch. A [dune-project] marker at the root flips the project check to
present. Neither changes the exit code — a workspace without OCaml tooling is a
warning, not a failure. (The live dune build-health verdict is deliberately not read
by doctor; it rides the workspace notice channel at turn preparation.)

  $ use_trusted_workspace
  $ printf '#!/bin/sh\ntrue\n' > dune-stub
  $ chmod +x dune-stub
  $ export MENTAT_DUNE="$PWD/dune-stub"
  $ touch dune-project
  $ MENTAT_MODEL=openai/gpt-5.6-sol mentat doctor | censor
  [PASS] config: resolved ($TESTCASE_ROOT/config/mentat/config.json)
  [PASS] storage: session store at $TESTCASE_ROOT/data/mentat
  [PASS] sessions: 0 stored, 0 corrupt
  [PASS] trust: workspace trusted
  [WARN] auth: no connected provider; run `mentat auth login <provider>`
  [PASS] model: openai/gpt-5.6-sol
  [PASS] toolchain: dune at $TESTCASE_ROOT/dune-stub (via MENTAT_* override)
  [PASS] project: dune project
  [PASS] diagnostics: $TESTCASE_ROOT/state/mentat (0 log(s), 0 crash report(s))
  $ MENTAT_MODEL=openai/gpt-5.6-sol mentat doctor >/dev/null 2>&1; echo $?
  0

The toolchain override is authoritative: an override set but not executable is "not
found" (it never falls through to PATH), so the found path is exactly the override.

  $ MENTAT_MODEL=openai/gpt-5.6-sol mentat doctor --json | mentat_cram json '.checks[6].detail' | censor
  dune at $TESTCASE_ROOT/dune-stub (via MENTAT_* override)
  $ MENTAT_MODEL=openai/gpt-5.6-sol mentat doctor --json | mentat_cram json '.checks[7].detail'
  dune project
