`sandbox status` reports the read scope from the sealed capability. The suite's
unconfined `danger-full-access` posture keeps this deterministic across
platforms (real Seatbelt/Bubblewrap enforcement would name a platform backend
and a machine-dependent digest instead). The default read scope is `project`.

  $ use_trusted_workspace
  $ mentat sandbox status
  mode=danger-full-access
  read=project
  network=restricted
  evidence=not requested

`MENTAT_SANDBOX_READ=all` widens the read scope back to the whole host; the
status line and the `--json` envelope both reflect it, while mode and evidence
are unchanged.

  $ MENTAT_SANDBOX_READ=all mentat sandbox status
  mode=danger-full-access
  read=all
  network=restricted
  evidence=not requested
  $ MENTAT_SANDBOX_READ=all mentat sandbox status --json
  {"schema_version":1,"type":"sandbox.status","mode":"danger-full-access","read":"all","network":"restricted","evidence":{"kind":"not_requested"},"roots":[]}

Under an unconfined route the read scope seals no policy, so `explain` stays
`unconfined` (no identity, no roots) regardless of the read scope.

  $ MENTAT_SANDBOX_READ=all mentat sandbox explain
  identity=not requested
  evidence=not requested
  policy=unconfined
  $ MENTAT_SANDBOX_READ=all mentat sandbox explain --json
  {"schema_version":1,"type":"sandbox.explain","identity":"not requested","evidence":{"kind":"not_requested"},"policy":null,"roots":[]}

The removed per-command `--verbose` spelling is not retained as a compatibility
flag. The bare spelling is still accepted, but only as the process-wide
diagnostics flag every command shares: it is taken from argv before Cmdliner so
the level is set before the reporter is installed, and it raises the stderr log
level without adding a field to the report. Identical stdout is what says the
removed verbose status did not come back through it.

  $ mentat sandbox status --verbose 2>/dev/null
  mode=danger-full-access
  read=project
  network=restricted
  evidence=not requested
