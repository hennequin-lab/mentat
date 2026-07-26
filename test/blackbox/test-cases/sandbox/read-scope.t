`sandbox status` reports the read scope from the sealed capability. The suite's
unconfined `danger-full-access` posture keeps this deterministic across
platforms (real Seatbelt/Bubblewrap enforcement would name a platform backend
and a per-invocation digest instead). The default read scope is `project`.

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

The removed `--verbose` spelling is not retained as a compatibility flag.
Cmdliner rejects it before resolving a workspace.

  $ mentat sandbox status --verbose >verbose.out 2>&1; code=$?; grep -o "unknown option --verbose" verbose.out; echo $code
  unknown option --verbose
  124
