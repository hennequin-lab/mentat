The sandbox status/explain surface: a pure render over
the sealed workspace capability — no engine, no credentials, no session. The
suite pins MENTAT_SANDBOX_MODE=danger-full-access, so the sealed route is the
unconfined `direct` one: evidence `not requested`, no policy, no readable roots
— deterministic across platforms.

`sandbox status` reports the effective posture.

  $ use_trusted_workspace
  $ mentat sandbox status
  mode=danger-full-access
  read=project
  network=restricted
  evidence=not requested

`sandbox status --json` is one envelope carrying the posture and the sealed
evidence.

  $ mentat sandbox status --json
  {"schema_version":1,"type":"sandbox.status","mode":"danger-full-access","read":"project","network":"restricted","evidence":{"kind":"not_requested"},"roots":[]}

`sandbox explain` renders the concrete sealed identity and policy; an unconfined
route carries no policy.

  $ mentat sandbox explain
  identity=not requested
  evidence=not requested
  policy=unconfined

  $ mentat sandbox explain --json
  {"schema_version":1,"type":"sandbox.explain","identity":"not requested","evidence":{"kind":"not_requested"},"policy":null,"roots":[]}

Both are exit 0 (data), and neither resolves a provider credential or opens a
session.

  $ mentat sandbox status >/dev/null; echo $?
  0
  $ mentat sandbox explain >/dev/null; echo $?
  0
