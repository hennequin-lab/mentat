The sandbox posture reads its mode and read scope from the environment; an
unrecognized value is a runtime error (exit 1) naming what was rejected. These
are pure validation errors — no store is opened and no provider is contacted.

  $ use_trusted_workspace
  $ MENTAT_SANDBOX_MODE=bogus mentat sandbox status 2>&1
  mentat: unknown sandbox mode: bogus
  [1]
  $ MENTAT_SANDBOX_READ=bogus mentat sandbox status 2>&1
  mentat: unknown sandbox read scope: bogus
  [1]

The same validation guards `explain`.

  $ MENTAT_SANDBOX_MODE=bogus mentat sandbox explain 2>&1
  mentat: unknown sandbox mode: bogus
  [1]
