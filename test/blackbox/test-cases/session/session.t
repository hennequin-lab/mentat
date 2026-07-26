Session lifecycle fixes (F cluster). Sessions are minted with explicit ids for
determinism.

Seed active, archived, and deleted sessions, plus two sharing an id prefix.

  $ use_trusted_workspace
  $ mentat session create --id act-one >/dev/null
  $ mentat session create --id arch-one >/dev/null
  $ mentat session archive arch-one >/dev/null
  $ mentat session create --id del-one >/dev/null
  $ mentat session delete del-one --yes >/dev/null
  $ mentat session create --id pre-alpha >/dev/null
  $ mentat session create --id pre-beta >/dev/null

F5 — lifecycle filters are inclusive: `--archived --deleted` shows the active set
plus archived plus deleted, a union rather than an exclusive slice.

  $ mentat session list --archived --deleted | awk '{print $1, $3}' | sort
  ID LIFECYCLE
  act-one active
  arch-one archived
  del-one deleted
  pre-alpha active
  pre-beta active

F1 — the LIFECYCLE column appears only with a lifecycle filter; the plain list
omits it.

  $ mentat session list | head -n 1
  ID         PHASE  AGE  TITLE

F1 — the CWD column appears only under `--all` (so a row's workspace is legible
when the view spans workspaces).

  $ mentat session list --all | head -n 1 | grep -oE 'PHASE  AGE  CWD'
  PHASE  AGE  CWD

F2 — an ambiguous id prefix names the matching ids so the user can disambiguate.

  $ mentat session show pre- 2>&1
  mentat: ambiguous session id prefix "pre-": matches pre-alpha, pre-beta
  [2]

F8 / F9 — empty or malformed ids and titles are usage errors (exit 2), never an
uncaught Invalid_argument crash, across create / rename / fork.

  $ mentat session create --id "" 2>&1
  mentat: session id must not be empty
  [2]
  $ mentat session create --id ok-id --title "" 2>&1
  mentat: title must not be empty
  [2]
  $ mentat session fork act-one --id "bad id" 2>&1
  mentat: session id must contain only letters, digits, '.', '_', or '-'
  [2]

session diff (B5) — the offline-store twin over the mutation ledger. An idle
session with no Mentat-authored edits has an empty diff, exit 0 (the enumeration
path over Store.Mutation reads the empty ledger without an engine).

  $ mentat session diff act-one 2>&1
  changed 0 file(s) (+0 -0)
  $ mentat session diff act-one --json 2>/dev/null
  {"schema_version":1,"type":"session.diff","files":0,"additions":0,"deletions":0,"changes":0,"diff":[]}

session export (B2) — the offline-store twin over the store bundle. `--format
json` streams the complete, integrity-verified bundle (header, document,
terminal manifest) under a held fence.

  $ mentat session export act-one --format json > bundle.jsonl 2>/dev/null
  $ head -n 1 bundle.jsonl
  {"export":"mentat.session","format_version":2,"document_version":1}
  $ grep -o '"section":"end"' bundle.jsonl
  "section":"end"

session rewind (B4) — client-routed (a new-journal-minting op the engine
authors). The anchor is mandatory; a rewind with no --to-turn is a usage error.

  $ mentat session rewind act-one 2>&1
  mentat: rewind requires --to-turn TURN
  [2]

A --to-turn naming a turn the session does not have reaches the engine's anchor
resolution and refuses cleanly (exit 1) — proving the client rewind path is
wired end to end.

  $ mentat session rewind act-one --to-turn no-such-turn --id child 2>&1
  mentat: turn is not in the session: no-such-turn
  [1]
