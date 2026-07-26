Session Summary surface: `session search` and the `session show` detail view
(Session_view), both offline-store twins over the session-owned projections.

  $ use_trusted_workspace
  $ mentat session create --id alpha --title "Refactor the parser" >/dev/null
  $ mentat session create --id beta --title "Fix the config loader" >/dev/null
  $ mentat session create --id gamma >/dev/null

`session search QUERY` keeps the rows whose id, title, preview, or cwd contains
QUERY (case-insensitively) — the same columns and shape as `session list`.

  $ mentat session search parser | awk '{print $1}' | tail -n +2
  alpha

The match spans the id and is case-insensitive.

  $ mentat session search GAMMA | awk '{print $1}' | tail -n +2
  gamma

A query matching nothing prints only the header, so no data rows follow.

  $ mentat session search nomatch | tail -n +2

Search is a listing filter, so it is applied before the row cap. The newer
non-matching rows do not consume the one matching slot.

  $ mentat session search parser --limit 1 | awk '{print $1}' | tail -n +2
  alpha

An empty search is rejected at the CLI boundary; there is no ambient empty
query for the protocol responder to interpret.

  $ mentat session search "" 2>&1
  mentat: search must not be empty
  [2]

Archived sessions are excluded from search by default (F5 lifecycle set);
`--archived` adds them, newest first.

  $ mentat session create --id delta --title "parser tests" >/dev/null
  $ mentat session archive delta >/dev/null
  $ mentat session search parser | awk '{print $1}' | tail -n +2
  alpha
  $ mentat session search parser --archived | awk '{print $1}' | tail -n +2 | sort
  alpha
  delta

`session show` renders the single-session detail projection (Session_view): the
shared summary fields plus execution detail. A never-run session is idle with
zero turns and no execution rows.

  $ mentat session show alpha | grep -E '^(id|phase|lifecycle|title|turns)='
  id=alpha
  phase=idle
  lifecycle=active
  title=Refactor the parser
  turns=0

`--json` emits the detail as a structured object with the shed turn count.

  $ mentat session show alpha --json | grep -o '"turns":0'
  "turns":0

Corrupt rows remain raw offline facts. Listing keeps every healthy row, warns
on stderr, and JSON preserves the exact id/path/message control fields.

  $ mkdir -p "$XDG_DATA_HOME/mentat/sessions/mix-bad"
  $ mkdir "$XDG_DATA_HOME/mentat/sessions/mix-bad/session.json"
  $ mentat session list --json >corrupt.json 2>/dev/null
  $ mentat_cram json '.corrupt[0].id' corrupt.json
  mix-bad
  $ mentat_cram json '.corrupt[0].path' corrupt.json
  sessions/mix-bad/session.json
  $ mentat_cram json '.corrupt[0].message' corrupt.json
  is not a regular file
  $ mentat session list >/dev/null 2>corrupt.err
  $ sed -n '1p' corrupt.err | censor
  mentat: corrupt session document at sessions/mix-bad/session.json: is not a regular file

Exact lookup reports exact corruption without scanning it away. Prefix lookup
counts corrupt ids alongside healthy ids, marks unavailable candidates, and
unrelated corruption never blocks an exact healthy document.

  $ mentat session show mix-bad 2>&1 | sed -n '1p'
  mentat: session mix-bad is invalid
  [1]
  $ mentat session create --id mix-good >/dev/null
  $ mentat session show mix- 2>&1 | sed -n '1p'
  mentat: ambiguous session id prefix "mix-": matches mix-bad (unavailable), mix-good
  [2]
  $ mentat session show alpha | grep '^id='
  id=alpha
