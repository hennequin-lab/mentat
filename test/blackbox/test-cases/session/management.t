Session lifecycle management: create, rename, archive/restore, delete/purge, and
fork are metadata operations over the saved store, each with a stable human line
and JSON envelope. Ids are minted explicitly for determinism.

  $ use_trusted_workspace

Creating a session persists a document and makes it visible in the default list.

  $ mentat session create --id demo --title Demo
  demo
  $ mentat session list
  ID    PHASE  AGE  TITLE
  demo  idle   0s   Demo

A duplicate id is rejected (exit 1) rather than overwriting the document.

  $ mentat session create --id demo 2>&1
  mentat: session already exists: demo
  [1]

Rename is a metadata update; the new title shows immediately.

  $ mentat session rename demo Renamed
  demo
  $ mentat session show demo --cwd "$PWD" | grep -E '^(id|title)='
  id=demo
  title=Renamed
  $ mentat session rename demo Demo >/dev/null

An untitled session omits the title row entirely (no placeholder is invented).

  $ mentat session create --id untitled >/dev/null
  $ mentat session show untitled --cwd "$PWD" | censor
  id=untitled
  phase=idle
  lifecycle=active
  cwd=$TESTCASE_ROOT
  turns=0
  context=in:0 out:0 responses:0 tool_calls:0

Archive then restore round-trips the lifecycle; each command echoes the id.

  $ mentat session archive demo
  demo
  $ mentat session show demo --cwd "$PWD" | grep -E '^lifecycle='
  lifecycle=archived
  $ mentat session restore demo
  demo
  $ mentat session show demo --cwd "$PWD" | grep -E '^lifecycle='
  lifecycle=active

Delete demands an explicit confirmation (exit 2); with --yes it becomes a
recoverable tombstone, and purge reclaims the tombstoned document.

  $ mentat session delete demo 2>&1
  mentat: refusing to delete without --yes
  [2]
  $ mentat session delete --yes untitled
  untitled
  $ mentat session show untitled --cwd "$PWD" | grep -E '^lifecycle='
  lifecycle=deleted
  $ mentat session purge --yes
  purged 1 session(s)

Fork mints a new session from an existing one; both then coexist in the store.

  $ mentat session fork demo --id kid --title Kid
  kid
  $ mentat session list | awk '{print $1}' | tail -n +2 | sort
  demo
  kid

Export renders human text and markdown projections that both point at the JSON
bundle for the full transcript.

  $ mentat session export demo --format text 2>/dev/null | censor
  session demo
  title: Demo
  phase: idle
  lifecycle: active
  cwd: $TESTCASE_ROOT
  events: 0
  $ mentat session export demo --format markdown 2>/dev/null | censor | grep .
  # Session demo
  - **title**: Demo
  - **phase**: idle
  - **lifecycle**: active
  - **cwd**: $TESTCASE_ROOT
  - **events**: 0

The JSON envelopes are terse and structural: create names the new id, and list
enumerates the store newest-first.

  $ mentat session create --json --id jj --title JJ | censor
  {"schema_version":1,"type":"session","session":"jj"}
  $ mentat session list --json | mentat_cram json '.sessions[].id'
  jj
  kid
  demo

Purge distinguishes an empty healthy scan from an incomplete one. Corruption
prevents "nothing to purge" success, confirmation makes no mutation, and a
confirmed purge removes every independently healthy tombstone before returning
failure for the incomplete scan.

  $ mkdir -p "$XDG_DATA_HOME/mentat/sessions/damaged"
  $ printf '{\n' > "$XDG_DATA_HOME/mentat/sessions/damaged/session.json"
  $ mentat session purge --yes 2>&1 | grep -E 'nothing|prevent a complete purge'
  mentat: 1 corrupt session document(s) prevent a complete purge
  [1]
  $ mentat session create --id doomed >/dev/null
  $ mentat session delete --yes doomed >/dev/null
  $ mentat session purge 2>&1 | tail -n 1
  mentat: refusing to purge 1 deleted session(s) without --yes
  [2]
  $ test -f "$XDG_DATA_HOME/mentat/sessions/doomed/session.json" && echo retained
  retained
  $ mentat session purge --yes 2>&1 | grep -E 'purged|purge incomplete'
  purged 1 session(s)
  mentat: purge incomplete: 0 removal failure(s), 1 corrupt session document(s)
  [1]
  $ test ! -e "$XDG_DATA_HOME/mentat/sessions/doomed" && echo removed
  removed

A held run fence makes one removal fail, but purge still attempts the later
tombstone and returns failure after reporting the partial progress. The helper
uses the same POSIX record-lock primitive as the store and prints readiness only
after the lock is held.

  $ mentat session create --id a-locked >/dev/null
  $ mentat session delete --yes a-locked >/dev/null
  $ mentat session create --id z-removed >/dev/null
  $ mentat session delete --yes z-removed >/dev/null
  $ mentat_cram lock "$XDG_DATA_HOME/mentat/sessions/a-locked/run.lock" >purge-lock.ready &
  $ export PURGE_LOCK_PID=$!
  $ mentat_cram wait-file purge-lock.ready
  $ mentat session purge --yes >purge.out 2>purge.err
  [1]
  $ grep '^mentat: could not purge a-locked:' purge.err | sed -E 's/held by .*/held by another driver/'
  mentat: could not purge a-locked: session a-locked is held by another driver
  $ grep '^mentat: purge incomplete:' purge.err
  mentat: purge incomplete: 1 removal failure(s), 1 corrupt session document(s)
  $ cat purge.out
  purged 1 session(s)
  $ test -d "$XDG_DATA_HOME/mentat/sessions/a-locked" && echo locked-retained
  locked-retained
  $ test ! -e "$XDG_DATA_HOME/mentat/sessions/z-removed" && echo later-removed
  later-removed
  $ kill "$PURGE_LOCK_PID"
  $ wait "$PURGE_LOCK_PID" 2>/dev/null || true
