Session discovery: the store is global durable data, but the default listing is
scoped to the invoking workspace, ages rows against the injected clock, orders
newest-first, and caps the row count. Ids and the MENTAT_NOW clock are pinned for
determinism.

  $ use_trusted_workspace

A session recorded in another directory is global data — addressable by id — but
the cwd-scoped default list hides it. `--all` widens the scope and adds the CWD
column so each row's workspace is legible.

  $ mkdir -p proj-a
  $ mentat session create --id in-root --title InRoot --cwd "$PWD" >/dev/null
  $ mentat session create --id in-a --title InA --cwd "$PWD/proj-a" >/dev/null
  $ mentat session list --cwd "$PWD" | awk '{print $1}'
  ID
  in-root
  $ mentat session list --all --cwd "$PWD" | censor | sed -E 's/  +/ /g'
  ID PHASE AGE CWD TITLE
  in-a idle 0s $TESTCASE_ROOT/proj-a InA
  in-root idle 0s $TESTCASE_ROOT InRoot

The AGE column buckets the elapsed time against MENTAT_NOW (unix ms): pinning the
clock at creation and reading it later renders minutes, hours, and days.

  $ MENTAT_NOW=1000000000000 mentat session create --id agex --title Age --cwd "$PWD" >/dev/null
  $ MENTAT_NOW=1000000120000 mentat session list --cwd "$PWD" | awk '$1=="agex" {print $3}'
  2m
  $ MENTAT_NOW=1000007200000 mentat session list --cwd "$PWD" | awk '$1=="agex" {print $3}'
  2h
  $ MENTAT_NOW=1000172800000 mentat session list --cwd "$PWD" | awk '$1=="agex" {print $3}'
  2d

The list is ordered by most-recent update first: the freshly created rows precede
the clock-pinned ancient one.

  $ mentat session list --cwd "$PWD" --json | mentat_cram json '.sessions[].id'
  in-root
  agex

The default list caps at 25 rows; `--limit 0` is unbounded, and a negative limit
is a usage error (exit 2).

  $ mkdir -p bulk
  $ for i in $(seq 1 26); do mentat session create --id "b$i" --cwd "$PWD/bulk" >/dev/null; done
  $ mentat session list --cwd "$PWD/bulk" | tail -n +2 | wc -l | tr -d ' '
  25
  $ mentat session list --cwd "$PWD/bulk" --limit 0 | tail -n +2 | wc -l | tr -d ' '
  26
  $ mentat session list --cwd "$PWD/bulk" --limit=-1 2>&1
  mentat: limit must not be negative
  [2]

A delegated child session is durable data addressable by id, but it is a
subagent's own transcript rather than a resumable conversation, so the default
listing hides it. The delegation is driven end to end: the parent turn calls
spawn and the runtime persists the child under its own `sub-` id.

  $ mkdir -p deleg/.git
  $ mentat trust "$PWD/deleg" >/dev/null
  $ cat > spawn.jsonl <<'JSONL'
  > {"expect":{"body_contains":["spawn a child"]},"response":{"id":"sp-1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"spawn-item","call_id":"spawn-call","name":"spawn","arguments":"{\"task\":\"inspect the tree\"}"}]}}
  > {"expect":{"body_contains":["\"model\":\"gpt-5.6-sol\""]},"response":{"id":"sp-2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done"}]}]}}
  > {"expect":{"body_contains":["\"model\":\"gpt-5.6-sol\""]},"response":{"id":"sp-3","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done"}]}]}}
  > JSONL
  $ start_fake_openai spawn.jsonl capture-spawn spawn-port
  $ mentat run start --id spawn-parent "spawn a child" --cwd "$PWD/deleg" >/dev/null 2>&1
  $ wait_fake_server

The child has its own persisted document under a `sub-` id ...

  $ find "$XDG_DATA_HOME/mentat/sessions" -maxdepth 1 -type d -name 'sub-*' | wc -l | tr -d ' '
  1

... yet the conversation listing in that workspace shows only the parent.

  $ mentat session list --cwd "$PWD/deleg" --json | mentat_cram json '.sessions[].id'
  spawn-parent

`--last` applies the same membership rule as the listing rather than a raw
recency filter: the child settled after the parent and is the most recently
updated session in this workspace, yet `--last` resolves to the parent, never
the subagent's transcript.

  $ mentat session show --last --cwd "$PWD/deleg" --json | mentat_cram json '.id'
  spawn-parent
