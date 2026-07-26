`debug session` reports a saved session's identity, state, and on-disk artifacts,
and correlates the diagnostics log that mentions it — all offline, from the store
and the state home.

  $ use_trusted_workspace

A completed turn creates a session; the run logs to an explicit file at debug
level so the boundary line names the session in the log.

  $ cat > turn.jsonl <<'JSONL'
  > {"expect":{"body_contains":["hello"]},"response":{"id":"r1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done"}]}]}}
  > JSONL
  $ start_fake_openai turn.jsonl
  $ export MENTAT_LOG=debug
  $ export MENTAT_LOG_FILE="$PWD/run.log"
  $ mentat run start --id sess1 "hello" --cwd "$PWD" >/dev/null 2>&1
  $ wait_fake_server

The human report names the session and its state, resolves the on-disk document,
and prints the follow-up commands.

  $ mentat debug session sess1 --cwd "$PWD" | grep -o 'Session sess1'
  Session sess1
  $ mentat debug session sess1 --cwd "$PWD" | grep -o 'lifecycle: active'
  lifecycle: active
  $ mentat debug session sess1 --cwd "$PWD" | grep -o 'last outcome: completed'
  last outcome: completed
  $ mentat debug session sess1 --cwd "$PWD" | grep -o 'mentat session show sess1'
  mentat session show sess1

The JSON report carries the state projection, the artifact probes, and the
correlated log.

  $ mentat debug session sess1 --cwd "$PWD" --json > rep.json
  $ mentat_cram json .type rep.json
  debug_session
  $ mentat_cram json .session.id rep.json
  sess1
  $ mentat_cram json .session.lifecycle rep.json
  active
  $ mentat_cram json .session.last_outcome rep.json
  completed
  $ mentat_cram json .artifacts[0].kind rep.json
  session.json
  $ mentat_cram json .artifacts[0].exists rep.json
  true
  $ mentat_cram json '.logs[].path' rep.json | grep -c 'run.log'
  1

An absent target with no --last is a usage error.

  $ mentat debug session --cwd "$PWD" 2>&1
  mentat: requires SESSION or --last; run `mentat session list`
  [2]
