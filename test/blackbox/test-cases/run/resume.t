`run resume` resolves a saved session before it starts a turn: a name that
does not resolve, an ambiguous prefix, and a non-active lifecycle each fail at
the session boundary — before any provider IO — while a live resume by id runs
the next turn to completion on an idle session.

  $ use_trusted_workspace

The interactive entry points reject a non-terminal before changing terminal
state. Cram deliberately supplies pipes, so both Home and interactive resume
stop at the same executable-owned boundary with the same clean runtime error.

  $ mentat --cwd "$PWD" 2>&1
  mentat: interactive terminal required to run the TUI
  [1]
  $ mentat resume --cwd "$PWD" 2>&1
  mentat: interactive terminal required to run the TUI
  [1]

A PROMPT is required to start a new turn on an idle target, so every resolution
case below carries one; the resolution failure, not the prompt gate, is what
fails. A zero-candidate prefix preserves the exact store [Not_found] as a
runtime failure.

  $ mentat run resume no-such "p" --cwd "$PWD" 2>&1
  mentat: session not found: no-such
  [1]

An ambiguous id prefix names every candidate so the user can disambiguate.

  $ mentat session create --id twin-a >/dev/null
  $ mentat session create --id twin-b >/dev/null
  $ mentat run resume twin "p" --cwd "$PWD" 2>&1
  mentat: ambiguous session id prefix "twin": matches twin-a, twin-b
  [2]

A unique prefix resolves to the one matching session; here the resolved turn
reaches the provider and fails only on the closed port, proving the prefix
selected a target (exit 1, not the exit-2 resolution errors above).

  $ OPENAI_API_KEY=test-key MENTAT_MODEL=openai/gpt-5.6-sol MENTAT_OPENAI_BASE_URL=http://127.0.0.1:9/v1 mentat run resume twin-a "p" --cwd "$PWD" 2>&1 | grep -o 'provider transport failure: connection refused'
  provider transport failure: connection refused
  [1]

An archived session refuses resume by its lifecycle — after the run-start
posture block but before any provider connection (no credential is configured,
yet the archived refusal is what stops it). Exit 1.

  $ mentat session create --id keep --title Keep >/dev/null
  $ mentat session archive keep >/dev/null
  $ mentat run resume keep "more work" --cwd "$PWD" 2>&1
  mentat: sandbox: danger-full-access (read project, network restricted)
  mentat: session is archived
  [1]

A deleted session is terminal and refuses the same way.

  $ mentat session create --id trash >/dev/null
  $ mentat session delete --yes trash >/dev/null
  $ mentat run resume trash "more work" --cwd "$PWD" 2>&1
  mentat: sandbox: danger-full-access (read project, network restricted)
  mentat: session has been deleted
  [1]

A live resume by id runs the next turn against the fake server and completes:
the assistant text is printed and the idle session now carries one turn.

  $ mentat session create --id live --title Live >/dev/null
  $ cat > resume.jsonl <<'JSONL'
  > {"expect":{"body_contains":["resumed prompt"]},"response":{"id":"r1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"resumed final"}]}]}}
  > JSONL
  $ start_fake_openai resume.jsonl capture-resume port-resume
  $ mentat run resume live "resumed prompt" --cwd "$PWD" 2>/dev/null
  resumed final
  $ wait_fake_server
  $ mentat session show live --cwd "$PWD" | grep -E '^(phase|turns)='
  phase=idle
  turns=1
