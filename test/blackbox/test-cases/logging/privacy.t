Prompt text never reaches a diagnostics log. A log records what the process did,
not what the user asked — only the resolved subcommand token is recorded — so a
user can attach one to a bug report without disclosing the conversation. The
session document is where the conversation lives, and exporting it is a separate,
consented act.

  $ use_trusted_workspace

A completed turn whose prompt is a token that cannot occur incidentally.

  $ cat > turn.jsonl <<'JSONL'
  > {"expect":{"body_contains":["ZZQQ-SECRET-PROMPT-7391"]},"response":{"id":"r1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"ok"}]}]}}
  > JSONL
  $ start_fake_openai turn.jsonl
  $ MENTAT_LOG=debug MENTAT_LOG_FILE="$PWD/run.log" mentat run start --id privacy1 "ZZQQ-SECRET-PROMPT-7391" --cwd "$PWD" >/dev/null 2>&1
  $ wait_fake_server

The run logged at the most verbose level, so the file is not vacuously clean and
the assertion below has something to be true about.

  $ grep -c 'mentat started version=dev command=run' run.log
  1

The prompt appears nowhere in it, at any level.

  $ grep -c 'ZZQQ-SECRET-PROMPT-7391' run.log || true
  0

Neither does the model's reply: a log is not a transcript.

  $ grep -c 'output_text' run.log || true
  0

Nor the credential. MENTAT_LOG governs Mentat's own sources only — a linked HTTP
client that dumps request bodies and Authorization headers at its own debug
level must not be turned on by a knob we hand to users reporting bugs.

  $ grep -c 'test-key' run.log || true
  0
  $ grep -ci 'authorization' run.log || true
  0

The run is still attributed and still diagnostic: the boundary line names the
session, so the log correlates to the session document that does hold the
conversation.

  $ grep -c 'session privacy1 opened' run.log
  1
