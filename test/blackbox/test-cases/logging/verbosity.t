Diagnostics are off by default for a headless command, and `-v` turns them on for
the whole run. The flag is taken from argv before any command starts, so it also
covers the startup a per-command option would arrive too late to describe.

  $ use_trusted_workspace

Silent by default: a command that succeeds says nothing on standard error.

  $ mentat config path --cwd . >/dev/null 2>bare.err
  $ cat bare.err

One -v is info.

  $ mentat -v config path --cwd . >/dev/null 2>v.err
  $ grep -c 'INFO \[mentat.next\] mentat started' v.err
  1

The flag is removed from argv before the command is resolved, so it never becomes
the subcommand and the recorded token is the real one.

  $ grep -o 'command=[a-z]*' v.err
  command=config

Two of them is debug, which is strictly more than info rather than a different
trail: the info records are still there.

  $ mentat -vv config path --cwd . >/dev/null 2>vv.err
  $ grep -c 'INFO \[mentat.next\] mentat started' vv.err
  1

MENTAT_LOG alone still governs, including its silence.

  $ MENTAT_LOG=quiet mentat config path --cwd . >/dev/null 2>quiet.err
  $ cat quiet.err

A flag beats the environment: someone typing -v is asking about the run in front
of them, not about whatever their shell profile set.

  $ MENTAT_LOG=quiet mentat -v config path --cwd . >/dev/null 2>override.err
  $ grep -c 'INFO \[mentat.next\] mentat started' override.err
  1

Raising the level never lifts the ceiling on linked libraries, so -vv cannot put
a request body or an Authorization header on disk.

  $ cat > turn.jsonl <<'JSONL'
  > {"expect":{"body_contains":["QQZZ-VERBOSE-PROMPT"]},"response":{"id":"r1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"ok"}]}]}}
  > JSONL
  $ start_fake_openai turn.jsonl
  $ mentat -vv run start --id verbose1 "QQZZ-VERBOSE-PROMPT" --cwd "$PWD" >/dev/null 2>deep.err
  $ wait_fake_server
  $ grep -c 'QQZZ-VERBOSE-PROMPT' deep.err || true
  0
  $ grep -ci 'authorization' deep.err || true
  0
  $ grep -c 'test-key' deep.err || true
  0

It is still a debug trail, not an empty one.

  $ grep -c 'session verbose1 opened' deep.err
  1
