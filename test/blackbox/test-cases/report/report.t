`mentat report` packages what a maintainer needs into one NDJSON bundle: the host
facts, the session's state projection, and the diagnostics files whose records
name that session. It collects, it does not upload, and it asks first.

  $ use_trusted_workspace

A completed turn, logged to an explicit file so the run is attributable.

  $ cat > turn.jsonl <<'JSONL'
  > {"expect":{"body_contains":["bundle me"]},"response":{"id":"r1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"ok"}]}]}}
  > JSONL
  $ start_fake_openai turn.jsonl
  $ export MENTAT_LOG=info
  $ export MENTAT_LOG_FILE="$PWD/run.log"
  $ mentat run start --id bundleme "bundle me" --cwd "$PWD" >/dev/null 2>&1
  $ wait_fake_server

Standard input is not a terminal here, so a report without --yes is declined and
nothing is written. Consent is not a formality that a redirect can satisfy.

  $ mentat report bundleme -o declined.ndjson --cwd "$PWD"
  mentat: report declined
  [1]
  $ test -f declined.ndjson && echo written || echo "not written"
  not written

With --yes the bundle lands. Each record is one line, and the kinds are the
header, the session projection, the correlated log, and the terminal manifest.

  $ mentat report bundleme --yes -o report.ndjson --cwd "$PWD" 2>/dev/null
  $ grep -c '"record":"report"' report.ndjson
  1
  $ grep -c '"record":"doctor"' report.ndjson
  1
  $ grep -c '"record":"config"' report.ndjson
  1
  $ grep -c '"record":"session"' report.ndjson
  1
  $ grep -c '"record":"log"' report.ndjson
  1
  $ grep -c '"record":"manifest"' report.ndjson
  1

The session record is the same projection `debug session --json` prints, so the
two commands cannot disagree about what correlates.

  $ grep -o '"type":"debug_session"' report.ndjson
  "type":"debug_session"
  $ grep -o '"id":"bundleme"' report.ndjson | head -1
  "id":"bundleme"

The bundle carries no credential, and no prompt: a diagnostics log records what
the process did, and the session record is state, not a transcript.

  $ grep -c 'test-key' report.ndjson || true
  0
  $ grep -ci 'authorization' report.ndjson || true
  0
  $ grep -c 'bundle me' report.ndjson || true
  0

A secret-bearing configuration value is redacted by the library's own view, so
the bundle cannot carry one however it was set.

  $ mentat config set web.exa_api_key SUPER-SECRET-EXA-KEY --cwd "$PWD" >/dev/null
  $ mentat report bundleme --yes -o secret.ndjson --cwd "$PWD" 2>/dev/null
  $ grep -c 'SUPER-SECRET-EXA-KEY' secret.ndjson || true
  0
  $ grep -c 'REDACTED' secret.ndjson
  1

The conversation is opt-in. With --with-session-export the bundle gains the whole
session as one nested record, and the prompt appears there and nowhere else.

  $ mentat report bundleme --yes --with-session-export -o full.ndjson --cwd "$PWD" 2>/dev/null
  $ grep -c '"record":"session_export"' full.ndjson
  1
  $ grep -c '"encoding":"base64"' full.ndjson
  1

The export is nested whole rather than spliced line-by-line among the report's
records, which is what preserves the integrity envelope its own manifest claims
over its own bytes. One consequence is directly observable: the conversation is
encoded, so it never appears as plain text even in the bundle that carries it.

  $ grep -c 'bundle me' full.ndjson || true
  0

A diagnostics file is bytes from outside this process: MENTAT_LOG_FILE can name
any file, and rotation or a killed writer can cut a record mid-character. An
invalid sequence must not yield a bundle that no JSON reader can parse — which
would be worse than failing, since the manifest would still verify over exactly
the bytes emitted.

  $ printf '%s' '2026-01-01T00:00:00.000 [1] [run=x] [s=bundleme] INFO [mentat.next] session bundleme opened ' > bad.log
  $ printf 'binary \377\376 tail\n' >> bad.log
  $ MENTAT_LOG_FILE="$PWD/bad.log" mentat report bundleme --yes -o bad.ndjson --cwd "$PWD" 2>/dev/null
  $ iconv -f UTF-8 -t UTF-8 bad.ndjson >/dev/null 2>&1 && echo valid || echo invalid
  valid

The record says the copy was lossy rather than presenting it as faithful.

  $ grep -c '"lossy":true' bad.ndjson
  1

A file's bytes are not the only outside input. An environment value and the note
the user typed reach the bundle too, and either can hold the same byte.

  $ TERM="$(printf 'xterm\377bad')" mentat report --no-session --yes --note "$(printf 'note\377bad')" -o env.ndjson --cwd "$PWD" 2>/dev/null
  $ iconv -f UTF-8 -t UTF-8 env.ndjson >/dev/null 2>&1 && echo valid || echo invalid
  valid

Asking for the export without a session is flag-carried misuse.

  $ mentat report --no-session --with-session-export --yes --cwd "$PWD"
  mentat: --with-session-export needs a session
  [2]

An unknown kind is flag-carried input, so it is a usage error.

  $ mentat report --kind nonsense --no-session --yes --cwd "$PWD"
  mentat: unknown report kind nonsense; expected bug, crash, question, feature
  [2]
