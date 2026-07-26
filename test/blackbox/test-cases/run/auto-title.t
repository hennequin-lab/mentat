A fresh, unnamed session earns a short title from its first prompt. After the
turn settles, the executable makes a second small-model provider request — the
titling side-call — and renames the session with the cleaned result. The suite
default MENTAT_AUTO_TITLE=0 suppresses the extra request, so this case opts back
in.

  $ use_trusted_workspace
  $ export MENTAT_AUTO_TITLE=1

The fixture serves two ordered requests: the titling side-call runs first (it
must persist before the turn's driver attaches and holds the session guard), then
the turn itself. Only the side-call carries the title-generator system prompt, so
the two are matched apart.

  $ cat > title.jsonl <<'JSONL'
  > {"expect":{"body_contains":["You are a title generator","add rate limiting"]},"response":{"id":"resp-title","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"Rate limiting implementation"}]}]}}
  > {"expect":{"body_contains":["add rate limiting"],"body_not_contains":["You are a title generator"]},"response":{"id":"resp-turn","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done"}]}]}}
  > JSONL
  $ start_fake_openai title.jsonl capture-title port-title
  $ mentat run start --id auto-title "add rate limiting" --cwd "$PWD" 2>/dev/null
  done
  $ wait_fake_server

The session now carries the model's title, cleaned to a single line.

  $ mentat session show auto-title --cwd "$PWD" | grep -E '^title='
  title=Rate limiting implementation

With MENTAT_AUTO_TITLE=0 the side-call never fires: the fixture holds a single
turn response and the session stays untitled (its show output omits the title
row entirely).

  $ export MENTAT_AUTO_TITLE=0
  $ cat > off.jsonl <<'JSONL'
  > {"expect":{"body_contains":["add caching"]},"response":{"id":"resp-off","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done"}]}]}}
  > JSONL
  $ start_fake_openai off.jsonl capture-off port-off
  $ mentat run start --id no-title "add caching" --cwd "$PWD" 2>/dev/null
  done
  $ wait_fake_server
  $ mentat session show no-title --cwd "$PWD" | grep -E '^title=' || true
