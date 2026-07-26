Credential resolution is deterministic: an environment credential wins over a
stored one, and an empty environment variable is ignored so the store is used.
The reduced `auth status` surface reports only presence, so precedence is
observed where it matters — the Authorization header the run actually sends,
captured at a fake provider.

  $ use_trusted_workspace

A stored default credential is the fallback.

  $ mkdir -p "$XDG_CONFIG_HOME/mentat"
  $ printf '{"version":1,"credentials":{"openai":{"default":{"kind":"api_key","api_key":"store-key-AAA"}}}}' > "$XDG_CONFIG_HOME/mentat/auth.json"
  $ chmod 600 "$XDG_CONFIG_HOME/mentat/auth.json"
  $ export MENTAT_MODEL=openai/gpt-5.6-sol
  $ cat > resp.jsonl <<'JSONL'
  > {"expect":{"body_contains":["precedence prompt"]},"response":{"id":"r1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"resolved answer"}]}]}}
  > JSONL

With both present, the environment credential is the one sent.

  $ start_fake_server resp.jsonl cap-env port-env
  $ export MENTAT_OPENAI_BASE_URL="http://127.0.0.1:$(cat port-env)/v1"
  $ OPENAI_API_KEY=env-key-BBB mentat run start --id p-env "precedence prompt" --cwd "$PWD" 2>/dev/null
  resolved answer
  $ wait_fake_server
  $ grep -o 'authorization: Bearer env-key-BBB' cap-env/request-1.headers
  authorization: Bearer env-key-BBB

An empty environment variable is ignored and falls back to the stored key.

  $ start_fake_server resp.jsonl cap-store port-store
  $ export MENTAT_OPENAI_BASE_URL="http://127.0.0.1:$(cat port-store)/v1"
  $ OPENAI_API_KEY= mentat run start --id p-store "precedence prompt" --cwd "$PWD" 2>/dev/null
  resolved answer
  $ wait_fake_server
  $ grep -o 'authorization: Bearer store-key-AAA' cap-store/request-1.headers
  authorization: Bearer store-key-AAA
