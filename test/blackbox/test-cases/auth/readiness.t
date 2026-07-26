Readiness is checked when a credential is saved and is ephemeral: the phase
drives the command's exit code but is never persisted. The provider is the only
authority on validity, so passive status always reads presence only — never the
checked phase. `auth save` runs the same persist-then-check as login, without
the interactive method.

  $ use_trusted_workspace

A key the provider accepts checks ready and exits 0; passive status afterward is
unchecked, not a cached "ready".

  $ cat > ok.jsonl <<'JSONL'
  > {"expect":{"request_line":"GET /v1/models HTTP/1.1"},"http":{"status":200,"json":{"data":[{"id":"gpt-5.6-sol"}]}}}
  > JSONL
  $ start_fake_issuer ok.jsonl cap-ok port-ok
  $ printf sk-openai-ready | mentat auth save openai --api-key-stdin >save-ok.out
  $ wait_fake_server
  $ grep '^Checked:' save-ok.out
  Checked: ready
  $ mentat auth status openai --json | mentat_cram json .providers[0].phase
  unchecked

The readiness decoder accepts the ChatGPT Codex `{"models":[{"slug":…}]}` shape as
readily as the `/v1/models` `{"data":[{"id":…}]}` one, so any 2xx body it can read
checks ready, never degraded. The OAuth route that actually reaches the Codex
backend — and the `client_version` query its `/models` probe requires — is
exercised in oauth.t.

  $ printf '{"version":1,"credentials":{}}' > "$XDG_CONFIG_HOME/mentat/auth.json"
  $ cat > codex.jsonl <<'JSONL'
  > {"expect":{"request_line":"GET /v1/models HTTP/1.1"},"http":{"status":200,"json":{"models":[{"slug":"gpt-5.6-sol"}]}}}
  > JSONL
  $ start_fake_issuer codex.jsonl cap-codex port-codex
  $ printf sk-openai-codex | mentat auth save openai --api-key-stdin >save-codex.out
  $ wait_fake_server
  $ grep '^Checked:' save-codex.out
  Checked: ready

A key the provider rejects checks blocked and exits 1 — a readiness fact, not a
save failure. The credential stays saved, so status still reads it as connected
(and unchecked, never a cached "blocked").

  $ printf '{"version":1,"credentials":{}}' > "$XDG_CONFIG_HOME/mentat/auth.json"
  $ cat > bad.jsonl <<'JSONL'
  > {"expect":{"request_line":"GET /v1/models HTTP/1.1"},"http":{"status":401,"json":{"error":{"message":"bad key"}}}}
  > JSONL
  $ start_fake_issuer bad.jsonl cap-bad port-bad
  $ printf sk-openai-bad | mentat auth save openai --api-key-stdin >save.out
  [1]
  $ wait_fake_server
  $ grep '^Checked:' save.out
  Checked: blocked
  $ mentat auth status openai --json
  {"schema_version":1,"type":"auth_status","providers":[{"provider":"openai","phase":"unchecked","connected":true}]}
