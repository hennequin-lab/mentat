The local auth store persists API-key credentials at mode 0600 and never prints
the secret. `auth save` is the non-interactive writer; `logout`/`remove` delete a
credential slot. A fake issuer answers each post-save readiness probe so the
settle is deterministic.

  $ use_trusted_workspace

Two API-key saves land two named slots under the provider. Each save reports the
full settle block and leaves the store at 0600.

  $ cat > ok.jsonl <<'JSONL'
  > {"expect":{"request_line":"GET /v1/models HTTP/1.1"},"http":{"status":200,"json":{"data":[{"id":"gpt-5.6-sol"}]}}}
  > {"expect":{"request_line":"GET /v1/models HTTP/1.1"},"http":{"status":200,"json":{"data":[{"id":"gpt-5.6-sol"}]}}}
  > JSONL
  $ start_fake_issuer ok.jsonl cap-ok port-ok
  $ printf sk-openai-default | mentat auth save openai --api-key-stdin >save.out
  $ printf sk-openai-work | mentat auth save openai --name work --api-key-stdin >/dev/null
  $ wait_fake_server
  $ censor < save.out
  Logged in to openai with api-key.
  Saved: default (file store $TESTCASE_ROOT/config/mentat/auth.json)
  Checked: ready
  Next: run `mentat models list` to see available models.

The store file is 0600 and carries both slots as api_key credentials.

  $ mentat_cram stat perms "$XDG_CONFIG_HOME/mentat/auth.json"
  600
  $ mentat_cram json .credentials.openai.default.kind "$XDG_CONFIG_HOME/mentat/auth.json"
  api_key
  $ mentat_cram json .credentials.openai.work.kind "$XDG_CONFIG_HOME/mentat/auth.json"
  api_key

The secret never appears in status output.

  $ mentat auth status openai --json | grep -c sk-openai-default
  0
  [1]

Status reads the store live: a saved provider is connected but unchecked (the
readiness check belongs to the command that ran it, never cached).

  $ mentat auth status openai --json
  {"schema_version":1,"type":"auth_status","providers":[{"provider":"openai","phase":"unchecked","connected":true}]}

A named credential is removed independently and does not disturb the default
slot: the provider stays connected. This case discards the presentation because
the typed logout settlement is exercised separately in [logout.t].

  $ mentat auth logout openai --name work >/dev/null
  $ mentat auth status openai --json
  {"schema_version":1,"type":"auth_status","providers":[{"provider":"openai","phase":"unchecked","connected":true}]}
  $ mentat_cram json .credentials.openai.work.kind "$XDG_CONFIG_HOME/mentat/auth.json"
  mentat_cram json: no member "work"
  [1]

Removing the last credential empties the provider: status reads missing and the
single-provider view fails.

  $ mentat auth remove openai >/dev/null
  $ mentat auth status openai --json
  {"schema_version":1,"type":"auth_status","providers":[{"provider":"openai","phase":"missing","connected":false}]}
  [1]
