AUTH-6 — `auth login` saves first, then checks readiness for this command only,
and prints the whole settle block: where the credential landed, the readiness
result, and what to run next. A fake issuer answers the post-save `/v1/models`
probe so the phase is deterministic. (The TTY refusal for `--api-key-stdin`
cannot be exercised without a PTY, so that path is review-only.)

  $ use_trusted_workspace

A key the provider accepts (200) logs in ready and names the next command. The
whole settle block is on stdout; the login has no stderr.

  $ cat > ok.jsonl <<'JSONL'
  > {"expect":{"request_line":"GET /v1/models HTTP/1.1"},"http":{"status":200,"json":{"data":[{"id":"gpt-5.6-sol"}]}}}
  > JSONL
  $ start_fake_issuer ok.jsonl cap-ok port-ok
  $ printf sk-openai-ready | mentat auth login openai --method api-key --api-key-stdin >login.out 2>login.err
  $ wait_fake_server
  $ censor < login.out
  Logged in to openai with api-key.
  Saved: default (file store $TESTCASE_ROOT/config/mentat/auth.json)
  Checked: ready
  Next: run `mentat models list` to see available models.
  $ censor < login.err

The readiness check belonged to the login that ran it: passive status afterward
reports presence only, never a cached "ready".

  $ mentat auth status openai --json
  {"schema_version":1,"type":"auth_status","providers":[{"provider":"openai","phase":"unchecked","connected":true}]}

A key the provider rejects (401) reports blocked, drops the Next line, and exits
1 — but the credential stays saved, so rerunning login with a good key is the
repair rather than re-entering a lost one.

  $ cat > bad.jsonl <<'JSONL'
  > {"expect":{"request_line":"GET /v1/models HTTP/1.1"},"http":{"status":401,"json":{"error":{"message":"bad key"}}}}
  > JSONL
  $ start_fake_issuer bad.jsonl cap-bad port-bad
  $ printf sk-openai-bad | mentat auth login openai --method api-key --api-key-stdin >blocked.out 2>blocked.err
  [1]
  $ wait_fake_server
  $ censor < blocked.out
  Logged in to openai with api-key.
  Saved: default (file store $TESTCASE_ROOT/config/mentat/auth.json)
  Checked: blocked
  $ mentat auth status openai --json
  {"schema_version":1,"type":"auth_status","providers":[{"provider":"openai","phase":"unchecked","connected":true}]}
