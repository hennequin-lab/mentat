The conservative pre-tool checkpoint (the `Before_turn_tools` snapshot the
mutation-recovery backend captures before a turn's first tool) covers the same
set the tools see: non-ignored regular files. Ignored trees — build outputs,
opam switches, vendored references — are never walked or content-hashed, so a
large `.gitignore`d directory cannot turn a read into a minute-long capture.

  $ use_trusted_workspace
  $ printf 'TRACKED_CONTENT_MARKER\n' > tracked.ml
  $ mkdir -p vendor
  $ printf 'IGNORED_CONTENT_MARKER\n' > vendor/reference.ml
  $ printf 'vendor/\n' > .gitignore

Drive one real `read_file` turn; its first tool triggers the capture regardless
of the tool's declared permissions.

  $ cat > read.jsonl <<'JSONL'
  > {"expect":{"body_contains":["capture the workspace","\"name\":\"read_file\""]},"response":{"id":"resp-read-1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"item-read-1","call_id":"call-read-1","name":"read_file","arguments":"{\"path\":\"tracked.ml\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","call-read-1"]},"response":{"id":"resp-read-2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"captured"}]}]}}
  > JSONL
  $ start_fake_openai read.jsonl capture-read port-read
  $ mentat run start --permission bypass --id checkpoint-ignore "capture the workspace" --cwd "$PWD" 2>/dev/null
  captured
  $ wait_fake_server

The snapshot backend writes each covered file's raw bytes as a content blob. The
tracked file's bytes are present; the `.gitignore`d file's bytes never are.

  $ if grep -rq TRACKED_CONTENT_MARKER "$XDG_DATA_HOME/mentat/snapshots"; then echo tracked-captured; else echo tracked-missing; fi
  tracked-captured
  $ if grep -rq IGNORED_CONTENT_MARKER "$XDG_DATA_HOME/mentat/snapshots"; then echo ignored-captured; else echo ignored-skipped; fi
  ignored-skipped
