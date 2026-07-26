macOS Seatbelt, workspace-write + read=project: this proves the OS sandbox
enforces the read scope end to end and that it is a real confidentiality
boundary. A `shell` tool call reads one file INSIDE the project (which
succeeds) and one file OUTSIDE the project roots (which the kernel refuses), and
the out-of-project contents never reach the model.

The workspace is `ws`; the out-of-project secret lives in a sibling `outside/`.

  $ mkdir -p ws/.git outside
  $ printf INSIDE > ws/allowed.txt
  $ printf SECRET > outside/secret.txt
  $ mentat trust ws >/dev/null

The dune cram harness injects an unexpanded `OCAML_TOPLEVEL_PATH=%{toplevel}%`
placeholder into the environment; under read=project the toolchain-root resolver
correctly fails closed on that malformed root (a real shell never sets it). Drop
it here for hermeticity — it is a harness artifact, not a posture under test.

  $ unset OCAML_TOPLEVEL_PATH

The model emits one shell command that reads both files, reporting a non-zero
status for the refused read.

  $ cat > read.jsonl <<'JSONL'
  > {"response":{"id":"r1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"i1","call_id":"rc1","name":"shell","arguments":"{\"command\":\"cat allowed.txt; echo; cat ../outside/secret.txt || echo refused-read:$?\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","rc1"]},"response":{"id":"r2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done"}]}]}}
  > JSONL
  $ start_fake_openai read.jsonl

read=project is set explicitly; the command runs confined under Seatbelt with
permission review bypassed, so any refused read is the sandbox, not policy.

  $ MENTAT_SANDBOX_MODE=workspace-write MENTAT_SANDBOX_READ=project MENTAT_SHELL=/bin/sh mentat run start --json --permission bypass --cwd "$PWD/ws" --id read "r" >run.out 2>/dev/null; echo exit:$?
  exit:0
  $ wait_fake_server

The resolved posture is project read scope, and the command was confined by
Seatbelt.

  $ grep '"type":"run.started"' run.out | mentat_cram json .sandbox.read
  project
  $ grep -o 'enforced backend=macos-seatbelt' capture/request-2.json
  enforced backend=macos-seatbelt

The in-project read reached the model; the out-of-project read was refused; and
crucially the out-of-project file's contents never appear in the tool result —
project scope is a confidentiality boundary, not merely a write boundary.

  $ grep -o 'INSIDE' capture/request-2.json | head -1
  INSIDE
  $ grep -o 'refused-read:1' capture/request-2.json
  refused-read:1
  $ grep -c 'SECRET' capture/request-2.json
  0
  [1]
