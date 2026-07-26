Linux Bubblewrap, workspace-write + read=project: this proves the OS sandbox
enforces the read scope end to end and is a real confidentiality boundary, the
bubblewrap counterpart of seatbelt-read.t. A `shell` tool call reads one file
INSIDE the project (which succeeds) and one file OUTSIDE the project roots (which
the namespace refuses), and the out-of-project contents never reach the model.

Honest boundary: gated to %{system} linux, skipped elsewhere, and NOT run on the
macOS host that authored it — exercised only by Linux CI. Assertions are
wording- and exit-code-agnostic: bubblewrap refuses an out-of-root read by not
mounting the path (ENOENT) rather than with Seatbelt's EPERM, so only the
security-relevant facts are pinned — the in-project read reached the model, the
out-of-project read did not succeed, and the secret never leaked. `brc=0` cannot
appear in the command text, which contains only `brc=$?`.

  $ mkdir -p ws/.git outside
  $ printf INSIDE > ws/allowed.txt
  $ printf SECRET > outside/secret.txt
  $ mentat trust ws >/dev/null

The dune cram harness injects an unexpanded `OCAML_TOPLEVEL_PATH=%{toplevel}%`
placeholder into the environment; under read=project the toolchain-root resolver
correctly fails closed on that malformed root (a real shell never sets it). Drop
it here for hermeticity — it is a harness artifact, not a posture under test.

  $ unset OCAML_TOPLEVEL_PATH
  $ cat > read.jsonl <<'JSONL'
  > {"response":{"id":"r1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"i1","call_id":"rc1","name":"shell","arguments":"{\"command\":\"cat allowed.txt 2>/dev/null; echo; cat ../outside/secret.txt 2>/dev/null; echo brc=$?\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","rc1"]},"response":{"id":"r2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done"}]}]}}
  > JSONL
  $ start_fake_openai read.jsonl
  $ MENTAT_SANDBOX_MODE=workspace-write MENTAT_SANDBOX_READ=project MENTAT_SHELL=/bin/sh mentat run start --json --permission bypass --cwd "$PWD/ws" --id read "r" >run.out 2>/dev/null; echo exit:$?
  exit:0
  $ wait_fake_server

The resolved posture is project read scope, confined by bubblewrap.

  $ grep '"type":"run.started"' run.out | mentat_cram json .sandbox.read
  project
  $ grep -o 'enforced backend=linux-bubblewrap' capture/request-2.json
  enforced backend=linux-bubblewrap

The in-project read reached the model; the out-of-project read did not succeed
(its exit-0 marker is absent); and the out-of-project file's contents never
appear in the tool result.

  $ grep -o 'INSIDE' capture/request-2.json | head -1
  INSIDE
  $ grep -c 'brc=0' capture/request-2.json
  0
  [1]
  $ grep -c 'SECRET' capture/request-2.json
  0
  [1]
