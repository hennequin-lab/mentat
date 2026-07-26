Linux Bubblewrap, workspace-write: this proves the OS sandbox enforces the write
boundary end to end, the bubblewrap counterpart of seatbelt-write.t. A `shell`
tool call writes one file INSIDE the workspace (which succeeds) and one file
OUTSIDE it (which the namespace refuses), and the tool result records that
linux-bubblewrap guarded the command.

Honest boundary: this file is gated to %{system} linux and is skipped on other
hosts, so it is exercised only by Linux CI where /usr/bin/bwrap is available and
was NOT run on the macOS host that authored it. Its assertions are deliberately
wording- and exit-code-agnostic (filesystem state, the recorded backend, and a
runtime-constructed status marker) because bubblewrap refuses an out-of-root
write differently from Seatbelt — an unmounted or read-only path rather than
EPERM — and may report a different exit code; the security-relevant facts (the
write did not take effect and the command was confined) are what is pinned. The
`outrc=0` counted-absence proof cannot be fooled by the command text, which
contains only the literal `outrc=$?`.

The workspace is `ws` so a sibling `outside/` is harness-writable yet outside the
sealed write root.

  $ mkdir -p ws/.git outside
  $ mentat trust ws >/dev/null

The resolved posture selects the bubblewrap backend (the digest is per-run, and
`sandbox status` lists no roots — those appear in `sandbox explain`).

  $ MENTAT_SANDBOX_MODE=workspace-write mentat sandbox status | censor
  mode=workspace-write
  read=project
  network=restricted
  evidence=enforced (linux-bubblewrap $DIGEST)
  note=profile digest is per-invocation; identity is the stable fingerprint

The model emits one shell command that attempts both writes and reports each
outcome as an exit-code marker.

  $ cat > write.jsonl <<'JSONL'
  > {"response":{"id":"w1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"i1","call_id":"wc1","name":"shell","arguments":"{\"command\":\"echo IN > in-ws.txt; echo inrc=$?; echo OUT > ../outside/out.txt 2>/dev/null; echo outrc=$?\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","wc1"]},"response":{"id":"w2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done"}]}]}}
  > JSONL
  $ start_fake_openai write.jsonl
  $ MENTAT_SANDBOX_MODE=workspace-write MENTAT_SHELL=/bin/sh mentat run start --json --permission bypass --cwd "$PWD/ws" --id write "w" >run.out 2>/dev/null; echo exit:$?
  exit:0
  $ wait_fake_server

The command was spawned under the sandbox and returned; the in-workspace write
took effect and the out-of-workspace write did not.

  $ grep '"type":"tool.finished"' run.out | mentat_cram json .outcome
  returned
  $ cat ws/in-ws.txt
  IN
  $ test ! -f outside/out.txt && echo absent
  absent

The tool result records the bubblewrap backend, that the in-workspace write
reported success, and that the out-of-workspace write never reported success
(its exit-0 marker is absent from the result).

  $ grep -o 'enforced backend=linux-bubblewrap' capture/request-2.json
  enforced backend=linux-bubblewrap
  $ grep -o 'inrc=0' capture/request-2.json | head -1
  inrc=0
  $ grep -c 'outrc=0' capture/request-2.json
  0
  [1]
