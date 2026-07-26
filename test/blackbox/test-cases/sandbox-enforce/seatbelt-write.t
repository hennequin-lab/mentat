macOS Seatbelt, workspace-write: this proves the OS sandbox actually enforces
the write boundary end to end. A `shell` tool call driven through `mentat run`
writes one file INSIDE the workspace (which succeeds) and one file OUTSIDE it
(which the kernel refuses), and the tool result records that macOS Seatbelt —
not danger-full-access — guarded the command. This is real enforcement, not the
suite's default unconfined posture.

The workspace is a subdirectory `ws` so a sibling `outside/` is a normal,
harness-writable directory that is nonetheless outside the sealed write root:
any block therefore comes from the sandbox, not from a missing path.

  $ mkdir -p ws/.git outside
  $ mentat trust ws >/dev/null

The model emits one shell command that attempts both writes and reports which
succeeded, then a closing message once it sees the tool result.

  $ cat > write.jsonl <<'JSONL'
  > {"response":{"id":"w1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"i1","call_id":"wc1","name":"shell","arguments":"{\"command\":\"echo IN > in-ws.txt && echo wrote-inside; echo OUT > ../outside/out.txt && echo wrote-outside || echo blocked-outside:$?\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","wc1"]},"response":{"id":"w2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done"}]}]}}
  > JSONL
  $ start_fake_openai write.jsonl

`--permission bypass` removes permission review from the picture, so the only
thing that can block the outside write is the OS sandbox. The mode is set
explicitly, overriding the suite-wide danger-full-access pin; the shell is
pinned to /bin/sh for a host-independent command runner.

  $ MENTAT_SANDBOX_MODE=workspace-write MENTAT_SHELL=/bin/sh mentat run start --json --permission bypass --cwd "$PWD/ws" --id write "w" >run.out 2>/dev/null; echo exit:$?
  exit:0
  $ wait_fake_server

The run resolved a workspace-write posture and the command actually ran and
returned (it was spawned under the sandbox, not refused).

  $ grep '"type":"run.started"' run.out | mentat_cram json .sandbox.mode
  workspace-write
  $ grep '"type":"tool.finished"' run.out | mentat_cram json .outcome
  returned

The in-workspace write took effect on the real filesystem; the out-of-workspace
write did not.

  $ cat ws/in-ws.txt
  IN
  $ test ! -f outside/out.txt && echo absent
  absent

The tool result the model saw records that Seatbelt guarded the command, that
the command observed a non-zero status on the blocked write, and that the OS
reported the refusal (not a Mentat crash).

  $ grep -o 'enforced backend=macos-seatbelt' capture/request-2.json
  enforced backend=macos-seatbelt
  $ grep -o 'wrote-inside' capture/request-2.json | head -1
  wrote-inside
  $ grep -o 'blocked-outside:1' capture/request-2.json
  blocked-outside:1
  $ grep -o 'Operation not permitted' capture/request-2.json | head -1
  Operation not permitted
