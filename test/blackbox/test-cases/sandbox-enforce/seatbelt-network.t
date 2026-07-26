macOS Seatbelt, workspace-write network posture: this proves the OS sandbox
enforces the network boundary end to end, with a positive/negative control that
isolates the sandbox as the cause. The SAME `nc` connect to the SAME closed
loopback port is refused with a socket permission error under
network=restricted but reaches the socket (and fails only with connection-
refused) under network=enabled. Only the sandbox posture differs, so the
"Operation not permitted" can come from nothing else.

Port 9 (discard) on loopback is used precisely because nothing listens there:
under `enabled` the connect is allowed and fails at the peer (Connection
refused); under `restricted` Seatbelt denies the socket operation itself
(Operation not permitted), before any peer is contacted.

  $ mkdir -p ws/.git
  $ mentat trust ws >/dev/null
  $ printf 9 > ws/netport.txt

Pin the read scope to `all` for this network-boundary test. Its positive/negative
control keys on whether the connect reports "Operation not permitted"; under the
default project read scope a blocked out-of-project read by `nc` injects that
same signature and defeats the control, so `all` isolates the network axis. Read
confinement is proven separately in seatbelt-read.t.

  $ export MENTAT_SANDBOX_READ=all

The model emits the same verbose `nc` probe in both runs.

  $ cat > net.jsonl <<'JSONL'
  > {"response":{"id":"n1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"i1","call_id":"nc1","name":"shell","arguments":"{\"command\":\"nc -zv -w2 127.0.0.1 $(cat netport.txt) 2>&1; echo done:$?\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","nc1"]},"response":{"id":"n2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done"}]}]}}
  > JSONL

Restricted network: Seatbelt refuses the socket. The connect never leaves the
host.

  $ start_fake_openai net.jsonl capture-r port-r
  $ MENTAT_SANDBOX_MODE=workspace-write MENTAT_SANDBOX_NETWORK=restricted MENTAT_SHELL=/bin/sh mentat run start --json --permission bypass --cwd "$PWD/ws" --id net-r "n" >run-r.out 2>/dev/null; echo exit:$?
  exit:0
  $ wait_fake_server
  $ grep '"type":"run.started"' run-r.out | mentat_cram json .sandbox.network
  restricted
  $ grep -o 'enforced backend=macos-seatbelt' capture-r/request-2.json
  enforced backend=macos-seatbelt
  $ grep -o 'Operation not permitted' capture-r/request-2.json | head -1
  Operation not permitted
  $ grep -c 'Connection refused' capture-r/request-2.json
  0
  [1]

Enabled network: the identical probe reaches the socket and fails only because
the port is closed. The sandbox is still enforced (Seatbelt guards the filesystem
either way); it is only the network permission that changed.

  $ start_fake_openai net.jsonl capture-e port-e
  $ MENTAT_SANDBOX_MODE=workspace-write MENTAT_SANDBOX_NETWORK=enabled MENTAT_SHELL=/bin/sh mentat run start --json --permission bypass --cwd "$PWD/ws" --id net-e "n" >run-e.out 2>/dev/null; echo exit:$?
  exit:0
  $ wait_fake_server
  $ grep '"type":"run.started"' run-e.out | mentat_cram json .sandbox.network
  enabled
  $ grep -o 'enforced backend=macos-seatbelt' capture-e/request-2.json
  enforced backend=macos-seatbelt
  $ grep -o 'Connection refused' capture-e/request-2.json | head -1
  Connection refused
  $ grep -c 'Operation not permitted' capture-e/request-2.json
  0
  [1]
