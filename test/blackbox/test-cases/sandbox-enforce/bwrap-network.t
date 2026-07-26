Linux Bubblewrap, workspace-write + network=restricted: this proves the OS
sandbox refuses network end to end, the bubblewrap counterpart of the restricted
half of seatbelt-network.t. A `shell` tool call attempting a loopback connect is
blocked because bubblewrap places the command in a separate network namespace.

Honest boundary: gated to %{system} linux, skipped elsewhere, and NOT run on the
macOS host that authored it — exercised only by Linux CI. Unlike the Seatbelt
case this file carries no network=enabled positive control: distinguishing a
sandbox-denied socket from an ordinary closed port there relied on the OS error
wording (permission vs refused), which bubblewrap does not report uniformly. The
connect is instead proven blocked by a wording- and exit-code-agnostic marker —
its exit-0 success marker never appears — under the restricted namespace.
`netrc=0` cannot appear in the command text, which contains only `netrc=$?`.

  $ mkdir -p ws/.git
  $ mentat trust ws >/dev/null
  $ printf 9 > ws/netport.txt

Pin the read scope to `all` for this network-boundary test, matching
seatbelt-network.t: under the default project read scope a blocked out-of-project
read could make `nc` fail for a non-network reason, muddying the proof that the
network namespace blocks the connect. Read confinement is proven separately in
bwrap-read.t.

  $ export MENTAT_SANDBOX_READ=all
  $ cat > net.jsonl <<'JSONL'
  > {"response":{"id":"n1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"i1","call_id":"nc1","name":"shell","arguments":"{\"command\":\"nc -z -w2 127.0.0.1 $(cat netport.txt); echo netrc=$?\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","nc1"]},"response":{"id":"n2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done"}]}]}}
  > JSONL
  $ start_fake_openai net.jsonl
  $ MENTAT_SANDBOX_MODE=workspace-write MENTAT_SANDBOX_NETWORK=restricted MENTAT_SHELL=/bin/sh mentat run start --json --permission bypass --cwd "$PWD/ws" --id net "n" >run.out 2>/dev/null; echo exit:$?
  exit:0
  $ wait_fake_server

The resolved posture is restricted network, confined by bubblewrap; the connect
never succeeds (its exit-0 marker is absent from the result).

  $ grep '"type":"run.started"' run.out | mentat_cram json .sandbox.network
  restricted
  $ grep -o 'enforced backend=linux-bubblewrap' capture/request-2.json
  enforced backend=linux-bubblewrap
  $ grep -c 'netrc=0' capture/request-2.json
  0
  [1]
