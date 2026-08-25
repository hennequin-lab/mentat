The workspace instance a daemon boots for an attaching client resolves against
that client's environment, not the daemon's own. Dune folds configuration read
from the environment into every rule digest, so a daemon that resolved from
whichever shell happened to spawn it would configure every confined child
differently from the shell the user builds in — the agent's build and the
user's manual build would invalidate each other on every alternation. The
client's snapshot rides every handshake; the instance fixes it at boot.

  $ use_trusted_workspace
  $ trap stop_daemon EXIT

A durable config rule pre-allows shell execution, the attach-lifecycle pattern:
a headless run over --attach cannot answer a permission prompt, and a review
posture set over the wire does not reach an unattended turn's sealed contract.

  $ export MENTAT_SHELL=/bin/sh
  $ mkdir -p "$XDG_CONFIG_HOME/mentat"
  $ cat > "$XDG_CONFIG_HOME/mentat/config.json" <<'JSON'
  > { "permission": { "rules": { "version": 1, "items": [
  >   { "action": "allow", "matcher": { "type": "command", "pattern": { "type": "any" } } } ] } } }
  > JSON

The model runs one shell command that reports the dune-family configuration it
sees, then closes on the tool result.

  $ cat > script.jsonl <<'JSONL'
  > {"response":{"id":"e1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"i1","call_id":"ec1","name":"shell","arguments":"{\"command\":\"echo cache=${DUNE_CACHE:-ABSENT} param=${OCAMLPARAM:-ABSENT}\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","ec1"]},"response":{"id":"e2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done"}]}]}}
  > JSONL
  $ start_fake_openai script.jsonl

The daemon starts with no DUNE_CACHE or OCAMLPARAM in its environment; only
the attaching client exports them. Its handshake boots the workspace instance,
so the daemon-hosted turn's confined child must read the client's values.

  $ start_daemon
  $ DUNE_CACHE=enabled-except-user-rules OCAMLPARAM=_,w=+a \
  >   mentat run start --attach --json --id envdemo "e" --cwd "$PWD" >run.out 2>/dev/null; echo exit:$?
  exit:0
  $ grep '"type":"tool.finished"' run.out | mentat_cram json .outcome
  returned

The tool result the model saw carries the client's configuration, spelled by
the child, through the daemon's engine.

  $ grep -o 'cache=enabled-except-user-rules param=_,w=+a' capture/request-2.json
  cache=enabled-except-user-rules param=_,w=+a
