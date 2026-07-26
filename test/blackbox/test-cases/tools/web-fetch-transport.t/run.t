The Web family's private transport pins each vetted connection while preserving
the URL authority for HTTP and TLS. Same-authority redirects stay inside one
deadline; cross-authority redirects return without contacting their target.

  $ use_trusted_workspace
  $ mkdir -p "$XDG_CONFIG_HOME/mentat"
  $ cat > "$XDG_CONFIG_HOME/mentat/config.json" <<'JSON'
  > {"permission":{"rules":{"version":1,"items":[{"action":"allow","matcher":{"type":"any"}}]}},"web":{"enabled":true,"allow_private_network":true,"fetch_max_bytes":32,"output_max_chars":1000,"timeout_ms":30000,"max_timeout_ms":30000}}
  > JSON
  $ export OCAML_EXTRA_CA_CERTS="$PWD/server-cert.pem"
  $ python3 server.py server-cert.pem server-key.pem web-port web-requests.log web-handshakes.log web-hang.hit web-hang.closed >web-server.out 2>&1 &
  $ WEB_SERVER_PID=$!
  $ trap 'kill "$WEB_SERVER_PID" 2>/dev/null || true' EXIT
  $ wait_for_file web-port
  $ WEB_PORT=$(cat web-port)

The first call starts with HTTP, so the portable tool upgrades it to HTTPS. The
transport then authenticates `localhost`, keeps that Host authority on the
pinned socket, follows the relative redirect, and returns its text body. The
same scripted turn distinguishes Content-Length preflight from streaming
overflow, rejects a non-decimal length, enforces ten same-authority redirects,
and spends one deadline across a slow redirect chain.

  $ cat > web-contracts.jsonl <<JSONL
  > {"expect":{"body_contains":["transport contracts","\"name\":\"web_fetch\""]},"response":{"id":"web-1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"web-item-1","call_id":"web-same","name":"web_fetch","arguments":"{\"url\":\"http://localhost:$WEB_PORT/same\",\"format\":\"text\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","web-same","transport ok"]},"response":{"id":"web-2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"web-item-2","call_id":"web-cross","name":"web_fetch","arguments":"{\"url\":\"https://localhost:$WEB_PORT/cross\",\"format\":\"text\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","web-cross","Redirect 302","/not-contacted"]},"response":{"id":"web-3","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"web-item-3","call_id":"web-large","name":"web_fetch","arguments":"{\"url\":\"https://localhost:$WEB_PORT/large\",\"format\":\"text\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","web-large","Response exceeded the 32-byte limit after 0 bytes"]},"response":{"id":"web-4","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"web-item-4","call_id":"web-stream","name":"web_fetch","arguments":"{\"url\":\"https://localhost:$WEB_PORT/stream\",\"format\":\"text\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","web-stream","Response exceeded the 32-byte limit after"]},"response":{"id":"web-5","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"web-item-5","call_id":"web-invalid-length","name":"web_fetch","arguments":"{\"url\":\"https://localhost:$WEB_PORT/invalid-length\",\"format\":\"text\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","web-invalid-length","invalid Content-Length"]},"response":{"id":"web-6","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"web-item-6","call_id":"web-redirect-limit","name":"web_fetch","arguments":"{\"url\":\"https://localhost:$WEB_PORT/redirect-limit/0\",\"format\":\"text\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","web-redirect-limit","exceeded the 10-redirect limit"]},"response":{"id":"web-7","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"web-item-7","call_id":"web-timeout","name":"web_fetch","arguments":"{\"url\":\"https://localhost:$WEB_PORT/chain/0\",\"format\":\"text\",\"timeout_ms\":350}"}]}}
  > {"expect":{"body_contains":["function_call_output","web-timeout","web fetch timed out"]},"response":{"id":"web-8","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"transport contracts observed"}]}]}}
  > JSONL
  $ start_fake_openai web-contracts.jsonl capture-web port-openai-web
  $ mentat run start --id web-contracts "transport contracts" --cwd "$PWD" 2>/dev/null
  transport contracts observed
  $ wait_fake_server

The request log is the host boundary: the upgraded and redirected requests all
carry the original authority and only the tool-owned, non-credential headers.
Every TLS handshake retains that DNS name as SNI, while the certificate trusts
no loopback IP. The cross-authority target and the third slow hop are absent.

  $ cut -d ' ' -f 1 web-requests.log
  /same
  /ok
  /cross
  /large
  /stream
  /invalid-length
  /redirect-limit/0
  /redirect-limit/1
  /redirect-limit/2
  /redirect-limit/3
  /redirect-limit/4
  /redirect-limit/5
  /redirect-limit/6
  /redirect-limit/7
  /redirect-limit/8
  /redirect-limit/9
  /redirect-limit/10
  /chain/0
  /chain/1
  $ sed "s/:$WEB_PORT/:PORT/g" web-requests.log | cut -d ' ' -f 2- | sort -u
  host=localhost:PORT ua=mentat/0 (+https://github.com/invariant-hq/mentat) language=en-US,en;q=0.9 authorization=false cookie=false
  $ sort -u web-handshakes.log
  localhost
  $ test "$(wc -l < web-handshakes.log)" -eq "$(wc -l < web-requests.log)"; echo $?
  0
  $ test "$(grep -c '/not-contacted' web-requests.log)" -eq 0; echo $?
  0
  $ test "$(grep -c '^/redirect-limit/11 ' web-requests.log)" -eq 0; echo $?
  0
  $ test "$(grep -c '^/chain/2 ' web-requests.log)" -eq 0; echo $?
  0

SIGINT while the peer withholds its response cancels the live network call,
closes the connection, records an interrupted turn, and leaves the session
idle instead of waiting for the transport timeout.

  $ cat > web-cancel.jsonl <<JSONL
  > {"expect":{"body_contains":["cancel web","\"name\":\"web_fetch\""]},"response":{"id":"cancel-1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"cancel-item","call_id":"cancel-call","name":"web_fetch","arguments":"{\"url\":\"https://localhost:$WEB_PORT/hang\",\"format\":\"text\"}"}]}}
  > JSONL
  $ start_fake_openai web-cancel.jsonl capture-cancel port-openai-cancel
  $ hold_run web-cancel "cancel web"
  $ wait_for_file web-hang.hit
  $ CANCEL_STARTED=$SECONDS
  $ kill -INT "$MENTAT_HELD_PID"
  $ wait_exit "$MENTAT_HELD_PID"
  130
  $ test "$((SECONDS - CANCEL_STARTED))" -lt 5; echo $?
  0
  $ wait_for_file web-hang.closed
  $ cat web-hang.closed
  connection closed
  $ wait_fake_server
  $ mentat session show web-cancel --cwd "$PWD" | grep -E '^phase='
  phase=idle
  $ grep '^/hang ' web-requests.log | sed "s/:$WEB_PORT/:PORT/g"
  /hang host=localhost:PORT ua=mentat/0 (+https://github.com/invariant-hq/mentat) language=en-US,en;q=0.9 authorization=false cookie=false

  $ kill "$WEB_SERVER_PID"
  $ wait "$WEB_SERVER_PID" 2>/dev/null || true
  $ trap - EXIT
