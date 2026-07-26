Mid-stream SIGINT interrupt (RUN-05 / F6, streaming variant) — a provider that
streams partial deltas then stalls before the terminal event holds the client
parked in a native Eio read (lib/llm/http/transport.ml single_read). The
cooperative `cancelled` flag is polled only between provider events, so during
the stall it is never observed; the graceful interrupt lands solely through the
worker-switch cancel in the driver's interrupt_worker. This proves the
switch-fail — not the flag — breaks a blocking transport read: the exit is 130
either way, but only a working switch-fail makes it prompt (a flag-only cancel
waits out the whole hold). The same native read primitive backs the HTTP
(transport.ml) and local-GGUF (lib/llm/local/api.ml) paths, and TLS-over-Eio is
pure Eio, so all three cancel alike.

  $ cat > stream.jsonl <<'JSONL'
  > {"stream_delay_ms":10000,"expect":{"body_contains":["work"]},"response":{"id":"r1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"here is a streaming answer being generated slowly"}]}]}}
  > JSONL
  $ start_fake_openai stream.jsonl
  $ use_trusted_workspace
  $ hold_run streamed "work"
  $ wait_captured 1

The server has flushed the deltas and is holding before the terminal event, so
the client is parked in the provider read. One SIGINT cancels it promptly: the
exit is 130 and the elapsed time is far below the server's 10s hold. The bound
is generous (well under the 10s broken-case floor) so it never flakes, yet a
flag-only cancel — which waits out the hold — reads as `slow`.

  $ date +%s > start_ts
  $ kill -INT "$MENTAT_HELD_PID"
  $ wait_exit "$MENTAT_HELD_PID"
  130
  $ elapsed=$(( $(date +%s) - $(cat start_ts) )); [ "$elapsed" -lt 5 ] && echo prompt || echo "slow: ${elapsed}s"
  prompt

The interrupted session settled to idle and is resumable — nothing was stranded.

  $ mentat session show streamed --cwd "$PWD" | grep -E '^phase='
  phase=idle
