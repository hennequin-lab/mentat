SIGINT graceful interrupt (#7 / RUN-05 / F6) — #52 landed the systhread fix (the
delay_ms whole-response read is now cancellable). A run genuinely held mid-turn (a
long delay_ms, with wait_captured proving the process is in-turn before the signal)
and sent SIGINT cancels the in-flight request, saves the session, and exits 130 (the
SIGINT convention, 128+2), with the session settled to idle — exactly the #52
graceful-interrupt contract.

  $ cat > hold.jsonl <<'JSONL'
  > {"delay_ms":10000,"expect":{"body_contains":["work"]},"response":{"id":"r1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done"}]}]}}
  > JSONL
  $ start_fake_openai hold.jsonl
  $ use_trusted_workspace
  $ hold_run held "work"
  $ wait_captured 1
  $ kill -INT "$MENTAT_HELD_PID"
  $ wait_exit "$MENTAT_HELD_PID"
  130

The interrupted session settled to idle and is resumable — nothing was stranded.

  $ mentat session show held --cwd "$PWD" | grep -E '^phase='
  phase=idle
