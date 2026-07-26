Automatic Context_overflow recovery: a turn's model request overflows, so the
engine compacts once (reason context_overflow) and retries the request over the
reduced view within the same turn (a per-turn bound). The
overflow arrives as a provider 413, which the OpenAI adapter classifies as
Context_overflow.

  $ use_trusted_workspace
  $ cat > overflow.jsonl <<'JSONL'
  > {"expect":{"body_contains":["hello prompt"]},"http":{"status":413,"json":{"error":{"message":"context window exceeded"}}}}
  > {"expect":{"body_contains":["Summarize this conversation"]},"response":{"id":"s1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"Compacted history summary."}]}]}}
  > {"response":{"id":"r2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"Recovered answer."}]}]}}
  > JSONL
  $ start_fake_openai overflow.jsonl capture-o port-o

The turn overflows, compacts, retries, and prints the recovered answer — the
overflow is handled internally, so the run succeeds.

  $ mentat run "hello prompt" --cwd "$PWD" --id overflow-turn 2>/dev/null
  Recovered answer.
  $ wait_fake_server

Exactly three requests reached the fake: the overflowing turn request, the
summary, and the retry over the reduced view.

  $ ls capture-o/request-*.json | wc -l | tr -d ' '
  3
  $ grep -o 'Summarize this conversation' capture-o/request-2.json
  Summarize this conversation

The reissued retry (request 3) carries the verbatim summary framed by the resume
notice, so the model continues the work rather than acknowledging the summary.

  $ grep -o 'Compacted history summary.' capture-o/request-3.json | head -1
  Compacted history summary.
  $ grep -o 'compacted to save context' capture-o/request-3.json | head -1
  compacted to save context

The durable compaction carries the context_overflow reason.

  $ mentat session export overflow-turn --format json 2>/dev/null | grep -o '"reason":"context_overflow"' | head -1
  "reason":"context_overflow"
