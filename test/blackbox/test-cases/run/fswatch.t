The filesystem watch lane brackets every tool claim's attribution window with
poll boundaries on the engine's driver fiber: paths that change while a claim
runs are observationally attributed to that claim as durable `tool_observed`
mutation evidence, and only changes outside every window queue as
external-change notices, drained at the next turn preparation. A headless
one-shot run admits one turn — its drain runs before any change can exist, and
the watcher's baseline is per-process — so the external-notice half needs a
live second turn and stays manual: in a TUI session, edit a workspace file
from another terminal between two prompts and the second request carries a
`- [info] fswatch: 1 workspace file change since the last scan` notice naming
the path, recorded as a durable turn-scoped notice. This suite
pins the in-run halves: window observation, the ignore rules, and the config
gate.

  $ use_trusted_workspace

An opaque shell write during a claim becomes `tool_observed` evidence for that
claim. The write is the shell child's own effect — no editor receipt exists,
so only the watch lane's window diff can attribute it.

  $ cat > observe.jsonl <<'JSONL'
  > {"expect":{"body_contains":["observe a shell write"]},"response":{"id":"o1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"item-o1","call_id":"call-o1","name":"shell","arguments":"{\"command\":\"printf watched > watched.txt\",\"description\":\"write one file\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","call-o1"]},"response":{"id":"o2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"wrote"}]}]}}
  > JSONL
  $ start_fake_openai observe.jsonl capture-observe port-observe
  $ mentat run "observe a shell write" --cwd "$PWD" --permission bypass --id observe-run 2>/dev/null
  wrote
  $ wait_fake_server
  $ mentat session export observe-run --format json 2>/dev/null | grep -o '"tool_observed"' | head -1
  "tool_observed"
  $ mentat session export observe-run --format json 2>/dev/null | grep -o 'watched.txt' | head -1
  watched.txt

Ignored subtrees never enter a window: a shell write confined to version
control metadata, build and switch artifacts, or mentat's own workspace
directory yields no observation.

  $ cat > ignored.jsonl <<'JSONL'
  > {"expect":{"body_contains":["write only ignored paths"]},"response":{"id":"i1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"item-i1","call_id":"call-i1","name":"shell","arguments":"{\"command\":\"mkdir -p _build _opam .mentat && printf x > _build/x.txt && printf x > _opam/x.txt && printf x > .mentat/x.txt && printf x > .git/x.txt\",\"description\":\"write ignored paths\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","call-i1"]},"response":{"id":"i2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"ignored"}]}]}}
  > JSONL
  $ start_fake_openai ignored.jsonl capture-ignored port-ignored
  $ mentat run "write only ignored paths" --cwd "$PWD" --permission bypass --id ignored-run 2>/dev/null
  ignored
  $ wait_fake_server
  $ mentat session export ignored-run --format json 2>/dev/null | grep -c '"tool_observed"'
  0
  [1]

The workspace's own ignore files prune the watch the same way they prune the
capture walk and glob: rules are read from the root's `.gitignore` at watcher
start. One claim writes both a gitignored path and a tracked path; only the
tracked path is observed, so the rules discriminate rather than the lane being
dead.

  $ printf 'vendor/\n' > .gitignore
  $ cat > gitignored.jsonl <<'JSONL'
  > {"expect":{"body_contains":["write inside and outside vendor"]},"response":{"id":"v1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"item-v1","call_id":"call-v1","name":"shell","arguments":"{\"command\":\"mkdir -p vendor && printf x > vendor/inside.txt && printf x > tracked.txt\",\"description\":\"write two files\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","call-v1"]},"response":{"id":"v2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"pruned"}]}]}}
  > JSONL
  $ start_fake_openai gitignored.jsonl capture-gitignored port-gitignored
  $ mentat run "write inside and outside vendor" --cwd "$PWD" --permission bypass --id gitignored-run 2>/dev/null
  pruned
  $ wait_fake_server

The assertion greps the tool_observed evidence, not the whole export: the opaque
shell command text echoes vendor/inside.txt literally, so a whole-export grep
would falsely match the gitignored path the watcher correctly pruned.

  $ mentat session export gitignored-run --format json 2>/dev/null | grep -oE '"type":"tool_observed"[^}]*' | grep -o 'tracked.txt'
  tracked.txt
  $ mentat session export gitignored-run --format json 2>/dev/null | grep -oE '"type":"tool_observed"[^}]*' | grep -c 'vendor/inside.txt'
  0
  [1]
  $ rm .gitignore

The `notices.fswatch` gate disables the lane wholesale — no watcher, no window
polls, no observation — so the same shell write leaves no evidence.

  $ mentat config set notices.fswatch false >/dev/null
  $ cat > gated.jsonl <<'JSONL'
  > {"expect":{"body_contains":["gated shell write"]},"response":{"id":"g1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"item-g1","call_id":"call-g1","name":"shell","arguments":"{\"command\":\"printf gated > gated.txt\",\"description\":\"write one file\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","call-g1"]},"response":{"id":"g2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"gated"}]}]}}
  > JSONL
  $ start_fake_openai gated.jsonl capture-gated port-gated
  $ mentat run "gated shell write" --cwd "$PWD" --permission bypass --id gated-run 2>/dev/null
  gated
  $ wait_fake_server
  $ mentat session export gated-run --format json 2>/dev/null | grep -c '"tool_observed"'
  0
  [1]
