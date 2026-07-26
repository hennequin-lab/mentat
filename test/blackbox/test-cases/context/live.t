Workspace context assembly. A normal turn's request prelude carries the system
prompt, the workspace block, and the project's AGENTS.md instructions — the
executable assembling `Mentat_context` into the engine's per-turn execution
selection. Context is recomputed per turn, so an edited AGENTS.md
takes effect at the next turn.

  $ use_trusted_workspace
  $ cat > AGENTS.md <<'MD'
  > Always answer in exactly one word.
  > MD
  $ cat > live.jsonl <<'JSONL'
  > {"expect":{"body_contains":["hello there"]},"response":{"id":"r1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"ok"}]}]}}
  > JSONL
  $ start_fake_openai live.jsonl capture port

  $ mentat run "hello there" --cwd "$PWD" --id ctx-live 2>/dev/null
  ok
  $ wait_fake_server

The request carries the system prompt.

  $ grep -o 'You are Mentat, an OCaml coding agent' capture/request-1.json
  You are Mentat, an OCaml coding agent

The request carries the environment block naming the run directory.

  $ grep -o '# Environment' capture/request-1.json
  # Environment

The request carries the project AGENTS.md instructions inside the instruction
block.

  $ grep -o 'AGENTS.md instructions for' capture/request-1.json
  AGENTS.md instructions for
  $ grep -o 'Always answer in exactly one word' capture/request-1.json
  Always answer in exactly one word

The request carries the skills catalog: built-in skills are active by default,
so the catalog fragment names the available skills, and the `skill` tool is
declared.

  $ grep -o 'Available skills:' capture/request-1.json
  Available skills:
  $ grep -o '"name":"skill"' capture/request-1.json | head -1
  "name":"skill"

A later turn re-reads the workspace: editing AGENTS.md changes the instructions
the next request carries.

  $ cat > AGENTS.md <<'MD'
  > Respond only with haiku.
  > MD
  $ cat > live2.jsonl <<'JSONL'
  > {"expect":{"body_contains":["second"]},"response":{"id":"r2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done"}]}]}}
  > JSONL
  $ start_fake_openai live2.jsonl capture2 port2

  $ mentat run "second" --cwd "$PWD" --id ctx-live-2 2>/dev/null
  done
  $ wait_fake_server

  $ grep -o 'Respond only with haiku' capture2/request-1.json
  Respond only with haiku

A plan-mode turn (`run start --mode plan`) seals a read-only planning contract:
the request prelude carries the plan-mode instruction and the `propose_plan`
tool is declared, so the model plans rather than edits.

  $ cat > plan.jsonl <<'JSONL'
  > {"expect":{"body_contains":["plan this task","You are in plan mode"]},"response":{"id":"p1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"planned"}]}]}}
  > JSONL
  $ start_fake_openai plan.jsonl capture-plan port-plan

  $ mentat run start --mode plan --cwd "$PWD" --id ctx-plan "plan this task" 2>/dev/null
  planned
  $ wait_fake_server

  $ grep -o 'You are in plan mode' capture-plan/request-1.json
  You are in plan mode
  $ grep -o '"name":"propose_plan"' capture-plan/request-1.json | head -1
  "name":"propose_plan"

The durable turn contract records the plan mode.

  $ mentat session export ctx-plan --format json 2>/dev/null | grep -o '"mode":"plan"' | head -1
  "mode":"plan"
