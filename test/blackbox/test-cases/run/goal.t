A goal declared at start (`run start --goal`) is minted engine-side and appended
before the turn plans, so the --json stream emits goal.declared carrying the
objective ahead of turn.started, and the goal is durably recorded on the session.

  $ use_trusted_workspace
  $ cat > goal.jsonl <<'JSONL'
  > {"expect":{"body_contains":["pursue the port"]},"response":{"id":"g1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"started"}]}]}}
  > JSONL
  $ start_fake_openai goal.jsonl capture-goal port-goal
  $ mentat run start --json --goal "Port the parser" --goal-budget 200000 --id goaled "pursue the port" >goal.out 2>/dev/null
  $ wait_fake_server
  $ grep '"type":"goal.declared"' goal.out | mentat_cram json .goal.objective
  Port the parser
  $ grep '"type":"turn.finished"' goal.out | mentat_cram json .text
  started

The goal is durably declared on the session, with its budget.

  $ mentat session export goaled --format json 2>/dev/null | grep -o 'Port the parser' | head -1
  Port the parser
  $ mentat session export goaled --format json 2>/dev/null | grep -o '"token_budget":200000' | head -1
  "token_budget":200000

`--goal-budget` without `--goal` is a usage error before any run.

  $ mentat run start --goal-budget 1000 --cwd "$PWD" --id nogoal "x" 2>&1
  mentat: --goal-budget requires --goal
  [2]

`run reply` carries the goal lifecycle verbs, acting on the declared goal and
reporting its resulting state; they take no pending decision and start no turn.
Edit replaces the objective:

  $ mentat run reply goaled --edit-goal "Port the lexer" --cwd "$PWD" 2>&1 | grep -o 'Port the lexer'
  Port the lexer
  $ mentat session export goaled --format json 2>/dev/null | grep -o 'Port the lexer' | head -1
  Port the lexer

Pause, resume, and clear each succeed against the live goal:

  $ mentat run reply goaled --pause-goal --cwd "$PWD" 2>&1 | grep -o 'paused'
  paused
  $ mentat run reply goaled --resume-goal --goal-budget 500 --cwd "$PWD" 2>&1 | grep -o 'active'
  active
  $ mentat run reply goaled --clear-goal --cwd "$PWD" 2>&1 | grep -o 'cleared'
  cleared

Once cleared, the goal is terminal, so a further transition is rejected.

  $ mentat run reply goaled --pause-goal --cwd "$PWD" 2>&1 | grep -o 'is not allowed'
  is not allowed
  [2]
