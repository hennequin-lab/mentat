`debug prompt` prints the exact model-visible prelude fragments a run seals — the
system prompt, the workspace block, the active instructions, and the mode prompt
— offline. Skills are switched off so the fragment set depends only on the
context and the mode.

  $ use_trusted_workspace
  $ mentat config set skills.enabled false >/dev/null
  $ cat > AGENTS.md <<'MD'
  > Prefer small diffs.
  > MD

The default build prelude carries the active workspace instructions.

  $ mentat debug prompt | grep -o 'Prefer small diffs.'
  Prefer small diffs.

The JSON view enumerates each fragment's role and text under one envelope; the
first fragment is always the system prompt.

  $ mentat debug prompt --json > prompt.json
  $ mentat_cram json .type prompt.json
  context_prompt
  $ mentat_cram json .mode prompt.json
  build
  $ mentat_cram json .fragments[0].role prompt.json
  system

Build seals the system prompt, the workspace block, and the instruction block.

  $ mentat debug prompt --json | mentat_cram json '.fragments[].role' | wc -l | tr -d ' '
  3

Plan adds one developer mode fragment on top of that set.

  $ mentat debug prompt --mode plan --json > plan.json
  $ mentat_cram json .mode plan.json
  plan
  $ mentat debug prompt --mode plan --json | mentat_cram json '.fragments[].role' | wc -l | tr -d ' '
  4
