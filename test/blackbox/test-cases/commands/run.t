A headless `/name` prompt expands the matching command template — substituting
the arguments — and drives the turn from the expanded text. An unmatched `/x`
that names no command is not intercepted: it passes through as a literal prompt.

  $ use_trusted_workspace
  $ mkdir -p .mentat/commands
  $ cat > .mentat/commands/greet.md <<'MD'
  > Say hello to $1 warmly.
  > MD

The expanded body reaches the model, not the raw `/greet` invocation, and the
assistant reply prints on stdout.

  $ cat > greet.jsonl <<'JSONL'
  > {"expect":{"body_contains":["Say hello to World warmly."]},"response":{"id":"g1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"greeted"}]}]}}
  > JSONL
  $ start_fake_openai greet.jsonl capture-greet port-greet
  $ mentat run "/greet World" --cwd "$PWD" --id greet-turn 2>/dev/null
  greeted
  $ wait_fake_server
  $ grep -oF 'Say hello to World warmly.' capture-greet/request-1.json
  Say hello to World warmly.

An unknown `/nope` token is sent literally: the raw string reaches the model
because it matches no discovered command.

  $ cat > literal.jsonl <<'JSONL'
  > {"expect":{"body_contains":["/nope stays literal"]},"response":{"id":"l1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"ok"}]}]}}
  > JSONL
  $ start_fake_openai literal.jsonl capture-literal port-literal
  $ mentat run "/nope stays literal" --cwd "$PWD" --id literal-turn 2>/dev/null
  ok
  $ wait_fake_server
  $ grep -oF '/nope stays literal' capture-literal/request-1.json
  /nope stays literal

A command whose expansion is blank for the given arguments is an empty prompt
even when `--skill` pins guidance ahead of it: the run is rejected, never sent
as a skills-only turn. It fails before any provider call, so no fake server is
needed.

  $ mkdir -p .mentat/skills/help
  $ cat > .mentat/skills/help/SKILL.md <<'MD'
  > ---
  > description: Help guidance.
  > ---
  > Some guidance.
  > MD
  $ cat > .mentat/commands/pos.md <<'MD'
  > $1
  > MD
  $ mentat run "/pos" --skill help --cwd "$PWD" 2>&1 | grep -o 'prompt must not be empty'
  prompt must not be empty
  [2]

An `@file` reference in a command body embeds the named workspace file's bytes
in place, spliced into the surrounding template before the turn is sent.

  $ printf 'remember to water the plants\n' > notes.txt
  $ cat > .mentat/commands/review.md <<'MD'
  > Review these notes: @notes.txt
  > MD
  $ cat > review.jsonl <<'JSONL'
  > {"expect":{"body_contains":["Review these notes: remember to water the plants"]},"response":{"id":"r1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"reviewed"}]}]}}
  > JSONL
  $ start_fake_openai review.jsonl capture-review port-review
  $ mentat run "/review" --cwd "$PWD" --id review-turn 2>/dev/null
  reviewed
  $ wait_fake_server
  $ grep -oF 'Review these notes: remember to water the plants' capture-review/request-1.json
  Review these notes: remember to water the plants

An `@file` that resolves outside the workspace — here through a symlink — is a
hard failure: no turn is sent, the run exits nonzero, and the error names the
failing path. It fails before any provider call, so no fake server is needed.

  $ ln -s /etc/hosts escape.txt
  $ cat > .mentat/commands/leak.md <<'MD'
  > Embed this: @escape.txt
  > MD
  $ mentat run "/leak" --cwd "$PWD" > leak.out 2>&1 || echo "exit=$?"
  exit=1
  $ grep -oF 'cannot resolve @escape.txt: resolves outside the workspace' leak.out
  cannot resolve @escape.txt: resolves outside the workspace
