`debug context` shows the workspace context a run would assemble — the root, the
instruction enablement with origins, and the discovered AGENTS.md sources —
offline, without the engine.

  $ use_trusted_workspace
  $ cat > AGENTS.md <<'MD'
  > Project house rules.
  > MD

The human view reports the workspace block, the .git root marker, and the active
project instruction source.

  $ mentat debug context | grep -o 'Workspace:'
  Workspace:
  $ mentat debug context | grep -o 'root marker: .git'
  root marker: .git
  $ mentat debug context | grep -o 'project ./AGENTS.md'
  project ./AGENTS.md

The JSON view carries the enveloped workspace facts, the typed sources, and the
projection digest.

  $ mentat debug context --json > ctx.json
  $ mentat_cram json .type ctx.json
  context_show
  $ mentat_cram json .workspace.root_marker ctx.json
  .git
  $ mentat_cram json .workspace.global_instructions.enabled ctx.json
  true
  $ mentat_cram json .projection.rendered_digest ctx.json | grep -o '^sha256:'
  sha256:
