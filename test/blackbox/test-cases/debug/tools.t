`debug tools` prints the exact provider-facing tool declarations a run would seal
for a mode and model — the same boot-fixed catalog the engine seals — projected
offline, without opening a client.

  $ use_trusted_workspace

Build mode for an apply-patch model carries the apply_patch editor plus the shell
tool.

  $ mentat debug tools --model openai/gpt-5.6-sol > build.out
  $ grep -o '## apply_patch' build.out
  ## apply_patch
  $ grep -o '## shell' build.out
  ## shell
  ## shell
  ## shell

The JSON view enumerates the declarations under one envelope with the resolved
mode and model.

  $ mentat debug tools --model openai/gpt-5.6-sol --json > tools.json
  $ mentat_cram json .type tools.json
  debug_tools
  $ mentat_cram json .mode tools.json
  build
  $ mentat_cram json .model tools.json
  openai/gpt-5.6-sol
  $ mentat_cram json '.tools[].name' tools.json | grep -q '^apply_patch$' && echo present || echo absent
  present

Plan is read-only: it drops the file-mutation and shell tools.

  $ mentat debug tools --mode plan --model openai/gpt-5.6-sol --json > plan.json
  $ mentat_cram json .mode plan.json
  plan
  $ mentat_cram json '.tools[].name' plan.json | grep -q '^apply_patch$' && echo present || echo absent
  absent
  $ mentat_cram json '.tools[].name' plan.json | grep -q '^shell$' && echo present || echo absent
  absent

An unknown provider is a usage error (exit 2).

  $ mentat debug tools --model bogus/nope 2>&1
  mentat: unknown provider "bogus"; known providers: openai, anthropic, google, local, ollama
  [2]
