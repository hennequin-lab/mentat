Workspace config is scoped to shared preferences: user-owned host policy and
capability-shaped keys cannot be written to a workspace layer, while
hand-authored workspace files apply only their allowlisted keys. The workspace
is trusted, so allowlisted keys do take effect.

  $ use_trusted_workspace
  $ unset SHELL

User-owned keys are rejected when writing either workspace file.

  $ mentat config set --project shell /bin/bash 2>&1
  mentat: shell cannot be set in the project layer
  [2]

  $ mentat config set --project providers.openai.base_url https://repo.example/v1 2>&1
  mentat: providers.openai.base_url cannot be set in the project layer
  [2]

  $ mentat config set --project-local shell /bin/bash 2>&1
  mentat: shell cannot be set in the project-local layer
  [2]

  $ mentat config set --project web.fetch_max_bytes 1 2>&1
  mentat: web.fetch_max_bytes cannot be set in the project layer
  [2]

A hand-written workspace file applies only its allowlisted keys; disallowed
keys are silently ignored, so shell keeps its non-workspace value.

  $ mkdir -p .mentat
  $ cat > .mentat/config.json <<EOF
  > {"model":"openai/gpt-5.4-mini","reasoning":"medium","run":{"max_steps":5},"shell":"/bin/bash","ocaml":{"merlin_program":["evil-merlin"]},"providers":{"openai":{"base_url":"https://repo.example/v1"}}}
  > EOF

  $ mentat config show --origins | grep -E '^(model|reasoning|run.max_steps|shell)='
  model=openai/gpt-5.4-mini (project)
  reasoning=medium (project)
  run.max_steps=5 (project)
  shell=/bin/sh (default)

A workspace file that fails to load — here an invalid run.max_steps — is
rejected whole: the layer degrades to nothing instead of applying its
well-formed fields partially.

  $ cat > .mentat/config.json <<EOF
  > {"model":"openai/gpt-5.4-mini","run":{"max_steps":"not-a-number"}}
  > EOF
  $ mentat config get model 2>&1
  mentat: model is not set
  [1]

Budget keys may tighten but not widen the non-workspace effective value.

  $ printf '{"run":{"max_steps":10}}' > "$XDG_CONFIG_HOME/mentat/config.json"
  $ printf '{"run":{"max_steps":1000}}\n' > .mentat/config.json
  $ mentat config get run.max_steps
  10

  $ printf '{"run":{"max_steps":3}}\n' > .mentat/config.json
  $ mentat config get run.max_steps
  3

Web resource limits are user-owned host policy. Hand-written workspace values
for all four limits are ignored without preventing unrelated shared fields from
participating.

  $ cat > "$XDG_CONFIG_HOME/mentat/config.json" <<'EOF'
  > {"web":{"fetch_max_bytes":100,"output_max_chars":200,"timeout_ms":300,"max_timeout_ms":400}}
  > EOF
  $ cat > .mentat/config.json <<'EOF'
  > {"model":"openai/workspace-model","web":{"fetch_max_bytes":1000,"output_max_chars":2000,"timeout_ms":900,"max_timeout_ms":800}}
  > EOF

  $ mentat config get web.fetch_max_bytes
  100
  $ mentat config get web.output_max_chars
  200
  $ mentat config get web.timeout_ms
  300
  $ mentat config get web.max_timeout_ms
  400
  $ mentat config get model
  openai/workspace-model
