Environment variables override effective config for reads without being
persisted by edits.

  $ use_trusted_workspace

On Unix, COMSPEC from cross-platform environments does not override SHELL for
the default shell program.

  $ COMSPEC='C:\Windows\System32\cmd.exe' SHELL=/bin/zsh mentat config get shell
  /bin/zsh

Seed the user file with values the environment will shadow.

  $ mentat config set model openai/gpt-5.4-mini >/dev/null
  $ mentat config set reasoning medium >/dev/null
  $ mentat config set run.max_steps 7 >/dev/null
  $ mentat config set shell /bin/bash >/dev/null
  $ mentat config set providers.openai.base_url https://file.example/v1 >/dev/null

Environment variables win for reads.

  $ MENTAT_MODEL=openai/env-model mentat config get model
  openai/env-model

  $ MENTAT_REASONING=xhigh mentat config get reasoning
  xhigh

An invalid environment value fails closed (exit 1).

  $ MENTAT_REASONING=extreme mentat config get reasoning 2>&1
  mentat: unknown reasoning effort: extreme
  [1]

  $ MENTAT_MAX_STEPS=22 mentat config get run.max_steps
  22

  $ MENTAT_MAX_STEPS=9007199254740992 mentat config get run.max_steps 2>&1
  mentat: MENTAT_MAX_STEPS must be at most 9007199254740991
  [1]

  $ MENTAT_SHELL=/bin/zsh mentat config get shell
  /bin/zsh

  $ MENTAT_OPENAI_BASE_URL=https://env.example/v1 mentat config get providers.openai.base_url
  https://env.example/v1

  $ MENTAT_OLLAMA_BASE_URL=http://127.0.0.1:8080 mentat config get providers.ollama.base_url
  http://127.0.0.1:8080

`MENTAT_SMALL_MODEL` overrides the `small_model` field for reads, exactly like
`MENTAT_MODEL` does for `model`; like every environment override it is never
persisted by an edit — a `config set` made while it and a supported environment
override are present writes only the edited file value, neither environment one.

  $ MENTAT_SMALL_MODEL=openai/env-small mentat config get small_model
  openai/env-small

  $ MENTAT_MODEL=openai/env-model MENTAT_SMALL_MODEL=openai/env-small mentat config set reasoning high >/dev/null
  $ cat "$XDG_CONFIG_HOME/mentat/config.json"
  {"model":"openai/gpt-5.4-mini","reasoning":"high","shell":"/bin/bash","run":{"max_steps":7},"providers":{"openai":{"base_url":"https://file.example/v1"}}}
