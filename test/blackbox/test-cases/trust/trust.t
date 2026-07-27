TRUST-1 (#16) — the `trust`/`untrust` commands flip workspace_trust and gate the
project config layer. A fresh workspace is untrusted; project config it declares is
NOT applied until the workspace is trusted, and untrusting gates it again. This is the
trust-gated config layering round-trip.

  $ use_untrusted_workspace

A project sets a config value in .mentat/config.json (the project layer).

  $ mentat config set --project model openai/gpt-5.4-mini --cwd .
  $ test -f .mentat/config.json && echo wrote-project-config
  wrote-project-config

Untrusted, the project layer is gated: the value is not applied and reads unset
(exit 1), and config show reports the workspace as untrusted.

  $ mentat config get model --cwd . 2>&1
  mentat: warning: workspace config file disabled: workspace trust is untrusted (project: $TESTCASE_ROOT/.mentat/config.json)
  mentat: model is not set
  [1]
  $ mentat config show --cwd . | grep -E '^workspace_trust='
  mentat: warning: workspace config file disabled: workspace trust is untrusted (project: $TESTCASE_ROOT/.mentat/config.json)
  workspace_trust=untrusted

Trusting the workspace honors the project layer: the value now resolves, and the
trust command echoes the trusted root.

  $ mentat trust . | censor
  trusted $TESTCASE_ROOT
  $ mentat config get model --cwd .
  openai/gpt-5.4-mini
  $ mentat config show --cwd . | grep -E '^(workspace_trust|model)='
  model=openai/gpt-5.4-mini
  workspace_trust=trusted

Trust is idempotent; untrust flips it back and re-gates the project layer.

  $ mentat trust . | censor
  trusted $TESTCASE_ROOT
  $ mentat untrust . | censor
  untrusted $TESTCASE_ROOT
  $ mentat config get model --cwd . 2>&1
  mentat: warning: workspace config file disabled: workspace trust is untrusted (project: $TESTCASE_ROOT/.mentat/config.json)
  mentat: model is not set
  [1]
