Config edits the shared project layer and the gitignored project-local layer
as separate files, and project-local takes precedence over shared. Both apply
because the workspace is trusted by setup.

  $ use_trusted_workspace

The shared project file is created and written through typed keys.

  $ mentat config init --project >/dev/null
  $ cat .mentat/config.json
  {}

  $ mentat config set --project model openai/gpt-5.4-mini >/dev/null
  $ cat .mentat/config.json
  {"model":"openai/gpt-5.4-mini"}

A layer read sees the shared value, and the effective read honours it in this
trusted workspace.

  $ mentat config get --project model
  openai/gpt-5.4-mini
  $ mentat config get model
  openai/gpt-5.4-mini

The project-local file is created and shadows the shared file.

  $ mentat config init --project-local >/dev/null
  $ cat .mentat/config.local.json
  {}

  $ mentat config set --project-local model openai/gpt-5.4-nano >/dev/null
  $ mentat config get --project-local model
  openai/gpt-5.4-nano
  $ mentat config get model
  openai/gpt-5.4-nano

Source flags are mutually exclusive for reads (command-line parse error).

  $ mentat config get --user --project model 2>&1
  Usage: mentat config get [--help] [OPTION]… KEY
  mentat: options --user and --project cannot be present at the same time
  [124]
