Project-local config init maintains .mentat/.gitignore so the gitignored layer
is never committed. Only the project-local layer scaffolds the ignore entry;
the shared project layer does not.

  $ use_trusted_workspace

Initialization creates the ignore entry.

  $ mentat config init --project-local >/dev/null
  $ cat .mentat/.gitignore
  config.local.json

Re-running does not duplicate it.

  $ mentat config init --project-local >/dev/null
  $ cat .mentat/.gitignore
  config.local.json

Existing content is preserved and the entry is appended.

  $ printf 'cache\n' > .mentat/.gitignore
  $ mentat config init --project-local >/dev/null
  $ cat .mentat/.gitignore
  cache
  config.local.json

A missing trailing newline is handled cleanly.

  $ printf 'cache' > .mentat/.gitignore
  $ mentat config init --project-local >/dev/null
  $ cat .mentat/.gitignore
  cache
  config.local.json

An existing entry surrounded by others is not duplicated.

  $ printf 'cache\nconfig.local.json\nlogs\n' > .mentat/.gitignore
  $ mentat config init --project-local >/dev/null
  $ cat .mentat/.gitignore
  cache
  config.local.json
  logs

The shared project layer is committable, so initializing it alone does not
scaffold the local-config ignore entry.

  $ rm -rf .mentat
  $ mentat config init --project >/dev/null
  $ test -e .mentat/.gitignore || echo "no gitignore"
  no gitignore
