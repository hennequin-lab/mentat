Config help documents the subcommands and their editing targets and flags in
user-facing language. Help is long and terminal-width sensitive, so these
checks grep the load-bearing phrases under --help=plain rather than pinning
whole pages.

  $ use_trusted_workspace

The group help lists the configuration subcommands.

  $ mentat config --help=plain | grep -c 'CONFIGURATION COMMANDS'
  1

Set documents the three editing targets in plain language.

  $ mentat config set --help=plain | grep -o 'Operate on the user config layer.'
  Operate on the user config layer.
  $ mentat config set --help=plain | grep -o 'Operate on the shared project config layer.'
  Operate on the shared project config layer.
  $ mentat config set --help=plain | grep -o 'Operate on the gitignored project-local config layer.'
  Operate on the gitignored project-local config layer.

Show documents its JSON and origins flags.

  $ mentat config show --help=plain | grep -o 'Emit machine-readable JSON instead of human text.'
  Emit machine-readable JSON instead of human text.
  $ mentat config show --help=plain | grep -o "Include each value's source."
  Include each value's source.

Validate documents what strict mode changes: the severity of an inert key, not
whether it is noticed.

  $ mentat config validate --help=plain | grep -o 'Fail on keys that have no effect, not just warn.'
  Fail on keys that have no effect, not just warn.
