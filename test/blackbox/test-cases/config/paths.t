Config file paths resolve from the hermetic test environment; authority
variables fail closed rather than falling through to another root.

  $ use_trusted_workspace

The user file defaults under the XDG config home.

  $ mentat config path | censor
  $TESTCASE_ROOT/config/mentat/config.json
  $ mentat config path --user | censor
  $TESTCASE_ROOT/config/mentat/config.json

The project files live under the workspace-local .mentat directory.

  $ mentat config path --project | censor
  $TESTCASE_ROOT/.mentat/config.json
  $ mentat config path --project-local | censor
  $TESTCASE_ROOT/.mentat/config.local.json

MENTAT_CONFIG_HOME redirects the user config root (the file sits directly under
it, no mentat/ segment).

  $ MENTAT_CONFIG_HOME="$PWD/custom-config" mentat config path | censor
  $TESTCASE_ROOT/custom-config/config.json

An authority variable must be absolute; a relative value fails closed.

  $ MENTAT_CONFIG_HOME=relative mentat config path 2>&1
  mentat: MENTAT_CONFIG_HOME must be an absolute path: relative
  [1]

With no config home resolvable at all, path discovery fails closed rather than
falling back to the repository.

  $ env -u MENTAT_CONFIG_HOME -u XDG_CONFIG_HOME -u HOME mentat config path 2>&1
  mentat: cannot resolve MENTAT_CONFIG_HOME: $HOME is unset or relative
  [1]

Target flags are mutually exclusive.

  $ mentat config path --user --project 2>&1
  Usage: mentat config path [--help] [OPTION]…
  mentat: options --user and --project cannot be present at the same time
  [124]
