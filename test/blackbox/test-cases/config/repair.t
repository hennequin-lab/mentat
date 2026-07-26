Config file operations that do not depend on effective resolution stay robust:
unset is a no-op on an absent file, init never rewrites an existing file, and a
command-line scalar is stored verbatim as a string, never re-parsed as JSON.

  $ use_trusted_workspace

Unset is a no-op when the key is absent and the file does not exist.

  $ test -e "$XDG_CONFIG_HOME/mentat/config.json" || echo missing
  missing
  $ mentat config unset model
  $ test -e "$XDG_CONFIG_HOME/mentat/config.json" || echo still-missing
  still-missing

Initialization creates a missing file but does not rewrite an existing one.

  $ mentat config init >/dev/null
  $ cat "$XDG_CONFIG_HOME/mentat/config.json"
  {}
  $ printf '{"unknown":true}\n' > "$XDG_CONFIG_HOME/mentat/config.json"
  $ mentat config init >/dev/null
  $ cat "$XDG_CONFIG_HOME/mentat/config.json"
  {"unknown":true}

Command-line scalar values are stored as shell strings, not parsed as JSON, and
preserve the unknown field already in the file.

  $ mentat config set model openai/gpt-5.4-mini >/dev/null
  $ mentat config get model
  openai/gpt-5.4-mini
  $ cat "$XDG_CONFIG_HOME/mentat/config.json"
  {"unknown":true,"model":"openai/gpt-5.4-mini"}
