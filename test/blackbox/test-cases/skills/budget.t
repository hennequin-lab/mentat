The skill catalog fits active-skill descriptions under `skills.catalog_max_bytes`.
A tight budget drops non-built-in descriptions to names only. `list --json`
surfaces those fit facts through the catalog projection.

  $ use_trusted_workspace
  $ mentat config set skills.builtin false >/dev/null
  $ mkdir -p .mentat/skills/alpha
  $ cat > .mentat/skills/alpha/SKILL.md <<'MD'
  > ---
  > description: A reasonably long description of the alpha skill that occupies a good number of bytes in the rendered catalog listing so the budget can bite.
  > ---
  > Alpha body.
  > MD

A generous budget lists the skill with nothing trimmed and full descriptions.

  $ mentat skills list --json > big.json
  $ mentat_cram json .catalog.names_only big.json
  false
  $ mentat_cram json .catalog.trimmed big.json
  []

A budget too small to hold even the entry prefix forces the description to drop,
leaving names only.

  $ mentat config set skills.catalog_max_bytes 8 >/dev/null
  $ mentat skills list --json > tiny.json
  $ mentat_cram json .catalog.names_only tiny.json
  true
