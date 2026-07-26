Skills config keys: enablement booleans, the extra-roots and disabled-name
lists, and the catalog byte budget. Invalid spellings reject without mutating
the stored value.

  $ use_trusted_workspace

Defaults are resolved built-ins.

  $ mentat config get skills.enabled
  true
  $ mentat config get skills.builtin
  true
  $ mentat config get skills.project
  true
  $ mentat config get skills.compat
  true
  $ mentat config get skills.paths
  []
  $ mentat config get skills.catalog_max_bytes
  8192

Boolean keys accept exactly true or false; a bad value is a runtime error and
leaves the stored value unchanged.

  $ mentat config set skills.enabled false >/dev/null
  $ mentat config get skills.enabled
  false
  $ mentat config set skills.compat yes 2>&1
  mentat: skills.compat must be true or false
  [1]
  $ mentat config get skills.compat
  true

skills.paths is a JSON array of non-empty strings, and its enveloped read
carries the array as a string value.

  $ mentat config set skills.paths '["/opt/skills", "/srv/pack"]' >/dev/null
  $ mentat config get skills.paths
  ["/opt/skills","/srv/pack"]
  $ mentat config get --json skills.paths
  {"schema_version":1,"type":"config.value","key":"skills.paths","value":"[\"/opt/skills\",\"/srv/pack\"]"}

Invalid list spellings reject without mutating the stored value.

  $ mentat config set skills.paths '/opt/skills' 2>&1
  mentat: skills.paths must be a JSON array of strings, for example ["/a", "/b"]
  [1]
  $ mentat config set skills.paths '["/opt/skills", 3]' 2>&1
  mentat: skills.paths must be a string
  [1]
  $ mentat config set skills.paths '["", "/srv/pack"]' 2>&1
  mentat: skills.paths must not be empty
  [1]
  $ mentat config get skills.paths
  ["/opt/skills","/srv/pack"]

skills.disabled names individual skills to exclude; unknown names are preserved.

  $ mentat config get skills.disabled
  []
  $ mentat config set skills.disabled '["ocaml-benchmarking", "never-seen"]' >/dev/null
  $ mentat config get skills.disabled
  ["ocaml-benchmarking","never-seen"]
  $ mentat config get --json skills.disabled
  {"schema_version":1,"type":"config.value","key":"skills.disabled","value":"[\"ocaml-benchmarking\",\"never-seen\"]"}
  $ mentat config set skills.disabled 'ocaml-benchmarking' 2>&1
  mentat: skills.disabled must be a JSON array of strings, for example ["/a", "/b"]
  [1]
  $ mentat config get skills.disabled
  ["ocaml-benchmarking","never-seen"]
  $ mentat config unset skills.disabled

The catalog budget is a positive integer; zero is not a disable spelling.

  $ mentat config set skills.catalog_max_bytes 4096 >/dev/null
  $ mentat config get skills.catalog_max_bytes
  4096
  $ mentat config set skills.catalog_max_bytes 0 2>&1
  mentat: skills.catalog_max_bytes must be positive
  [1]
  $ mentat config set skills.catalog_max_bytes nope 2>&1
  mentat: skills.catalog_max_bytes must be an integer
  [1]
  $ mentat config get skills.catalog_max_bytes
  4096

Unset returns keys to their built-in defaults, visible in origins.

  $ mentat config unset skills.enabled
  $ mentat config unset skills.paths
  $ mentat config unset skills.catalog_max_bytes
  $ mentat config show --origins | grep '^skills\.'
  skills.builtin=true (default)
  skills.catalog_max_bytes=8192 (default)
  skills.compat=true (default)
  skills.disabled=[] (default)
  skills.enabled=true (default)
  skills.paths=[] (default)
  skills.project=true (default)
