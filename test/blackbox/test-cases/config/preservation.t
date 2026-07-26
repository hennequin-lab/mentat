Config edits preserve unknown JSON data through a canonical rewrite, so
hand-authored fields survive at every depth: unknown members at the top level,
unknown members inside provider objects, and unknown members nested inside a
declared section object (here skills) whose schema the current build does not
know.

  $ use_trusted_workspace

Seed a user file with known and unknown fields.

  $ mkdir -p "$XDG_CONFIG_HOME/mentat"
  $ cat > "$XDG_CONFIG_HOME/mentat/config.json" <<EOF
  > {"unknown":true,"model":"openai/old","providers":{"openai":{"base_url":"https://old.example/v1","organization":"org_123"}}}
  > EOF

Setting a known top-level field preserves unknown top-level and provider fields.

  $ mentat config set model openai/gpt-5.6-sol >/dev/null
  $ cat "$XDG_CONFIG_HOME/mentat/config.json"
  {"unknown":true,"model":"openai/gpt-5.6-sol","providers":{"openai":{"base_url":"https://old.example/v1","organization":"org_123"}}}

Setting a provider field preserves unknown provider fields.

  $ mentat config set providers.openai.base_url https://new.example/v1 >/dev/null
  $ cat "$XDG_CONFIG_HOME/mentat/config.json"
  {"unknown":true,"model":"openai/gpt-5.6-sol","providers":{"openai":{"base_url":"https://new.example/v1","organization":"org_123"}}}

Unsetting known fields preserves unknown fields.

  $ mentat config unset model
  $ cat "$XDG_CONFIG_HOME/mentat/config.json"
  {"unknown":true,"providers":{"openai":{"base_url":"https://new.example/v1","organization":"org_123"}}}

  $ mentat config unset providers.openai.base_url
  $ cat "$XDG_CONFIG_HOME/mentat/config.json"
  {"unknown":true,"providers":{"openai":{"organization":"org_123"}}}

Re-seed with a known section holding both a known and an unknown member, to
cover preservation one level deeper: inside a declared section object.

  $ cat > "$XDG_CONFIG_HOME/mentat/config.json" <<EOF
  > {"skills":{"enabled":true,"experimental_rank":3},"model":"openai/old"}
  > EOF

Setting an unrelated known field preserves the unknown nested member.

  $ mentat config set model openai/gpt-5.6-sol >/dev/null
  $ cat "$XDG_CONFIG_HOME/mentat/config.json"
  {"model":"openai/gpt-5.6-sol","skills":{"enabled":true,"experimental_rank":3}}

Setting the known member inside the section preserves its unknown sibling.

  $ mentat config set skills.enabled false >/dev/null
  $ cat "$XDG_CONFIG_HOME/mentat/config.json"
  {"model":"openai/gpt-5.6-sol","skills":{"enabled":false,"experimental_rank":3}}

Unsetting the known member keeps the section alive for its unknown sibling.

  $ mentat config unset skills.enabled
  $ cat "$XDG_CONFIG_HOME/mentat/config.json"
  {"model":"openai/gpt-5.6-sol","skills":{"experimental_rank":3}}
