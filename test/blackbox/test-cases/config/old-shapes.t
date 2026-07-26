Config does not migrate, alias, or normalize legacy reference-agent field
shapes. Unknown fields are preserved by non-strict edits and rejected loudly by
strict validation. `small_model`, by contrast, is a supported field — a
`provider/model` selector — so it loads and validates like `model`.

  $ use_trusted_workspace

Seed a user file mixing legacy fields with two supported fields.

  $ mkdir -p "$XDG_CONFIG_HOME/mentat"
  $ cat > "$XDG_CONFIG_HOME/mentat/config.json" <<EOF
  > {"model":"openai/gpt-5.6-sol","approval_policy":"never","model_reasoning_effort":"high","small_fast_model":"x","small_model":"openai/gpt-5.4-mini"}
  > EOF

Effective loading works and silently ignores the unrelated reference-agent
fields; no reasoning effort is derived from them.

  $ mentat config get model
  openai/gpt-5.6-sol

`small_model` is a real supported field and reads back its configured selector.

  $ mentat config get small_model
  openai/gpt-5.4-mini

Legacy names are not addressable keys and gain no compatibility alias — the
unknown-key gate fires (its full hint block is pinned once in errors.t).

  $ mentat config get approval_policy 2>err.txt; echo "exit=$?"
  exit=2
  $ head -n 1 err.txt
  mentat: unknown config key: approval_policy

Settings mentat does not define — subagent-wake, the anchored-edit tool, and the
web search backend — carry no field, alias, or decoder. They are ordinary unknown
fields: default validation tolerates them so an edit can preserve them, and
strict validation names each one.

  $ printf '%s\n' '{"run":{"subagent_wake":false}}' > removed-wake.json
  $ mentat config validate removed-wake.json
  ok
  $ mentat config validate --strict removed-wake.json 2>&1
  removed-wake.json run unknown field: subagent_wake
  [1]

  $ printf '%s\n' '{"tools":{"anchored_edits":true}}' > removed-anchored-edits.json
  $ mentat config validate removed-anchored-edits.json
  ok
  $ mentat config validate --strict removed-anchored-edits.json 2>&1
  removed-anchored-edits.json tools unknown field: anchored_edits
  [1]

  $ printf '%s\n' '{"web":{"search_backend":"brave"}}' > removed-web-search.json
  $ mentat config validate removed-web-search.json
  ok
  $ mentat config validate --strict removed-web-search.json 2>&1
  removed-web-search.json web unknown field: search_backend
  [1]

`small_model`, being supported, validates cleanly under both modes — strict
validation does not flag it as unknown.

  $ printf '%s\n' '{"small_model":"openai/gpt-5.4-mini"}' > small-model.json
  $ mentat config validate --strict small-model.json
  ok

A typed edit rewrites the file canonically, re-emitting every supported field
and carrying every unknown field through with its value intact.

  $ mentat config set model anthropic/claude-sonnet-5 >/dev/null
  $ cat "$XDG_CONFIG_HOME/mentat/config.json"
  {"small_fast_model":"x","model_reasoning_effort":"high","approval_policy":"never","model":"anthropic/claude-sonnet-5","small_model":"openai/gpt-5.4-mini"}

Default validation tolerates the unknown fields; strict validation rejects the
file naming each remaining unknown one — but not `small_model`.

  $ mentat config validate "$XDG_CONFIG_HOME/mentat/config.json"
  ok

  $ mentat config validate --strict "$XDG_CONFIG_HOME/mentat/config.json" 2>err.txt; echo "exit=$?"
  exit=1
  $ censor <err.txt
  $TESTCASE_ROOT/config/mentat/config.json unknown field: small_fast_model
  $TESTCASE_ROOT/config/mentat/config.json unknown field: model_reasoning_effort
  $TESTCASE_ROOT/config/mentat/config.json unknown field: approval_policy
