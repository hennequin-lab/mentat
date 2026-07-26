Config validate checks a config file's syntax and supported field types, with
an enveloped --json report and a strict mode that also rejects unknown fields.
CFG-6: a corrupt effective config reader fails closed with a clean diagnostic.

  $ use_trusted_workspace

With no file to check, and for a well-formed file, validation passes.

  $ mentat config validate
  ok
  $ cat > valid.json <<EOF
  > {"model":"openai/gpt-5.6-sol","run":{"max_steps":3}}
  > EOF
  $ mentat config validate valid.json
  ok

A missing explicit path has nothing to validate and is reported ok (config_io
treats an absent file as the empty config).

  $ mentat config validate does-not-exist.json
  ok

Unknown fields are tolerated by default so editing can preserve them, and named
by strict validation. Errors are prefixed with the file and carry no mentat:
banner.

  $ cat > unknown.json <<EOF
  > {"model":"openai/gpt-5.6-sol","extra":true}
  > EOF
  $ mentat config validate unknown.json
  ok
  $ mentat config validate --strict unknown.json 2>&1
  unknown.json unknown field: extra
  [1]

  $ cat > unknown-nested.json <<EOF
  > {"run":{"max_steps":3,"extra":true}}
  > EOF
  $ mentat config validate --strict unknown-nested.json 2>&1
  unknown-nested.json run unknown field: extra
  [1]

  $ cat > unknown-provider.json <<EOF
  > {"providers":{"openai":{"base_url":"https://api.example","extra":true}}}
  > EOF
  $ mentat config validate --strict unknown-provider.json 2>&1
  unknown-provider.json providers.openai unknown field: extra
  [1]

A member of a known section that names no supported field is an unknown member
too, whatever an earlier release meant by it — [permission.mode] is one.

  $ cat > unknown-permission.json <<EOF
  > {"permission":{"mode":"auto"}}
  > EOF
  $ mentat config validate unknown-permission.json
  ok
  $ mentat config validate --strict unknown-permission.json 2>&1
  unknown-permission.json permission unknown field: mode
  [1]

The --json report is a single enveloped object with a valid flag and an errors
list.

  $ mentat config validate --json valid.json
  {"schema_version":1,"type":"config.validate","valid":true,"errors":[]}
  $ mentat config validate --json --strict unknown.json
  {"schema_version":1,"type":"config.validate","valid":false,"errors":["unknown.json unknown field: extra"]}
  [1]

Malformed JSON fails (the parse error carries the mentat: banner).

  $ printf 'not json\n' > malformed.json
  $ mentat config validate malformed.json 2>&1
  mentat: Expected u while parsing null but found: o
  File "-", line 1, characters 0-2:
  [1]

The JSON root must be an object.

  $ printf '[]\n' > array.json
  $ mentat config validate array.json 2>&1
  array.json config must be a JSON object
  [1]

Supported fields are type checked.

  $ printf '{"model":1}\n' > bad-model.json
  $ mentat config validate bad-model.json 2>&1
  bad-model.json model must be a string
  [1]

  $ printf '{"run":1}\n' > bad-run.json
  $ mentat config validate bad-run.json 2>&1
  bad-run.json run must be an object
  [1]

  $ printf '{"run":{"max_steps":"x"}}\n' > bad-steps.json
  $ mentat config validate bad-steps.json 2>&1
  bad-steps.json run.max_steps must be an integer
  [1]

  $ printf '{"run":{"max_steps":9007199254740992}}\n' > bad-large-steps.json
  $ mentat config validate bad-large-steps.json 2>&1
  bad-large-steps.json run.max_steps must be at most 9007199254740991
  [1]

  $ printf '{"reasoning":"hyper"}\n' > bad-reasoning.json
  $ mentat config validate bad-reasoning.json 2>&1
  bad-reasoning.json reasoning: unknown reasoning effort: hyper
  [1]

  $ printf '{"providers":{"openai":{"base_url":""}}}\n' > bad-provider-url.json
  $ mentat config validate bad-provider-url.json 2>&1
  bad-provider-url.json providers.openai.base_url must not be empty
  [1]

  $ printf '{"instructions":{"project":"no"}}\n' > bad-instructions-bool.json
  $ mentat config validate bad-instructions-bool.json 2>&1
  bad-instructions-bool.json instructions.project must be a boolean
  [1]

  $ printf '{"instructions":{"project_max_bytes":0}}\n' > bad-instructions-budget.json
  $ mentat config validate bad-instructions-budget.json 2>&1
  bad-instructions-budget.json instructions.project_max_bytes must be positive
  [1]

Validation reports every error it finds in one file, in a stable order.

  $ cat > many-errors.json <<EOF
  > {"model":1,"reasoning":"hyper","run":{"max_steps":0,"extra":true},"permission":{"mode":"fast"},"extra":true}
  > EOF
  $ mentat config validate --strict many-errors.json 2>&1
  many-errors.json model must be a string
  many-errors.json reasoning: unknown reasoning effort: hyper
  many-errors.json run.max_steps must be positive
  many-errors.json unknown field: extra
  many-errors.json run unknown field: extra
  many-errors.json permission unknown field: mode
  [1]

CFG-6 — a corrupt effective config reader fails closed. Plant a malformed user
file; a bare `config validate` resolves the effective config first and reports
the parse error against the user path, exit 1.

  $ printf 'not json\n' > "$XDG_CONFIG_HOME/mentat/config.json"
  $ mentat config validate >/dev/null 2>err.txt; echo "exit=$?"
  exit=1
  $ censor <err.txt
  mentat: $TESTCASE_ROOT/config/mentat/config.json: Expected u while parsing null but found: o
  File "-", line 1, characters 0-2:
