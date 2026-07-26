Config edits the user layer through typed keys, and `get --json` returns the
enveloped value codec (schema_version/type/key/value) rather than a bare JSON
scalar.

  $ use_trusted_workspace

Initialize the user file.

  $ mentat config init >/dev/null
  $ cat "$XDG_CONFIG_HOME/mentat/config.json"
  {}

Scalar set/get round-trips.

  $ mentat config set model openai/gpt-5.6-sol >/dev/null
  $ mentat config get model
  openai/gpt-5.6-sol

  $ mentat config set reasoning high >/dev/null
  $ mentat config get reasoning
  high
  $ mentat config get --json reasoning
  {"schema_version":1,"type":"config.value","key":"reasoning","value":"high"}

  $ mentat config set shell /bin/bash >/dev/null
  $ mentat config get shell
  /bin/bash

  $ mentat config set run.max_steps 12 >/dev/null
  $ mentat config get run.max_steps
  12
  $ mentat config get --json run.max_steps
  {"schema_version":1,"type":"config.value","key":"run.max_steps","value":"12"}

Boolean instruction keys round-trip and the enveloped value is the string
rendering of the boolean.

  $ mentat config set instructions.project false >/dev/null
  $ mentat config get instructions.project
  false
  $ mentat config get --json instructions.project
  {"schema_version":1,"type":"config.value","key":"instructions.project","value":"false"}

An effective read resolves a built-in default; a layer read does not, and its
enveloped form carries an error instead of a value.

  $ mentat config get instructions.global
  true
  $ mentat config get --user instructions.global 2>&1
  mentat: instructions.global is not set
  [1]
  $ mentat config get --user --json instructions.global
  {"schema_version":1,"type":"config.value","error":"instructions.global is not set"}
  [1]

Provider keys are namespaced by provider id.

  $ mentat config set providers.openai.base_url https://api.openai.example/v1 >/dev/null
  $ mentat config get providers.openai.base_url
  https://api.openai.example/v1

  $ mentat config set providers.openrouter.base_url https://openrouter.example/api/v1 >/dev/null
  $ mentat config get providers.openrouter.base_url
  https://openrouter.example/api/v1

Unset removes a persisted value; the effective read then fails closed.

  $ mentat config unset model
  $ mentat config get model 2>&1
  mentat: model is not set
  [1]

XCUT-2 — the enveloped read of an unset key carries the error inside the same
config.value envelope, with no key/value members.

  $ mentat config get model --json
  {"schema_version":1,"type":"config.value","error":"model is not set"}
  [1]

Unsetting nested values collapses the empty containers they leave behind.

  $ mentat config unset reasoning
  $ mentat config unset run.max_steps
  $ mentat config unset instructions.project
  $ mentat config unset providers.openrouter.base_url
  $ mentat config unset providers.openai.base_url
  $ cat "$XDG_CONFIG_HOME/mentat/config.json"
  {"shell":"/bin/bash"}
