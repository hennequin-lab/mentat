Config output never prints credential material, even when credential
environment variables are set in the invoking environment and every output
path has real values to leak through.

  $ use_trusted_workspace
  $ export OPENAI_API_KEY=sk-SECRET-SENTINEL
  $ export ANTHROPIC_API_KEY=sk-ant-SECRET-SENTINEL
  $ mentat config set model openai/gpt-5.6-sol >/dev/null
  $ mentat config set providers.openai.base_url https://api.openai.example/v1 >/dev/null

Get output prints the requested value only.

  $ mentat config get model 2>&1 | grep -c SECRET-SENTINEL
  0
  [1]

  $ mentat config get --json providers.openai.base_url 2>&1 | grep -c SECRET-SENTINEL
  0
  [1]

Configure every secret-bearing field with visible sentinels. The provider URL
contains credentials in its userinfo component; both web API key fields contain
direct credential values.

  $ mentat config set providers.openai.base_url 'https://url-user-SECRET-SENTINEL:url-pass-SECRET-SENTINEL@api.openai.example/v1' >/dev/null
  $ mentat config set web.exa_api_key exa-SECRET-SENTINEL >/dev/null
  $ mentat config set web.parallel_api_key parallel-SECRET-SENTINEL >/dev/null

Text, origins, and JSON views retain every field while replacing its value with
the stable redaction marker. The complete outputs contain no sentinel bytes.

  $ mentat config show >show.txt 2>&1
  $ grep -E '^(providers\.openai\.base_url|web\.exa_api_key|web\.parallel_api_key)=' show.txt
  providers.openai.base_url=[REDACTED]
  web.exa_api_key=[REDACTED]
  web.parallel_api_key=[REDACTED]
  $ grep -c SECRET-SENTINEL show.txt
  0
  [1]

  $ mentat config show --origins >show-origins.txt 2>&1
  $ grep -E '^(providers\.openai\.base_url|web\.exa_api_key|web\.parallel_api_key)=' show-origins.txt
  providers.openai.base_url=[REDACTED] (user)
  web.exa_api_key=[REDACTED] (user)
  web.parallel_api_key=[REDACTED] (user)
  $ grep -c SECRET-SENTINEL show-origins.txt
  0
  [1]

  $ mentat config show --json >show.json 2>&1
  $ grep -oE '"(providers\.openai\.base_url|web\.exa_api_key|web\.parallel_api_key)":"[^"]*"' show.json
  "providers.openai.base_url":"[REDACTED]"
  "web.exa_api_key":"[REDACTED]"
  "web.parallel_api_key":"[REDACTED]"
  $ grep -c SECRET-SENTINEL show.json
  0
  [1]

  $ mentat config show --json --origins >show-json-origins.json 2>&1
  $ grep -oE '"(providers\.openai\.base_url|web\.exa_api_key|web\.parallel_api_key)":\{"value":"\[REDACTED\]","source":"user"\}' show-json-origins.json
  "providers.openai.base_url":{"value":"[REDACTED]","source":"user"}
  "web.exa_api_key":{"value":"[REDACTED]","source":"user"}
  "web.parallel_api_key":{"value":"[REDACTED]","source":"user"}
  $ grep -c SECRET-SENTINEL show-json-origins.json
  0
  [1]

Credential-store failure is scoped to account-consuming operations: a corrupt
auth.json does not prevent an unrelated configuration read.

  $ printf '{"version":' > "$XDG_CONFIG_HOME/mentat/auth.json"
  $ mentat config get model
  openai/gpt-5.6-sol
  $ rm "$XDG_CONFIG_HOME/mentat/auth.json"

Config errors stay credential-free: break the user file and check the failure
diagnostics carry no sentinel.

  $ printf 'not json\n' > "$XDG_CONFIG_HOME/mentat/config.json"
  $ mentat config show >/dev/null 2>err.txt; echo "exit=$?"
  exit=1
  $ censor <err.txt
  mentat: $TESTCASE_ROOT/config/mentat/config.json: Expected u while parsing null but found: o
  File "-", line 1, characters 0-2:
  $ grep -c SECRET-SENTINEL err.txt
  0
  [1]
