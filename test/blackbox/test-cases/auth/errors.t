Auth commands fail before mutating state when the provider, method, or credential
material is invalid, and a malformed store fails loudly rather than reading as
empty and silently losing every stored credential.

  $ use_trusted_workspace

An unknown provider is a usage error on the login write (exit 2), and a runtime
error on the save write once the key has been read (exit 1).

  $ mentat auth login nope --method api-key --api-key-stdin 2>&1
  mentat: unknown provider: nope
  [2]
  $ printf some-key | mentat auth save nope --api-key-stdin 2>&1
  mentat: unknown provider: nope
  [1]

An unknown login method is a usage error naming the provider's actual methods.

  $ mentat auth login openai --method telepathy 2>&1
  mentat: unknown login method: telepathy; available: browser, device-code, api-key
  [2]

API-key entry requires stdin so a secret is never accepted as a command-line
argument or read from a terminal without the explicit flag.

  $ mentat auth login openai --method api-key 2>&1
  mentat: reading an API key without a terminal requires --api-key-stdin
  [2]
  $ mentat auth save openai 2>&1
  mentat: reading an API key without a terminal requires --api-key-stdin
  [2]

Empty API-key input is rejected before a credential is saved.

  $ printf '' | mentat auth login openai --method api-key --api-key-stdin 2>&1
  mentat: API key must not be empty
  [2]

Without a terminal a defaulted interactive login refuses to pick a method
silently and names the explicit alternatives.

  $ mentat auth login openai 2>&1
  mentat: auth login openai needs an explicit method without a terminal; use --method device-code or --method api-key --api-key-stdin
  [2]

Anthropic declares only the api-key method, so a bare login falls back to it and
also requires stdin.

  $ mentat auth login anthropic 2>&1
  mentat: reading an API key without a terminal requires --api-key-stdin
  [2]

A malformed auth store fails loudly, naming the file, rather than reading as
empty.

  $ mkdir -p "$XDG_CONFIG_HOME/mentat"
  $ printf '{"version":' > "$XDG_CONFIG_HOME/mentat/auth.json"
  $ mentat auth status openai --json >out.txt 2>err.txt
  [1]
  $ censor < err.txt
  mentat: $TESTCASE_ROOT/config/mentat/auth.json: not a valid credential store (corrupt or obsolete format)

An unsupported store version is the same loud failure, not a silent reset.

  $ printf '{"version":2,"credentials":{}}' > "$XDG_CONFIG_HOME/mentat/auth.json"
  $ mentat auth status openai --json >out.txt 2>err.txt
  [1]
  $ censor < err.txt
  mentat: $TESTCASE_ROOT/config/mentat/auth.json: not a valid credential store (corrupt or obsolete format)

A store path that is a directory fails loudly too.

  $ rm "$XDG_CONFIG_HOME/mentat/auth.json"
  $ mkdir "$XDG_CONFIG_HOME/mentat/auth.json"
  $ mentat auth status openai --json >out.txt 2>err.txt
  [1]
  $ censor < err.txt
  mentat: $TESTCASE_ROOT/config/mentat/auth.json: is a directory
  $ rmdir "$XDG_CONFIG_HOME/mentat/auth.json"

No failing command created a stored credential: with the faults cleared, the
provider reads missing.

  $ mentat auth status openai --json
  {"schema_version":1,"type":"auth_status","providers":[{"provider":"openai","phase":"missing","connected":false}]}
  [1]
