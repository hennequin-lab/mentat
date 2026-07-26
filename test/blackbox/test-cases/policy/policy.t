The four composition-root policies, asserted through the binary:
input validation (P1), output discipline (P2), the error ladder (P3), and the
top-level guard + observable staging (P4). These close the QA crash / injection /
leak / staging classes at the waist, not per instance.

P1 — an empty id is a usage error (exit 2) with a clean message, never the old
uncaught Invalid_argument backtrace (exit 125, F8).

  $ use_trusted_workspace
  $ mentat session create --id "" 2>&1
  mentat: session id must not be empty
  [2]

  $ mentat session rename anything "" 2>&1
  mentat: title must not be empty
  [2]

P1 — a permissive/leaky id shape is rejected uniformly (F9), not stored.

  $ mentat session create --id "a/b" 2>&1
  mentat: session id must contain only letters, digits, '.', '_', or '-'
  [2]

P1 — a negative row limit is a usage error, not silently accepted (F11).

  $ mentat session list --limit=-1 2>&1
  mentat: limit must not be negative
  [2]

P1 — an unknown config key is a usage error (exit 2), not runtime (exit 1, CFG-7).

  $ mentat config get not.a.key >/dev/null 2>err.txt
  [2]
  $ head -n 1 err.txt
  mentat: unknown config key: not.a.key

P1 — the same "unknown provider" input yields exit 2 everywhere (XCUT-1): the auth
status view, the config set, and the models show that used to disagree.

  $ mentat auth status bogusprovider >/dev/null 2>&1; echo $?
  2
  $ mentat config set model bogus/nope >/dev/null 2>&1; echo $?
  2
  $ mentat models show bogus/nope >/dev/null 2>&1; echo $?
  2

P2 — cmdliner usage/error output carries no ANSI escape to a non-TTY (RUN-01,
CFG-5): the styler is stripped on the way out regardless of TERM.

  $ TERM=xterm-256color mentat config get 2>err.txt; echo "exit=$?"
  exit=124
  $ mentat_cram count-ctl --bytes 27 < err.txt
  0

P2 — a Jsont decode error carries no ANSI either (CFG-5).

  $ printf 'not json {' > bad.json
  $ mentat config validate bad.json --json 2>/dev/null > vj.txt || true
  $ mentat_cram count-ctl --bytes 27 < vj.txt
  0

P3 — absent is not malformed: validating a missing config file is ok, exit 0, not
an "end of text" parse error (CFG-6).

  $ mentat config validate
  ok

P4 — doctor never dies on the conditions it exists to diagnose: a broken config
is a reported [FAIL] check, exit 1, with the storage/trust checks still run and no
backtrace (DOCTOR-1).

  $ printf '{"model":"x' > "$XDG_CONFIG_HOME/mentat/config.json"
  $ mentat doctor > doctor.txt 2>&1; echo "exit=$?"
  exit=1
  $ grep -oE '^\[(PASS|WARN|FAIL)\] (config|storage|trust)' doctor.txt
  [FAIL] config
  [PASS] storage
  [PASS] trust
  $ grep -c 'Raised\|Called from' doctor.txt || true
  0

P4 — doctor --json still emits its envelope on a failed stage (DOCTOR-1).

  $ mentat doctor --json 2>/dev/null > dj.txt || true
  $ grep -o '"type":"doctor"' dj.txt
  "type":"doctor"
  $ rm -f "$XDG_CONFIG_HOME/mentat/config.json"
