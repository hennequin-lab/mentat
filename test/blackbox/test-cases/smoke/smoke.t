The binary boots and reports its version.

  $ use_trusted_workspace
  $ mentat --version
  dev

The root help lists only commands with real responders. The interactive TUI is
the default term, so help is requested explicitly here.

  $ mentat --help=plain 2>&1 | head -n 2
  NAME
         mentat - The OCaml coding agent.

Every command group has a real responder; the root command tree carries no
placeholder entries. Review is registered and fails honestly outside a git
worktree rather than as an unknown command.

  $ mentat --help=plain | grep -cE '^       (review|skills|permission|debug|completion)( |$)'
  5

Manual compaction is a wired, client-routed command taking a [SESSION] target:
it resolves the target first, so a nonexistent id reports not-found (exit 1).

  $ mentat session --help=plain | grep '^       compact '
         compact [OPTION]… [SESSION]
  $ mentat session compact old-shape --json --cwd . 2>&1
  mentat: session not found: old-shape
  [1]

A malformed command line — here a required argument missing — is a cmdliner
parse failure (exit 124), the baseline convention for argument-layer errors.

  $ mentat config get
  Usage: mentat config get [--help] [OPTION]… KEY
  mentat: required argument KEY is missing
  [124]

A structured usage error inside a command (missing target) exits 2.

  $ mentat session show --cwd .
  mentat: requires SESSION or --last; run `mentat session list`
  [2]

`purge` reclaims only tombstoned sessions: it is a no-op when nothing is
deleted, and it never disturbs an active survivor. The refusal-without-yes, the
removed count, and the physical-removal facts live in session/management.t.

  $ mentat session create --id purge-victim --cwd . > /dev/null
  $ mentat session create --id purge-keep --cwd . > /dev/null
  $ mentat session purge --cwd .
  no deleted sessions to purge
  $ mentat session delete --yes purge-victim --cwd . > /dev/null
  $ mentat session purge --yes --cwd . > /dev/null
  $ mentat session show purge-keep --cwd . | grep -E '^lifecycle='
  lifecycle=active

Headless commands are opt-in: without MENTAT_LOG nothing is logged, stderr stays
clean, and no per-run file appears under the state home.

  $ mentat config path --cwd . >path.out 2>err.out
  $ cat err.out
  $ test -d state/mentat/logs && echo has-logs || echo no-logs
  no-logs

MENTAT_LOG turns diagnostics on and MENTAT_LOG_FILE captures the trail. The log
goes to the file; stdout carries only the command result, never a log line.

  $ MENTAT_LOG=debug MENTAT_LOG_FILE="$PWD/diag.log" mentat config path --cwd . >path.out 2>err.out
  $ cat path.out | censor
  $TESTCASE_ROOT/config/mentat/config.json
  $ cat err.out
  $ grep -c 'run=' path.out err.out || true
  path.out:0
  err.out:0
  $ grep -c 'INFO \[mentat.next\] mentat started version=dev command=config' diag.log
  1

A record is one physical line, prefix and message together, so that every reader
of these files can be line-oriented. Nothing in the log begins anywhere but at a
record's timestamp.

  $ awk '!/^[0-9][0-9][0-9][0-9]-/ { print "continuation: " $0 }' diag.log

An invalid MENTAT_LOG value is a runtime error (exit 1): env-sourced input
never classifies as usage — only flag-carried input exits 2. It flows through
the one error ladder, never a parse crash or a backtrace.

  $ MENTAT_LOG=bogus mentat config path --cwd .
  mentat: MENTAT_LOG: "bogus": unknown log level
  [1]

MENTAT_LOG_FILE must be absolute; a relative path is the same runtime error.

  $ MENTAT_LOG=debug MENTAT_LOG_FILE=diag.log mentat config path --cwd .
  mentat: MENTAT_LOG_FILE must be an absolute path: diag.log
  [1]
