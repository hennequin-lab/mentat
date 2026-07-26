The environment-only report stages no store, so a user whose install is too broken
to open one can still file a report. That is the user who most needs to: with no
session to name and no log to correlate, the host facts are the whole evidence.
The fault is planted before any store-open, so the report is the first thing to
touch it.

  $ use_broken_store nondir

An ordinary session command fails closed on the same store.

  $ mentat session list --cwd "$PWD" > /dev/null 2>&1; echo $?
  1

The report does not, because it never opens one.

  $ mentat report --no-session --yes -o broken.ndjson --cwd "$PWD" 2>/dev/null
  $ grep -c '"record":"report"' broken.ndjson
  1
  $ grep -c '"record":"manifest"' broken.ndjson
  1

A session target against that store still fails closed, rather than pretending a
bundle is complete.

  $ mentat report --last --yes -o unused.ndjson --cwd "$PWD" > /dev/null 2>&1; echo $?
  1
  $ test -f unused.ndjson && echo written || echo "not written"
  not written
