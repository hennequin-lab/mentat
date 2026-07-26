STORE-1 (#14) — store access failure fails closed with exit 1 across commands.
setup.sh no longer opens the store at source time, so use_broken_store plants a fault
BEFORE any store-open: the lazy store-open is the first thing to touch it. FINDING:
the store-open error still carries a raw `Eio.Io` type in its rendered message (a
message-quality polish item, cross-ref RUN-02's "clean message, not raw Eio.Io"); the
exit is nonetheless a clean 1 under a `mentat:` prefix (no uncaught crash), which is
STORE-1's fail-closed contract. Pinned here as CURRENT behavior — when the store-open
error is given a clean message, this golden breaks and should be re-promoted.

An unwritable data/mentat dir makes the lazy store-open fail with EACCES; both a
session-creating and a listing command fail closed (exit 1).

  $ use_broken_store unwritable
  $ mentat session create --id s1 --cwd "$PWD" 2>&1 | censor
  mentat: $TESTCASE_ROOT/data/mentat: open failed: Eio.Io Fs Permission_denied Unix_error (Permission denied, "openat", "$TESTCASE_ROOT/data/mentat"),
    opening directory <fs:$TESTCASE_ROOT/data/mentat> (report saved: $TESTCASE_ROOT/state/mentat/crashes/$DIGEST-$PID.log)
  [1]
  $ mentat session list --all --cwd "$PWD" >/dev/null 2>&1; echo $?
  1

A file where the sessions/ directory belongs (ENOTDIR) is the corrupt-structure
variant. Reset the unwritable fault first so the marker file can be planted.

  $ chmod 755 "$XDG_DATA_HOME/mentat" 2>/dev/null; rm -rf "$XDG_DATA_HOME/mentat"
  $ use_broken_store nondir
  $ mentat session list --all --cwd "$PWD" 2>&1 | censor
  mentat: $TESTCASE_ROOT/data/mentat/sessions: not a directory (report saved: $TESTCASE_ROOT/state/mentat/crashes/$DIGEST-$PID.log)
  [1]
