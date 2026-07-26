STORE-1 (#14) — store access failure fails closed with exit 1 across commands.
setup.sh no longer opens the store at source time, so use_broken_store plants a fault
BEFORE any store-open: the lazy store-open is the first thing to touch it. FINDING:
the store-open error still carries a raw `Eio.Io` type in its rendered message (a
message-quality polish item, cross-ref RUN-02's "clean message, not raw Eio.Io").
That raw type also spells the failing syscall differently per backend (`openat2`
under eio_linux, `openat` under eio_posix), so the operating-system half of the
sentence is folded here and only the fail-closed contract is pinned: the `mentat:`
prefix, the directory it could not open, a saved crash report, and exit 1.

A data/mentat that cannot be opened makes the lazy store-open fail; both a
session-creating and a listing command fail closed (exit 1).

  $ use_broken_store unopenable
  $ mentat session create --id s1 --cwd "$PWD" >create.out 2>&1; echo exit:$?
  exit:1
  $ sed 's/open failed:.*/open failed: <os error>/' create.out | sed -n 1p | censor
  mentat: $TESTCASE_ROOT/data/mentat: open failed: <os error>
  $ grep -c 'report saved:' create.out
  1
  $ mentat session list --all --cwd "$PWD" >/dev/null 2>&1; echo $?
  1

A file where the sessions/ directory belongs (ENOTDIR) is the corrupt-structure
variant. Clear the unopenable fault first so the marker file can be planted.

  $ rm -f "$XDG_DATA_HOME/mentat"
  $ use_broken_store nondir
  $ mentat session list --all --cwd "$PWD" 2>&1 | censor
  mentat: $TESTCASE_ROOT/data/mentat/sessions: not a directory (report saved: $TESTCASE_ROOT/state/mentat/crashes/$DIGEST-$PID.log)
  [1]
