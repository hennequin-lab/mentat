Stop a background shell command.

Given a handle from a shell call started with run_in_background, this
terminates the command (a graceful signal, then a forced kill) and returns
its final output and status.

Usage:
- Pass the handle from the background shell call (for example bg_1).
- Killing an already-exited command is fine: it returns the recorded final
  status without doing anything.
- Only the command's own process is stopped. A command that spawned its own
  workers or a process group may leave those running; stop them explicitly if
  needed.
- An unknown handle — one that never started, or one from before a restart —
  returns not-found.
