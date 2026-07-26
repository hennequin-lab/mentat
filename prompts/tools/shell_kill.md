Stop a background shell command.

Given a handle from a shell call started with background=true, this terminates
the command (a graceful signal, then a forced kill) and returns its final
output and status.

Usage:
- Pass the handle from the background shell call (for example bg_1).
- The signal reaches the command's process group, so the workers it forked stop
  with it. A worker that moved itself into another process group, or that
  ignores the graceful signal and outlives the command, keeps running; stop it
  explicitly if needed.
- Killing an already-exited command is fine: it returns the recorded final
  status without doing anything. Nothing is signalled in that case, so workers
  the command left behind keep running.
- An unknown handle — one that never started, or one from before a restart —
  returns not-found.
