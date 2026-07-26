Read new output from a background shell command.

Given a handle from a shell call started with background=true, this waits for
the command to produce output and returns everything it produced since your
last read of that handle, plus whether the command is still running or has
exited.

Usage:
- Pass the handle from the background shell call (for example bg_1).
- The read waits: it returns the moment new output arrives or the command
  exits, and otherwise once wait_ms has passed. wait_ms defaults to 5000 ms;
  the accepted range is 5000-300000 ms. Raise it when you are waiting on a step
  you know takes longer.
- Each read returns only the new output since your previous read; the cursor
  is tracked per handle, so you do not pass it.
- Optionally pass filter, a regular expression, to keep only matching output
  lines — useful for noisy dev-server or watcher logs.
- Read when you need what the command has printed: to check that a server came
  up, or before a step that depends on its output. A read that returns nothing
  waited the full budget and got nothing; reading again does not make the
  command progress, so do the next useful thing instead.
- A read is bounded: if a lot of output accumulated, you get the most recent
  bytes and a note that older bytes rolled off.
- An unknown handle — one that never started, or one from before a restart —
  returns not-found. Background processes do not survive a restart.
