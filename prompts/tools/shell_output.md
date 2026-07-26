Read new output from a background shell command.

Given a handle from a shell call started with run_in_background, this returns
the output produced since your last read of that handle, plus whether the
command is still running or has exited. Output is polled, not streamed: call
this again to get more.

Usage:
- Pass the handle from the background shell call (for example bg_1).
- Each read returns only the new output since your previous read; the cursor
  is tracked per handle, so you do not pass it.
- Optionally pass filter, a regular expression, to keep only matching output
  lines — useful for noisy dev-server or watcher logs.
- A read is bounded: if a lot of output accumulated, you get the most recent
  bytes and a note that older bytes rolled off. Poll more often for a long
  chatty command.
- An unknown handle — one that never started, or one from before a restart —
  returns not-found. Background processes do not survive a restart.
