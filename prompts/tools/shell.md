Run one non-interactive shell command in a workspace directory.

Prefer the dedicated tools over shell equivalents: read_file over cat and
ls, search_text over grep or rg, glob over find, and the edit tools over sed
or heredocs. Reach for shell when the task is a
real command: builds, tests, git, package managers, running the project's
binaries.

Usage:
- Each call is independent: workdir defaults to the workspace root, and
  shell state does not persist between calls. Chain dependent steps with
  && in one call; put independent commands in parallel calls.
- For a long-lived command — a dev server, a file watcher, a log tail — set
  background=true. The call returns a handle immediately instead of waiting;
  read new output with shell_output and stop it with shell_kill. A background
  command runs confined (the sandbox cannot be widened for it) and its
  timeout is the session, not timeout_ms.
- Quote paths that contain spaces. Keep commands non-interactive; anything
  that prompts for input will hang until the timeout.
- Do not sleep, poll, or retry a failing command unchanged — diagnose the
  failure first.
- The host selects the shell, sandbox, environment, and timeout and output
  bounds. If a command fails because of a sandbox restriction, read the
  message for the path it names and retry that one command with
  grant_write=["/that/path"]: the sandbox stays in force everywhere else and
  the widening lasts only for that command. Reach for escalate=true only when
  the access is genuinely broader than a set of paths, and say why in
  description. Both need explicit user approval, both are unavailable in
  read-only runs, and they cannot be combined.
