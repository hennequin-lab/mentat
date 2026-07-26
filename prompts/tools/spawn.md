Delegate bounded work to a child session with a fresh context. The call
returns immediately with the child's session id and the child runs detached,
so you keep working while it runs; call wait with that id when your next step
needs its result.

The child has not seen this conversation. Brief it like a colleague who just
walked in: the goal and why it matters, the exact paths involved, what you
have already ruled out, how thorough to be (a quick look at one area versus an
exhaustive sweep), and precisely what its final message must contain. Do not
delegate synthesis you have not done — "figure out what matters and fix it"
produces shallow work; delegate a question or a change you can state
precisely, with file paths and line numbers.

Pass a short `description` — three to five words naming the child's mission
(`audit the config loader`) — alongside the full `task`. It labels the child
wherever it is shown at a glance; the `task` stays the complete briefing.

Set `role` to pick the child's capability. `general` — the default when
omitted — is a full-capability delegate: it reads, searches, runs commands,
and edits files with your full toolset, under the same permission rules; a
high-impact action it takes prompts the user exactly as your own would. The
specialists are read-only: `explore` for a fast search, `review` to report
findings by severity, `verify` to adversarially break a change. Any task that
must change the workspace needs a `general` child — a specialist cannot
write, so do not assign one a writing task.

Delegate writes with a precise brief — name the files to change and what the
change is, not "figure out what matters and fix it". Children share your one
workspace with no isolation, so parallel children must own disjoint files: two
children editing the same file will clobber each other. When you only need the
result back, have the child return the exact content or diff and apply it
yourself.

Do not spawn for a needle lookup: a known file → read_file; a specific symbol
or string → search_text; a couple of known files → read them directly.

Independent delegations go out in one response, in parallel; collect them with
one wait naming every child id. Do not redo delegated work yourself while it
runs. The child's output is not shown to the user — relay what matters in your
own message. Do not end your turn while a spawned child you still need has not
been collected with wait.
