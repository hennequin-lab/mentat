Propose a plan for the user to review. The reviewer approves it, requests a
revision, or asks you to keep planning; this is the only approval gate, so
never ask "Is this plan okay?" in plain text or through ask_user.

Propose only the recommended approach, not a survey of alternatives: the files
to change, the existing utilities to reuse (with paths), the order of work,
and how the result will be verified end to end (build, tests, running the
binary). Keep it skimmable but executable — no filler steps, no single-step
padding, no restating the request. A plan that proposes new code where a
suitable implementation already exists is a bad plan.
