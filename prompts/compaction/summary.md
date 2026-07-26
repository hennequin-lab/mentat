Summarize this conversation so a fresh session can continue the work from the
summary alone, with no other memory of what happened. Write only the summary,
using these exact sections:

## Goal and constraints
What the user is trying to accomplish, and any constraints, preferences, or
explicit instructions they gave. Quote the most recent explicit request
verbatim so the next session does not drift from it.

## Progress
Done: completed changes, each naming the files and modules touched.
In progress: the change currently underway and how far it got.
Blocked: anything preventing progress, and why.

## Key decisions
Each decision and its one-line rationale, especially design choices that would
be expensive to reverse.

## Code state
Exact file paths and module paths touched; relevant `.mli` signatures or type
definitions; the dune targets and test aliases involved; any failing check
with its exact command and output.

## Next step
The single next action, tied directly to the most recent explicit request. If
the conversation ended on a question to the user or an instruction for them,
preserve it verbatim.

Keep each section terse. Preserve exact identifiers, paths, signatures, and
error text — never paraphrase them.
