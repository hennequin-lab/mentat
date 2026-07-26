Per-command escalation surfaces as a SEPARATE permission access: this proves
that dropping sandbox confinement for one command cannot ride in on an ordinary
command approval. When the model sets escalate=true in a workspace-write
sandbox, the permission request carries two items — the command itself, now with
direct (unconfined) execution identity, AND a distinct shell.escalate custom
access whose subject is the exact command text. A grant for the command alone
therefore never authorizes the escalation.

This is a policy-shape property independent of which OS backend enforces, so the
test is not platform-gated. MENTAT_SANDBOX_REQUIRE=off lets the run start and
the decision surface on any host regardless of backend availability, and
MENTAT_PERMISSION_UNATTENDED=block parks the review so the shape is inspectable;
the command never runs.

  $ mkdir -p ws/.git
  $ mentat trust ws >/dev/null
  $ cat > esc.jsonl <<'JSONL'
  > {"response":{"id":"e1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"i1","call_id":"ec1","name":"shell","arguments":"{\"command\":\"echo needs-net\",\"escalate\":true}"}]}}
  > JSONL
  $ start_fake_openai esc.jsonl

The escalated call blocks for review under the unattended block posture.

  $ MENTAT_SANDBOX_MODE=workspace-write MENTAT_SANDBOX_REQUIRE=off MENTAT_PERMISSION_UNATTENDED=block mentat run start --json --cwd "$PWD/ws" --id esc "e" >esc.out 2>/dev/null; echo exit:$?
  exit:3
  $ wait_fake_server
  $ grep '"type":"session.waiting"' esc.out > waiting.json

The parked decision is a permission review whose request groups exactly two
accesses.

  $ mentat_cram json .decision.request.type waiting.json
  permission

The first access is the command itself, and its execution identity is direct —
an escalation asks to run without Mentat confinement, and permission sees that
truthfully rather than as the enforced posture.

  $ mentat_cram json '.decision.request.review.request.items[0].access.type' waiting.json
  command
  $ mentat_cram json '.decision.request.review.request.items[0].access.execution.kind' waiting.json
  direct

The second access is the distinct shell.escalate custom access, subjected to the
exact command text. It is what a reviewer must separately approve to drop
confinement.

  $ mentat_cram json '.decision.request.review.request.items[1].access.type' waiting.json
  custom
  $ mentat_cram json '.decision.request.review.request.items[1].access.name' waiting.json
  shell.escalate
  $ mentat_cram json '.decision.request.review.request.items[1].access.subject' waiting.json
  echo needs-net
