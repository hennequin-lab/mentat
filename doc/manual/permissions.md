# Permission policy

Permission policy decides whether a host-described operation is allowed,
denied, or requires review. It does not confine a process or grant an
operating-system capability; that is the [command sandbox](sandbox.md).

This page covers how a decision is reached. For the JSON matcher format and
authoring workflow, see [Permission rules](permission-rules.md); for how the
three boundaries fit together, see [Security](security.md).

Tools construct trusted access facts from already decoded input before they run
an operation. The built-in facts describe workspace path reads and writes,
commands, network targets, and tool-specific custom operations. Display text
and proposed diffs are review evidence; they do not change permission identity.

Policy evaluation is pure and ordered:

1. The active workflow contract denies commands and writes when the workflow
   does not admit them.
2. Durable rules are evaluated in order.
3. Conversation rules installed by a reviewer follow.
4. Fixed product rules review high-impact commands before granting narrow
   execution credit, native workspace operations, and curated documentation.
5. For each access, the first matching rule wins.
6. If no rule matches, an exact session grant may allow the access.
7. Otherwise the access requires review.

Rules take precedence over session grants. A later deny or review rule can
therefore override an earlier session approval. In a grouped request, any
denied access denies the request; otherwise only the unmatched or explicitly
reviewed accesses are presented for review.

The shell tool parses simple commands into structured argv facts when it can
and falls back to the original shell text when parsing is ambiguous. This
improves rule matching and review display, but it is not a security parser:
confinement does not depend on the parse succeeding.

## Permission review behavior

The default behavior applies durable and conversation rules first, then fixed
product rules. Native workspace operations are allowed. Ordinary commands are
allowed only when their host-produced execution identity proves project reads
with restricted networking, or records an explicitly selected external
boundary. Read-all, network-enabled, and direct commands require review.

Before automatic command credit, the product reviews plainly visible
high-impact operations such as recursive `rm`, forced Git operations,
direct-device `dd`, `shred`, and `mkfs`. Opaque source and shell syntax are left
to confinement rather than classified as high impact merely because they are
hard to inspect. A non-match is not authority and is not a proof that a command
is harmless.

`bypass` is intentionally per-run only:

```sh
mentat run --permission bypass "PROMPT"
```

Config files reject permission behavior because it is a per-run choice. Under
`bypass`, review outcomes are allowed, including outcomes from review rules;
deny rules remain denials. Plan and review workflows independently deny command
execution and writes, so bypass cannot make those operations available there.

## Reviews and conversation grants

The interactive permission dialog offers:

- allow once;
- allow this exact access for the conversation;
- deny, optionally with model-visible feedback.

An allow-once answer authorizes only the blocked operation. An exact-conversation
answer reconstructs an exact grant from the durable permission request whenever
the conversation is replayed. It does not broaden a file to its directory, a
command to its prefix, or a host to all network traffic. Explicit review and
deny rules still take precedence over exact grants.

Family approvals are durable protocol values, but the interactive dialog does
not guess or offer them. A future family editor must show the complete matcher
before saving it; until then, configure family rules explicitly.

Headless runs use the same durable permission facts. With the default
`permission.unattended=block`, a required review saves the session, exits with
code 3, and prints commands such as:

```sh
mentat run reply SESSION --decision DECISION_ID --allow
mentat run reply SESSION --decision DECISION_ID --allow-conversation
mentat run reply SESSION --decision DECISION_ID --deny
mentat run reply SESSION --decision DECISION_ID --deny --message TEXT
```

`--decision` targets the exact pending decision (its id is printed with the
blocked session and carried in `session.waiting`), so a stale reply never
answers whatever happens to be pending.

With `permission.unattended=deny`, a required review is automatically denied,
the denial is recorded with `unattended` provenance, and the run continues so
the model can respond. Unattended resolution never allows an operation, creates
a session grant, or writes a policy rule.

## Durable rules

Durable rules are structured JSON under `permission.rules`. They may come from
the user config or the explicitly selected extra config file. The extra file's
rules have higher precedence than user rules. Project and project-local rules
are always stripped, and environment variables and run flags cannot carry
rules. The value is a version-1 object whose `items` array evaluates in order;
bare arrays and unknown versions fail loading. The complete JSON matcher
reference and authoring workflow are in
[Permission rules](permission-rules.md).

Rules are currently hand-authored; `mentat config set` does not edit the
structured rule list. For example:

```json
{
  "permission": {
    "rules": {
      "version": 1,
      "items": [
        {
          "action": "deny",
          "matcher": {
            "type": "path-exact-relative",
            "relative": ".env"
          }
        },
        {
          "action": "allow",
          "matcher": {
            "type": "command",
            "pattern": {
              "type": "argv-prefix",
              "execution": {
                "kind": "enforced",
                "read": "project",
                "write": "workspace",
                "network": "restricted"
              },
              "cwd": { "type": "workspace" },
              "program": "dune",
              "args": ["build"]
            }
          }
        }
      ]
    }
  }
}
```

Relative path matchers are portable across workspace roots. Exact workspace
matchers include the workspace root identity. Command matchers can match an
exact structured command, an argv prefix, or every command. Network matchers
use the normalized protocol, host, and explicit port supplied by the host; they
do not resolve DNS aliases or infer default ports.

Inspect the static rule table with:

```sh
mentat permission list
mentat permission list --json
mentat permission remove RULE_ID
```

Rule ids are derived from rule content, not list position. `permission list`
shows durable rules followed by fixed product rules. `permission remove` edits
writable user config; rules from an explicitly supplied extra config must be
removed from that file directly.
