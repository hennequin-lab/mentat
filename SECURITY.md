# Security policy

Mentat runs commands and edits files on your machine on behalf of a language
model. Its security model is three separate boundaries — durable permission
rules, a command sandbox that fails closed, and workspace trust — described in
full in [the security manual](doc/manual/security.md). A defect in any of them
is a vulnerability, and we want to hear about it before your users do.

## Supported versions

Mentat is pre-1.0 and experimental. Only the most recent release receives
fixes; there are no backports to earlier versions. If you are running a build
from source, reproduce against the current `main` before reporting.

## Reporting a vulnerability

Report privately. Do not open a public issue, and do not post a proof of
concept to a public forum first.

Use GitHub's private vulnerability reporting on the
[Mentat repository](https://github.com/invariant-hq/mentat/security/advisories/new).
If that is unavailable to you, email the maintainer at
`thibaut.mattio@gmail.com` with `SECURITY` in the subject line.

A useful report includes:

- The version (`mentat --version`) and the platform.
- Which boundary is affected: permission rules, the command sandbox, or
  workspace trust.
- The sandbox mode in effect, from the run banner or `mentat sandbox status`.
- A minimal reproduction, and what an attacker gains if it succeeds.

You will get an acknowledgement that a human has read the report, and an
assessment of whether we agree it is a vulnerability. We will tell you when a
fix ships and credit you in the release notes unless you would rather we did
not.

## In scope

- Escaping the command sandbox: running a command outside the confinement the
  active mode promises, or writing outside the writable set.
- The sandbox failing open rather than closed when a platform backend is
  missing or broken.
- Bypassing durable permission rules, or causing a rule to match something it
  should not.
- Bypassing workspace trust: getting project configuration, instructions,
  skills, or tooling from an untrusted workspace to take effect.
- Disclosure of stored credentials, including leaking them into session data,
  logs, model requests, or tool output.
- Unauthorized access to the session store, the daemon's local socket, or the
  loopback web frontend.
- Content in a repository — source, `AGENTS.md`, skills, review comments —
  that causes Mentat to exceed the permissions in effect for that workspace.

That last case is the one we care most about. Mentat reads untrusted text and
acts on it, so the question is never whether a model can be misled. It is
whether being misled is enough to cross a boundary that was supposed to hold.

## Out of scope

- The model writing incorrect, insecure, or undesirable code. Review the diff:
  that is what `mentat session diff` is for.
- Anything that requires `danger-full-access`. That mode removes the sandbox
  by design and is documented as doing so.
- Anything that requires the user to trust a workspace they should not have
  trusted, where trust is what grants the capability.
- Vulnerabilities in model providers, or in the content they return.
- Missing hardening with no demonstrated impact.

If you are unsure which side of the line something falls on, report it. We
would rather triage a non-issue than miss a real one.
