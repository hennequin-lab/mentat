# Security

Mentat keeps three policy questions separate: whether repository-controlled
inputs and processes may activate, whether a described operation is approved,
and what operating-system authority the operation receives. They reinforce one
another, but none substitutes for another.

The default posture is:

- permission review behavior `default`;
- unattended permission policy `block`;
- sandbox mode `workspace-write`;
- sandbox read scope `project`;
- sandbox requirement `enforced`;
- command network access `restricted`;

On a supported host, and absent an earlier durable rule, this lets native
workspace edits run without review. Under the default project read scope with
restricted networking, ordinary commands also run without review; high-impact
commands retain a review interlock, and a command that tries to read outside the
project roots is refused by the sandbox rather than prompted. Widening the read
scope to `all` returns ordinary commands to review, because a command that can
read the whole filesystem is credited only after review. If the platform cannot
enforce the requested sandbox, the run fails before provider credentials are
loaded or a session is created.

## Data leaving your machine

Mentat runs on your machine, but a hosted coding-agent turn is not local data
processing. Permission review and command sandboxing constrain tool operations;
they do not redact the model request assembled after those operations. Data a
tool returns to the conversation can be sent on the next model request.

| Surface | What crosses the boundary | Control and responsibility |
| --- | --- | --- |
| Hosted model requests | The full model-visible request goes to the selected provider or configured base URL. It can include Mentat's system and mode instructions, enabled global and project instructions, workspace notices, the active skill catalog, loaded skill bodies and resources, user prompts and custom-command expansions, conversation history, source text and command/build/search output returned by tools, web results, and tool declarations. Attached or tool-returned image bytes are included when the selected model and provider channel support images. | Select the model and endpoint deliberately. Mentat does not control or verify the provider's retention, training, abuse-monitoring, or regional-processing policy. The user is responsible for the selected provider account's policy; for a custom base URL, the endpoint operator owns it. |
| Auto-title request | A fresh untitled `mentat run` or TUI session makes a separate request on `small_model` before its first turn. That request contains a title-generator instruction and the first prompt text, but not the turn's assembled project instructions or tool catalog. `small_model` defaults to the main model. | Set an explicit `--title` for a headless start, or set `MENTAT_AUTO_TITLE=0` (`false`, `no`, and `off` also work) to suppress the side call. A hosted small model has the same provider-retention responsibility as the main model. |
| Managed `local` provider | Prompt requests go to a Mentat-managed `llama-server` bound to `127.0.0.1`; they are not sent to a hosted model service. | This statement applies to the built-in `local` provider. The `ollama` provider sends the same model-visible request to its configured `providers.ollama.base_url`; it stays on-machine only when that endpoint is actually local. |
| `web_fetch` | When `web.enabled=true`, the target site receives the requested public URL, the machine's network address, and ordinary generated HTTP headers. Returned page text becomes tool output and can then enter a hosted model request. | Web tools are disabled by default. `web_fetch` sends no Mentat provider credential, upgrades HTTP URLs to HTTPS, and applies its separate URL, private-network, redirect, timeout, and size policy. The destination site controls its own logs and retention. |
| `web_search` | The query and requested result count are posted to the configured Exa or Parallel remote search endpoint. An optional search API key is sent only to that endpoint. Search result text becomes tool output and can then enter a hosted model request. | Set `web.search_provider=off` to withhold search while retaining fetch, or `web.enabled=false` to withhold both. The search service controls query retention. |
| Managed model downloads | `mentat models download local/MODEL`, or the first request for a missing managed local model, downloads its GGUF artifact from the catalog URL on Hugging Face. The request reveals normal network metadata and the selected artifact, not prompts, instructions, source, or session contents. | Downloaded weights are size- and SHA-256-verified and stored under the data home. An explicit GGUF path is read locally and is not downloaded. |
| Logs and crash reports | TUI diagnostics are written under the state home; headless logging is off unless `MENTAT_LOG` is set. An unexpected internal error writes a local crash report containing version/run metadata, the active session id, and a backtrace. Diagnostic files can contain paths and error details. | Mentat does not automatically upload logs or crash reports. TUI logs and crash reports each retain the newest 20 local files. Inspect and redact them before attaching them to an issue or sending them to another party. |

Provider login, OAuth token refresh, login-time account checks, and remote
revocation also contact the applicable provider. `mentat auth status` is
passive. Merely listing local sessions, running offline diagnostics, or opening
the daemon's loopback web UI does not make a model request. Actions taken
through that UI still use the selected model and web tools normally; see
[Daemon and web](daemon-and-web.md).

## The three boundaries

| Boundary | What it decides | What it does not do |
| --- | --- | --- |
| Repository activation | Whether repository configuration, instructions, skills, executable tools, and project processes may activate. | It does not approve an operation or widen the selected sandbox. |
| Permission policy | Whether a host-described operation is allowed, denied, or requires review. | It does not confine a process or grant an OS capability. |
| Runtime confinement | What an approved native tool or spawned process may access. | It does not approve the operation or activate project inputs. |

Runtime confinement has two implementations. Native file tools resolve typed
workspace paths, check realpath containment, and protect metadata. Standard
command-bearing tools and trusted automatic Dune/Merlin/Git integrations spawn
through the sealed command sandbox. Explicit frontend operations such as a
login browser remain outside the model-tool boundary. Provider calls and web
tools use their own host APIs; `sandbox.network` is not a process-wide firewall
and does not disable those services.

Each boundary has its own page:

- [Permission policy](permissions.md) — how an operation is allowed, denied,
  or sent to review; review behavior, conversation grants, and answering a
  blocked headless run.
- [Permission rules](permission-rules.md) — the durable JSON matcher format,
  evaluation order, and authoring workflow.
- [Command sandbox](sandbox.md) — modes, filesystem read scope, writable and
  protected paths, network policy, child environments, and enforcement
  backends.
- [Workspace trust](workspace-trust.md) — what activation means, what a
  restricted workspace can still do, and the config allowlist that survives
  trust.

## Inspection and audit

Use these commands before a run to inspect the effective posture:

```sh
mentat config show --origins
mentat doctor
mentat permission list
mentat sandbox status
mentat sandbox explain
```

`config show` reports the effective `workspace_trust` state and omits disabled
project values. `doctor` reports the trust-store path and validity plus the
canonical root and resolved state without contacting a provider or starting
project tooling. Both doctor and sandbox explain omit project-local `_opam`
lookup until the workspace is trusted. `sandbox status` compactly reports mode,
read scope, network posture, enforcement evidence, and admitted roots without
loading provider credentials or creating a session. `sandbox explain` reports
the sealed identity and its readable, writable, protected, and network policy.
Both commands support `--json`; status has no verbose variant.

At run start, text and JSONL output record the effective review behavior and
sandbox posture. Each shell result records its actual enforcement evidence.
Permission requests and replies are durable session events, including whether a
denial came from a reviewer or unattended policy. Inspect them with:

```sh
mentat session show SESSION
mentat session show --json SESSION
mentat session export SESSION
```

Use structured JSON, exit codes, and session events for automation. Human
diagnostic wording is not a stable matching interface.
