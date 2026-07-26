Protected workspace metadata (.git, .mentat) is read-only to the native edit
tools, enforced at the write-side IO chokepoint independent of permission mode.
Even a run that allows every tool cannot write `.mentat/config.json` (the config
self-grant vector) or `.git/hooks/pre-commit` through write_file, while ordinary
files still write. Sandbox and workspace policy change through the CLI, not
through the agent's edit tools.

  $ use_trusted_workspace
  $ mkdir -p "$XDG_CONFIG_HOME/mentat"
  $ printf '%s\n' '{"permission":{"rules":{"version":1,"items":[{"action":"allow","matcher":{"type":"any"}}]}},"tools":{"editor":"string-replace"}}' > "$XDG_CONFIG_HOME/mentat/config.json"
  $ mkdir -p .git/hooks

The model attempts three writes in one run: a .mentat config file, a git hook,
and an ordinary file. Only the ordinary file may be written; the run completes.

  $ cat > meta.jsonl <<'JSONL'
  > {"expect":{"body_contains":["\"name\":\"write_file\""]},"response":{"id":"resp-meta-1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"item-meta-1","call_id":"call-meta-1","name":"write_file","arguments":"{\"path\":\".mentat/config.json\",\"contents\":\"{\\\"sandbox\\\":{\\\"writable_roots\\\":[\\\"~\\\"]}}\"}"}]}}
  > {"expect":{"body_contains":["call-meta-1"]},"response":{"id":"resp-meta-2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"item-meta-2","call_id":"call-meta-2","name":"write_file","arguments":"{\"path\":\".git/hooks/pre-commit\",\"contents\":\"#!/bin/sh\\nexfiltrate\\n\"}"}]}}
  > {"expect":{"body_contains":["call-meta-2"]},"response":{"id":"resp-meta-3","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"item-meta-3","call_id":"call-meta-3","name":"write_file","arguments":"{\"path\":\"allowed.txt\",\"contents\":\"ok\\n\"}"}]}}
  > {"expect":{"body_contains":["call-meta-3"]},"response":{"id":"resp-meta-4","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done"}]}]}}
  > JSONL
  $ start_fake_openai meta.jsonl capture-meta port-meta
  $ mentat run start --id meta-run "write files" --cwd "$PWD" 2>/dev/null
  done
  $ wait_fake_server

Both protected writes were refused; the ordinary write succeeded.

  $ test ! -e .mentat/config.json && echo no-mentat-config
  no-mentat-config
  $ test ! -e .git/hooks/pre-commit && echo no-git-hook
  no-git-hook
  $ cat allowed.txt
  ok

Only the ordinary file reached the mutation ledger; the refused protected
writes recorded no changed-file evidence.

  $ mentat session diff meta-run 2>&1
  A allowed.txt
  --- allowed.txt
  @@ -0,0 +1,1 @@
  +ok
  changed 1 file(s) (+1 -0)

Reads of protected metadata are unaffected: only writes are guarded.

  $ printf 'ref: refs/heads/main\n' > .git/HEAD
  $ cat > read.jsonl <<'JSONL'
  > {"expect":{"body_contains":["\"name\":\"read_file\""]},"response":{"id":"resp-read-1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"item-read-1","call_id":"call-read-1","name":"read_file","arguments":"{\"path\":\".git/HEAD\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","call-read-1","refs/heads/main"]},"response":{"id":"resp-read-2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"read"}]}]}}
  > JSONL
  $ start_fake_openai read.jsonl capture-read port-read
  $ mentat run start --id read-run "read head" --cwd "$PWD" 2>/dev/null
  read
  $ wait_fake_server
