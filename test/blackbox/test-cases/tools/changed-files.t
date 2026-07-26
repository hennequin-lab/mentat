Changed-file evidence is a product surface over the mutation ledger: a
tool-driven edit records a typed change row that `session diff` renders — human
and JSON — with no model in the loop, and a failed edit records nothing. The
workspace is not a git repository, which also proves the ledger works
backend-less. The file-mutation editor is model-conditional, so these runs pin
the string-replace family explicitly while the model stays gpt-5.6-sol for the fake
OpenAI-Responses backend.

  $ use_trusted_workspace
  $ mkdir -p "$XDG_CONFIG_HOME/mentat"
  $ printf '%s\n' '{"permission":{"rules":{"version":1,"items":[{"action":"allow","matcher":{"type":"any"}}]}},"tools":{"editor":"string-replace"}}' > "$XDG_CONFIG_HOME/mentat/config.json"

An edit_file run records a change row and prints the tool line on the human
transcript (stderr); the final assistant text is the only thing on stdout.

  $ printf 'hello world\n' > note.txt
  $ cat > edit.jsonl <<'JSONL'
  > {"expect":{"body_contains":["\"name\":\"edit_file\""]},"response":{"id":"resp-edit-1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"item-edit-1","call_id":"call-edit-1","name":"edit_file","arguments":"{\"path\":\"note.txt\",\"old_string\":\"hello\",\"new_string\":\"goodbye\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","call-edit-1"]},"response":{"id":"resp-edit-2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"edited"}]}]}}
  > JSONL
  $ start_fake_openai edit.jsonl capture-edit port-edit
  $ mentat run start --id edit-run "change the greeting" --cwd "$PWD" 2>edit.err
  edited
  $ wait_fake_server
  $ grep 'tool edit_file' edit.err
  • tool edit_file
  $ cat note.txt
  goodbye world

`session diff` renders the typed change without asking a model, collapsed to one
net entry per path: the net operation and path, the hunk body on stdout, the
change summary on stderr.

  $ mentat session diff edit-run 2>&1
  M note.txt
  --- note.txt
  @@ -1,1 +1,1 @@
  -hello world
  +goodbye world
  changed 1 file(s) (+1 -1)

The same evidence is structured under --json, with the net operation and the
contiguity of the recorded history per file.

  $ mentat session diff --json edit-run
  {"schema_version":1,"type":"session.diff","files":1,"additions":1,"deletions":1,"changes":1,"diff":[{"path":"note.txt","operation":"M","contiguous":true,"hunks":[{"old_start":1,"new_start":1,"lines":[{"kind":"removed","text":"hello world","newline":true},{"kind":"added","text":"goodbye world","newline":true}]}]}]}

A write_file that creates a file records an add.

  $ cat > create.jsonl <<'JSONL'
  > {"expect":{"body_contains":["\"name\":\"write_file\""]},"response":{"id":"resp-create-1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"item-create-1","call_id":"call-create-1","name":"write_file","arguments":"{\"path\":\"new.txt\",\"contents\":\"fresh line\\n\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","call-create-1"]},"response":{"id":"resp-create-2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"created"}]}]}}
  > JSONL
  $ start_fake_openai create.jsonl capture-create port-create
  $ mentat run start --id create-run "create a file" --cwd "$PWD" 2>/dev/null
  created
  $ wait_fake_server
  $ mentat session diff create-run 2>&1
  A new.txt
  --- new.txt
  @@ -0,0 +1,1 @@
  +fresh line
  changed 1 file(s) (+1 -0)

A failed edit reports the failure to the model, leaves the file unchanged, and
records no changed-file evidence: the diff is empty and exits 0.

  $ printf 'red red blue\n' > fail.txt
  $ cat > editfail.jsonl <<'JSONL'
  > {"expect":{"body_contains":["\"name\":\"edit_file\""]},"response":{"id":"resp-fail-1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"item-fail-1","call_id":"call-fail-1","name":"edit_file","arguments":"{\"path\":\"fail.txt\",\"old_string\":\"missing\",\"new_string\":\"green\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","call-fail-1"]},"response":{"id":"resp-fail-2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"edit failed"}]}]}}
  > JSONL
  $ start_fake_openai editfail.jsonl capture-fail port-fail
  $ mentat run start --id fail-run "try a failing edit" --cwd "$PWD" 2>/dev/null
  edit failed
  $ wait_fake_server
  $ cat fail.txt
  red red blue
  $ mentat session diff fail-run 2>&1
  changed 0 file(s) (+0 -0)
