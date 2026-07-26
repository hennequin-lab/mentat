The ollama provider is Mentat's OpenAI-compatible route for a local daemon and,
by the same endpoint shape, for llama.cpp / vLLM / LM Studio. It streams the
Chat-Completions wire shape at /v1/chat/completions, and its auth is optional,
so a bare local endpoint runs with no credential. Here it runs against the fake
server's chat mode, pointed at it via MENTAT_OLLAMA_BASE_URL.

  $ use_trusted_workspace
  $ cat > text.jsonl <<'JSONL'
  > {"expect":{"body_contains":["\"model\":\"qwen3-coder\"","hello ollama","\"stream\":true"]},"chat":{"model":"qwen3-coder","content":"hi from the daemon"}}
  > JSONL
  $ start_fake_server text.jsonl capture-text port-text
  $ export MENTAT_MODEL=ollama/qwen3-coder
  $ export MENTAT_OLLAMA_BASE_URL="http://127.0.0.1:$(cat port-text)"
  $ mentat run "hello ollama" --cwd "$PWD" --id ollama-text 2>stderr.txt
  hi from the daemon
  $ cat stderr.txt
  mentat: sandbox: danger-full-access (read project, network restricted)
  mentat: session saved; resume with: mentat run resume 'ollama-text'
  $ wait_fake_server

The request reached the fake at the chat-completions endpoint carrying the
ollama model id — the provider's dynamic model constructor accepts any
daemon-served id, so no static catalog entry is needed.

  $ grep -o '"model":"qwen3-coder"' capture-text/request-1.json
  "model":"qwen3-coder"

The full agent loop runs over the chat-completions wire: a tool-call chunk drives
the shell tool, its result is fed back as a role:tool message on the next
request, and the daemon completes the turn with final text. The --json stream
surfaces the tool lifecycle correlated by claim, ahead of the terminal
turn.finished.

  $ echo 'note body' > note.txt
  $ cat > tool.jsonl <<'JSONL'
  > {"expect":{"body_contains":["read the note","\"name\":\"shell\""]},"chat":{"model":"qwen3-coder","tool_calls":[{"id":"tc-1","name":"shell","arguments":"{\"command\":\"cat note.txt\"}"}],"finish_reason":"tool_calls"}}
  > {"expect":{"body_contains":["\"role\":\"tool\"","tc-1"]},"chat":{"model":"qwen3-coder","content":"read the note ok"}}
  > JSONL
  $ start_fake_server tool.jsonl capture-tool port-tool
  $ export MENTAT_OLLAMA_BASE_URL="http://127.0.0.1:$(cat port-tool)"
  $ mentat run start --json --permission bypass --cwd "$PWD" --id ollama-tool "read the note" >tool.out 2>/dev/null
  $ wait_fake_server
  $ grep '"type":"tool.started"' tool.out | mentat_cram json .tool
  shell
  $ grep '"type":"tool.finished"' tool.out | mentat_cram json .outcome
  returned
  $ grep '"type":"turn.finished"' tool.out | mentat_cram json .text
  read the note ok
