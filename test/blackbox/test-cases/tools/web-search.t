web_search joins the tool catalog only when web is enabled and the search
provider is not `off`. The fake provider's request carries the offered tool
declarations, so the boot gate is asserted from the catalog itself — no live
search endpoint is contacted. The tool's own request/response contract is pinned
by the web_search expect suite.

  $ use_trusted_workspace
  $ mkdir -p "$XDG_CONFIG_HOME/mentat"

With web enabled and the default provider (exa), the request offers both
web_fetch and web_search. The fake provider's expectation fails the run unless
both tool declarations are present, so a completed run is the assertion.

  $ cat > "$XDG_CONFIG_HOME/mentat/config.json" <<'JSON'
  > {"web":{"enabled":true}}
  > JSON
  $ cat > on.jsonl <<JSONL
  > {"expect":{"body_contains":["catalog probe","\"name\":\"web_fetch\"","\"name\":\"web_search\""]},"response":{"id":"on-1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"ok"}]}]}}
  > JSONL
  $ start_fake_openai on.jsonl capture-on port-on
  $ mentat run start --id on "catalog probe" --cwd "$PWD" 2>/dev/null
  ok
  $ wait_fake_server

Setting the provider to `off` withholds web_search while web_fetch stays: the
captured request carries web_fetch but not web_search.

  $ cat > "$XDG_CONFIG_HOME/mentat/config.json" <<'JSON'
  > {"web":{"enabled":true,"search_provider":"off"}}
  > JSON
  $ cat > off.jsonl <<JSONL
  > {"expect":{"body_contains":["catalog probe","\"name\":\"web_fetch\""]},"response":{"id":"off-1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"ok"}]}]}}
  > JSONL
  $ start_fake_openai off.jsonl capture-off port-off
  $ mentat run start --id off "catalog probe" --cwd "$PWD" 2>/dev/null
  ok
  $ wait_fake_server
  $ printf '%s\n' "$(cat capture-off/request-*.json | grep -o '"name":"web_search"' | wc -l | tr -d ' ')"
  0
  $ printf '%s\n' "$(cat capture-off/request-*.json | grep -o '"name":"web_fetch"' | wc -l | tr -d ' ')"
  1

Disabling web withholds the whole family, web_search included.

  $ cat > "$XDG_CONFIG_HOME/mentat/config.json" <<'JSON'
  > {"web":{"enabled":false}}
  > JSON
  $ cat > disabled.jsonl <<JSONL
  > {"expect":{"body_contains":["catalog probe"]},"response":{"id":"d-1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"ok"}]}]}}
  > JSONL
  $ start_fake_openai disabled.jsonl capture-disabled port-disabled
  $ mentat run start --id disabled "catalog probe" --cwd "$PWD" 2>/dev/null
  ok
  $ wait_fake_server
  $ printf '%s\n' "$(cat capture-disabled/request-*.json | grep -o '"name":"web_search"' | wc -l | tr -d ' ')"
  0
  $ printf '%s\n' "$(cat capture-disabled/request-*.json | grep -o '"name":"web_fetch"' | wc -l | tr -d ' ')"
  0
