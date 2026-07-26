`mentat session export --format html` renders the durable Fact.t projection as
one self-contained, asset-free HTML document, escaping every untrusted payload
by construction. A read_file tool call carries adversarial bytes from a file
into the transcript so the escaping law is exercised on the raw insertion path,
and the assistant prose carries a markdown-injection payload through the ~safe
cmarkit path.

  $ use_trusted_workspace
  $ printf '<script>alert(1)</script><img onerror=alert(2)> & "><script>x</script> '\''q'\'' </details>\n' > payload.txt
  $ cat > adversarial.jsonl <<'JSONL'
  > {"expect":{"body_contains":["read the payload"]},"response":{"id":"r1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"item-1","call_id":"call-1","name":"read_file","arguments":"{\"path\":\"payload.txt\"}"}]}}
  > {"expect":{"body_contains":["call-1"]},"response":{"id":"r2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"Done. <script>alert(3)</script> and <img onerror=alert(4)> markdown injection."}]}]}}
  > JSONL
  $ start_fake_openai adversarial.jsonl capture-adv port-adv
  $ mentat run "read the payload" --cwd "$PWD" --id adv >/dev/null 2>&1
  $ wait_fake_server

The `-o FILE` form writes the document to a path and leaves stdout empty.

  $ mentat session export adv --format html -o run.html >stdout.txt 2>/dev/null
  $ test -s run.html && echo "file written"
  file written
  $ test -s stdout.txt && echo "stdout non-empty" || echo "stdout empty"
  stdout empty

It is a single self-contained document: one `<!doctype html>`, exactly one
inline `<style>`, exactly one inline `<script>` (the expand/collapse control),
and a hash-pinned CSP.

  $ grep -c '<!doctype html' run.html
  1
  $ grep -c '<style>' run.html
  1
  $ grep -c '<script>' run.html
  1
  $ grep -o "script-src 'sha256-" run.html | head -1
  script-src 'sha256-
  $ grep -o "default-src 'none'" run.html | head -1
  default-src 'none'

No external asset is referenced: no external scheme, no `<script src>`, no
`<link>`, no CSS `url(`/`@import`, no `<iframe>`, and no scheme-relative `//`
source (grep -c is 0 with a non-zero exit, absorbed by `|| true`).

  $ grep -Ec 'https?://' run.html || true
  0
  $ grep -c '<script src' run.html || true
  0
  $ grep -c '<link' run.html || true
  0
  $ grep -c 'url(' run.html || true
  0
  $ grep -c '@import' run.html || true
  0
  $ grep -c '<iframe' run.html || true
  0
  $ grep -Ec '(src|href)="?//' run.html || true
  0

The escaping law holds on both the raw text path (the tool result) and the
attribute-relevant characters. The adversarial `<script>`/`<img onerror>`/
`</details>` payload and its `"><script>` attribute-breakout attempt appear only
escaped: no raw `<script>` beyond the one control script, no raw `<img>` tag, no
raw `"><script` breakout, and the payload bytes — including both quote forms —
are present in their escaped form.

  $ grep -o '&lt;script&gt;alert(1)&lt;/script&gt;' run.html | head -1
  &lt;script&gt;alert(1)&lt;/script&gt;
  $ grep -o '&lt;img onerror=alert(2)&gt;' run.html | head -1
  &lt;img onerror=alert(2)&gt;
  $ grep -o '&lt;/details&gt;' run.html | head -1
  &lt;/details&gt;
  $ grep -o '&amp;' run.html | head -1
  &amp;
  $ grep -o '&quot;&gt;&lt;script&gt;' run.html | head -1
  &quot;&gt;&lt;script&gt;
  $ grep -o '&#39;q&#39;' run.html | head -1
  &#39;q&#39;
  $ grep -c '"><script' run.html || true
  0
  $ grep -c '<img' run.html || true
  0

The markdown-injection payload in the assistant prose is neutralised by the
~safe cmarkit renderer: it produces no raw `<script>` or `<img>` (the single
raw `<script>` counted above is the control), so the raw-tag counts above stay
at the control-only values.

The brand and the per-family fact markers are present, so the rendering matrix
stays honest (one stable marker per family exercised by this run).

  $ grep -c '▂▄▆▄▂' run.html
  1
  $ grep -o 'data-fact="turn.started"' run.html | head -1
  data-fact="turn.started"
  $ grep -o 'data-fact="turn.assistant"' run.html | head -1
  data-fact="turn.assistant"
  $ grep -o 'data-fact="tool.started"' run.html | head -1
  data-fact="tool.started"
  $ grep -o 'data-fact="tool.returned"' run.html | head -1
  data-fact="tool.returned"

The default sink is stdout, matching the other formats.

  $ mentat session export adv --format html 2>/dev/null | grep -o '<!doctype html' | head -1
  <!doctype html

`--no-timestamp` yields byte-reproducible output; two exports are identical.

  $ mentat session export adv --format html --no-timestamp >a.html 2>/dev/null
  $ mentat session export adv --format html --no-timestamp >b.html 2>/dev/null
  $ diff a.html b.html && echo identical
  identical

The whole-document guard fails loudly rather than emit an oversized file,
pointing at `--format json` for a full archive.

  $ mentat session export adv --format html --max-bytes 200 2>err-big.txt; echo "exit=$?"
  exit=1
  $ grep -o 'use --format json' err-big.txt | head -1
  use --format json

An unknown format is a usage error (exit 2) naming html among the formats.

  $ mentat session export adv --format xml 2>err-fmt.txt; echo "exit=$?"
  exit=2
  $ grep -o 'expected json, text, markdown, or html' err-fmt.txt | head -1
  expected json, text, markdown, or html

An unknown session id fails (exit 1) without emitting a document.

  $ mentat session export no-such-session --format html >out-missing.html 2>/dev/null; echo "exit=$?"
  exit=1
  $ test -s out-missing.html && echo "wrote a document" || echo "no document"
  no document

An attached image embeds as an inline data URI: the CLI resolves the journal's
`Ref to the stored attachment bytes so the export shows the real image, not a
placeholder. This is also the one untrusted-attribute sink (the img `src`), so
an adversarial media type must appear only escaped.

  $ printf '%s' 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==' | base64 -d > shot.png
  $ cat > img.jsonl <<'JSONL'
  > {"expect":{"body_contains":["look"]},"response":{"id":"g1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"a dot"}]}]}}
  > JSONL
  $ start_fake_openai img.jsonl capture-img port-img
  $ mentat run -i shot.png "look" --cwd "$PWD" --id img 2>/dev/null
  a dot
  $ wait_fake_server

The resolve pass embeds the real image bytes as an inline base64 data URI.

  $ mentat session export img --format html --no-timestamp 2>/dev/null | grep -o 'data:image/png;base64,iVBORw0KGgo' | head -1
  data:image/png;base64,iVBORw0KGgo

Patch the journal's media type to a `"><script>` attribute breakout: the resolved
image still embeds, but the adversarial type reaches only the escaped `src` sink.
No raw breakout or injected tag survives.

  $ sed 's|"media_type":"image/png"|"media_type":"image/png\\"><script>alert(9)</script>"|' data/mentat/sessions/img/session.json > img-patched.json
  $ mv img-patched.json data/mentat/sessions/img/session.json
  $ mentat session export img --format html --no-timestamp >img.html 2>/dev/null
  $ grep -o '&quot;&gt;&lt;script&gt;alert(9)&lt;/script&gt;' img.html | head -1
  &quot;&gt;&lt;script&gt;alert(9)&lt;/script&gt;
  $ grep -c '"><script>alert(9)' img.html || true
  0
