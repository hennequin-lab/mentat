The read path: read_file on an image file returns it to the model as media
instead of rejecting it as a binary file. The engine externalizes the returned
image to a content reference at tool settlement (the journal stays small), then
resolves it back to base64 for the next provider request.

  $ use_trusted_workspace
  $ printf '%s' 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==' | base64 -d > shot.png

  $ cat > read.jsonl <<'JSONL'
  > {"expect":{"body_contains":["look at shot.png"]},"response":{"id":"r1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"function_call","id":"item-r1","call_id":"call-r1","name":"read_file","arguments":"{\"path\":\"shot.png\"}"}]}}
  > {"expect":{"body_contains":["function_call_output","call-r1"]},"response":{"id":"r2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"I see a tiny dot"}]}]}}
  > JSONL
  $ start_fake_openai read.jsonl capture port
  $ mentat run "look at shot.png" --cwd "$PWD" --permission bypass --id read-turn 2>/dev/null
  I see a tiny dot
  $ wait_fake_server

The second request (after the tool settled) carries the read image as an
input_image data URL in the tool result.

  $ grep -o 'input_image' capture/request-2.json | head -1
  input_image
  $ grep -o 'data:image/png;base64,iVBORw0KGgo' capture/request-2.json | head -1
  data:image/png;base64,iVBORw0KGgo

The Tool_settled fact holds a content reference to the image, never inline bytes.

  $ mentat session export read-turn --format json 2>/dev/null | censor | grep -o 'sha256:[^"]*:70' | head -1
  sha256:$DIGEST8:70

The read reports the honest image shape, not a zero-line file.

  $ mentat session export read-turn --format json 2>/dev/null | grep -o '"shape":"image"' | head -1
  "shape":"image"
