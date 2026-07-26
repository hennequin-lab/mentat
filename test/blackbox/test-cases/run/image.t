The -i/--image flag attaches image files as model-visible content ahead of the
prompt. The CLI reads the file, the attach flow sniffs and stores it fence-free
as a session attachment (a content-addressed `Ref in the journal), and the bytes
reach the provider as a base64 data URL after the engine resolves the reference.

  $ use_trusted_workspace

A tiny 1x1 PNG fixture, generated deterministically — no binary is checked in.

  $ printf '%s' 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==' | base64 -d > shot.png

  $ cat > describe.jsonl <<'JSONL'
  > {"expect":{"body_contains":["what is this"]},"response":{"id":"i1","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"a tiny dot"}]}]}}
  > JSONL
  $ start_fake_openai describe.jsonl capture port
  $ mentat run -i shot.png "what is this" --cwd "$PWD" --id img-turn 2>/dev/null
  a tiny dot
  $ wait_fake_server

The captured provider request carries the image as an input_image data URL whose
payload is the base64 of the PNG bytes (the same bytes, resolved from the ref).

  $ grep -o 'input_image' capture/request-1.json | head -1
  input_image
  $ grep -o 'data:image/png;base64,iVBORw0KGgo' capture/request-1.json | head -1
  data:image/png;base64,iVBORw0KGgo

The journal holds a content-addressed reference, never the inline bytes.

  $ mentat session export img-turn --format json 2>/dev/null | censor | grep -o 'sha256:[^"]*:70' | head -1
  sha256:$DIGEST4:70

-i is repeatable: two images become two input_image blocks ahead of the prompt.

  $ printf '%s' 'R0lGODdhAQABAIAAAAAAAAAAACwAAAAAAQABAAACAkQBADs=' | base64 -d > shot.gif
  $ cat > two.jsonl <<'JSONL'
  > {"expect":{"body_contains":["two images"]},"response":{"id":"i2","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"saw two"}]}]}}
  > JSONL
  $ start_fake_openai two.jsonl capture-two port-two
  $ mentat run -i shot.png -i shot.gif "two images" --cwd "$PWD" --id two-turn 2>/dev/null
  saw two
  $ wait_fake_server
  $ grep -o 'input_image' capture-two/request-1.json | wc -l | tr -d ' '
  2

A missing image file is a loud pre-turn usage error (exit 2); no turn is admitted.

  $ mentat run -i nope.png "x" --cwd "$PWD" --id missing-turn 2>&1
  mentat: sandbox: danger-full-access (read project, network restricted)
  mentat: --image: file not found: nope.png
  [2]

A non-image file is rejected by magic-byte sniffing, not its extension.

  $ printf 'this is plain text, not an image' > notimage.png
  $ mentat run -i notimage.png "x" --cwd "$PWD" --id notimage-turn 2>&1
  mentat: sandbox: danger-full-access (read project, network restricted)
  mentat: --image notimage.png: the file is not a recognized image
  [2]
