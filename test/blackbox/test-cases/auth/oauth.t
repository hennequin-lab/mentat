OAuth login is wired end to end against a local fake issuer. The device-code and
browser flows each run the authorization dance, exchange the code, save the
OAuth credential, and settle through the same persist-then-check policy as the
api-key path. Token material never appears in status output. The auth base-URL
override reroots the issuer endpoints onto the fake; the provider base-URL
override routes the post-save `/v1/models` check there too. The OAuth route
targets the ChatGPT Codex backend, whose `/models` probe carries the
`client_version` query that endpoint requires and answers with the
`{"models":[{"slug":…}]}` shape.

  $ use_trusted_workspace

The device-code flow prints the verification challenge, exchanges the authorized
code, and settles ready. The challenge (Go to / Enter code) is on stderr; the
settle block is on stdout.

  $ cat > device.jsonl <<'JSONL'
  > {"expect":{"request_line":"POST /v1/api/accounts/deviceauth/usercode HTTP/1.1"},"http":{"status":200,"json":{"device_auth_id":"dev-1","user_code":"CODE-1234","expires_in":300,"interval":0}}}
  > {"expect":{"request_line":"POST /v1/api/accounts/deviceauth/token HTTP/1.1"},"http":{"status":200,"json":{"authorization_code":"auth-code-1","code_challenge":"7HWTEa2cNqSU6dY1pSKj_i_9al1m6oEXjMUJBGJybWE","code_verifier":"test-verifier-test-verifier-test-verifier-1"}}}
  > {"expect":{"request_line":"POST /v1/oauth/token HTTP/1.1","body_contains":["grant_type=authorization_code","code=auth-code-1"]},"http":{"status":200,"json":{"id_token":"e30.e30.e30","access_token":"oauth-access-device","refresh_token":"refresh-device-1","expires_in":3600}}}
  > {"expect":{"request_line":"GET /v1/models?client_version=0.0.0 HTTP/1.1"},"http":{"status":200,"json":{"models":[{"slug":"gpt-5.6-sol"}]}}}
  > JSONL
  $ start_fake_issuer device.jsonl cap-dev port-dev
  $ mentat auth login openai --method device-code >dev.out 2>dev.err
  $ wait_fake_server
  $ censor < dev.out
  Logged in to openai with device-code.
  Saved: default (file store $TESTCASE_ROOT/config/mentat/auth.json)
  Checked: ready
  Next: run `mentat models list` to see available models.
  $ censor < dev.err
  Go to: http://127.0.0.1:$PORT/v1/codex/device
  Enter code: CODE-1234

The saved credential is connected and its token material is redacted from status.

  $ mentat auth status openai --json
  {"schema_version":1,"type":"auth_status","providers":[{"provider":"openai","phase":"unchecked","connected":true}]}
  $ mentat auth status openai --json | grep -Ec 'oauth-access-device|refresh-device-1'
  0
  [1]

The browser flow listens on the loopback callback. A stray request carrying a
forged state is answered with an error and does not consume the attempt; the
real redirect carries the state minted for this authorization.

  $ mentat auth logout openai >/dev/null
  $ cat > browser.jsonl <<'JSONL'
  > {"expect":{"request_line":"POST /oauth/token HTTP/1.1","body_contains":["grant_type=authorization_code","code=browser-code-1"]},"http":{"status":200,"json":{"id_token":"e30.e30.e30","access_token":"oauth-access-browser","refresh_token":"refresh-browser-1","expires_in":3600,"token_type":"Bearer"}}}
  > {"expect":{"request_line":"GET /v1/models?client_version=0.0.0 HTTP/1.1"},"http":{"status":200,"json":{"models":[{"slug":"gpt-5.6-sol"}]}}}
  > JSONL
  $ start_fake_issuer browser.jsonl cap-br port-br
  $ mentat auth login openai --method browser >browser.out 2>browser.err &
  $ login_pid=$!
  $ mentat_cram wait-line "Listening" browser.err
  $ fake_browser_get "http://localhost:1455/auth/callback?code=evil&state=forged"
  400

The served pages carry the brand and are fully self-contained — a monospace
mentat card with the paprika heap mark, an outcome line, and no external assets
(so they render from the loopback with no network). The forged callback gets the
failure page; it does not consume the attempt, so the real redirect still wins.

  $ fake_browser_body "http://localhost:1455/auth/callback?code=evil&state=forged" > forged.html
  $ grep -cF "this OpenAI callback" forged.html
  1
  $ grep -Ec "https?://" forged.html
  0
  [1]
  $ state=$(grep -oE 'state=[A-Za-z0-9_-]+' browser.err | head -n 1 | sed 's/state=//')
  $ fake_browser_body "http://localhost:1455/auth/callback?code=browser-code-1&state=$state" > success.html
  $ grep -cF "signed in to OpenAI" success.html
  1
  $ grep -cF "return to your terminal" success.html
  1
  $ grep -Ec "https?://" success.html
  0
  [1]
  $ wait "$login_pid"
  $ wait_fake_server
  $ censor < browser.out
  Logged in to openai with browser.
  Saved: default (file store $TESTCASE_ROOT/config/mentat/auth.json)
  Checked: ready
  Next: run `mentat models list` to see available models.
  $ mentat auth status openai --json | grep -Ec 'oauth-access-browser|refresh-browser-1'
  0
  [1]

A denial redirects to the callback with an error and a matching state, so the
loopback settles the attempt as failed and serves the branded denial page — which
names the provider and the returned reason. No token is exchanged on denial, so
no issuer is needed here.

  $ mentat auth logout openai >/dev/null
  $ mentat auth login openai --method browser >denied.out 2>denied.err &
  $ login_pid=$!
  $ mentat_cram wait-line "Listening" denied.err
  $ state=$(grep -oE 'state=[A-Za-z0-9_-]+' denied.err | head -n 1 | sed 's/state=//')
  $ fake_browser_body "http://localhost:1455/auth/callback?error=access_denied&error_description=user%20refused&state=$state" > denied.html
  $ grep -cF "OpenAI denied" denied.html
  1
  $ grep -cF "access_denied: user refused" denied.html
  1
  $ grep -Ec "https?://" denied.html
  0
  [1]
  $ wait "$login_pid"
  [1]
