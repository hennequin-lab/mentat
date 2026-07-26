An extra config file named by MENTAT_CONFIG layers above the user file. The
value must be an absolute path.

  $ use_trusted_workspace

The extra file outranks the user file for effective reads.

  $ mentat config set model openai/gpt-5.6-sol >/dev/null
  $ cat > extra.json <<EOF
  > {"model":"openai/extra-model"}
  > EOF
  $ MENTAT_CONFIG="$PWD/extra.json" mentat config get model
  openai/extra-model

A malformed extra file fails effective validation with a clean diagnostic and
exit 1; the path is reported.

  $ printf '[' > bad-extra.json
  $ MENTAT_CONFIG="$PWD/bad-extra.json" mentat config validate >/dev/null 2>err.txt; echo "exit=$?"
  exit=1
  $ censor <err.txt
  mentat: $TESTCASE_ROOT/bad-extra.json: Expected JSON value but found end of text
  File "-", line 1, characters 1-2:
  File "-", line 1, characters 1-2: at index 0 of
  File "-", line 1, characters 0-2: array<json>

A relative MENTAT_CONFIG is a runtime error at the boundary — env-sourced input
flows through the error ladder (exit 1), never an internal-error backtrace. The
shape check precedes the existence check, so even a nonexistent relative path
is loud.

  $ MENTAT_CONFIG=relative.json mentat config path
  mentat: MENTAT_CONFIG must be an absolute path: relative.json
  [1]
