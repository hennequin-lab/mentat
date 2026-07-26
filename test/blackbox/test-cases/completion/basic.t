`completion SHELL` prints a self-contained script that drives the binary's own
cmdliner completion protocol. The scripts name the `mentat` binary and carry no
hardcoded command tree, so a new command needs no regenerated script.

The bash script registers a completion function bound to `mentat`.

  $ mentat completion bash > comp.bash
  $ grep -c 'complete -F _mentat_cmdliner mentat' comp.bash
  1
  $ grep -c '_mentat_cmdliner()' comp.bash
  1

The completion is driven at runtime through the binary's `--__complete`
protocol, so the script reflects the current command tree without embedding it.

  $ grep -q -- '--__complete' comp.bash && echo protocol || echo missing
  protocol

The zsh script carries the compdef line for mentat.

  $ mentat completion zsh | grep -o '#compdef mentat'
  #compdef mentat

The pwsh script registers a native argument completer for mentat.

  $ mentat completion pwsh | grep -o 'CommandName mentat'
  CommandName mentat

An unknown shell is rejected by the cmdliner enum (exit 124).

  $ mentat completion fish >/dev/null 2>&1; echo exit:$?
  exit:124
