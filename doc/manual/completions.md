# Shell completions

`mentat completion SHELL` prints a self-contained completion script for
`bash`, `zsh`, or `pwsh`. The script drives the binary's cmdliner completion
protocol, so commands, subcommands, and options complete without any
external helper — and without a hardcoded command tree, so a new command
needs no regenerated script.

## zsh

```sh
mkdir -p ~/.local/share/zsh/site-functions
mentat completion zsh > ~/.local/share/zsh/site-functions/_mentat
```

and make sure the directory is on `fpath` before `compinit` in `~/.zshrc`:

```sh
fpath=(~/.local/share/zsh/site-functions $fpath)
autoload -Uz compinit && compinit
```

## bash

```sh
mkdir -p ~/.local/share/bash-completion/completions
mentat completion bash > ~/.local/share/bash-completion/completions/mentat
```

The `bash-completion` package sources it on demand.

## PowerShell

```powershell
mentat completion pwsh >> $PROFILE
```

## Verifying

`mentat --__complete --__complete=se` prints the raw completion protocol
(one `item` per candidate); if that lists `session`, the binary side works
and any remaining issue is shell setup.
