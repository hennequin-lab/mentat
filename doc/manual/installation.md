# Installation

Prebuilt Mentat binaries are published for macOS on Apple Silicon and Intel and
for Linux on x86-64 and arm64. Native Windows is not supported. WSL2 can use the
Linux binary when the Linux sandbox probe succeeds; WSL1 cannot enforce the
default sandbox.

## Prerequisites

Release archives contain the Mentat binary, not every external program a tool
may invoke.

| Installation or feature | Needed on the host | Observable behavior when absent |
| --- | --- | --- |
| Installer script | POSIX `sh`, `uname`, `id`, `tar`, `mktemp`, `awk`, either `curl` or `wget`, and either `sha256sum` or `shasum` | Installation stops before publishing the binary. |
| Default Linux run | A working Bubblewrap executable at exactly `/usr/bin/bwrap` | The default `sandbox.require=enforced` startup gate fails before credentials are loaded or a session is created. A different `bwrap` on `PATH` is not used. |
| Source search on any platform | `rg` (ripgrep) on `PATH` | The `search_text` tool reports `ripgrep executable not found`; unrelated tools remain available. |
| Built-in `local` provider | `llama-server` on `PATH`, or `MENTAT_LOCAL_SERVER_BINARY` naming or resolving to it | Local inference reports that the server binary is unavailable. Hosted providers and an independently configured `ollama` endpoint are unaffected. |
| Source build | Git, Dune 3.22 or newer, a native C compiler/build toolchain, `pkg-config` or compatible `pkgconf`, and discoverable GMP development headers and libraries | Dependency or native linking fails. zstd is optional unless enabled by the selected OCaml toolchain. |

The released Linux binary is fully static. The macOS release carries its native
library linkage. Installing either release therefore does not require the
source-build development packages. Package names vary by operating system and
distribution; verify that `pkg-config` can discover GMP rather than relying on a
command copied for another distribution.

`dune show depexts` names the system packages the locked OCaml dependencies
need, for the lock file you resolved rather than for one distribution. It
reports only those: the C toolchain that compiles them and the runtime programs
in the table above are outside the lock file, so a host that has neither still
fails after installing everything the command prints. On Debian or Ubuntu the
toolchain is `build-essential`; `bwrap` ships as `bubblewrap` and `rg` as
`ripgrep`. macOS runs Seatbelt through the built-in `sandbox-exec` and needs no
Bubblewrap.

Confirm that each prerequisite is discoverable rather than merely installed:

```sh
cc --version                  # a C toolchain the build can drive
pkg-config --modversion gmp   # GMP is visible to the build
ls /usr/bin/bwrap             # Linux only, and this exact path
command -v rg
```

Use `mentat doctor`, `mentat sandbox status`, and `mentat sandbox explain` to
inspect the resulting host and sandbox posture without making a model request.
`mentat sandbox status` reports `evidence=enforced (linux-bubblewrap ...)` once
Bubblewrap is usable at the required path.

## Release installer

```sh
curl -fsSL https://raw.githubusercontent.com/invariant-hq/mentat/main/scripts/install.sh | sh
```

The installer resolves the latest release unless `MENTAT_VERSION=X.Y.Z` is set,
downloads the archive and `SHA256SUMS` over HTTPS, verifies the archive, and
atomically installs `mentat` to `~/.local/bin`. Set `MENTAT_INSTALL_DIR` or pass
`--dir DIR` to select another directory. The downloaded binary is run once
before it is published, so an archive that cannot execute on this machine never
replaces a working installation.

`curl` is used when present and `wget` otherwise. The BusyBox `wget` found on
minimal images cannot report which release is the latest one; pass
`--version X.Y.Z` on those hosts.

The installer refuses to run under `sudo` when it would install into a home
directory, because `sudo` resolves `$HOME` to root's on most systems and the
invoking shell would never see the command. To install for every user on the
machine, name the directory, which also leaves shell startup files untouched:

```sh
curl -fsSL https://raw.githubusercontent.com/invariant-hq/mentat/main/scripts/install.sh | \
  sudo sh -s -- --dir /usr/local/bin
```

If the install directory is not already present in the current `PATH`, the
installer appends one line to a shell startup file selected from `$SHELL`:

| Shell | File and line |
| --- | --- |
| zsh | `${ZDOTDIR:-$HOME}/.zshrc`, with `export PATH="DIR:$PATH"` |
| bash | The first existing file among `~/.bashrc`, `~/.bash_profile`, and `~/.profile`; otherwise `~/.bashrc` |
| fish | `${XDG_CONFIG_HOME:-$HOME/.config}/fish/config.fish`, with `fish_add_path DIR` |
| other or unset | `~/.profile`, with the POSIX `export` line |

The installer labels the appended block `Added by the mentat installer` and
does not add the same line twice. Restart the shell after a file is changed. If
a custom install directory contains characters that require shell quoting, the
installer leaves startup files unchanged and asks you to update `PATH` manually.

To install without editing a shell startup file, pass the script argument
through `sh`:

```sh
curl -fsSL https://raw.githubusercontent.com/invariant-hq/mentat/main/scripts/install.sh | \
  sh -s -- --no-modify-path
```

The flag prevents shell startup-file edits and prints the `export PATH=...`
line to apply yourself. In GitHub Actions, when `$GITHUB_PATH` is set, the
installer appends the install directory to that runner-managed path file instead
of editing a shell startup file.

## Homebrew

```sh
brew install invariant-hq/tap/mentat
```

Homebrew installs the release binary and generates bash and zsh completions. It
does not install `rg`, Linux `/usr/bin/bwrap`, or `llama-server` as Mentat
runtime dependencies; provide the feature-specific prerequisites from the
matrix above.

## Build from source

Dune package management provisions OCaml 5.5 and the OCaml package dependencies.
The native prerequisites in the matrix must already be visible to the compiler,
linker, and `pkg-config`-compatible command.

```sh
git clone https://github.com/invariant-hq/mentat.git
cd mentat
dune pkg lock
dune build
```

The checkout pins the Mosaic packages directly to one public source revision so
locking does not depend on a maintainer's local checkout.

Run from the checkout with `dune exec mentat --`. To install under a user prefix:

```sh
dune install --prefix ~/.local
```

Ensure `~/.local/bin` is on `PATH` when using that prefix. Source builds still
need `/usr/bin/bwrap` at runtime on Linux under the default enforced sandbox,
and still need `rg` for `search_text`.

## Local inference

The built-in `local` provider manages model weights and a `llama-server`
subprocess. It resolves the executable from `MENTAT_LOCAL_SERVER_BINARY` first,
then `PATH`. The override may be an explicit path or a command name to resolve
on `PATH`.

```sh
mentat doctor
mentat models --provider local
mentat models download local/MODEL
mentat models select local/MODEL
```

`mentat models download` retrieves a cataloged GGUF artifact; the first local
request also downloads missing managed weights automatically. This download is
network activity even though inference stays on loopback. See
[Data leaving your machine](security.md#data-leaving-your-machine) and
[Providers and accounts](providers.md#selecting-models).
