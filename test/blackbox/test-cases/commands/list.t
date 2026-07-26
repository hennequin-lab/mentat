Custom command discovery reports what was found and how it is classified.
`list` and `show` are pure offline views over one discovery snapshot: config and
trust resolution, no engine and no model call.

  $ use_trusted_workspace

A project command with a description and an argument hint, plus a namespaced one
from a subdirectory.

  $ mkdir -p .mentat/commands/git
  $ cat > .mentat/commands/review.md <<'MD'
  > ---
  > description: Review a pull request.
  > argument-hint: <pr>
  > ---
  > Review PR $1 for bugs.
  > MD
  $ cat > .mentat/commands/git/commit.md <<'MD'
  > Commit the staged changes.
  > MD

The human view reports the policy header and the active project commands with
their kind, and a description when present.

  $ mentat commands list | grep -o 'Commands: enabled'
  Commands: enabled
  $ mentat commands list | grep -o 'project: true  compat: true'
  project: true  compat: true
  $ mentat commands list | grep -o 'review  project'
  review  project
  $ mentat commands list | grep -o 'Review a pull request.'
  Review a pull request.
  $ mentat commands list | grep -o 'git:commit  project'
  git:commit  project

The JSON view carries the config, the typed command projections, and warnings.

  $ mentat commands list --json > list.json
  $ mentat_cram json .type list.json
  commands_list
  $ mentat_cram json .config.enabled list.json
  true
  $ mentat_cram json .commands[0].name list.json
  git:commit
  $ mentat_cram json .commands[0].kind list.json
  project
  $ mentat_cram json .commands[0].state list.json
  active

`show` prints the resolved template of one active command, including its raw
body with the unexpanded placeholders.

  $ mentat commands show review | grep -o 'name: review'
  name: review
  $ mentat commands show review | grep -oF 'Review PR $1 for bugs.'
  Review PR $1 for bugs.

An unknown command name exits nonzero.

  $ mentat commands show nope 2>err.txt
  [1]
  $ grep -o 'unknown command' err.txt
  unknown command

Switching commands off entirely empties the snapshot and the human view says so.

  $ mentat config set commands.enabled false >/dev/null
  $ mentat commands list | grep -o 'disabled (commands.enabled)'
  disabled (commands.enabled)
