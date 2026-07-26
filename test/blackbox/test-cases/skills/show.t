`skills show NAME` prints one active skill's metadata and full guidance; an
unknown name is a runtime error listing the available skills.

  $ use_trusted_workspace
  $ mentat config set skills.builtin false >/dev/null
  $ mkdir -p .mentat/skills/greet
  $ cat > .mentat/skills/greet/SKILL.md <<'MD'
  > ---
  > description: Greet the user warmly.
  > ---
  > Say hello to the user.
  > MD

The human view carries the name, kind, and the guidance text.

  $ mentat skills show greet | grep -o 'name: greet'
  name: greet
  $ mentat skills show greet | grep -o 'kind: project'
  kind: project
  $ mentat skills show greet | grep -o 'Say hello to the user.'
  Say hello to the user.

The JSON view carries the typed skill projection and the raw skill text.

  $ mentat skills show --json greet > show.json
  $ mentat_cram json .type show.json
  skills_show
  $ mentat_cram json .skill.name show.json
  greet
  $ mentat_cram json .text show.json | grep -o 'Say hello to the user.'
  Say hello to the user.

An unknown skill is a runtime error (exit 1) that lists the available names.

  $ mentat skills show nope 2>&1
  mentat: unknown skill "nope"; available skills: greet
  [1]
