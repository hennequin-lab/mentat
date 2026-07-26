Skill discovery reports what was found and how it is classified. `list` is a
pure offline view over one discovery snapshot: config and trust resolution, no
engine and no model call.

  $ use_trusted_workspace

Built-ins are switched off so the golden depends only on the project skill under
test.

  $ mentat config set skills.builtin false >/dev/null
  $ mkdir -p .mentat/skills/greet
  $ cat > .mentat/skills/greet/SKILL.md <<'MD'
  > ---
  > description: Greet the user warmly.
  > ---
  > Say hello to the user.
  > MD

The human view reports the policy header and the active project skill with its
kind and description.

  $ mentat skills list | grep -o 'Skills: enabled'
  Skills: enabled
  $ mentat skills list | grep -o 'builtin: false  project: true'
  builtin: false  project: true
  $ mentat skills list | grep -o 'greet  project'
  greet  project
  $ mentat skills list | grep -o 'Greet the user warmly.'
  Greet the user warmly.

The JSON view carries the config, the typed catalog and skill projections, and
the warnings, under one enveloped shape.

  $ mentat skills list --json > list.json
  $ mentat_cram json .type list.json
  skills_list
  $ mentat_cram json .config.enabled list.json
  true
  $ mentat_cram json .config.builtin list.json
  false
  $ mentat_cram json .skills[0].name list.json
  greet
  $ mentat_cram json .skills[0].kind list.json
  project
  $ mentat_cram json .skills[0].state list.json
  active

Switching skills off entirely empties the snapshot and the human view says so.

  $ mentat config set skills.enabled false >/dev/null
  $ mentat skills list | grep -o 'disabled (skills.enabled)'
  disabled (skills.enabled)
