Durable permission rules are hand-authored config facts: content-derived ids
that survive file reordering, inspection in evaluation order, and removal that
edits exactly one config file. The fixed product policy is composed at turn time
by the engine and is deliberately not part of this durable-rule surface.

  $ use_trusted_workspace
  $ mkdir -p "$XDG_CONFIG_HOME/mentat"

An empty config has no durable rules.

  $ mentat permission list
  no permission rules

Hand-written rules list in file order, each with its content-derived id, action,
matcher, and source file.

  $ cat > "$XDG_CONFIG_HOME/mentat/config.json" <<'JSON'
  > { "permission": { "rules": { "version": 1, "items": [
  >   { "action": "allow", "matcher": { "type": "path-under-relative", "relative": "notes" } },
  >   { "action": "deny", "matcher": { "type": "path-exact-relative", "relative": ".env" } } ] } } }
  > JSON
  $ mentat permission list | sed -E 's/  +/ /g'
  # RULE ACTION MATCH SOURCE
  1 cce4ddd1a4eb allow path-under-relative relative=notes user $TESTCASE_ROOT/config/mentat/config.json
  2 f7cbde737157 deny path-exact-relative relative=.env user $TESTCASE_ROOT/config/mentat/config.json

The JSON envelope carries each rule's id, full rule codec, source kind, and file
location.

  $ mentat permission list --json | grep -o '"id":"[^"]*"'
  "id":"cce4ddd1a4eb"
  "id":"f7cbde737157"

Reordering the file changes positions but not ids, because identity comes from
content, not list position.

  $ cat > "$XDG_CONFIG_HOME/mentat/config.json" <<'JSON'
  > { "permission": { "rules": { "version": 1, "items": [
  >   { "action": "deny", "matcher": { "type": "path-exact-relative", "relative": ".env" } },
  >   { "action": "allow", "matcher": { "type": "path-under-relative", "relative": "notes" } } ] } } }
  > JSON
  $ mentat permission list | sed -E 's/  +/ /g' | sed -n '2,3p'
  1 f7cbde737157 deny path-exact-relative relative=.env user $TESTCASE_ROOT/config/mentat/config.json
  2 cce4ddd1a4eb allow path-under-relative relative=notes user $TESTCASE_ROOT/config/mentat/config.json

Removal addresses one rule by id and edits only the durable file.

  $ mentat permission remove f7cbde737157
  removed rule f7cbde737157 from user $TESTCASE_ROOT/config/mentat/config.json
  $ cat "$XDG_CONFIG_HOME/mentat/config.json"
  {"permission":{"rules":{"version":1,"items":[{"action":"allow","matcher":{"type":"path-under-relative","relative":"notes"}}]}}}

A unique id prefix resolves to the same rule; removing the last rule collapses
the empty container.

  $ mentat permission remove cce4
  removed rule cce4ddd1a4eb from user $TESTCASE_ROOT/config/mentat/config.json
  $ cat "$XDG_CONFIG_HOME/mentat/config.json"
  {}

A well-formed id that matches nothing is a runtime error (exit 1); a malformed id
is a usage error (exit 2).

  $ mentat permission remove ffffffffffff
  mentat: no durable permission rule matching ffffffffffff; run `mentat permission list` to see rule ids
  [1]
  $ mentat permission remove NOThex
  mentat: permission rule id "NOThex" must be lowercase hexadecimal
  [2]

Obsolete unversioned arrays and duplicate ids fail at the configuration
boundary, so the durable-rule surface never lists a shape it could not remove.

  $ cat > "$XDG_CONFIG_HOME/mentat/config.json" <<'JSON'
  > { "permission": { "rules": [] } }
  > JSON
  $ mentat permission list
  mentat: $TESTCASE_ROOT/config/mentat/config.json permission.rules uses the obsolete unversioned array format; use { "version": 1, "items": [...] }
  [1]

  $ cat > "$XDG_CONFIG_HOME/mentat/config.json" <<'JSON'
  > { "permission": { "rules": { "version": 1, "items": [
  >   { "action": "deny", "matcher": { "type": "path-exact-relative", "relative": ".env" } },
  >   { "action": "deny", "matcher": { "type": "path-exact-relative", "relative": ".env" } } ] } } }
  > JSON
  $ mentat permission list
  mentat: $TESTCASE_ROOT/config/mentat/config.json permission.rules contains duplicate rule f7cbde737157
  [1]
