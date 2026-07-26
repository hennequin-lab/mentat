TRUST-2 — concurrent trust commands serialize the shared trust-store update, so
each independently recorded workspace survives the reload-modify-write cycle.

  $ use_untrusted_workspace
  $ mkdir -p writers/{1..12}/.git
  $ mkfifo start
  $ exec 3<>start
  $ pids=()
  $ for root in writers/*/; do
  >   (read -r <&3; mentat trust "$root" >"${root%/}.out" 2>&1) &
  >   pids+=("$!")
  > done
  $ for pid in "${pids[@]}"; do printf 'go\n' >&3; done
  $ result=0
  $ for pid in "${pids[@]}"; do wait "$pid" || result=1; done
  $ exec 3>&-
  $ rm start
  $ echo "writers=$result"
  writers=0

The final store is valid and contains every update; no writer lost an entry that
another writer committed while contending for the lock.

  $ trusted=0
  $ for root in writers/*/; do
  >   if mentat config show --cwd "$root" | grep -q '^workspace_trust=trusted$'; then
  >     trusted=$((trusted + 1))
  >   fi
  > done
  $ echo "trusted=$trusted"
  trusted=12
