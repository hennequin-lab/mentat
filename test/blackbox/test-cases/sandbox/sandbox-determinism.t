GATED-UPSTREAM (NEW-1, #18). Parked body: `sandbox explain --json`'s profile_hash
must be byte-stable across two identical invocations of a confined route. Today the
suite pins danger-full-access (unconfined), so the hash is never emitted, and the
product fix — making profile_hash a pure function of the sealed policy rather than of
a temp path — is upstream and currently UNTRACKED. The test seam is
`MENTAT_TEST_SANDBOX_CONFINED=1`, forcing a confined seal over a canonical, sanitized
workspace root so two identical invocations hash identical inputs. This is the one
remaining gated-upstream row; it flips when the purity fix lands. The `blackbox-gated`
alias + `MENTAT_TEST_GATED` guard that keeps it off the default `blackbox` alias lives
in this directory's dune (see sandbox/dune); it neither runs nor false-greens until
the upstream fix and the `MENTAT_TEST_SANDBOX_CONFINED` seam land.

  $ use_trusted_workspace
  $ MENTAT_TEST_SANDBOX_CONFINED=1 mentat sandbox explain --json --cwd "$PWD" | mentat_cram json .profile_hash > hash-a.txt
  $ MENTAT_TEST_SANDBOX_CONFINED=1 mentat sandbox explain --json --cwd "$PWD" | mentat_cram json .profile_hash > hash-b.txt
  $ diff hash-a.txt hash-b.txt && echo stable
