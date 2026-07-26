(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The [serve] command: run (or stop) the per-user mentat daemon. Opt-in; the
    in-process client stays the default and [--attach] is the daemon's consumer.
*)

val cmd : int Cmdliner.Cmd.t
