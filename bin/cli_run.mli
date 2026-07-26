(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The [run] command group: start (default), resume, and reply. Headless turns
    use the client without token streaming and print final text once at turn
    end. Reply targets one exact pending decision and continues its turn; plan
    approval continues through the admitted Build turn. *)

val cmd : int Cmdliner.Cmd.t
