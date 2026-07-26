(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The [config] command group: path, show, get, set, unset, init, validate. *)

val cmd : int Cmdliner.Cmd.t

val resolved_json : Composition.t -> Jsont.json
(** [resolved_json t] is the effective configuration with each value's
    provenance, as [config show --json --origins] renders it. Secret-bearing
    fields are [[REDACTED]] because the library's view projects them that way,
    so {!Cli_report} can bundle this without a redaction pass of its own. *)
