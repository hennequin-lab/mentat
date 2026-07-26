(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Deriving a change's stable identifier from its provenance.

    Private to the mutation library. A change id is content-derived so that
    replay reconstructs the same id from the same provenance rather than minting
    a fresh tag — the derivation is the integrity check {!Event.of_edit} re-runs
    to reject a forged id. The two constructors cover the two provenances a
    change can have: an edit under a tool claim, and a restoration under a
    revert. *)

val for_claim : claim:string -> ordinal:int -> Mentat_workspace.Path.t -> string
(** [for_claim ~claim ~ordinal path] is the change id for the [ordinal]-th edit
    of [path] made under tool claim [claim]. *)

val for_revert : revert:string -> Mentat_workspace.Path.t -> string
(** [for_revert ~revert path] is the change id for the restoration of [path]
    performed by revert [revert]. *)
