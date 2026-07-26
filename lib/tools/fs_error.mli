(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Shared workspace-observation failure reporting for the read-side tools.

    Maps a {!Mentat_workspace_io.File_error.t} — the failure the capability
    returns for {!Mentat_workspace_io.File} reads — onto the coarse tool failure
    class every observation tool reports, and renders a human-facing message.
    Only the classification and the generic message are shared; a tool with
    tool-specific wording renders its own. *)

val failure : Mentat_workspace_io.File_error.t -> Mentat_tool.Result.failure
(** [failure error] is the coarse tool failure class for [error]: an absent
    target is [`Not_found], a workspace-relation failure or an escape is
    [`Invalid_input], an oversized read is [`Invalid_input], and a genuine I/O
    fault is [`Failed]. *)

val message : Mentat_workspace_io.File_error.t -> string
(** [message error] is a human-readable diagnostic naming the offending path.
    The wording is for models and logs, not a stable matching surface. *)

val failed :
  ?message:string -> Mentat_workspace_io.File_error.t -> 'a Mentat_tool.Result.t
(** [failed ?message error] is a {!Mentat_tool.Result.failed} carrying
    {!failure} of [error] and [message] (defaulting to {!message} of [error]). A
    caller supplies [message] when its wording is tool-specific. *)
