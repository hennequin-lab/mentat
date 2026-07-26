(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The single home for the resolve-then-continue skeleton the tools share, in
    both the permission-planning form ({!with_resolved}) and the run form
    ({!with_resolved_run}). The two differ only in how they treat an
    unresolvable spelling. *)

val with_resolved :
  Mentat_workspace_io.t ->
  string ->
  (Mentat_workspace.Path.t -> Mentat_permission.Request.t list) ->
  Mentat_permission.Request.t list
(** [with_resolved workspace_io spelling k] resolves [spelling] against
    [workspace_io] and, on success, returns the requests [k] computes from the
    resolved path. A spelling that does not resolve yields no requests: the
    requests a call declares are advisory, and an unresolvable spelling re-fails
    when the call runs, so swallowing the resolve error here keeps permission
    planning total without hiding the eventual failure. *)

val with_resolved_run :
  Mentat_workspace_io.t ->
  string ->
  (Mentat_workspace.Path.t -> 'a Mentat_tool.Result.t) ->
  'a Mentat_tool.Result.t
(** [with_resolved_run workspace_io spelling f] resolves [spelling] and runs [f]
    on the resolved path. Unlike {!with_resolved}, this is the run-side form: an
    unresolvable spelling is the tool's own [`Invalid_input] failure carrying
    {!Mentat_workspace.Resolve_error.message}, since a run cannot proceed
    without a target. *)
