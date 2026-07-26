(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Shared edit-failure reporting for the native-write tools.

    The write tools apply through {!Mentat_workspace_io.Edit.apply}, whose
    failure is a {!Mentat_edit.Apply_error.t} wrapping a {!Mentat_edit.Error.t}.
    This module classifies that error onto the coarse tool failure class,
    renders its message, and lowers it to a {!Mentat_tool.Result.t}, so the edit
    and patch tools report failures identically. *)

val failure : Mentat_edit.Error.t -> Mentat_tool.Result.failure
(** [failure error] is the coarse tool failure class for [error]: a stale
    precondition is [`Stale], a missing modify target is [`Not_found], other
    plan violations are [`Invalid_input], and an I/O or out-of-workspace fault
    is [`Failed]. *)

val message : Mentat_edit.Error.t -> string
(** [message error] is a human-readable diagnostic naming the target path. *)

val failed : Mentat_edit.Error.t -> 'a Mentat_tool.Result.t
(** [failed error] is
    [Mentat_tool.Result.failed (failure error) (message error)]. *)

val failed_apply : Mentat_edit.Apply_error.t -> 'a Mentat_tool.Result.t
(** [failed_apply error] is {!failed} of [error]'s underlying
    {!Mentat_edit.Apply_error.error}. *)

val applied :
  Mentat_workspace_io.t -> Mentat_edit.t -> output:'a -> 'a Mentat_tool.Result.t
(** [applied workspace_io edit ~output] applies [edit] to the workspace and, on
    success, is the completed result carrying [output]; a failure is the edit's
    {!failed_apply} tool result. It folds the apply-then-classify tail every
    native-write tool shares. *)
