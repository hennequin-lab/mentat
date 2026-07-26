(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Input-only planned-change evidence for the write tools' permission requests.

    Evidence bytes originate from the decoded tool input — never from filesystem
    reads — so permission planning stays deterministic across block and resume,
    and rendering evidence can never disclose file content the model has not
    already supplied. {!Textdiff} fits that law: it renders caller-supplied
    strings and reads nothing. Every render is display-bounded, since it runs on
    model-controlled input before any review. *)

val creation :
  path:Mentat_workspace.Path.t -> string -> Mentat_permission.Request.Change.t
(** [creation ~path contents] is create-change evidence for writing [contents]
    to [path]: an addition diff plus an exact added-line count. *)

val replacement :
  path:Mentat_workspace.Path.t -> string -> Mentat_permission.Request.Change.t
(** [replacement ~path contents] is overwrite evidence for replacing [path] with
    [contents]. The current contents cannot be read under the input-only rule,
    so it renders a full addition and omits the removal count. *)

val modify :
  path:Mentat_workspace.Path.t ->
  before:string ->
  after:string ->
  Mentat_permission.Request.Change.t
(** [modify ~path ~before ~after] is modify evidence for the [before]→[after]
    text change at [path], with exact added/removed counts when the display
    limits do not elide the hunks. *)
