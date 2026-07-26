(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Filesystem tools.

    The family contains the workspace's read, search, and mutation tools. Every
    effect crosses {!Mentat_workspace_io}; the modules expose no ambient
    filesystem authority. The apply-patch grammar and text applier are private
    implementation details of {!Apply_patch} and deliberately absent from this
    interface. *)

module Read_file = Read_file
(** Read a file or list a directory. *)

module Search_text = Search_text
(** Search file contents with a bounded external [rg] process. *)

module Glob = Glob
(** Find workspace paths by walking through the workspace capability. *)

module Write_file = Write_file
(** Create or replace one complete file. *)

module Edit_file = Edit_file
(** Replace one exact text occurrence in a file. *)

module Apply_patch = Apply_patch
(** Apply a multi-file patch document. *)
