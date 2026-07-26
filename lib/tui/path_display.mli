(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Home-relative path labels for the TUI.

    This module only chooses the semantic spelling of an absolute path. Mosaic
    owns the label's measurement and any presentation shortening required by its
    allocated layout. *)

val home_relative : home:Lpath.Abs.t option -> Lpath.Abs.t -> string
(** [home_relative ~home path] is ["~"] when [path] equals [home], and
    ["~/rest"] when [path] is below it. It is the absolute spelling of [path]
    when [home] is [None] or [path] is outside [home]. *)
