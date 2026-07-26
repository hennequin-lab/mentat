(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Diagnostic nouns for filesystem entry kinds.

    These render a stat entry kind as the tool-facing noun a diagnostic uses.
    The vocabulary is model-visible, so it lives at the tools layer rather than
    in the workspace I/O library, keeping that library free of presentation. *)

val kind_name : Eio.File.Stat.kind -> string
(** [kind_name kind] is the short human-readable noun for a stat entry [kind] —
    for example ["regular file"], ["directory"], or ["symbolic link"] — that a
    tool reports when a path resolves to the wrong kind of filesystem entry. *)
