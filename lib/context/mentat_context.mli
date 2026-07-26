(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Workspace instructions and skills.

    This library owns the two request-scoped discovery surfaces the executable
    injects into a turn: the workspace {!Context} — the system prompt, the
    working directory, and the [AGENTS.md] instruction files — and the {!Skills}
    catalog and on-demand [skill] tool. Both take one filesystem snapshot and
    then answer as pure views, so an inspector and the execution prelude cannot
    disagree.

    [Frontmatter] is the YAML-header parser both surfaces share with the
    build-time builtin-skill validator. *)

module Context = Context
(** Request-scoped workspace instructions. *)

module Skills = Skills
(** Skill discovery, catalog rendering, and the on-demand [skill] tool. *)

module Commands = Commands
(** User-invoked prompt-template command discovery and [$]-expansion. *)
