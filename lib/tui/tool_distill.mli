(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Typed tool results as transcript blocks.

    This module is the mandatory Tier-0 presentation boundary. It is total over
    the next tool lifecycle and depends only on durable claim and result data.
    Completed built-in tools decode their compact durable summary facts through
    [mentat.tools.output]. A missing, malformed, or unsupported built-in
    projection renders a short unavailable-details state, never its possibly
    large text as a summary. Failed and interrupted results additionally render
    their structured terminal message. Consequently live and replayed outputs
    have the same presentation without linking executors, workspace IO, or an
    OCaml toolchain into the TUI.

    Process results retain their typed status summary and disclose every
    normalized row from {!Mentat_tool.Output.text} in a last-edge, five-row
    Mosaic viewport. Executable-local shell results use {!local_shell} with that
    same erased result type and Process grammar. They remain local presentation
    history, while durable shell facts continue through {!returned}.

    The exact registered tool name chooses the semantic codec. This module does
    not guess from substrings, inspect executor-private output shapes, or infer
    arguments from accidental input field names. An empty header argument
    remains the honest fallback for extension tools; every durable output row
    appears only as disclosure detail. Mosaic owns its five-row viewport and
    overflow indication.

    Task boards and mutation evidence are not tool-output presentation. Their
    journal facts remain the sole sources rendered by {!Todo_board} and the
    future mutation-evidence translator, so blocks produced here never duplicate
    either one. *)

(** {1 Classification} *)

val verb_of_name : string -> Tool_block.verb
(** [verb_of_name name] classifies the exact registered next tool or fixed host
    verb [name]. Unknown names become [Tool_block.Other name], preserving the
    name instead of pretending to recognize it.

    The table is audited against the declarations in [lib/tools] and the fixed
    dispatch vocabulary in [lib/agent/step/catalog.ml]. It contains no
    compatibility aliases for removed legacy names. *)

(** {1 Lifecycle} *)

val running : Mentat_session.Tool_claim.Started.t -> Tool_block.t
(** [running claim] is the nonblank active block for [claim]. Its outcome dot is
    [Tool_block.Running]. *)

val prepared :
  Mentat_session.Tool_claim.Started.t -> description:string -> Tool_block.t
(** [prepared claim ~description] is the awaiting-approval block for [claim].
    [description] is the durable {!Mentat_tool.Prepared.description}; if it has
    no visible content, {!Tool_block.make} supplies its stable waiting fallback.
    Its outcome dot is [Tool_block.Awaiting]. *)

val returned :
  Mentat_session.Tool_claim.Started.t ->
  Mentat_tool.Output.t Mentat_tool.Result.t ->
  Tool_block.t
(** [returned claim result] is the settled block for [result]. Completed output
    uses a success dot. A recognized built-in output uses its compact durable
    summary; if decoding fails, it states that details are unavailable. Failure
    uses a failure dot and the structured failure message; interruption uses a
    warning dot and the structured interruption reason. A recognized partial
    output contributes its compact summary after that message. Process output
    text and unknown extension text are disclosure detail, not summary text;
    every source row is passed to Mosaic under a declarative five-row viewport.
    Process previews initially show the last edge, matching command-output
    reading order, while extension previews initially show the first. A
    truncated output carries a visible truncation fact.

    A decoded file read excludes its fixed envelope heading from disclosure and
    passes every following durable body and continuation row under the same
    five-row Mosaic viewport. Its summary derives only from the typed
    [Read.File.lines] fact; text is never parsed for status, counts, omissions,
    or continuation behavior.

    Whitespace-only extension output remains settled through the dot-specific
    nonblank summary. An absent built-in projection renders the stable
    unavailable-details summary, never a loading state. *)

val local_shell :
  command:string -> Mentat_tool.Output.t Mentat_tool.Result.t -> Tool_block.t
(** [local_shell ~command result] is the settled executable-local shell block
    for [result]. It uses [command] as the shell header argument and the same
    typed [Process] summary, terminal status, facts, and output-detail grammar
    as a durable [shell] result. No executor value or shell-specific result
    representation crosses this pure presentation boundary. *)

val ambiguous : Mentat_session.Tool_claim.Started.t -> Tool_block.t
(** [ambiguous claim] is a warning-colored, nonblank block stating that the
    callback may have run but no terminal outcome was recorded. Its outcome dot
    is [Tool_block.Ambiguous], distinct from known failure and interruption. *)
