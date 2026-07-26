(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Deterministic fixtures for the benchmark suites.

    The one place fixture sizes are pinned: a size axis moves here and every
    suite that reads it moves with it. All content is deterministic — no
    wall-clock, no unseeded randomness, no environment — so a baseline encodes
    the code's cost, never the day it ran. *)

module Bytes : sig
  (** Deterministic byte blobs at the checkpoint-hash size axis. *)

  val small : string
  (** 1 KiB of deterministic bytes. *)

  val medium : string
  (** 64 KiB of deterministic bytes. *)

  val large : string
  (** 1 MiB of deterministic bytes. *)
end

module Diff_fixture : sig
  (** Before/after file changes at the diff-render size axis. *)

  val small_change : Textdiff.File_change.t
  (** A one-line modification in a three-line file. *)

  val medium_change : Textdiff.File_change.t
  (** A single change inside a 200-line file. *)

  val multi_changes : Textdiff.File_change.t list
  (** Twenty modified files, one change each — a whole-turn multi-file edit. *)
end

module Session_fixture : sig
  (** Sessions of a pinned event count, with deterministic content and a pinned
      creation time. The resume axis. *)

  val event_sizes : (string * int) list
  (** The resume axis: [("100", 100); ("1k", 1000); ("10k", 10000)]. *)

  val build : ?id:string -> events:int -> unit -> Mentat_session.t
  (** [build ~events ()] is a session grown to [events] semantic events — a
      user-message log with deterministic content and creation time pinned to
      the epoch. [id] defaults to ["bench"]; the list bench passes distinct ids.
  *)

  val encoded : Mentat_session.t -> string
  (** [encoded session] is [session]'s persisted JSON bytes (the exact document
      a reopen decodes), via {!Mentat_session.jsont}. *)
end

module Ledger : sig
  (** Mutation ledgers with total events and edits-per-claim as independent
      axes. The claim-settle and revert-preview inputs. *)

  val settle_axis : (string * (int * int)) list
  (** [(label, (claims, edits_per_claim))] rows for the settle bench: a fixed
      claim count with a rising edits-per-claim (the O(k^2) probe) and a rising
      claim count at one edit each (linear reference). *)

  val build : claims:int -> edits_per_claim:int -> Mentat_mutation.Event.t list
  (** [build ~claims ~edits_per_claim] is a valid ledger of [claims] claims, each
      recording [edits_per_claim] applies to distinct paths. Total events =
      [claims * edits_per_claim]. *)

  type revert_case = {
    state : Mentat_mutation.State.t;
    selection : Mentat_mutation.Revert.Selection.t;
    evidence : Mentat_mutation.Revert.Evidence.t;
    resolve : Mentat_digest.Content_ref.t -> string option;
  }
  (** A prepared revert input: the replayed ledger, a whole-history selection,
      the plan-time evidence (current reads + resolvable blobs), and the blob
      resolver {!Mentat_mutation.Diff.compute} reads. *)

  val superseded_axis : (string * int) list
  (** The revert axis: percent of paths a later claim supersedes (0/50/100),
      exercising the three-way merge in preparation and the merge preview in the
      diff. *)

  val revert_case : superseded_pct:int -> revert_case
  (** [revert_case ~superseded_pct] builds a fixed-size ledger where
      [superseded_pct] of its paths are edited twice (the second edit moving the
      head past the first's net-after) with the current read still at the head,
      so preparation must merge. *)
end

module Workspace : sig
  (** A fixed on-disk workspace tree for the context/skills load cases. *)

  val materialize : root:string -> unit
  (** [materialize ~root] writes the pinned fixture tree under [root]: an
      [AGENTS.md], a small source tree, and a [.mentat/skills] skill. Uses only
      [Unix]/[Out_channel]; safe to call before entering an Eio run. *)
end

module Enter : sig
  (** Pure enter-overhead inputs: a session whose state feeds
      {!Mentat_session.State.model_transcript} and a model for
      {!Mentat_llm.Request.make}. *)

  val session : Mentat_session.t
  (** A fixed session for the transcript projection. *)

  val model : Mentat_llm.Model.t
  (** A fixed model identity for request construction. *)
end

module Transcript_fixture : sig
  (** TUI transcripts of a pinned block count. The node-construction axis. *)

  val block_sizes : (string * int) list
  (** The transcript axis: [("100", 100); ("1k", 1000); ("5k", 5000)]. *)

  val build : blocks:int -> Mentat_tui.Transcript.t
  (** [build ~blocks] is a transcript of [blocks] settled blocks, cycling
      user / assistant / reasoning / tool constructors, deterministic content.
  *)

  val palette : Mentat_tui.Theme.Palette.t
  (** The default palette {!Mentat_tui.Transcript.view} renders against. *)
end
