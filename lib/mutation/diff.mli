(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Netted display diffs over the mutation ledger.

    A diff collapses a selection's recorded change rows to net per-path
    transitions ({!State.net}) and renders each path's first-before to
    last-after line diff. It is display evidence, not replay authority: the
    {!Revert} lifecycle owns what a revert restores, and {!State.revertability}
    owns the safety answer this projection only carries. Image bytes are
    resolved through a caller [resolve] port — the ledger references content by
    hash but does not carry the bytes — so the computation performs no IO of its
    own and is a total function of its arguments.

    It additionally previews the Rung-2 merge: for a superseded path it
    computes, from the same resolved bytes plus an optional current read,
    whether a revert-now would merge cleanly. This is the byte-bearing
    feasibility a live surface needs and a plain revertability answer cannot
    carry. *)

module Merge_status : sig
  (** One netted path's Rung-2 merge preview. *)

  type t =
    | Not_superseded
        (** The recorded head still equals the selection's net-after: an
            ordinary revert, no merge involved. *)
    | Clean of Mentat_digest.Content_ref.t
        (** A superseded path whose three-way merge is clean; the reference
            identifies the merged image a revert-now would restore. *)
    | Conflict of Textdiff.Merge.Region.t list
        (** A superseded path whose merge conflicts; the regions carry the
            git-style markers a resolver renders. *)
    | Unresolvable of string
        (** A superseded path whose merge could not be computed — a bound
            exceeded, a missing blob, or a non-text current read. *)
end

module Entry : sig
  type t = private {
    path : Mentat_workspace.Path.t;
    operation : [ `Added | `Deleted | `Modified ];
        (** The forward transition the ledger recorded: a net-before [Missing]
            image is [`Added], a net-after [Missing] image is [`Deleted], two
            texts are [`Modified]. *)
    contiguous : bool;
        (** [false] iff an unrecorded writer intervened between the netted
            deltas — carried from {!Change.Net.entry}. *)
    hunks : Textdiff.Hunk.t list option;
        (** The before/after line diff, or [None] when an image's bytes were
            unavailable ([resolve] returned [None]) or a diff bound was
            exceeded. *)
    merge : Merge_status.t;  (** The Rung-2 merge preview for this path. *)
    sources : Change.Id.t list;
        (** The contributing change rows, in ledger order. *)
  }
  (** The type for one netted path's diff. *)
end

type t = private {
  entries : Entry.t list;  (** One per netted path, in first-seen path order. *)
  revertability : Revertability.t;
      (** The selection's safety answer, carried for a display trailer. *)
  additions : int;  (** Added lines summed across the rendered hunks. *)
  deletions : int;  (** Removed lines summed across the rendered hunks. *)
}
(** The type for a netted display diff. *)

val compute :
  state:State.t ->
  selection:Revert.Selection.t ->
  ?current:(Mentat_workspace.Path.t -> Mentat_edit.Observed.t option) ->
  resolve:(Mentat_digest.Content_ref.t -> string option) ->
  unit ->
  t
(** [compute ~state ~selection ~resolve ()] nets [selection] against [state]
    ({!State.net}), reads its safety answer ({!State.revertability}), and for
    each netted path resolves its net-before and net-after image bytes through
    [resolve] — a [Missing] image is the empty side — and diffs them into hunks.
    A path whose bytes [resolve] cannot supply carries [None] hunks rather than
    failing the whole diff, and its lines contribute nothing to [additions] or
    [deletions], which sum the rendered hunks (not the recorded change counts).
    [resolve] must return the exact recorded bytes for a reference it resolves,
    or [None]; the bytes are trusted, as the content-addressed store guarantees
    them.

    For a superseded path it also computes {!Merge_status}. [current] supplies
    the current read that feeds the merge's [ours] side; absent (or absent for a
    given path) it falls back to the recorded head, which makes the preview
    advisory — an unrecorded writer would make the applied merge differ. A live
    surface that will offer to apply must supply [current] from disk. *)

module Feasibility : sig
  (** Whether reverting a selection now would succeed, and carrying what — the
      byte-bearing fold of the entries' {!Merge_status}. Never stored, never on
      the settlement fact. *)

  type outcome =
    | Clean of Mentat_digest.Content_ref.t  (** The merged image reference. *)
    | Conflict of Textdiff.Merge.Region.t list
        (** The conflict regions to resolve. *)
    | Unresolvable of string  (** Why the merge could not be computed. *)

  type path_merge = { path : Mentat_workspace.Path.t; outcome : outcome }
  (** One superseded path's merge outcome. *)

  type t =
    | Trivial  (** No superseded path: a plain revert applies. *)
    | Mergeable of path_merge list
        (** Every superseded path merges clean; a revert-now applies. *)
    | Blocked of path_merge list
        (** At least one superseded path conflicts or cannot resolve; a
            revert-now would refuse. Carries all superseded paths so a surface
            can render the clean and conflicted alike. *)

  val ready : t -> bool
  (** [ready t] is [true] for {!Trivial} and {!Mergeable} — a revert-now would
      apply. *)
end

val feasibility : t -> Feasibility.t
(** [feasibility diff] folds [diff]'s per-path {!Merge_status} into the
    selection-wide answer. Accurate only when [diff] was computed with a
    disk-supplied [current] (otherwise the underlying merges are advisory). *)
