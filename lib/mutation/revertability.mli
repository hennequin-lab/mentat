(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The single-owner revert-safety answer, derived from validated mutation
    evidence: history continuity, opaque observations, and unresolved reverts
    all enter the answer. Query it through {!State.revertability}.

    {b The law, defined once:} [Unavailable] means sufficient evidence proves
    the requested operation cannot safely proceed as asked; [Incomplete] means
    the evidence cannot prove whether it can. [Unavailable] outranks
    [Incomplete]. Blob availability is deliberately not an input: under the
    no-GC rule a missing blob is integrity corruption, reported at preparation
    where bytes are actually resolved, so the replayed answer is identical
    before and after restart. *)

(** The type for one degradation reason. The reasons are the branching surface;
    no caller matches prose. Only {!Superseded} is proof — it alone yields
    [Unavailable]; every other reason leaves the answer [Incomplete]. *)
type reason =
  | Superseded of { path : Mentat_workspace.Path.t; by : Change.Id.t }
      (** The selection's net-after for [path] is not the ledger's recorded head
          for [path]: a later recorded change outside the selection would be
          silently undone. Proven impossible as asked; the surface may offer
          widening the selection. *)
  | Non_contiguous of { path : Mentat_workspace.Path.t }
      (** The selection's recorded deltas for [path] do not join: an unrecorded
          writer intervened. Revert requires the explicit override. *)
  | Observed of { claim : Mentat_session.Tool_claim.Id.t }
      (** A tool-observed window overlaps the selection: paths changed without
          recorded transitions, so the evidence cannot prove what a revert of
          that span restores. *)
  | Uncertain of {
      claim : Mentat_session.Tool_claim.Id.t;
      path : Mentat_workspace.Path.t;
    }
      (** A recorded apply under [claim] attempted to mutate [path] and did not
          confirm: the on-disk state may or may not carry the write, so the
          evidence cannot prove what a revert touching [path] restores. Unlike
          {!Observed} the causal tie is known — the apply is recorded — but the
          outcome is not. *)
  | Unresolved_revert of Revert_data.Id.t
      (** A started revert has no settlement; history is in an unknown
          intermediate state until recovery settles it. *)

(** The type for the revert-safety answer. Reasons accumulate; the arm is the
    worst case found. *)
type t =
  | Available
  | Unavailable of reason list  (** Non-empty. *)
  | Incomplete of reason list  (** Non-empty. *)

val message : t -> string
(** [message t] is a human-readable summary. Presentation only; no caller
    matches on it. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same answer. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats {!message} output. *)

val jsont : t Jsont.t
(** [jsont] maps answers to JSON objects by a per-arm tag, rejecting unknown
    tags and empty reason lists. *)
