(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Durable undo-boundary updates.

    An undo boundary is the durable half of the TUI [/undo]/[/redo] affordance:
    while armed, it marks the point in the transcript before which the model
    view is derived, excluding the crossed suffix, exactly as {!Compaction}
    excludes a summarized prefix — two views over one journal, nothing deleted.
    An {!Update.Armed} records the boundary at (before) an anchor turn together
    with the stable id of the mutation-ledger revert that took the working tree
    back to that turn's state; an {!Update.Released} clears it.

    Latest-wins, like {!Queue.Update} and {!Compaction}: the folded state is the
    last update, an armed record or none. A commit is not an update — it
    physically truncates the crossed turns and their boundary events out of the
    journal, after which the fold sees no update at all.

    The revert is carried as its stable {b string} form
    ({!Mentat_mutation.Revert.Id.to_string}), never the typed id: the session
    layer sits below the mutation layer and cannot name a mutation type. The
    typed id is reconstructed at the mutation boundary when a widen, narrow, or
    cancel must un-revert. *)

(** Undo-boundary updates. *)
module Update : sig
  (** The type for one undo-boundary update. *)
  type t = private
    | Armed of { anchor : Turn.Id.t; revert : string option }
        (** The boundary was set or moved to just before [anchor]. [revert] is
            the stable string id of the ledger revert that took the working tree
            back, or [None] when the crossed turns recorded no edits.
            Reversible. *)
    | Released  (** The boundary was cleared: [/redo] to empty, or cancel. *)

  val armed : anchor:Turn.Id.t -> ?revert:string -> unit -> t
  (** [armed ~anchor ?revert ()] records the boundary at [anchor], carrying the
      optional ledger-revert string id [revert].

      Raises [Invalid_argument] if [revert] is present and empty. *)

  val released : t
  (** [released] clears the boundary. *)

  val anchor : t -> Turn.Id.t option
  (** [anchor t] is the armed anchor turn, or [None] when [t] is {!released}. *)

  val revert : t -> string option
  (** [revert t] is the armed revert's string id, or [None] when [t] is
      {!released} or the crossed turns recorded no edits. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same update. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an update for diagnostics. The output is not stable storage
      syntax. *)

  val jsont : t Jsont.t
  (** [jsont] maps updates to JSON objects by a per-arm tag ([armed],
      [released]), rejecting unknown tags, unknown members, and an empty
      [revert]. *)
end
