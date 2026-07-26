(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Durable file-level change facts.

    A change is one applied file transition with content-addressed before/after
    images and the line counts honest replay rendering needs. It is a leaf
    value: it has no supported constructor — introduction is the library's own,
    through the {{!section-internal}Internal} seam — and carries no source,
    session, turn, or checkpoint members — provenance lives on the enclosing
    {!Event.t}. Changes are introduced only by the event lowerings
    ({!Event.of_edit}, {!Event.of_attempt}), by revert settlement
    ({!Revert.settle}), and by decode, which run the same validation. *)

module Id : sig
  type t
  (** The type for change identifiers. Derived internally from the enclosing
      source coordinates and the path — never accepted from callers:
      [H("mentat.mutation.change.v2", "claim", claim, ordinal, path)] for the
      ordinal-th recorded apply under a tool claim, and
      [H("mentat.mutation.change.v2", "revert", revert, path)] for a revert
      settlement. Within one event, paths are unique, and the apply ordinal
      separates repeated applies of one path under one claim, so the derivation
      is total and collision-free, and a continuation process re-lowering the
      same evidence re-derives the same id.

      Invariant: an identifier's stable textual form is non-empty. *)

  val of_string : string -> t
  (** [of_string s] is [s] as a change id.

      Raises [Invalid_argument] if [s] is empty. *)

  val to_string : t -> string
  (** [to_string id] is [id]'s stable string representation. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same change id. *)

  val compare : t -> t -> int
  (** [compare a b] orders ids by their stable string representations,
      compatibly with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an id for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps change ids to JSON strings, validating the non-empty
      invariant of {!of_string} on decode. *)
end

type t
(** The type for one applied file transition.

    Invariant: the image pair is one of the three valid transitions —
    [Missing -> Text], [Text -> Text], or [Text -> Missing] — and the line
    counts are non-negative. A change must alter the file: [Missing -> Missing]
    and a byte-identical [Text -> Text] pair are unrepresentable — constructors
    never build them and decode rejects them. *)

val id : t -> Id.t
(** [id t] is [t]'s derived change id. *)

val path : t -> Mentat_workspace.Path.t
(** [path t] is the path [t] mutated. *)

val before : t -> Image.t
(** [before t] is the path's image before the transition. *)

val after : t -> Image.t
(** [after t] is the path's image after the transition. *)

val additions : t -> int
(** [additions t] is [t]'s recorded added line count, computed from the two
    texts while the event was constructed. *)

val deletions : t -> int
(** [deletions t] is [t]'s recorded deleted line count. *)

val kind : t -> Mentat_edit.kind
(** [kind t] is the applied transition kind, an eliminator over the image pair:
    [Missing -> Text] is [`Create], [Text -> Text] is [`Modify], and
    [Text -> Missing] is [`Delete]. There is no stored operation member to
    disagree with the images. *)

val of_changes : t list -> Textdiff.stats
(** [of_changes changes] is the aggregate {!Textdiff.stats} for [changes]:
    [files] counts distinct row paths; [additions] and [deletions] sum the
    recorded line counts. This is the run-cumulative trailer input; the counts
    cross the wire through the diff owner's {!Textdiff.Stats.jsont}, so no
    mutation-owned totals shape duplicates it. Netted display statistics are
    recomputed from images at render time. *)

(** {1:netting Netting} *)

module Net : sig
  (** Netted per-path endpoints. *)

  type entry = private {
    path : Mentat_workspace.Path.t;
    before : Image.t;  (** First observed image for [path]. *)
    after : Image.t;  (** Last observed image for [path]. *)
    contiguous : bool;
        (** [false] iff some delta's before did not equal the previous delta's
            after: evidence of unrecorded interleaved mutation. A safety input —
            it degrades revertability to [Incomplete] and gates preparation
            behind an explicit override — not display decoration. *)
    sources : Id.t list;  (** Contributing change rows in observation order. *)
  }
  (** The type for one netted path. *)
end

val net : t list -> Net.entry list
(** [net changes] is the per-path endpoint netting of [changes] in list order:
    for each path, the first before image and the final after image, with
    contributing sources and the contiguity fact. Entries are in first-seen path
    order; paths whose endpoint images are equal are dropped. Netting is total:
    no sequence of valid transitions fails to fold. *)

(** {1:presentation Presentation} *)

module Hunks_error : sig
  (** Structured {!hunks} refusals. *)

  (** The type for hunk resolution failures. A missing blob and a bounded-diff
      refusal are different facts; no caller may conflate them. *)
  type t =
    | Missing_blob of Mentat_digest.Content_ref.t
        (** The resolver returned no bytes for a referenced image — because
            blobs are never garbage collected, a referenced image's bytes are
            always present, so absence is integrity corruption, not a normal
            miss. *)
    | Bound_exceeded
        (** {!Textdiff.hunks} refused under [max_edit_distance]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a refusal for diagnostics. *)
end

val hunks :
  ?max_edit_distance:int ->
  blob:(Mentat_digest.Content_ref.t -> string option) ->
  t ->
  (Textdiff.Hunk.t list, Hunks_error.t) result
(** [hunks ?max_edit_distance ~blob t] is the before/after line diff of one
    change, resolving its text images through [blob] and grouping with
    {!Textdiff.hunks}. [max_edit_distance] is forwarded because recorded texts
    are replay input. This backs the protocol's mutation diff query; it lives
    here so the diff engine stays out of journal readers. [blob] must return the
    exact recorded bytes for a reference it resolves; the bytes are trusted, as
    the content-addressed store guarantees them. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same change. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a change for diagnostics. *)

val jsont : t Jsont.t
(** [jsont] maps changes to JSON objects, validating image pairs and
    non-negative counts on decode. The id member is re-validated against its
    derivation by the enclosing decoders — {!Event.jsont}'s edit case and the
    revert settlement codec — which know the enclosing source. *)

(** {1:internal Internal}

    Lowering shared by the event and revert constructors. The facade recital
    drops {!of_entry}, so {!type:t}'s "no supported constructor" holds. Change
    id spelling and derivation live in a dune-private helper. *)

val of_entry : context:string -> id:string -> Mentat_edit.Result.Entry.t -> t
(** [of_entry ~context ~id entry] lowers one applied edit entry to a change:
    images by the proven two-case narrowing, line counts from the two texts, and
    the internally derived [id].

    Raises [Invalid_argument] (naming [context]) if the entry carries a non-text
    state. *)
