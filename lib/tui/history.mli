(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Persistent prompt history and its reverse-search view.

    This pure module owns the next TUI's versioned JSONL record, newest-first
    load semantics, and [ctrl+r] search state. It performs no file I/O, locking,
    or clock reads: the runtime supplies file contents and timestamps, persists
    the lines returned by {!encode}, and renders {!Search.view} above the
    composer.

    Each encoded line has [schema_version] [1], type
    ["mentat.tui.history_entry"], a session id, an exact decimal-string Unix
    timestamp, and a canonical {!Draft.History_entry.t}. All known fields of a
    line are validated atomically; {!load} provides best-effort recovery by
    returning valid entries together with line-numbered rejection details. *)

(** {1:entries Entries} *)

module Entry : sig
  type t
  (** The type for one canonical prompt-history record. It carries the session
      that produced the draft and a runtime-supplied Unix timestamp. *)

  val of_draft :
    session:Mentat_session.Id.t -> ts:int -> Draft.History_entry.t -> t option
  (** [of_draft ~session ~ts draft] is [Some entry] when [draft]'s expanded,
      trimmed submission is nonblank, and [None] otherwise.

      The draft is canonicalized through {!Draft.of_history_entry} and
      {!Draft.submit}: stale structured metadata is omitted, malformed UTF-8 is
      replaced, text is trimmed, and the byte ranges of file references and
      paste placeholders that survive trimming are preserved. A draft whose
      expanded, trimmed submission is empty yields [None].

      [ts] is interpreted as Unix seconds and may use the full OCaml [int]
      range; the codec stores it as canonical decimal text. *)

  val session : t -> Mentat_session.Id.t
  (** [session entry] is the session that produced [entry]. *)

  val draft : t -> Draft.History_entry.t
  (** [draft entry] is the canonical structured draft suitable for restoration
      into the composer. *)

  val text : t -> string
  (** [text entry] is the visible text of {!draft}. It is nonempty and has no
      leading or trailing whitespace under {!Draft.submit}'s trimming law. *)
end

(** {1:codec Codec} *)

module Error : sig
  type json_kind = [ `Array | `Boolean | `Null | `Number | `Object | `String ]
  (** The JSON value kinds reported by {!Wrong_kind}. *)

  (** The type for recoverable failures to decode a persisted record. Cases are
      stable for recovery and diagnostics; callers need not parse {!message}. *)
  type t =
    | Malformed_json of string
        (** The line is not JSON. The payload is the JSON decoder's diagnostic,
            for presentation rather than programmatic matching. *)
    | Missing_member of string
        (** A required member is absent. The payload is its JSON path. *)
    | Wrong_kind of { path : string; expected : json_kind; found : json_kind }
        (** A member has the wrong JSON kind. *)
    | Unsupported_version of float
        (** The [schema_version] number is not [1]. *)
    | Unsupported_type of string
        (** The record [type] is not ["mentat.tui.history_entry"]. *)
    | Invalid_decimal_integer of { path : string; value : string }
        (** A decimal-string integer has noncanonical syntax or exceeds the
            platform's [int] range. *)
    | Invalid_session_id  (** [session_id] is empty. *)
    | Empty_value of string
        (** A value required to be nonempty is empty. The payload is its JSON
            path. *)
    | Invalid_span of { path : string; first : int; last : int }
        (** A decoded span has a negative start or an end before its start. *)
    | Noncanonical_draft
        (** Structured draft metadata is stale, overlapping, misaligned, or
            otherwise changes when restored canonically. *)
    | Blank_draft  (** The draft is blank after paste expansion and trimming. *)

  val message : t -> string
  (** [message error] is a human-readable explanation of [error]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf error] formats {!message} for diagnostics. *)
end

val encode : Entry.t -> string
(** [encode entry] is one JSON object without a trailing newline. The runtime
    appends the newline when persisting the record.

    Raises [Invalid_argument] if the JSON encoder rejects an internal value. *)

val decode : string -> (Entry.t, Error.t) result
(** [decode line] is [Ok entry] exactly when [line] is a version-1
    ["mentat.tui.history_entry"] object with:

    - a nonempty [session_id];
    - a canonical decimal-string [timestamp] representable as an OCaml [int];
    - a nonempty, already-trimmed draft [text];
    - optional [file_refs] and [pending_pastes] arrays whose every item is
      valid, whose [start] and [end] offsets are canonical decimal strings,
      whose byte ranges are aligned with the text, and whose elements do not
      overlap.

    Missing metadata arrays denote empty arrays. Unknown object members are
    ignored. Malformed JSON, another schema version or type, noncanonical or
    overflowing decimal integers, malformed metadata, and noncanonical draft
    records yield a structured [Error error]; decoding external contents does
    not raise. *)

type rejection = { line : int; error : Error.t }
(** The type for one rejected nonblank JSONL line. [line] is its one-based
    physical line number in the loaded contents. *)

type loaded = {
  entries : Entry.t list;  (** Every accepted record, newest first. *)
  rejected : rejection list;
      (** Rejected nonblank lines in ascending physical line order. *)
}
(** The recoverable result of loading a prompt-history file. *)

val load : string -> loaded
(** [load contents] decodes newline-delimited records independently. Physical
    line order is authoritative; timestamps do not reorder records. Blank lines
    are ignored. Every valid nonblank line appears in [entries], and every
    invalid nonblank line appears in [rejected]. This module applies no byte or
    record-count retention policy; the caller owns storage bounds. Accepted
    records are not deduplicated; {!Search.make} owns deduplication for its
    surface. *)

(** {1:search Reverse Search} *)

module Search : sig
  type t
  (** The type for an immutable [ctrl+r] reverse-search session: canonical
      entries, a query, and a selected ranked match. *)

  val make : ?current:Mentat_session.Id.t -> entries:Entry.t list -> unit -> t
  (** [make ?current ~entries ()] opens an empty-query search over [entries].
      [entries] must be newest first. Exact duplicate texts are collapsed before
      ranking, keeping the first and therefore newest record. Matches from
      [current] rank before all other matches; order within both blocks remains
      newest first. If [current] is omitted, no record receives that preference.
  *)

  val refresh : ?current:Mentat_session.Id.t -> entries:Entry.t list -> t -> t
  (** [refresh ?current ~entries search] replaces the records and current
      session attribution while preserving the query. The records are
      deduplicated and reranked as by {!make}; the numeric selection is clamped
      to the resulting matches. Omitting [current] removes current-session
      preference. *)

  val with_query : string -> t -> t
  (** [with_query query search] filters [search] by grapheme-subsequence match:
      each extended grapheme in the trimmed [query] must occur in order in an
      entry's complete text, but need not be adjacent. ASCII letters match
      case-insensitively; non-ASCII graphemes match exactly. Malformed UTF-8 in
      [query] is replaced with [Uchar.rep].

      The stored query is both UTF-8-normalized and trimmed. Changing that
      canonical value selects the first match; reapplying it preserves and, if
      necessary, clamps the selection. *)

  val selected_entry : t -> Draft.History_entry.t option
  (** [selected_entry search] is the selected match's structured draft, or
      [None] when there are no matches. Restoring this value edits the composer;
      it does not submit the prompt. *)

  type msg =
    | Select of int
    | Activate of int
        (** Messages emitted by the controlled completion table. [Select index]
            reports its selected row; [Activate index] reports the row activated
            by Enter or pointer input. *)

  type event =
    | Stay
    | Activated of Draft.History_entry.t
        (** The semantic result of {!update}. [Activated entry] carries the
            exact structured draft selected for insertion into the composer. *)

  val update : msg -> t -> t * event
  (** [update message search] applies a completion-table [message]. Row indices
      are clamped to the current ranked matches. Selection changes yield [Stay];
      activation yields [Activated entry], or [Stay] when no match exists. *)

  val move : [ `Up | `Down ] -> t -> t
  (** [move direction search] selects the adjacent match in [direction],
      wrapping at both ends. A search without matches is unchanged. *)

  val view : palette:Theme.Palette.t -> t -> msg Mosaic.t
  (** [view search] renders a single truncating [reverse-i-search:] header
      followed by the shared controlled {!Completion_list} table. Rows contain
      only the first logical line of each prompt, treating either carriage
      return or line feed as a boundary. Mosaic owns column measurement,
      ellipsis, scrolling, selection visibility, and activation.

      No stored entries renders [no prompt history]; a nonempty history with no
      query matches renders [no matching prompts]. Both notes are inert Mosaic
      text rows and truncate to their allocation. *)
end
