(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Structured composer drafts.

    A draft is the composer input buffer plus the structured payloads that plain
    text cannot carry safely. Its visible text is well-formed UTF-8. Byte ranges
    may mark file references and large-paste placeholders that edit and delete
    as atomic elements.

    This module is pure and carries no Mosaic types: it owns the text algebra
    (span edits, paste collapse, [@]-token detection, prompt-history snapshots)
    and hands the composer a plain styled-run projection ({!runs}) to feed the
    textarea. Positions are UTF-8 byte offsets, not extended-grapheme indexes; a
    textarea adapter must translate its grapheme cursor positions before calling
    {!with_cursor}.

    The intended flow is:

    - construct with {!empty}, {!of_text}, or {!of_history_entry};
    - update with {!insert_text}, {!replace_range}, {!replace_visible_text},
      {!insert_file_ref}, {!replace_active_file_ref_token}, and {!insert_paste};
    - project for rendering with {!text}, {!cursor}, and {!runs};
    - submit with {!submit}, which expands paste placeholders and returns a
      structured history entry. *)

(** {1:spans Spans} *)

module Span : sig
  type t
  (** The type for half-open byte ranges \[[first];[last]\) in draft text.
      Values satisfy [0 <= first <= last]. *)

  val make : first:int -> last:int -> t
  (** [make ~first ~last] is the span \[[first];[last]\).

      Raises [Invalid_argument] if [first < 0] or [last < first]. *)

  val cursor : int -> t
  (** [cursor pos] is the empty span at [pos]. *)

  val first : t -> int
  (** [first t] is [t]'s first byte offset. *)

  val last : t -> int
  (** [last t] is [t]'s exclusive last byte offset. *)

  val length : t -> int
  (** [length t] is [last t - first t]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same bounds. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for debugging. *)
end

(** {1:file_refs File References} *)

module File_ref : sig
  type t
  (** The type for a file reference carried by the draft. *)

  val make : ?label:string -> string -> t
  (** [make ?label path] is a file reference to [path]. [label] defaults to
      [path] and is the visible text inserted in the draft. Malformed UTF-8 in
      the label is replaced with the Unicode replacement character; [path]
      remains unchanged.

      Raises [Invalid_argument] if [path] or the resulting label is empty. *)

  val path : t -> string
  (** [path t] is the referenced path. *)

  val label : t -> string
  (** [label t] is the visible text for [t]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same path and label. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for debugging. *)
end

(** {1:image_refs Image References} *)

module Image_ref : sig
  type t
  (** The type for an attached image carried by the draft: the model-visible
      media content block a later prompt includes. Unlike a file reference, its
      visible [[Image #N]] placeholder is not sent as prompt text; only its
      media block reaches the model. *)

  val make : Mentat_llm.Content.t -> t
  (** [make media] wraps [media], the media content block (source [`Ref]) that
      the attach flow returned, as an atomic draft image.

      Raises [Invalid_argument] if [media] is not a media block. *)

  val media : t -> Mentat_llm.Content.t
  (** [media t] is [t]'s model-visible media content block. *)

  val media_type : t -> string
  (** [media_type t] is the MIME type of [t]'s media, e.g. ["image/png"]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] wrap equal media blocks. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for debugging. *)
end

(** {1:elements Structured Elements} *)

(** The type for structured draft elements. *)
type element =
  | File_ref of File_ref.t
      (** An atomic file reference visible as {!File_ref.label}. *)
  | Paste_placeholder of string
      (** A visible placeholder for a large paste payload. *)
  | Image of Image_ref.t
      (** An atomic attached image visible as [[Image #N]]. Its media block is
          submitted ahead of the prompt text, not as visible text. *)

type range = { span : Span.t; element : element }
(** A structured element bound to its exact visible bytes in draft text. *)

type pending_paste = { paste_placeholder : string; paste_text : string }
(** A large paste payload not currently expanded in visible text. *)

(** {1:history History Entries} *)

module History_entry : sig
  type t
  (** The type for draft state stored in prompt history. *)

  val make :
    ?file_refs:(Span.t * File_ref.t) list ->
    ?pending_pastes:pending_paste list ->
    string ->
    t
  (** [make ?file_refs ?pending_pastes text] is a history entry whose visible
      text is [text]. [file_refs] and [pending_pastes] default to [[]]. Metadata
      is retained verbatim here and validated when the entry is restored with
      {!of_history_entry}. *)

  val of_text : string -> t
  (** [of_text text] is [make text]. *)

  val text : t -> string
  (** [text t] is [t]'s visible text. *)

  val file_refs : t -> (Span.t * File_ref.t) list
  (** [file_refs t] is every file reference carried by [t], in stored order. *)

  val pending_pastes : t -> pending_paste list
  (** [pending_pastes t] is every unexpanded large-paste payload carried by [t],
      in stored order. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same visible text,
      file-reference metadata, and pending paste payloads. *)
end

(** {1:drafts Drafts} *)

type t
(** The type for composer drafts.

    {!text} is well-formed UTF-8. The cursor is a byte offset on a Unicode
    scalar boundary; it is not an extended-grapheme index. Structured ranges are
    sorted, pairwise disjoint, and kept consistent with text edits. Editing
    through an atomic range removes the corresponding element rather than
    leaving a stale payload. *)

type submitted = {
  submitted_text : string;
      (** Text submitted to the model after trimming and paste expansion. Image
          placeholders are removed; their media rides {!submitted_media}. *)
  submitted_media : Mentat_llm.Content.t list;
      (** The media blocks of the draft's attached images, in ascending document
          order, model-visible content to place ahead of the prompt text. *)
  submitted_history_entry : History_entry.t;
      (** Structured entry suitable for prompt history. Large pastes remain
          collapsed with their payloads attached; attached images are not
          retained, since their referenced bytes are session-scoped. *)
}
(** The type for a non-empty submitted draft. *)

val large_paste_char_threshold : int
(** [large_paste_char_threshold] is the default number of Unicode scalar values
    above which {!insert_paste} inserts a placeholder instead of visible paste
    text. *)

val large_paste_line_threshold : int
(** [large_paste_line_threshold] is the default number of lines (one trailing
    newline not counted) at which {!insert_paste} inserts a placeholder instead
    of visible paste text. *)

val empty : t
(** [empty] is the empty draft with cursor at byte offset [0]. *)

val of_text : string -> t
(** [of_text text] is a plain-text draft with cursor at the end of [text].
    Malformed UTF-8 is replaced with the Unicode replacement character. *)

val text : t -> string
(** [text t] is the visible draft text. *)

val cursor : t -> int
(** [cursor t] is [t]'s cursor byte offset on a Unicode scalar boundary. *)

val ranges : t -> range list
(** [ranges t] is [t]'s pairwise-disjoint structured ranges in ascending span
    order. *)

val pending_pastes : t -> pending_paste list
(** [pending_pastes t] is [t]'s reachable unexpanded large-paste payloads in
    insertion order. *)

val is_blank : t -> bool
(** [is_blank t] is [true] iff expanded visible text is empty or consists only
    of Unicode White_Space scalars; equivalently, iff {!submit} returns [None].
*)

val with_cursor : int -> t -> t
(** [with_cursor pos t] is [t] with cursor at [pos].

    Raises [Invalid_argument] if [pos] is outside {!text} or not on a Unicode
    scalar boundary. Rendering adapters normally pass an extended-grapheme
    boundary converted to its byte offset. *)

val replace_range : Span.t -> string -> t -> t
(** [replace_range span replacement t] replaces [span] in [t]'s visible text.

    If [span] intersects an atomic range, the replacement expands to cover the
    entire atomic range. Ranges after the replacement are shifted. Ranges
    touched by the replacement are removed. The cursor moves to the end of the
    inserted [replacement]. Malformed UTF-8 in [replacement] is replaced with
    the Unicode replacement character.

    Raises [Invalid_argument] if [span] is outside {!text} or not aligned to
    Unicode scalar boundaries. *)

val replace_visible_text : string -> t -> t
(** [replace_visible_text text t] adapts [t] to a full replacement of its
    visible text, as emitted by textarea widgets that report only the new value.

    The replacement is interpreted as one contiguous edit between the common
    prefix and suffix of the old and new visible text. Structured ranges outside
    that edit are preserved and shifted; ranges touched by the edit follow
    {!replace_range} atomic-replacement semantics. Common prefix and suffix
    detection never splits a UTF-8 scalar. The cursor is placed at the end of
    the inferred edit. Malformed UTF-8 is replaced with the Unicode replacement
    character before comparison. If the normalized text is unchanged, [t] is
    returned unchanged. *)

val insert_text : string -> t -> t
(** [insert_text text t] inserts [text] at {!cursor}. Malformed UTF-8 is
    replaced with the Unicode replacement character. *)

val insert_file_ref : ?label:string -> path:string -> t -> t
(** [insert_file_ref ?label ~path t] inserts an atomic file reference at
    {!cursor}. The visible insertion is the file reference label. *)

val insert_image : Image_ref.t -> t -> t
(** [insert_image image t] inserts an atomic [[Image #N]] element at {!cursor}
    carrying [image]. [N] is the smallest unused positive placeholder ID across
    the draft, shared with paste placeholders. The element edits and deletes
    atomically, projects as an {!Atom} run, and its media is collected by
    {!submit}. *)

val active_file_ref_token_span : t -> Span.t option
(** [active_file_ref_token_span t] is the [@]-prefixed token containing
    {!cursor}, if any: the nearest [@] before the cursor with no whitespace
    between, mid-draft as well as at draft start. Tokens are delimited by ASCII
    whitespace. *)

val replace_active_file_ref_token : ?label:string -> path:string -> t -> t
(** [replace_active_file_ref_token ?label ~path t] replaces the active
    [@]-prefixed token with an atomic file reference. If there is no active
    token, the file reference is inserted at {!cursor}. *)

val insert_paste :
  ?char_threshold:int -> ?line_threshold:int -> string -> t -> t
(** [insert_paste pasted t] inserts [pasted] at {!cursor}.

    An empty paste leaves [t] unchanged. CRLF and lone carriage-return line
    endings are normalized to [\n], and malformed UTF-8 is replaced with the
    Unicode replacement character. The paste collapses when its Unicode scalar
    count is strictly greater than [char_threshold] or its content-line count is
    greater than or equal to [line_threshold]. One trailing newline does not add
    a content line.

    A collapsed paste is represented by an atomic [[Pasted text #N +M lines]]
    placeholder and its full normalized payload in {!pending_pastes}. [N] is the
    smallest unused positive paste or image placeholder ID across the visible
    text and pending metadata, so hand-written and history-restored placeholders
    do not collide without risking integer overflow. [M] is the newline count
    and is omitted when zero. [char_threshold] defaults to
    {!large_paste_char_threshold}; [line_threshold] defaults to
    {!large_paste_line_threshold}.

    Raises [Invalid_argument] if [char_threshold < 0] or [line_threshold <= 0].
*)

val expand_paste_placeholders : t -> t
(** [expand_paste_placeholders t] replaces any known paste placeholders in [t]
    with their full paste text and removes the consumed pending payloads.
    Unknown placeholders are left as literal visible text. File references stay
    structured and shift to their new spans. The cursor moves to the end. *)

(** {1:rendering Styled-run projection} *)

(** The style class of a run of visible draft text. *)
type run_kind =
  | Plain  (** Ordinary editable text. *)
  | Atom
      (** An atomic element — a file reference or a paste placeholder — rendered
          in the app-owned token color. *)

val runs : t -> (Span.t * run_kind) list
(** [runs t] partitions {!text} into consecutive styled runs, ascending: each is
    a maximal span of [Plain] editable text or an [Atom] element. The
    concatenation of the run substrings is exactly {!text}, so the composer can
    feed them to the textarea as styled spans. *)

(** {1:history_io Prompt history} *)

val history_entry : t -> History_entry.t
(** [history_entry t] is [t] as a structured prompt-history value, preserving
    file references and collapsed paste payloads. *)

val of_history_entry : History_entry.t -> t
(** [of_history_entry entry] is [entry] restored as an editable draft with
    cursor at the end of its visible text.

    Malformed UTF-8 in visible text and pending paste metadata is replaced with
    the Unicode replacement character. A file reference is ignored if its span
    is invalid, no longer matches its label, or overlaps an earlier valid file
    reference in stored order. Paste placeholders are rediscovered independently
    of metadata order and ignored when absent or overlapping retained metadata.
    Thus restored drafts always have sorted, pairwise-disjoint ranges. *)

val submit : t -> (submitted * t) option
(** [submit t] is [Some (submitted, empty)] if [t] contains non-blank text after
    paste expansion and trimming, or any attached image, and [None] otherwise.
    Trimming removes Unicode White_Space scalars at both ends. The model-facing
    {!submitted_text} contains expanded paste payloads but no image
    placeholders; each attached image's media block rides {!submitted_media} in
    document order. The history entry keeps the trimmed visible placeholders and
    their structured payloads, minus attached images. *)
