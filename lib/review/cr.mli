(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Source-backed inline code-review comments.

    [Cr] models CR comments stored directly in source files. A {!type:t} is
    normalized CR text without source comment delimiters. Construct values with
    {!make} or {!parse}, inspect them with {!status}, {!recipient}, and {!body},
    render them with {!to_string} or {!render}, and embed them with the pure
    text transformations below.

    {!scan} returns {!Occurrence.t} values: source evidence for CR-looking
    comments at caller-supplied paths and byte spans. Occurrences may be
    malformed so callers can report bad source comments instead of silently
    dropping them.

    This library is pure. It does not read or write files, build edit plans,
    request permissions, validate workspace membership, record session events,
    or know about review targets. *)

(** {1:errors Errors} *)

module Error : sig
  (** CR errors. *)

  (** Stable, matchable CR error classes.

      Human-readable messages are diagnostics only; use [kind] values for
      control flow and tests. *)
  type kind =
    | Invalid_handle
        (** A participant handle is empty or contains whitespace, [:], or NUL.
        *)
    | Invalid_body
        (** A CR body is empty, contains NUL, or cannot be rendered in the
            requested source comment syntax. *)
    | Invalid_syntax
        (** A source comment delimiter is empty, starts with forbidden
            whitespace, or contains NUL or a newline. *)
    | Invalid_comment  (** CR text does not match the CR grammar. *)
    | Invalid_anchor
        (** An insertion line is less than [1] or outside the supplied source
            text. *)
    | Stale_occurrence
        (** A scanned occurrence no longer matches the source text supplied to
            {!replace} or {!remove}. *)

  type t
  (** The type for CR errors. *)

  val kind : t -> kind
  (** [kind e] is [e]'s stable, matchable error class. *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic.

      Messages are not stable identifiers. Use {!kind} for control flow and
      tests. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] formats [e] for diagnostics. *)
end

(** {1:comments Comments} *)

module Handle : sig
  (** CR participant handles. *)

  type t
  (** The type for CR participant handles.

      Handles identify recipients and resolvers in source comments, for example
      [mentat] in [CR mentat: ...] or [agent] in [XCR agent for mentat: ...].
      Handles are non-empty source tokens and cannot contain whitespace, [:], or
      NUL. *)

  val of_string : string -> (t, Error.t) result
  (** [of_string s] is [s] as a CR handle.

      Errors with {!Error.Invalid_handle} if [s] is empty or contains
      whitespace, [:], or NUL. *)

  val to_string : t -> string
  (** [to_string t] is [t]'s source form. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same handle. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] as source handle text. *)
end

module Priority : sig
  (** Open CR priority. *)

  (** The priority of an open CR comment.

      [Now] renders as [CR]. [Soon] renders as [CR-soon]. Resolved comments do
      not carry a priority. *)
  type t =
    | Now  (** Immediate review priority. *)
    | Soon  (** Deferred review priority. *)

  val default : t
  (** [default] is the priority used by {!make} when [priority] is omitted. *)

  val to_string : t -> string
  (** [to_string t] is a stable descriptive string, not source syntax. *)
end

module Status : sig
  (** CR status. *)

  (** The lifecycle state of a CR comment.

      [Open Now] renders as [CR], [Open Soon] renders as [CR-soon], and
      [Resolved] renders as [XCR]. A resolved comment records the resolving
      handle; the addressed recipient, if any, is stored on the enclosing CR
      value. *)
  type t =
    | Open of Priority.t  (** An unresolved CR with its review priority. *)
    | Resolved of { resolver : Handle.t }  (** A CR resolved by [resolver]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same status. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. *)
end

type t
(** The type for valid CR comments.

    A CR is normalized at construction: bodies are trimmed, handles are
    validated, and rendering is canonical. Persistence is not part of this type.
    A CR is source-backed only when it appears in source text and is observed as
    an {!Occurrence.t}. *)

(** {2:constructing Constructing} *)

val make :
  ?priority:Priority.t ->
  ?recipient:Handle.t ->
  body:string ->
  unit ->
  (t, Error.t) result
(** [make ?priority ?recipient ~body ()] is an open CR comment.

    [priority] defaults to {!Priority.default}. [body] is trimmed and must not
    be empty or contain NUL. [recipient], when present, must already be a valid
    {!Handle.t}.

    Errors with {!Error.Invalid_body} if the trimmed body is empty or contains
    NUL. *)

val resolve : resolver:Handle.t -> ?body:string -> t -> (t, Error.t) result
(** [resolve ~resolver ?body cr] is [cr] marked resolved by [resolver].

    If [body] is omitted, [cr]'s body is retained. If [body] is provided, it is
    trimmed and validated like {!make}'s [body]. The recipient, if any, is
    retained.

    Errors with {!Error.Invalid_body} if the resolved body is invalid. *)

val parse : string -> (t, Error.t) result
(** [parse s] parses CR text without source comment delimiters.

    Accepted forms include [CR: body], [CR handle: body],
    [CR-soon handle: body], [XCR resolver: body], and
    [XCR resolver for handle: body]. Leading and trailing whitespace around the
    whole input is ignored. Parsed handles are validated as {!Handle.t}; parsed
    bodies are validated like {!make}'s [body]. Successful parses render
    canonically with {!to_string}.

    Errors with {!Error.Invalid_comment} if [s] does not match the CR grammar or
    contains an invalid parsed handle token. Errors with {!Error.Invalid_body}
    if the parsed body is invalid. *)

(** {2:inspecting Inspecting} *)

val status : t -> Status.t
(** [status t] is [t]'s status. *)

val recipient : t -> Handle.t option
(** [recipient t] is [t]'s addressed recipient, if any. *)

val body : t -> string
(** [body t] is [t]'s body. *)

val digest : t -> Mentat_digest.Content_ref.t
(** [digest t] is a stable identity of [t]'s normalized source text, as returned
    by {!to_string}. *)

(** {2:formatting Formatting} *)

val to_string : t -> string
(** [to_string t] renders [t] in canonical CR syntax without source comment
    delimiters. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] as source CR text without delimiters. *)

(** {1:syntax Source Comment Syntax} *)

module Syntax : sig
  (** Source comment syntax used for scanning and rendering. *)

  type t
  (** The type for source comment syntax.

      Syntax values describe only delimiters. They do not imply a file type,
      parser, workspace policy, path convention, or permission model. *)

  val ocaml : t
  (** [ocaml] is OCaml block-comment syntax, [(* ... *)]. *)

  val line : prefix:string -> (t, Error.t) result
  (** [line ~prefix] is line-comment syntax using [prefix].

      Scanning recognizes line comments only when [prefix] appears after
      optional indentation at the start of a source line; inline trailing
      comments are ignored. Scanned raw text excludes indentation and line
      endings. Rendering produces [prefix ^ " " ^ to_string cr].

      Errors with {!Error.Invalid_syntax} if [prefix] is empty, starts with
      whitespace, or contains NUL or newline. *)

  val block : open_:string -> close:string -> (t, Error.t) result
  (** [block ~open_ ~close] is block-comment syntax using [open_] and [close].

      Rendering produces [open_ ^ " " ^ to_string cr ^ " " ^ close]. OCaml block
      syntax scans nested comments and ignores apparent comment openers inside
      string literals; other block syntaxes close at the first [close]
      delimiter.

      Errors with {!Error.Invalid_syntax} if either delimiter is empty or
      contains NUL or newline. *)

  val of_path : Lpath.Rel.t -> t option
  (** [of_path path] is the conventional source comment syntax for [path]'s
      basename and extension: [dune] files use [;] line comments, OCaml sources
      use block comments, C-family sources use [//], script and configuration
      files use [#], and CSS uses [/* ... */]. [None] means the path has no
      conventional comment syntax and is not scanned.

      This is a naming convention table, not workspace policy: the returned
      syntax value still describes only delimiters and implies nothing about the
      file's contents. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same syntax. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. *)
end

val render : syntax:Syntax.t -> t -> (string, Error.t) result
(** [render ~syntax cr] is [cr] rendered as a source comment using [syntax].

    Line-comment syntax renders a single-line comment. Block-comment syntax
    renders a single block comment. Errors with {!Error.Invalid_body} if the
    rendered CR text contains a newline or a delimiter that cannot be
    represented safely in [syntax]. *)

(** {1:occurrences Source Occurrences} *)

module Occurrence : sig
  (** CR-looking source comment occurrences. *)

  type cr = t
  (** The type for parsed CR comments. *)

  type t
  (** The type for a CR-looking source comment occurrence.

      An occurrence is immutable source evidence returned by {!scan}. It records
      the path supplied by the caller, the source line and byte span of the raw
      source comment, and that raw text. It may contain a malformed CR comment;
      use {!comment} to distinguish valid comments from reportable malformed
      comments. *)

  val path : t -> Lpath.Rel.t
  (** [path t] is the path passed to {!scan}. *)

  val line : t -> int
  (** [line t] is the one-based source line where [t]'s comment starts. *)

  val raw : t -> string
  (** [raw t] is [t]'s exact source comment text including delimiters. *)

  val comment : t -> (cr, Error.t) result
  (** [comment t] is the parsed CR comment, or a parse error.

      Malformed occurrences are preserved so callers can surface diagnostics.
      The result is computed from the scanned payload, without comment
      delimiters. *)

  val digest : t -> Mentat_digest.Content_ref.t
  (** [digest t] is the identity of the parsed comment text when [t] is valid,
      otherwise the identity of {!raw}. This gives malformed occurrences a
      stable identity without pretending they are valid CR comments. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same path, source location,
      source comment syntax, and raw text. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. *)

  type counts = { open_ : int; addressed : int }
  (** The type for occurrence counts. [open_] is the number of valid, unresolved
      occurrences; [addressed] is the subset of those whose recipient is a
      queried handle. Both are non-negative and [addressed] is at most [open_].
  *)

  val counts : handle:Handle.t -> t list -> counts
  (** [counts ~handle occurrences] folds [occurrences] into open and addressed
      totals. An occurrence is open when it parses to a valid CR whose
      {!Status.t} is {!Status.Open} — malformed and resolved occurrences are
      skipped, matching {!Mentat_review.open_crs}. It is addressed when it is
      open and its {!recipient} equals [handle]. *)
end

(** {1:refs Serializable occurrence refs} *)

module Ref : sig
  (** A stable, serializable handle to one source occurrence.

      An occurrence's byte span is process-local and cannot cross a protocol
      wire; a ref names an occurrence by its path, content digest, and duplicate
      ordinal instead, so a frontend can address a CR without holding the opaque
      {!Occurrence.t}. The pair {!Mentat_review.Cr.views} /
      {!Mentat_review.Cr.resolve} mint and redeem refs. *)

  type t = {
    path : Lpath.Rel.t;  (** The occurrence's file. *)
    digest : Mentat_digest.Content_ref.t;
        (** The occurrence's content identity, see {!Occurrence.digest}. *)
    ordinal : int;
        (** The zero-based position among occurrences sharing this path and
            digest, in source order. *)
  }
  (** The type for occurrence refs. *)

  val make :
    path:Lpath.Rel.t -> digest:Mentat_digest.Content_ref.t -> ordinal:int -> t
  (** [make ~path ~digest ~ordinal] is the ref for those parts. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] name the same occurrence. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for refs, rejecting unknown members. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. *)
end

(** {1:views Serializable occurrence views} *)

module View : sig
  (** A serializable projection of one CR occurrence.

      A view carries the presentation-sufficient facts of an occurrence — its
      ref, source line, rendered body, and status — without the opaque source
      span. A malformed occurrence keeps its raw text as {!field-body} and is
      flagged {!field-malformed}. *)

  type t = {
    ref : Ref.t;  (** The occurrence's stable ref. *)
    line : int;  (** The one-based source line. *)
    body : string;
        (** The parsed CR body, or the raw source comment when malformed. *)
    resolved : bool;  (** [true] iff the CR is resolved ([XCR]). *)
    malformed : bool;  (** [true] iff the occurrence does not parse as a CR. *)
    recipient : string option;  (** The addressed recipient handle, if any. *)
  }
  (** The type for occurrence views. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are equal views. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for views, rejecting unknown members. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. *)
end

val views : Occurrence.t list -> View.t list
(** [views occurrences] projects each occurrence to a {!View.t}, assigning each
    a {!Ref.t} whose ordinal counts prior occurrences sharing its path and
    digest. Order is preserved, so a view's index matches the occurrence's. *)

val resolve_ref : Occurrence.t list -> Ref.t -> Occurrence.t option
(** [resolve_ref occurrences ref] is the occurrence [ref] names — the
    [ref.ordinal]-th occurrence in [occurrences] sharing [ref]'s path and digest
    — or [None] when no such occurrence exists (the source moved and the ref is
    stale). It is the inverse of {!views} over the same list. *)

val jsont : t Jsont.t
(** [jsont] is the JSON codec for a CR comment, encoding its canonical source
    text with {!to_string} and decoding it with {!parse}. A string that is not a
    valid CR comment fails to decode. *)

(** {1:edits Wire-safe CR edits} *)

module Edit : sig
  (** A serializable request to add, replace, or remove a CR comment.

      Unlike {!Mentat_review.Op.t}, an edit embeds no opaque {!Occurrence.t}: it
      names an existing occurrence by its serializable {!Ref.t}, so it crosses a
      protocol wire. A performer re-resolves each ref against a freshly loaded
      snapshot ({!resolve_ref}), building the effectful {!Mentat_review.Op.t}
      and failing loud when the source moved. *)

  type comment = t
  (** The type for CR comments, see {!Mentat_review.Cr.t}. *)

  (** The type for CR edits. *)
  type t =
    | Add of { path : Lpath.Rel.t; line : int; cr : comment }
        (** Insert [cr] before one-based [line] of [path]. *)
    | Replace of { ref : Ref.t; cr : comment }
        (** Rewrite the occurrence [ref] names with [cr]. *)
    | Remove of { ref : Ref.t }  (** Delete the occurrence [ref] names. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same edit. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for edits, tagged by a per-edit [kind] and
      rejecting unknown tags and members. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. *)
end

val scan :
  syntax:Syntax.t -> path:Lpath.Rel.t -> text:string -> Occurrence.t list
(** [scan ~syntax ~path ~text] returns all CR-looking source comment occurrences
    in [text], including malformed ones.

    Occurrences are returned in source order. A comment is CR-looking when its
    payload, after leading whitespace, starts with [CR] or [XCR]. Line-comment
    occurrences start at the comment prefix, after indentation. Block-comment
    occurrences start at the block opener.

    [path] is recorded on each occurrence; it is not read, resolved, or
    validated. *)

val scan_file : path:Lpath.Rel.t -> text:string -> Occurrence.t list
(** [scan_file ~path ~text] scans [text] using {!Syntax.of_path}'s conventional
    syntax for [path], or returns [[]] when [path] has no conventional comment
    syntax. Equivalent to {!Syntax.of_path} followed by {!scan}. *)

(** {1:editing Pure Text Transformations} *)

val add_before_line :
  syntax:Syntax.t -> text:string -> line:int -> t -> (string, Error.t) result
(** [add_before_line ~syntax ~text ~line cr] is [text] with [cr] inserted before
    one-based [line].

    The inserted comment uses the target line's indentation and ends with [\n].
    If insertion happens after non-newline text, a separating [\n] is added
    first.

    Errors with {!Error.Invalid_anchor} if [line] is less than [1] or outside
    [text]. Errors with {!Error.Invalid_body} if [cr] cannot be rendered using
    [syntax]. *)

val add_after_line :
  syntax:Syntax.t -> text:string -> line:int -> t -> (string, Error.t) result
(** [add_after_line ~syntax ~text ~line cr] is [text] with [cr] inserted after
    one-based [line].

    The inserted comment uses the following line's indentation, or no
    indentation at end of file, and ends with [\n]. If insertion happens after
    non-newline text, a separating [\n] is added first.

    Errors with {!Error.Invalid_anchor} if [line] is less than [1] or outside
    [text]. Errors with {!Error.Invalid_body} if [cr] cannot be rendered using
    [syntax]. *)

val add_at_end : syntax:Syntax.t -> text:string -> t -> (string, Error.t) result
(** [add_at_end ~syntax ~text cr] is [text] with [cr] inserted at end of file.

    The inserted comment ends with [\n]. If [text] does not end in a newline, a
    separating [\n] is added first.

    Errors with {!Error.Invalid_body} if [cr] cannot be rendered using [syntax].
*)

val replace : text:string -> Occurrence.t -> t -> (string, Error.t) result
(** [replace ~text occurrence cr] is [text] with [occurrence] replaced by [cr].

    The replacement uses [occurrence]'s scanned syntax. Only the raw source
    slice covered by [occurrence] is replaced; surrounding whitespace and line
    endings are preserved by the original [text].

    Errors with {!Error.Stale_occurrence} if [occurrence]'s span is outside
    [text] or the raw source slice no longer matches [text]. Errors with
    {!Error.Invalid_body} if [cr] cannot be rendered using the occurrence
    syntax. *)

val remove : text:string -> Occurrence.t -> (string, Error.t) result
(** [remove ~text occurrence] is [text] with [occurrence] removed.

    When the occurrence is the only non-whitespace content on its line or lines,
    the whole lines are removed — indentation, trailing whitespace, and line
    ending included. When it shares a line with other content, only the raw
    source slice covered by [occurrence] is removed.

    Errors with {!Error.Stale_occurrence} if [occurrence]'s span is outside
    [text] or the raw source slice no longer matches [text]. *)
