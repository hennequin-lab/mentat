(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Settled conversation documents.

    A transcript is an append-only sequence of presentation blocks in
    conversation order. Live facts and replayed facts use the same constructors
    and rendering path, so a resumed document has the same pixels as the
    equivalent settled live document. The document owns top-level spacing,
    consecutive replacement laws, and the global reasoning-disclosure lens;
    individual presenters own their block interiors.

    Construct blocks with {!banner}, {!user}, {!assistant}, {!reasoning},
    {!tool}, {!board}, and {!notice}; add them with {!append}; render the result
    with {!view}. *)

(** {1:types Types} *)

type block
(** The type for one settled transcript block.

    Block constructors normalize untrusted text to valid UTF-8 and reject values
    that cannot form a legible block. The representation is abstract so invalid
    message text, negative reasoning durations, and unnormalized notice text
    cannot enter a document. *)

type t
(** The type for a transcript document.

    Appending is constant time. A document contains at most one banner, and a
    banner, when present, is its first block. *)

(** {1:block-constructors Block constructors} *)

val banner : Snapshot.t -> block
(** [banner snapshot] is the launch record for [snapshot]. A banner carries its
    own top and bottom framing rows through {!Banner.record}. *)

val user : string -> block
(** [user text] is a settled user message.

    Invalid UTF-8 and terminal controls are replaced by [U+FFFD], CRLF and other
    line boundaries become line feeds, and tabs become two spaces.

    Raises [Invalid_argument] if the normalized text has no visible grapheme. *)

val assistant : string -> block
(** [assistant markdown] is settled assistant Markdown rendered by
    {!Prose.view}. Text normalization and the empty-text error are the same as
    for {!user}. *)

val reasoning : duration_s:int -> ?title:string -> body:string -> unit -> block
(** [reasoning ~duration_s ?title ~body ()] is one settled reasoning block.

    [duration_s] is an elapsed duration in whole seconds. [title] is normalized
    to one trimmed row and omitted when it has no visible grapheme. [body] is
    normalized as multiline Markdown and becomes empty when it contains only
    whitespace; an empty body is a valid collapsed-only reasoning record.

    Raises [Invalid_argument] if [duration_s < 0]. *)

val tool : Tool_block.t -> block
(** [tool tool_block] is a settled tool block. Tool output remains owned by
    {!Tool_block}; task boards and mutation evidence have separate fact-owned
    transcript paths. *)

val board : Mentat_session.Task.Board.t -> block
(** [board task_board] is the settled transcript record for a whole-board
    replacement.

    The block records a succeeded [Todo] header with total, completed, and
    running counts. The responsive activity surface owns the complete current
    board, so the transcript never duplicates its task rows. An empty board
    remains visible as a zero-count header, recording that the task list was
    cleared. *)

val notice : Notice.t -> block
(** [notice value] is a committed transcript notice.

    Text is normalized according to the notice field: headings, identifiers, and
    fact text become one trimmed row, while prose preserves line boundaries.
    Blank optional fields and blank {!Notice.Fact} values are omitted. Failure
    counts and numeric data counts must be non-negative where applicable.

    Raises [Invalid_argument] if a required event, command, failure, seam, or
    data-source field has no visible grapheme; if a failure [count < 1]; or if a
    {!Notice.Change} or {!Notice.Errors} count is negative. *)

(** {1:documents Documents} *)

val empty : t
(** [empty] is the document with no blocks. *)

val length : t -> int
(** [length t] is the number of blocks in [t]. Collapse and replacement laws in
    {!append} mean this counts settled blocks, not folded facts. *)

val split : int -> t -> t * t
(** [split n t] is [(kept, dropped)] where [kept] is the oldest [n] blocks of
    [t] and [dropped] is the rest, each a document in the same conversation
    order as [t]. [n] is clamped to [[0, length t]]; a banner, being the first
    block, always stays in [kept] once [n >= 1]. This is the rewind preview
    seam: the shell splits its own folded transcript at a turn boundary to show
    the tail a non-destructive rewind would discard.

    Raises [Invalid_argument] if [n < 0]. *)

val append : t -> block -> t
(** [append t block] is [t] followed by [block]. It runs in constant time.

    Three laws replace or collapse the last block instead of stacking:

    - a failure notice with the same normalized message and next step as the
      last failure increments the standing failure count by one, saturating at
      [max_int]; the incoming count does not contribute;
    - a data notice with the same normalized source as the last data notice
      replaces that notice;
    - a task board replaces the last task board.

    Each law is consecutive only. Any intervening block makes the earlier block
    settled history and the incoming block appends normally.

    Raises [Invalid_argument] if [block] is a banner and [t] is non-empty. *)

(** {1:rendering Rendering} *)

val user_block : palette:Theme.Palette.t -> string -> 'msg Mosaic.t
(** [user_block text] renders [text] as a full-width user-background block, with
    a fixed muted [❯ ] marker and wrapped text hanging under the prose. The
    trailing breathing room shrinks before the message when the parent offers
    less space; the semantic marker remains visible. This is the shared view for
    a live optimistic echo and its eventual settled {!user} block.

    Text normalization and the empty-text error are the same as for {!user}. *)

val view : ?expanded:bool -> palette:Theme.Palette.t -> t -> 'msg Mosaic.t
(** [view ?expanded t] renders [t] in the space allocated by its parent.

    [expanded] defaults to [false]. When [true], every non-empty reasoning body
    is visible below its one-row heading; when [false], all reasoning remains a
    heading. The heading gives its optional title the first shrink priority,
    then the [(ctrl+o)] hint, then the required elapsed-time label. Mosaic
    truncates each one-row segment at an extended-grapheme boundary.

    Top-level blocks have exactly one blank row between them, except that no row
    precedes the first block and no extra row follows a banner: the banner's own
    bottom margin is that separator. User and assistant bodies use a two-column
    hanging gutter. Task-board rows use the settled-detail inset; Mosaic owns
    all measurement, wrapping, shrinking, and overflow. *)
