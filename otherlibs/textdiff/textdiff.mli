(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Unified diffs and line statistics for in-memory text states.

    A {!File_change.t} is one labeled file creation, deletion, or modification
    over caller-supplied strings. The line diff of two text states projects
    three ways that never mix on one value: {!hunks} returns structured
    {!Hunk.t} regions for programmatic rendering; {!render} formats a bounded
    unified diff as display-safe text and returns its {!stats}; and
    {!Stats.of_changes} counts without rendering.

    {b Pure island.} This module depends on no other Mentat library and on no
    effectful package. It reads no files, resolves or validates no paths, hashes
    no content, detects no binary beyond what a caller already knows, consults
    no filesystem, and persists nothing. Its inputs are strings; its outputs are
    strings, integers, and structured hunks. A {!Label.t} is {e display text},
    not a path — [/dev/null], ["main"], and ["<empty>"] are all valid labels —
    so this module takes no path dependency; callers lower their typed paths to
    strings.

    {b The wire codecs.} {!Hunk.jsont} carries structured hunks across the
    [Mentat_protocol] waist, because a frontend renders {!Hunk.t} but never
    observes the workspace to compute it (it is delivered, computed once
    engine-side with the untrusted-input bound already applied). It is
    byte-exact: a line's raw content travels as a readable JSON string when it
    is valid UTF-8 and as a hex fallback otherwise, so no source byte is lost.
    {!Stats.jsont} is the one wire form for {!stats} counts, embedded by the
    fact that owns them. No other value is serialized: rendered text crosses as
    an opaque {!to_string} string. This module owns the hunk's and the stats'
    wire shapes and nothing else's.

    {b Evidence, not authority.} Rendered output is display and turn-diff
    evidence, not an applier and not an authority-bearing patch. It is not
    [git apply]-able: labels are display text, there is no [diff --git]/index
    header, and display escaping is not byte-faithful. Keep the source edit or a
    workspace observation when later code must apply or prove a change.

    {b Determinism and replay.} {!render} and {!hunks} are pure, deterministic
    functions of their arguments: the same two texts diff to byte-identical
    output whenever and wherever they run, so a live render and a replayed
    render of the same content agree. Newline presence is part of line identity,
    so the smallest real change — a trailing newline added or removed — never
    renders as unchanged.

    {b Line splitting.} Input is [\n]-delimited; a [\r] is ordinary line
    content, part of {!Hunk.Line.text} and of line identity, and display-escaped
    like any other control byte, so a CRLF file renders a [\r] marker on each
    line.

    {b Cost.} Unbounded rendering, hunking, and statistics are exact and may
    perform unbounded work: the Myers edit-distance search is O(N*D) in the line
    counts [N] and the edit distance [D]. Pass {!Limits.t} to {!render}, or
    [max_edit_distance] to {!hunks}, when input text is large, untrusted, or
    model-controlled; the caller owns the policy, this module the mechanism. *)

(** {1:labels Labels} *)

module Label : sig
  type t
  (** The type for file labels in rendered diffs. Labels appear after [---] and
      [+++] in file headers. They are display text, not filesystem paths, and
      are non-empty with no newline, carriage return, or NUL byte. *)

  val of_string : string -> t
  (** [of_string label] is the trusted diff-header label [label]. It is the
      checked constructor that defines label validity; {!escaped} is derived
      from it.

      Raises [Invalid_argument] if [label] is empty or contains a newline,
      carriage return, or NUL byte. This checks only diff-header structure; it
      does not make [label] safe for terminals, logs, or prompts — {!render}
      escapes display bytes. Use {!escaped} for arbitrary display text. *)

  val escaped : string -> t
  (** [escaped label] is a valid label for arbitrary display text [label].
      Invalid header characters and other non-printing bytes are escaped; the
      empty string becomes ["<empty>"]. The result round-trips: it passes
      through {!to_string} and is accepted again by {!of_string}. *)

  val to_string : t -> string
  (** [to_string label] is [label]'s display text. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf label] formats [label] as display text. *)
end

(** {1:file_changes File changes} *)

module File_change : sig
  type t
  (** The type for one file's text-state transition: a creation, deletion, or
      modification. It carries a {!Label.t} and at least one text state; the
      absent-to-absent state is not representable. It is a write-only input to
      {!render} and {!Stats.of_changes}; the eliminators below observe a change
      a caller holds. *)

  val of_states :
    label:Label.t -> before:string option -> after:string option -> t option
  (** [of_states ~label ~before ~after] is the change from [before] to [after].
      [before = None] is creation; [after = None] is deletion; both [None] is
      [None]. Equal [before] and [after] contents produce a no-op modification,
      which {!render} and the statistics omit. Creations and deletions count as
      changes even when empty. The boundary constructor for consumers holding
      [string option] sides (mutation, session). *)

  val create : label:Label.t -> contents:string -> t
  (** [create ~label ~contents] is a file creation. *)

  val delete : label:Label.t -> contents:string -> t
  (** [delete ~label ~contents] is a file deletion. *)

  val modify : label:Label.t -> before:string -> after:string -> t
  (** [modify ~label ~before ~after] is a modification. Equal contents are valid
      and are omitted by rendering and statistics. *)

  val label : t -> Label.t
  (** [label change] is [change]'s diff label. *)

  val before : t -> string option
  (** [before change] is [change]'s before contents, if any. *)

  val after : t -> string option
  (** [after change] is [change]'s after contents, if any. *)
end

(** {1:statistics Statistics} *)

module Stats : sig
  (** Structured diff statistics and their producers, codec, and equality. *)

  type t = private { files : int; additions : int; deletions : int }
  (** The type for structured diff statistics. Fields are non-negative. [files]
      counts non-noop file changes. [additions] and [deletions] count inserted
      and removed text lines; a non-empty final segment without a newline counts
      as a line, and {b newline presence is part of line identity} — a final
      segment without a newline never equals the same content with one. Counts
      are lines, not bytes, display columns, or changed filesystem objects.

      When {!Textdiff.render} omits a file's content because [limits] were
      exceeded, [files] still counts it while [additions] and [deletions]
      exclude its lines (see {!Textdiff.omitted}). Read fields directly
      ([s.additions]); the type is [private], so a caller reads its fields but
      never forges the record — obtain a value from {!v}, {!Textdiff.render},
      {!of_changes}, or a {!jsont} decode. On the wire it crosses in exactly one
      form, {!jsont}; wire owners embed that object rather than reframing the
      integers. *)

  val v : files:int -> additions:int -> deletions:int -> t
  (** [v ~files ~additions ~deletions] is a {!t} carrying counts computed
      outside this module — such as those from [git diff --numstat], combined
      with in-module counts for untracked files. Use it when the diff engine is
      not this module's and only the counts are wanted; {!Textdiff.render} and
      {!of_changes} are the in-module producers.

      Raises [Invalid_argument] if any argument is negative. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t} — the one wire form for diff counts,
      embedded by whichever fact carries them (a mutation change summary, a
      protocol fact). It encodes the strict object with integer members
      ["files"], ["additions"], and ["deletions"]:

      {[
       { "files": 2, "additions": 5, "deletions": 3 }
      ]}

      Decoding rejects missing members, unknown members, and negative counts, so
      a decoded value satisfies the type's non-negativity invariant by
      construction. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] carry the same [files], [additions],
      and [deletions] counts. *)

  val of_changes : File_change.t list -> t
  (** [of_changes changes] is the statistics for [changes] without rendering.
      No-op modifications are omitted; the result is exact and may perform
      unbounded diff work for modifications. Without rendering limits it equals
      [Textdiff.stats (Textdiff.render changes)]; use {!Textdiff.render} when
      display limits must affect the reported counts. *)
end

type stats = Stats.t = private { files : int; additions : int; deletions : int }
(** The structured diff statistics {!Stats.t}, re-exported at the top level so
    [(stats diff)] field reads and pattern matches stay unqualified. *)

(** {1:hunks Structured hunks} *)

module Hunk : sig
  (** Structured change regions computed by {!Textdiff.hunks}. A hunk is one
      contiguous change region with its surrounding context lines; hunks whose
      context ranges touch or overlap are merged, matching {!render}'s grouping.
      The programmatic diff surface: review scopes and freshness identities
      consume these engine-side, and a frontend renders them from {!jsont}. *)

  module Line : sig
    (** Lines within a hunk. A line is a {!type:kind}, its raw {!text} and
        {!newline} flag, and its 1-based number on each side ({!old_line},
        {!new_line}). *)

    (** The role of a line within a hunk. Removals precede additions within each
        change block, matching {!Hunk.lines} and rendered output. *)
    type kind =
      | Context  (** An unchanged line present in both text states. *)
      | Added  (** A line present only in the after text. *)
      | Removed  (** A line present only in the before text. *)

    type t
    (** The type for one hunk line. *)

    val kind : t -> kind
    (** [kind line] is [line]'s role. *)

    val text : t -> string
    (** [text line] is [line]'s content without its terminating newline. It is
        {e raw bytes} — not display-escaped and not guaranteed valid UTF-8; a
        consumer that renders a line into a terminal or prompt owns its safe
        display, as {!render} does for its own text path. *)

    val newline : t -> bool
    (** [newline line] is [true] iff [line] is newline-terminated in its source.
        Newline presence is part of line identity, relied on by the engine's
        line equality, by replay-rendering fidelity, and by the review evidence
        digest. *)

    val old_line : t -> int option
    (** [old_line line] is [line]'s 1-based line number in the before text, or
        [None] iff {!kind} is [Added]. *)

    val new_line : t -> int option
    (** [new_line line] is [line]'s 1-based line number in the after text, or
        [None] iff {!kind} is [Removed]. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf line] formats [line] as a raw structural line: a [' '], ['-'],
        or ['+'] prefix and the unescaped content, with no terminator or
        missing-newline marker. Inspection only; use {!render} for display. *)
  end

  type t
  (** The type for one contiguous change region with surrounding context. *)

  val old_start : t -> int
  (** [old_start hunk] is the 1-based first before-text line covered. When
      {!old_count} is [0] the hunk inserts, and [old_start] is the before-text
      position the insertion precedes. *)

  val old_count : t -> int
  (** [old_count hunk] is the number of before-text lines covered. *)

  val new_start : t -> int
  (** [new_start hunk] is the 1-based first after-text line covered. When
      {!new_count} is [0] the hunk only removes, and [new_start] is the
      after-text position following the removal. *)

  val new_count : t -> int
  (** [new_count hunk] is the number of after-text lines covered. *)

  val lines : t -> Line.t list
  (** [lines hunk] is [hunk]'s lines in unified-diff order: context interleaved
      with change blocks, removals before additions within each block, source
      order preserved within each side. *)

  val line_counts : t list -> int * int
  (** [line_counts hunks] is [(additions, deletions)]: the number of {!Line.t}s
      across [hunks] whose {!Line.kind} is {!Line.Added} and {!Line.Removed}
      respectively, context lines excluded. A single-file hunk list has no file
      count, so this is a pair, not a {!Stats.t}. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] cover the same ranges with equal
      lines. Consumed by a frontend to locate the cursor's hunk and by the
      review layer's scope matching. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf hunk] formats [hunk] as raw structural text: the [@@] header then
      one line per {!Line.t}, unescaped, missing final newlines unmarked.
      Inspection only; use {!render} for display. *)

  val render : t -> string
  (** [render hunk] is [hunk]'s display-safe unified-diff text: the [@@] header,
      then each line with its [' ']/['-']/['+'] prefix and content escaped
      exactly as the whole-diff {!Textdiff.render} escapes it — control and
      bidirectional-formatting bytes made inert, [\t] kept — with the standard
      missing-final-newline marker. This is the display path for a {!t} obtained
      from the wire (a mutation-diff or review projection); use
      {!Textdiff.render} for a full multi-file diff. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for one hunk. It carries structured diffs across
      the [Mentat_protocol] waist — a mutation-diff query response, a review
      feature projection — to a frontend that renders {!t} but cannot compute
      it.

      It encodes only the {e generating set}: [old_start], [new_start], and each
      line's [kind]/[newline] plus its content. It derives [old_count],
      [new_count], and every {!Line.old_line}/{!Line.new_line} on decode,
      walking the lines as the engine does — so a decoded hunk is internally
      consistent by construction and no derivable field can arrive corrupted
      from the wire. Line content is byte-exact: a valid-UTF-8 line carries a
      readable [text] string, any other line a lowercase-hex [hex] string
      ([Jsont.binary_string] semantics), exactly one member present; a line
      carrying both or neither is rejected on decode.

      {[
        {
          "old_start": 12,
          "new_start": 12,
          "lines": [
            { "kind": "context", "text": "unchanged line", "newline": true },
            { "kind": "removed", "text": "old line", "newline": true },
            { "kind": "added", "hex": "ff00ab", "newline": false }
          ]
        }
      ]}

      It round-trips every hunk {!Textdiff.hunks} produces, byte for byte.
      Consumers compose it for lists and wrap it in their own availability sum;
      this module owns the hunk's wire shape and nothing else's — the enclosing
      protocol fact owns the envelope version. Evolving the member shape is a
      protocol-version bump that exposes a new codec, never a silent mutation of
      this one. *)
end

module Merge : sig
  (** Three-way line merge — the [diff3] of two derived texts against a common
      base. A pure island over the same line splitting, newline identity, and
      Myers engine as {!hunks}; it renders no appliable patch text, and its
      structured {!Region.t}s — not its {!render_markers} projection — are the
      trust path a consumer reconciles.

      The merge keeps whatever [ours] changed relative to [base] and applies
      whatever [theirs] changed, conflicting only where the two touch the same
      region — the semantics of [git merge-file] / a three-way [apply]. Its
      driving use is reverting a superseded selection: [base] is the selection's
      recorded net-after, [ours] the current file carrying a later change,
      [theirs] the net-before to restore. *)

  module Region : sig
    (** A contiguous span of the merged result. *)

    (** The type for one merged span. Each string is a byte-exact slice with its
        newline flags preserved. *)
    type t =
      | Stable of string
          (** A span unchanged by both sides — the base bytes verbatim. *)
      | Resolved of string
          (** A span one side changed and the other left alone, or both changed
              identically: the agreed bytes, byte-exact. *)
      | Conflict of { base : string; ours : string; theirs : string }
          (** A span both sides changed incompatibly, carrying all three
              byte-exact texts so a consumer can reconcile them. *)

    val equal : t -> t -> bool
    (** [equal a b] is [true] iff [a] and [b] are the same case with equal
        bytes. *)

    val jsont : t Jsont.t
    (** [jsont] is the byte-exact codec for one region, tagged by [kind]
        ([stable]/[resolved]/[conflict]). Each string travels as a readable
        [text] member when its bytes are valid UTF-8, else a lowercase-hex [hex]
        member — the same line-content encoding as {!Hunk.jsont}. A leaf codec:
        it is not wired into any protocol response, and exists for this module's
        round-trip tests and a future resolution-carrying revert. *)
  end

  type t
  (** The type for a completed three-way merge: an ordered list of {!Region.t}s
      whose effective text (base for {!Region.Stable}, the agreed bytes for
      {!Region.Resolved}, a chosen or marked rendering for {!Region.Conflict})
      concatenates to the merged file. *)

  val v :
    ?max_edit_distance:int ->
    base:string ->
    ours:string ->
    theirs:string ->
    unit ->
    t option
  (** [v ~base ~ours ~theirs ()] is the three-way merge of [ours] and [theirs]
      over the common ancestor [base]. The result is [None] iff
      [max_edit_distance] is provided and either side's Myers edit distance from
      [base] exceeds it — a {e bound}, not an error, mirroring {!hunks}.

      Deterministic and canonical within a build: identical inputs give
      identical regions. Cross-version stability is not promised; replay never
      recomputes a merge, so only same-build agreement is required. *)

  val regions : t -> Region.t list
  (** [regions t] is [t]'s spans in file order. *)

  val is_clean : t -> bool
  (** [is_clean t] is [true] iff [t] has no {!Region.Conflict} span. *)

  val conflicts : t -> Region.t list
  (** [conflicts t] is [t]'s {!Region.Conflict} spans, in file order. *)

  val resolved : t -> string option
  (** [resolved t] is [Some text], the byte-exact merged file, iff [t] is
      {!is_clean}; [None] when any span conflicts. This is the restore image a
      clean revert merge produces. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have equal regions. *)

  val render_markers : t -> string
  (** [render_markers t] is [t] rendered whole with git-style conflict markers
      ([<<<<<<< ours], [=======], [>>>>>>> theirs]) wrapping each conflict span
      and every other span verbatim. Display for a model or human {e only}: the
      engine never re-parses markers — {!regions} is the structured trust path —
      so a base line that happens to look like a marker poses no hazard. *)

  val jsont : t Jsont.t
  (** [jsont] is the byte-exact codec for a whole merge: a [regions] array of
      {!Region.jsont}. A leaf codec, wired into no protocol response. *)
end

val hunks :
  ?context:int ->
  ?max_edit_distance:int ->
  before:string ->
  after:string ->
  unit ->
  Hunk.t list option
(** [hunks ~before ~after ()] is the line diff of the two texts grouped into
    context-separated hunks.

    [context] is the number of unchanged lines kept around each change region
    (default [3]). The result uses the same line splitting, Myers engine, and
    hunk grouping as {!render}, so ranges agree with rendered [@@] headers for
    the same inputs. Equal texts are [Some []]. The result is [None] iff
    [max_edit_distance] is provided and the Myers edit distance exceeds it — a
    {e bound}, not an error; a consumer maps [None] to its own too-large marker.
    Without it, hunking is exact and may perform unbounded diff work.

    Takes bare [before]/[after], not a {!File_change.t}: the structured path is
    single-file and label-free, since a label is a rendering concern. On
    untrusted input a caller bounds input length upstream and passes
    [max_edit_distance]; the bound is applied before any structured diff reaches
    a frontend.

    Raises [Invalid_argument] if [context] or [max_edit_distance] is negative.
*)

val word_spans :
  ?max_edit_distance:int ->
  before:string ->
  after:string ->
  unit ->
  (int * int) list * (int * int) list
(** [word_spans ~before ~after ()] pairs one changed line's before and after
    text and is [(removed_spans, added_spans)]: the half-open byte ranges to
    emphasize on the removed ([before]) and added ([after]) sides.

    Each side is tokenized on codepoint boundaries into maximal runs of one
    class — whitespace, an ASCII word character, or a single other codepoint —
    so a multi-space gap stays a recoverable token and space-free scripts such
    as CJK do not collapse into one whole-line token. The two token sequences
    diff through the same Myers pass as {!hunks}; a span's start byte is the
    summed byte length of the preceding tokens. Adjacent changed tokens merge
    into one span; a kept token between two changes breaks the run.

    The empty pair suppresses emphasis when the changed-token ratio exceeds one
    half — a whole-line rewrite shares too few words to read as an intra-line
    edit and renders as a plain add/remove pair. [max_edit_distance] bounds the
    token-level Myers pass; when the distance exceeds it the result is the empty
    pair, so a single very long line cannot blow up settle-time cost. Without it
    the pass is exact. *)

(** {1:limits Limits} *)

module Limits : sig
  type t
  (** The type for display rendering limits. Values bound rendered output and
      diff computation; all integer limits are non-negative. *)

  val make :
    max_files:int ->
    max_file_bytes:int ->
    max_lines:int ->
    ?max_edit_distance:int ->
    unit ->
    t
  (** [make ~max_files ~max_file_bytes ~max_lines ?max_edit_distance ()] is a
      display limit set. [max_files] bounds the number of non-noop file entries
      rendered before a remaining-file summary; [max_file_bytes] and [max_lines]
      bound each file by the larger of its before and after states;
      [max_edit_distance], when set, bounds the Myers distance searched per
      file. Files exceeding a byte, line, or edit-distance limit render only
      headers and an omission note.

      Raises [Invalid_argument] if any supplied limit is negative. *)
end

(** {1:rendering Rendering} *)

type t
(** The type for rendered unified-diff text and its structured statistics,
    produced by {!render}. Inspect the text with {!to_string} and the counts
    with {!stats}. Not serialized: its {!to_string} projection crosses the wire
    as an opaque string. *)

val render : ?limits:Limits.t -> ?context:int -> File_change.t list -> t
(** [render changes] is a display-safe unified diff for [changes].

    [context] defaults to [3]. Control and bidirectional-formatting bytes in
    labels and content are escaped (safe for prompts, logs, and terminals); [\t]
    is kept. Entries render in input order; no-op modifications are omitted;
    creations and deletions use [/dev/null] on the absent side; empty creations
    and deletions render headers without hunks; missing final newlines render
    the standard marker. When [limits] are exceeded, affected content is
    replaced by display-only omission notes and the returned {!stats} counts the
    omitted files but not their lines. Without [limits], rendering is exact and
    may perform unbounded diff work.

    Raises [Invalid_argument] if [context] is negative. *)

val stats : t -> stats
(** [stats diff] is [diff]'s structured statistics. *)

val omitted : t -> int
(** [omitted diff] is the number of file changes whose content {!render} elided
    because a display or edit-distance limit was exceeded. These are counted in
    [(stats diff).files] but contribute no additions or deletions. It is [0]
    when {!render} was called without limits, in which case [stats diff] is
    exact and equals {!Stats.of_changes} of the same changes. The evidence path
    gates count-trust on [omitted diff = 0]. *)

val to_string : t -> string
(** [to_string diff] is [diff]'s rendered unified-diff text. *)
