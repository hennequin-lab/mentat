(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Portable, normalized, POSIX-lexical path syntax.

    A path is slash-separated text after lexical normalization. {!Rel.t} is
    rooted at an implicit caller-owned root and renders ["."] at that root;
    {!Abs.t} is rooted at [/]. The two are deliberately distinct types, not one
    type with a rootedness predicate: {!Rel} and {!Abs} obey opposite [..] laws
    (a relative [..] that would rise above its root is an {e error}; an absolute
    [/..] {e clamps} to [/]), and the type of a value is what proves its
    rootedness to callers such as the security layer. A {!Rel.t} received by a
    function cannot secretly be an absolute path. Both forms reject malformed
    components at construction and store one canonical spelling per logical
    path, so two spellings of the same path are equal and
    {!Rel.compare}/{!Abs.compare} are total orders compatible with equality —
    safe {!Rel.Set} and {!Rel.Map} keys.

    {b Pure syntax, not evidence.} Values do not inspect the filesystem, resolve
    symlinks, consult a current directory, name a host authority, read an
    environment, or prove workspace containment. A {!Rel.t} is not a filesystem
    confinement proof. Resolve those properties in the workspace, security, or
    host-effect layer that owns them; this library imports no Eio, Unix,
    filesystem, or other mentat code.

    {b Normalization is lexical.} Construction folds [.], [//], and [..] purely
    over the component text; it never consults the filesystem. Under symbolic
    links a lexical result can differ from the real path — ["a/b/../c"]
    normalizes to ["a/c"] even if ["a/b"] is a symlink whose real parent is
    elsewhere. Where symlink-correct resolution is required, a host-effect layer
    runs [realpath] and re-checks containment with {!Abs.is_within} on the
    resolved value.

    {b Bytes, not text.} A component is an arbitrary byte string with a small
    structural exclusion set (see {!Rel.is_component}); this module applies no
    Unicode normalization, case folding, or UTF-8 validation. Newlines, spaces,
    colons, and high/UTF-8 bytes are all valid component bytes. Whether a
    component is well-formed UTF-8 is the concern of the layer that displays or
    edits it, not of this library; POSIX filenames are byte strings that need
    not be UTF-8.

    {b Slash only.} [/] is the sole separator. Backslash roots and drive
    prefixes (such as ["C:"]) are rejected, never interpreted as native Windows
    syntax; NUL is rejected. {!Abs.to_string} always uses [/]; there is no
    platform-dependent behaviour. The slash-only invariant is load-bearing for
    canonical-string equality, so Windows support would be a breaking redesign,
    not an addition. See {!Rel.of_string} and {!Abs.of_string} for how
    absolute-looking input is classified.

    Parse raw input with the result-returning parsers, then keep the narrower
    kind in the type. Compose typed values with {!Rel.append}, {!Rel.resolve},
    {!Abs.append_rel}, {!Abs.resolve}, and {!Abs.resolve_any}; test containment
    with {!Rel.relativize}/{!Rel.is_within}/{!Rel.is_strictly_within} and their
    {!Abs} counterparts. *)

(** {1:errors Errors} *)

module Error : sig
  (** Structured path-syntax errors, shared by {!Rel} and {!Abs}.

      One shared type serves both kinds so that a single {!t} can be embedded by
      downstream errors (workspace resolution, patch parsing) without callers
      converting between two per-kind error types. Match constructors for
      recovery — the durable rule and permission decoders do; {!message} and
      {!pp} are diagnostics whose wording is not a stable interface.

      {b Classification is relative to the parser, by design.} Each parser has a
      binary "is this my kind?" test and names the rejection after the syntactic
      category it detected — the category it does not accept — not after the
      kind it wanted. A {!Rel} parser refuses absolute-looking input (leading
      [/], backslash, or drive prefix) with {!Absolute}; an {!Abs} parser
      refuses any input that is not slash-rooted with {!Relative}. Because the
      two draw the boundary differently for backslash and drive prefixes, the
      same input can yield {!Absolute} from {!Rel} and {!Relative} from {!Abs}
      (["\\a"], ["C:a"]). That is not a contradiction: the classification
      depends on which parse was attempted. *)
  type t =
    | Empty  (** The input was [""], where empty input is not accepted. *)
    | Relative
        (** An {!Abs} parser required slash-rooted syntax and the input was not
            slash-rooted. Emitted only by {!Abs} parsers. A backslash- or
            drive-rooted input lacks a leading [/], so {!Abs} reports it here as
            {!Relative} (contrast {!Rel}, which reports the same input as
            {!Absolute}). *)
    | Absolute
        (** A {!Rel} parser — or {!Abs.resolve}/{!Abs.resolve_any} resolving a
            fragment below a base — required root-relative syntax and the input
            looked absolute: it began with [/], a backslash, or an ASCII-letter
            drive prefix such as ["C:"]. *)
    | Escapes_root
        (** Relative resolution of [..] would rise above the implicit root.
            Emitted only by {!Rel} parsers: {!Abs} clamps [/..] to [/] and never
            escapes (see {!Abs.of_string}). *)
    | Malformed_component of string
        (** [Malformed_component c] carries the rejected component [c]. A
            component is malformed iff it is empty, [.], [..], contains [/],
            backslash, or NUL, or begins with an ASCII-letter drive prefix such
            as ["C:"]. Exactly those bytes and forms are excluded; every other
            byte — newline, space, a non-leading [:], tab, high/UTF-8 bytes — is
            a valid component byte. See {!Rel.is_component}. *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic. For display, not programmatic
      matching. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same error. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] formats {!message}. *)
end

(** {1:rel Relative paths} *)

module Rel : sig
  type t
  (** A normalized root-relative lexical path.

      {b Invariant}, upheld by every constructor: {!root} renders ["."]; every
      other value is a non-empty [/]-separated list of components with no
      leading or trailing [/], no [//], no [.] or [..] segment, and every
      component satisfying {!is_component}. A [Rel.t] never begins with [..].
      There is exactly one [t] per logical path, which is what makes {!equal} a
      byte comparison and what {!is_within} and the security layer rely on.

      A [Rel.t] is not a filesystem confinement proof. *)

  val root : t
  (** The empty path below the implicit root. Renders ["."]. *)

  val is_root : t -> bool
  (** [is_root p] is [true] iff [p] is {!root}. *)

  (** {2:rel_parsing Parsing} *)

  val of_string : string -> (t, Error.t) result
  (** [of_string s] parses and normalizes root-relative [s].

      Normalization removes [.] segments, collapses repeated and trailing [/],
      and resolves [..] against preceding components. Errors:
      - {!Error.Empty} if [s] is [""];
      - {!Error.Absolute} if [s] begins with [/], a backslash, or a drive prefix
        (absolute-looking input is not reinterpreted as relative);
      - {!Error.Escapes_root} if a [..] would rise above {!root} (["../a"] and
        ["a/../../b"] are errors, not clamped);
      - {!Error.Malformed_component} if a surviving component is malformed.

      [/] is the only separator. The empty-string encoding of {!root} belongs to
      whatever boundary format owns it — here {!root} is ["."], never [""]. *)

  val of_string_exn : string -> t
  (** [of_string_exn s] is {!of_string} [s], raising [Invalid_argument] on
      failure.

      Use it for input the caller treats as a well-formedness {e invariant}
      whose violation is a bug: source-code literals (["src/main.ml"]) and
      host-supplied paths the caller has already established as good — for
      example a relative fragment derived from a validated absolute (see
      {!Abs.of_string_exn} for the absolute analogue). Use {!of_string} for any
      input that could legitimately be malformed (configuration, CLI, model tool
      arguments). *)

  (** {2:rel_rendering Rendering} *)

  val to_string : t -> string
  (** [to_string p] is [p]'s canonical string. {!root} is ["."]. This is the
      path's serialized form: {!of_string} [(to_string p)] is [Ok p]. *)

  val components : t -> string list
  (** [components p] is [p]'s ordered components. {!root} has none. Every
      element satisfies {!is_component}. Prefer this to splitting {!to_string}
      by hand. *)

  val is_component : string -> bool
  (** [is_component c] is [true] iff [c] is a valid single path component: [c]
      is non-empty, not [.] or [..], contains no [/], backslash, or NUL, and
      does not begin with an ASCII-letter drive prefix (an ASCII letter
      immediately followed by [:], such as ["C:"]).

      The exclusion set is narrow and exact: only NUL, [/], and backslash bytes
      are forbidden, and only an ASCII-letter-then-[:] prefix is a drive — any
      single ASCII letter, either case, so ["C:a"] and ["a:b"] are rejected
      while ["1:a"] and ["ab:cd"] are valid components. *)

  (** {2:rel_composing Composing} *)

  val add_component : t -> string -> (t, Error.t) result
  (** [add_component p c] is [Ok] of [p] extended by the single component [c],
      or [Error (Malformed_component _)] if [c] fails {!is_component}. Unlike
      the string parsers, [c] is never split on [/]: it is one component or it
      is rejected. This is the only appending operation that validates its right
      operand, because it takes an untyped string; the typed combinators below
      trust the invariant on their operands. *)

  val append : t -> t -> t
  (** [append a b] places [b] below [a]. {!root} is the identity on either side.
      Total and never re-validating: both operands are already normalized valid,
      so [components (append a b) = components a @ components b] with no check.
  *)

  val resolve : t -> string -> (t, Error.t) result
  (** [resolve base s] parses [s] as relative syntax below [base], identical to
      {!of_string} except that a leading [..] resolves against [base] rather
      than {!root} (["../a"] below ["src/lib"] is ["src/a"]).
      {!Error.Escapes_root} still fires if [..] would rise above {!root};
      [resolve root s] equals {!of_string} [s]. Errors: {!Error.Empty},
      {!Error.Absolute}, {!Error.Escapes_root}, {!Error.Malformed_component}. *)

  (** {2:rel_decomposing Decomposing} *)

  val parent : t -> t option
  (** [parent p] is [Some] of [p] without its last component, or [None] iff [p]
      is {!root}. [parent (of_string_exn "a") = Some root]. *)

  val basename : t -> string option
  (** [basename p] is [Some] of [p]'s last component, or [None] iff [p] is
      {!root}. *)

  (** {2:rel_containment Containment} *)

  val relativize : root:t -> t -> t option
  (** [relativize ~root p] is [Some suffix] iff [p] is [root] or lies below it
      by whole normalized components, where [append root suffix = p]; [None]
      otherwise. Equal paths give [Some] {!root} (equality counts as within).

      Containment is by component boundary, not string prefix: with [root] =
      ["src"], the sibling ["src-lib/a"] gives [None] even though ["src"] is a
      string prefix of ["src-lib"]. This boundary guard is load-bearing for the
      security layer. [relativize] never climbs with [..]. This is the
      containment core; the two predicates below are its boolean projections. *)

  val is_within : root:t -> t -> bool
  (** [is_within ~root p] is [Option.is_some (relativize ~root p)]: [p] is
      [root] or lies below it (inclusive). The named boolean a containment
      consumer uses instead of re-deriving containment from {!relativize}. *)

  val is_strictly_within : root:t -> t -> bool
  (** [is_strictly_within ~root p] is [is_within ~root p && not (equal root p)]:
      [p] lies {e strictly} below [root] (equivalently, [root] is a proper
      ancestor of [p]). Equivalently, [relativize ~root p] is [Some suffix] with
      [suffix] not {!root}.

      This is the strict-descendant predicate a containment consumer otherwise
      hand-rolls as [not (equal a b) && Option.is_some (relativize ~root:a b)].
      It is separate from {!is_within} because the equality boundary is
      load-bearing and easy to forget: a root de-duplicator using the inclusive
      form would prune every root as a descendant of itself. *)

  (** {2:rel_std Comparing and printing} *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same path. Exact byte
      equality of {!to_string}, because the stored form is canonical.
      Case-sensitive: ["A"] and ["a"] are distinct (lexical, not filesystem,
      equality). *)

  val compare : t -> t -> int
  (** [compare a b] is a total order consistent with {!equal}, ordering by the
      canonical string {e byte for byte}.

      {b This is not component order}: because [-] (0x2d) precedes [/] (0x2f),
      [compare (of_string_exn "a-b") (of_string_exn "a/b") < 0], whereas
      grouping by component would order ["a/b"] first. The order is for
      {!Set}/{!Map} keying and stable output; a consumer that needs
      parent-before-child tree order must sort by {!components}. *)

  val hash : t -> int
  (** [hash p] is an unseeded hash consistent with {!equal}. *)

  module Set : Set.S with type elt = t
  (** Sets of relative paths, keyed by {!compare}. *)

  module Map : Map.S with type key = t
  (** Maps keyed by relative paths, by {!compare}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf p] formats {!to_string}. *)
end

(** {1:abs Absolute paths} *)

module Abs : sig
  type t
  (** A normalized absolute lexical path.

      {b Invariant}: {!root} renders ["/"]; every other value begins with [/]
      and continues as [/]-separated components with no [//], no trailing [/],
      no [.] or [..] segment, and every component satisfying
      {!Rel.is_component}. One [t] per logical path.

      An [Abs.t] is portable slash-rooted {e syntax}, not evidence that a host
      path exists, is native to the current platform, or belongs to a workspace.
  *)

  val root : t
  (** The slash root. Renders ["/"]. *)

  val is_root : t -> bool
  (** [is_root p] is [true] iff [p] is {!root}. *)

  (** {2:abs_parsing Parsing} *)

  val of_string : string -> (t, Error.t) result
  (** [of_string s] parses and normalizes slash-rooted [s].

      Normalization removes [.] segments, collapses repeated and trailing [/],
      and resolves [..]. {b Resolving [..] at {!root} stays at {!root}} — the
      one behavioural asymmetry with {!Rel.of_string}: ["/../a"] normalizes to
      ["/a"] rather than erroring, because an absolute path cannot escape [/]
      (POSIX [/..] = [/]). Errors:
      - {!Error.Empty} if [s] is [""];
      - {!Error.Relative} if [s] does not begin with [/] (a backslash- or
        drive-rooted input is not slash-rooted, hence {!Error.Relative}, not
        {!Error.Absolute});
      - {!Error.Malformed_component} if a surviving component is malformed.

      An {!Abs} parser never returns {!Error.Escapes_root}. *)

  val of_string_exn : string -> t
  (** [of_string_exn s] is {!of_string} [s], raising [Invalid_argument] on
      failure. Same charter as {!Rel.of_string_exn}: source-code literals
      (["/"], ["/tmp"]) and host-supplied absolutes the caller has established
      as good — the process working directory, [$HOME], [Unix.realpath] output,
      or [Eio.Path.native_exn] of a live directory. Such values are
      known-well-formed absolutes for which the result form is pure noise; the
      exn form documents the caller's invariant and fails loudly if it is ever
      violated. Use {!of_string} for input that could legitimately be malformed.
  *)

  (** {2:abs_rendering Rendering} *)

  val to_string : t -> string
  (** [to_string p] is [p]'s canonical string. {!root} is ["/"].

      This is the single value a host-effect layer bridges to Eio
      ([Eio.Path.(fs / to_string p)]) and hands to [Unix.realpath]/[Unix.stat],
      owned once at that boundary rather than re-stringified per subsystem. It
      is the path's serialized form: {!of_string} [(to_string p)] is [Ok p]. *)

  val components : t -> string list
  (** [components p] is [p]'s ordered components. {!root} has none. Every
      element satisfies {!Rel.is_component}. *)

  (** {2:abs_composing Composing} *)

  val add_component : t -> string -> (t, Error.t) result
  (** [add_component p c] is [Ok] of [p] extended by the single component [c],
      or [Error (Malformed_component _)] if [c] fails {!Rel.is_component}. The
      only {!Abs} appender that validates an untyped operand. *)

  val append_rel : t -> Rel.t -> t
  (** [append_rel a r] places the relative path [r] below the absolute path [a].
      {!Rel.root} is the identity; appending a non-root [r] below {!root}
      preserves the slash root ([append_rel root r] renders
      ["/" ^ Rel.to_string r]). Total and never re-validating: both operands
      carry their invariants, so
      [components (append_rel a r) = components a @ Rel.components r] with no
      check. This is the bridge from the relative to the absolute kind. *)

  val resolve : t -> string -> (t, Error.t) result
  (** [resolve base s] parses [s] as a relative fragment below the absolute
      [base]. [..] climbs to the parent and clamps at {!root} (never escapes, so
      no {!Error.Escapes_root}). Absolute-looking [s] (leading [/], backslash,
      or drive prefix) is {!Error.Absolute}; use {!resolve_any} to accept
      slash-rooted input. This is the strict, reject-absolute counterpart of
      {!resolve_any}. Errors: {!Error.Empty}, {!Error.Absolute},
      {!Error.Malformed_component}. *)

  val resolve_any : base:t -> string -> (t, Error.t) result
  (** [resolve_any ~base s] resolves [s] whether it is absolute or relative: a
      slash-rooted [s] is normalized as-is ([base] ignored); any other [s] is
      resolved below [base] exactly as {!resolve}. This is the raw-input
      boundary primitive for "the model/config/CLI handed me a path that may be
      absolute or relative". Backslash- and drive-rooted inputs are not
      slash-rooted, so they fall to {!resolve} and yield {!Error.Absolute}.
      Errors: {!Error.Empty}, {!Error.Absolute}, {!Error.Malformed_component}.
  *)

  (** {2:abs_decomposing Decomposing} *)

  val parent : t -> t option
  (** [parent p] is [Some] of [p] without its last component, or [None] iff [p]
      is {!root}. [parent (of_string_exn "/a") = Some root]. *)

  val basename : t -> string option
  (** [basename p] is [Some] of [p]'s last component, or [None] iff [p] is
      {!root}. *)

  (** {2:abs_containment Containment} *)

  val relativize : root:t -> t -> Rel.t option
  (** [relativize ~root p] is [Some suffix] (a {!Rel.t}) iff [p] is [root] or
      lies below it by whole normalized components, where
      [append_rel root suffix = p]; [None] otherwise. Equal paths give [Some]
      {!Rel.root}. Same component-boundary guard as {!Rel.relativize}: a sibling
      that merely shares a string prefix gives [None]. The result is a {!Rel.t}
      — the workspace-import primitive recovers this suffix. *)

  val is_within : root:t -> t -> bool
  (** [is_within ~root p] is [Option.is_some (relativize ~root p)]: inclusive
      lexical containment. A host-effect layer calls it on a
      [Unix.realpath]-resolved [Abs.t] to re-check symlink containment after its
      impure step. *)

  val is_strictly_within : root:t -> t -> bool
  (** [is_strictly_within ~root p] is [is_within ~root p && not (equal root p)]:
      [root] is a proper ancestor of [p], excluding equality. See
      {!Rel.is_strictly_within}. *)

  (** {2:abs_std Comparing and printing} *)

  val equal : t -> t -> bool
  (** [equal a b] is exact byte equality of the canonical strings.
      Case-sensitive; see {!Rel.equal}. *)

  val compare : t -> t -> int
  (** [compare a b] is a total order consistent with {!equal}, by canonical
      string bytes — {b not} component order (see {!Rel.compare}). *)

  val hash : t -> int
  (** [hash p] is an unseeded hash consistent with {!equal}. *)

  module Set : Set.S with type elt = t
  (** Sets of absolute paths, keyed by {!compare}. *)

  module Map : Map.S with type key = t
  (** Maps keyed by absolute paths, by {!compare}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf p] formats {!to_string}. *)
end
