(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Boundary error diagnostics.

    A diagnostic is the validated, renderable form of a user-fixable boundary
    error: a message, optional context, and actionable hints. A producer
    carrying fixable knowledge builds one where that knowledge lives; hosts
    render every such error uniformly at one boundary with {!to_string}.
    Diagnostics are presentation values — build and render them; do not inspect
    or parse them for control flow. The only dependency is the pure [Jsont]
    codec, so any error producer can attach hints without a host runtime. *)

type t
(** The type for diagnostics.

    Invariant: [message] is non-empty and single-line, every hint is non-empty
    and single-line, and [context] is non-empty when present. The hint list
    itself may be empty. *)

val make : ?context:string -> ?hints:string list -> string -> t
(** [make ?context ?hints message] is a diagnostic with primary description
    [message]. [message] must be a single line: renderers may style it as a head
    line and the rest as secondary detail. Put multi-line prose in [context].

    [context], when present, is secondary detail printed verbatim after the
    message. It may contain multiple lines. [hints] defaults to [[]]; each hint
    is one actionable suggestion, rendered as a ["Hint: …"] line by
    {!to_string}.

    Raises [Invalid_argument] if [message] is empty or contains a newline, if
    any hint is empty or contains a newline, or if [context] is empty when
    present. *)

val of_text : ?hints:string list -> string -> t
(** [of_text ?hints text] is a diagnostic from opaque pre-rendered error text,
    such as a JSON decoder trace or transport-library message. The first line
    becomes the primary message; remaining text, when present, becomes
    [context]. A single trailing line break is ignored.

    Prefer {!make} for native errors whose primary message and context are
    already known separately.

    Raises [Invalid_argument] if [text] is empty, if its first line is empty, or
    if any hint is empty or contains a newline. *)

val of_text_or : ?hints:string list -> fallback:string -> string -> t
(** [of_text_or ?hints ~fallback text] is a total constructor over opaque error
    [text]: unlike {!of_text} it never rejects [text], falling back to
    [fallback] as the primary message when [text] cannot supply one itself.

    - [text] empty: the diagnostic is [make ~hints fallback].
    - [text]'s first line empty (it begins with a line break): the diagnostic is
      [make ~context:text ~hints fallback], carrying the {e full} original
      [text] as context.
    - otherwise: the diagnostic is [of_text ~hints text].

    [hints] threads into every branch. The only message that can violate
    {!make}'s invariants is the caller's own [fallback] or [hints], so those are
    the sole failures.

    Raises [Invalid_argument] if [fallback] is empty or contains a newline, or
    if any hint is empty or contains a newline. *)

(** {1:hints Hints}

    Produce hints where the candidate knowledge lives — the code that fails a
    lookup has the valid spellings. Both return a [string list] fragment (empty
    or one hint) to splice into {!make}'s [~hints]. *)

val did_you_mean : string -> candidates:string list -> string list
(** [did_you_mean s ~candidates] keeps candidates within edit distance two of
    [s], in [candidates] order, as one ["did you mean …?"] hint; empty when none
    is close. Exact matches are never suggested.

    Raises [Invalid_argument] if a candidate is empty or contains a newline. *)

val suggest : string list -> string list
(** [suggest candidates] renders [candidates] verbatim — the caller has already
    judged them relevant — as one ["did you mean …?"] hint: one directly, two
    joined with [" or "], longer lists with commas before the final [" or "].
    Empty for [[]].

    Raises [Invalid_argument] if a candidate is empty or contains a newline. *)

(** {1:preds Predicates and comparisons} *)

val message : t -> string
(** [message t] is [t]'s single-line primary description, without context or
    hints. One-line surfaces (status rows, footers) render this head alone;
    {!to_string} carries the full detail. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have the same message, context, and
    hints — iff they render identically. *)

(** {1:fmt Formatting} *)

val to_string : t -> string
(** [to_string t] renders [t] as user-facing text: the message, then the context
    when present, then one ["Hint: …"] line per hint, each on its own line. Not
    stable storage syntax; do not parse it. *)

(** {1:codec Codec} *)

val jsont : t Jsont.t
(** [jsont] encodes the strict JSON object with the string member ["message"],
    the optional string member ["context"] (omitted when absent), and the string
    array member ["hints"] (omitted when empty; an absent member decodes as the
    empty list). The codec is structure-preserving — message, context, and hints
    cross a boundary as themselves, not as rendered text — so a decoded
    diagnostic renders exactly as the encoded one. Decoding enforces {!make}'s
    invariants and rejects unknown members. *)
