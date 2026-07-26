(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Catalog metadata for a model identity.

    A {!t} annotates a {!Mentat_llm.Model.t} — the canonical request identity —
    with catalog-facing metadata: display name, limits, input and output
    modalities, capabilities, tiered pricing, and lifecycle status. It never
    redefines the provider namespace, API family, or model id; identity lives
    once, in [mentat.llm], and this type only annotates it.

    Construct model metadata with {!make} and read it with the observers below.
    {!Selection} returns a {!t}; extract {!llm} to build a
    {!Mentat_llm.Request}. *)

(** {1:metadata Metadata vocabularies} *)

module Date : sig
  (** Calendar dates for provider metadata, such as release dates. They have no
      timezone or time-of-day component. *)

  type t
  (** The type for calendar dates. Invariant: a valid Gregorian date with year
      in \[[1];[9999]\]. *)

  val make : year:int -> month:int -> day:int -> t
  (** [make ~year ~month ~day] is the date [year]-[month]-[day].

      Raises [Invalid_argument] if [year] is outside \[[1];[9999]\] or if the
      fields do not form a valid Gregorian calendar date. *)

  val of_string : string -> t option
  (** [of_string s] parses [s] as [YYYY-MM-DD]. The spelling must be exactly ten
      bytes with zero-padded month and day. Invalid dates are [None]. *)

  val to_string : t -> string
  (** [to_string t] is [t] formatted as [YYYY-MM-DD]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same date. *)

  val compare : t -> t -> int
  (** [compare a b] orders dates chronologically. The order is compatible with
      {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] as [YYYY-MM-DD]. *)

  val jsont : t Jsont.t
  (** [jsont] maps dates to and from their [YYYY-MM-DD] JSON string. Decoding
      errors on any string {!of_string} rejects. *)
end

module Month : sig
  (** Calendar months for provider metadata, such as knowledge cutoffs. They
      have a year and month but no day, time-of-day, or timezone component. *)

  type t
  (** The type for calendar months. Invariant: a year in \[[1];[9999]\] paired
      with a month in \[[1];[12]\]. *)

  val make : year:int -> month:int -> t
  (** [make ~year ~month] is the month [year]-[month].

      Raises [Invalid_argument] if [year] is outside \[[1];[9999]\] or [month]
      is outside \[[1];[12]\]. *)

  val of_string : string -> t option
  (** [of_string s] parses [s] as [YYYY-MM]. The spelling must be exactly seven
      bytes with a zero-padded month. Invalid months are [None]. *)

  val to_string : t -> string
  (** [to_string t] is [t] formatted as [YYYY-MM]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same month. *)

  val compare : t -> t -> int
  (** [compare a b] orders months chronologically. The order is compatible with
      {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] as [YYYY-MM]. *)

  val jsont : t Jsont.t
  (** [jsont] maps months to and from their [YYYY-MM] JSON string. Decoding
      errors on any string {!of_string} rejects. *)
end

module Modality : sig
  (** Model input and output modalities.

      Tags use the extension grammar accepted by {!extension}: a lowercase ASCII
      letter followed by lowercase letters, digits, ['-'], or ['_']. *)

  type t
  (** The type for a modality tag. *)

  val text : t
  (** [text] is the text modality. *)

  val image : t
  (** [image] is the image modality. *)

  val audio : t
  (** [audio] is the audio modality. *)

  val video : t
  (** [video] is the video modality. *)

  val pdf : t
  (** [pdf] is the PDF document modality. *)

  val extension : string -> t
  (** [extension s] is extension modality [s].

      Raises [Invalid_argument] if [s] is not valid modality syntax or if [s] is
      reserved for a dedicated constructor. *)

  val to_string : t -> string
  (** [to_string t] is [t]'s stable diagnostic spelling. *)

  val of_string : string -> t option
  (** [of_string s] parses [s] as a modality. Built-in tags decode to their
      dedicated values; unknown valid tags decode to [Some (extension s)];
      invalid tags are [None]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same modality. *)

  val compare : t -> t -> int
  (** [compare a b] orders modalities by stable spelling. The order is
      compatible with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps modalities to and from their tag string. Decoding errors on
      any string {!of_string} rejects. *)
end

module Capability : sig
  (** Provider-neutral model behavior capabilities.

      Modalities are represented by {!Modality.t}, not capabilities. Tags use
      the same extension grammar as {!Modality.extension}. *)

  type t
  (** The type for a capability tag. *)

  val tools : t
  (** [tools] is the capability for tool calling. *)

  val reasoning : t
  (** [reasoning] is the capability for provider-visible reasoning controls. *)

  val json_schema : t
  (** [json_schema] is the capability for structured JSON-schema output. *)

  val extension : string -> t
  (** [extension s] is extension capability [s].

      Raises [Invalid_argument] if [s] is not valid capability syntax or if [s]
      is reserved for a dedicated constructor. *)

  val to_string : t -> string
  (** [to_string t] is [t]'s stable diagnostic spelling. *)

  val of_string : string -> t option
  (** [of_string s] parses [s] as a capability tag. Built-in tags decode to
      their dedicated values; unknown valid tags decode to [Some (extension s)];
      invalid tags are [None]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same capability tag. *)

  val compare : t -> t -> int
  (** [compare a b] orders capabilities by stable spelling. The order is
      compatible with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps capabilities to and from their tag string. Decoding errors on
      any string {!of_string} rejects. *)
end

module Pricing : sig
  (** Tiered token pricing.

      A {!t} is a default {!Rate.t} plus optional per-context-window overrides.
      Prices are per million tokens; a missing rate means {e unknown}, not free
      (see {!Model.cost}). *)

  module Rate : sig
    (** Token pricing for one context tier. Missing fields mean unknown, not
        free. Every supplied rate is finite and non-negative. *)

    type t
    (** The type for a token pricing tier. *)

    val make :
      ?input_per_million:float ->
      ?cached_input_per_million:float ->
      ?output_per_million:float ->
      ?cache_write_5m_per_million:float ->
      ?cache_write_1h_per_million:float ->
      unit ->
      t
    (** [make ()] is a pricing tier. Every omitted lane is an unknown rate.

        Raises [Invalid_argument] if any supplied rate is negative or not
        finite. *)

    val input_per_million : t -> float option
    (** [input_per_million t] is [t]'s fresh-input rate, if known. *)

    val cached_input_per_million : t -> float option
    (** [cached_input_per_million t] is [t]'s cache-read rate, if known. *)

    val output_per_million : t -> float option
    (** [output_per_million t] is [t]'s output rate, if known. Reasoning tokens
        bill at this rate. *)

    val cache_write_5m_per_million : t -> float option
    (** [cache_write_5m_per_million t] is [t]'s 5-minute cache-write rate, if
        known. *)

    val cache_write_1h_per_million : t -> float option
    (** [cache_write_1h_per_million t] is [t]'s 1-hour cache-write rate, if
        known. *)

    val equal : t -> t -> bool
    (** [equal a b] is [true] iff [a] and [b] have the same rates. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf t] formats [t] for diagnostics. *)

    val jsont : t Jsont.t
    (** [jsont] maps rates to and from JSON objects. Omitted lanes are absent
        members. Decoding errors on a negative or non-finite rate and on unknown
        members. *)
  end

  type t
  (** The type for tiered pricing. *)

  val make : ?context_over:(int * Rate.t) list -> Rate.t -> t
  (** [make ?context_over default] is pricing with default tier [default].

      [context_over] pairs a context-token threshold with the rate that applies
      strictly above it; it defaults to [[]] and is stored sorted by threshold.
      Raises [Invalid_argument] if a threshold is negative or repeated. *)

  val default : t -> Rate.t
  (** [default t] is the rate that applies when no context threshold matches. *)

  val context_over : t -> (int * Rate.t) list
  (** [context_over t] is [t]'s context-threshold overrides, sorted ascending by
      threshold. *)

  val rate_for : ?context_tokens:int -> t -> Rate.t
  (** [rate_for ?context_tokens t] is the rate for a request of [context_tokens]
      input tokens. With [context_tokens] omitted it is {!default}; otherwise
      the greatest matching threshold wins. Raises [Invalid_argument] if
      [context_tokens] is negative. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same default and overrides.
  *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps pricing to and from JSON objects. An empty [context_over] is
      an absent member. Decoding errors on a negative or repeated threshold and
      on unknown members. *)
end

(** Static model lifecycle status.

    Runtime availability, credentials, config enablement, and policy are host
    facts, not model status. *)
type status =
  | Stable  (** Generally available: {!visible} and {!selectable}. *)
  | Preview
      (** Provider-marked preview or beta: {!visible} and {!selectable}. *)
  | Deprecated
      (** Kept {!visible} for history and diagnostics, but not {!selectable}. *)
  | Unavailable of string
      (** Known model that must not be offered: neither {!visible} nor
          {!selectable}. The payload is a user-facing reason and must not
          contain secrets. *)

(** {1:models Models} *)

type t
(** Catalog metadata for a {!Mentat_llm.Model.t}. The [mentat.llm] identity is
    the canonical request identity; this value adds static catalog metadata. *)

val make :
  Mentat_llm.Model.t ->
  ?display_name:string ->
  ?family:string ->
  ?released_on:Date.t ->
  ?knowledge_cutoff:Month.t ->
  ?context_window:int ->
  ?max_output_tokens:int ->
  ?default_reasoning:Mentat_llm.Request.Options.Reasoning_effort.t ->
  ?supported_reasoning:Mentat_llm.Request.Options.Reasoning_effort.t list ->
  ?input_modalities:Modality.t list ->
  ?output_modalities:Modality.t list ->
  ?capabilities:Capability.t list ->
  ?pricing:Pricing.t ->
  ?status:status ->
  unit ->
  t
(** [make llm ()] is catalog metadata for [llm].

    [input_modalities] and [output_modalities] default to [[Modality.text]].
    [status] defaults to [Stable]. Other list arguments default to [[]].
    Modality, capability, and reasoning lists are stored in deterministic order
    after duplicate rejection. If [supported_reasoning] is non-empty,
    [default_reasoning], when supplied, must appear in it.

    Raises [Invalid_argument] if [display_name] or [family] is empty,
    [context_window] or [max_output_tokens] is non-positive, modality,
    capability, or supported-reasoning lists contain duplicates, or
    [default_reasoning] is supplied but is not in a non-empty
    [supported_reasoning] list. *)

val llm : t -> Mentat_llm.Model.t
(** [llm t] is [t]'s canonical request model identity. *)

val provider : t -> Mentat_llm.Provider.t
(** [provider t] is the provider namespace of {!llm}[ t]. *)

val api : t -> Mentat_llm.Model.Api.t
(** [api t] is the provider-local API family of {!llm}[ t]. *)

val id : t -> string
(** [id t] is the provider-native model id of {!llm}[ t]. *)

val selector : t -> Selector.t
(** [selector t] is [provider t]/[id t] as a {!Selector.t} — the canonical
    user-facing selector for configuration and display. *)

val display_name : t -> string option
(** [display_name t] is [t]'s display name, if any. *)

val family : t -> string option
(** [family t] is [t]'s provider-supplied model family, if known. *)

val released_on : t -> Date.t option
(** [released_on t] is [t]'s release date, if known. *)

val knowledge_cutoff : t -> Month.t option
(** [knowledge_cutoff t] is the month through which [t]'s training data is
    documented, if known. *)

val context_window : t -> int option
(** [context_window t] is [t]'s context-window metadata, if known. *)

val max_output_tokens : t -> int option
(** [max_output_tokens t] is [t]'s output-token metadata, if known. *)

val default_reasoning :
  t -> Mentat_llm.Request.Options.Reasoning_effort.t option
(** [default_reasoning t] is [t]'s default reasoning effort, if known. *)

val supported_reasoning :
  t -> Mentat_llm.Request.Options.Reasoning_effort.t list
(** [supported_reasoning t] is [t]'s supported reasoning efforts in
    deterministic order. *)

val input_modalities : t -> Modality.t list
(** [input_modalities t] is [t]'s accepted input modalities in deterministic
    order. *)

val output_modalities : t -> Modality.t list
(** [output_modalities t] is [t]'s produced output modalities in deterministic
    order. *)

val has_input_modality : Modality.t -> t -> bool
(** [has_input_modality m t] is [true] iff [m] appears in
    {!input_modalities}[ t]. *)

val capabilities : t -> Capability.t list
(** [capabilities t] is [t]'s capabilities in deterministic order. *)

val has_capability : Capability.t -> t -> bool
(** [has_capability c t] is [true] iff [c] appears in {!capabilities}[ t]. *)

val pricing : t -> Pricing.t option
(** [pricing t] is [t]'s pricing metadata, if known. *)

val cost : t -> Mentat_llm.Usage.t -> float option
(** [cost t usage] is the monetary cost of [usage] under [t]'s pricing, or
    [None] when [t] has no pricing metadata, or when a lane [usage] actually
    spent has no rate (an unknown rate is not billed as free).

    The price tier is chosen with {!Pricing.rate_for} for a context of
    [Mentat_llm.Usage.input_total usage] (fresh input plus cache reads and
    writes), or [max_int] if that total overflows. Lanes bill against that tier
    as:

    - [input] at {!Pricing.Rate.input_per_million};
    - [cache_read] at {!Pricing.Rate.cached_input_per_million};
    - [cache_write] at {!Pricing.Rate.cache_write_5m_per_million} (the usage
      record does not distinguish cache TTLs);
    - [output] and [reasoning] at {!Pricing.Rate.output_per_million} (reasoning
      tokens bill as output).

    Each lane contributes [tokens / 1e6 *. rate]; a lane with zero tokens never
    forces [None] even if its rate is unknown. *)

val rate_per_million : t -> float option * float option
(** [rate_per_million t] is [t]'s [(input, output)] price per million tokens at
    the base tier — the fresh-input and output lanes of {!Pricing.default} —
    each [None] for an unpriced lane, and [(None, None)] when [t] has no pricing
    metadata. It is the base tier {!cost} bills a request that stays below every
    {!Pricing.context_over} threshold against, so for such a request the cost of
    a single lane in isolation is exactly [tokens / 1e6] times the rate reported
    here. A missing rate is unknown, never free. *)

val status : t -> status
(** [status t] is [t]'s static lifecycle status. *)

val visible : t -> bool
(** [visible t] is [true] iff [status t] is not [Unavailable _].

    This is a static lifecycle predicate. Runtime listing may also consider
    config, credentials, and policy. *)

val selectable : t -> bool
(** [selectable t] is [true] iff [status t] is [Stable] or [Preview].

    This is static lifecycle eligibility only. Runtime selection may also
    consider config, credentials, and policy. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have the same identity and metadata.
*)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] for diagnostics. *)

val jsont : t Jsont.t
(** [jsont] maps model metadata to and from JSON objects, for protocol model and
    configuration query projections. Every value {!make} accepts round-trips:
    absent modality members decode to the [[Modality.text]] default, an absent
    status to [Stable], and empty [supported_reasoning] and [capabilities]
    members are omitted on encode. Decoding errors on unknown members, on any
    object {!make} rejects, and on an ["unavailable"] status without a reason or
    a status reason paired with any other status. *)
