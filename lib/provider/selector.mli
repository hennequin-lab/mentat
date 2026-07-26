(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Parsed [provider/model] selectors.

    A selector names a provider namespace and a provider-local model id — the
    [provider/model] spelling users write in configuration, CLI flags, and
    diagnostics. It is pure syntax: it does not assert that the provider or the
    model is declared in any {!Catalog.t}. Resolve a selector against a catalog
    with {!Catalog.find} or {!Catalog.resolve}.

    Parse untrusted text with {!of_string} and keep the parsed value, rather
    than re-splitting the string downstream. Construct a selector from an
    already-typed provider with {!make}. *)

(** {1:errors Errors} *)

module Error : sig
  (** The reason a string is not a valid [provider/model] selector. *)
  type t =
    | Empty  (** The input was empty or only whitespace. *)
    | Missing_separator
        (** The input had no ['/'] separating the provider from the model. *)
    | Empty_provider
        (** The provider segment before the first ['/'] was empty. *)
    | Empty_model  (** The model segment after the first ['/'] was empty. *)
    | Invalid_provider of { input : string; message : string }
        (** The provider segment [input] is not a valid provider namespace.
            [message] is the diagnostic from {!Mentat_llm.Provider.make}. *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic for [e]. It is for display, not
      programmatic matching. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] formats {!message} on [ppf]. *)
end

(** {1:selectors Selectors} *)

type t
(** The type for parsed [provider/model] selectors. Two selectors are equal iff
    they name the same provider and the same provider-local model id. *)

val make : provider:Mentat_llm.Provider.t -> id:string -> t
(** [make ~provider ~id] is the selector for provider-local model [id] under
    [provider].

    Raises [Invalid_argument] if [id] is empty or has surrounding whitespace.
    This is trusted construction from an already-typed provider; parse untrusted
    text with {!of_string}. *)

val of_model : Mentat_llm.Model.t -> t
(** [of_model m] is
    [make ~provider:(Mentat_llm.Model.provider m) ~id:(Mentat_llm.Model.id m)].
    The bridge from a canonical model to its selector spelling. Raises like
    {!make} on an id [make] rejects. *)

val of_string : string -> (t, Error.t) result
(** [of_string s] parses [s] as [provider/model].

    Leading and trailing whitespace is ignored. Parsing splits on the {e first}
    ['/']; the remainder, trimmed of surrounding whitespace, is the
    provider-local model id and may itself contain ['/'] (local weight paths,
    model-daemon ids). The model id is not otherwise validated. Errors when [s]
    is empty, has no ['/'], has an empty provider or model segment, or has a
    provider segment that is not a valid provider namespace. *)

val provider : t -> Mentat_llm.Provider.t
(** [provider t] is [t]'s provider namespace. *)

val id : t -> string
(** [id t] is [t]'s provider-local model id. *)

val to_string : t -> string
(** [to_string t] is [provider t]/[id t], the canonical selector spelling.
    [of_string (to_string t)] is [Ok t]: both constructors only produce
    trim-stable ids, so the round-trip is unconditional. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] name the same provider and model id.
*)

val compare : t -> t -> int
(** [compare a b] orders selectors by provider, then by model id. The order is
    compatible with {!equal}. *)

val jsont : t Jsont.t
(** [jsont] maps selectors to and from their canonical [provider/model] JSON
    string. Decoding errors on any string {!of_string} rejects. *)
