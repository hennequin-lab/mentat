(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Indexed provider/model catalogs.

    A catalog is a validated set of {!Declaration.t} values with unique provider
    ids, supporting pure provider and model lookup. It holds no client,
    availability, or credential state and has no codec: a catalog may carry
    dynamic-model closures, and durable model identity is {!Mentat_llm.Model.t},
    not a catalog index. *)

(** {1:errors Errors} *)

(** Lookup failures for {!find}, {!resolve}, and {!models_for}.

    Catalog {e construction} does not fail this way: {!make}'s only failure is a
    duplicated provider, which it raises. *)
module Error : sig
  type t =
    | Invalid_selector of {
        input : string;
        error : Selector.Error.t;
        candidates : Selector.t list;
      }
        (** [input] is not a valid selector. [candidates] are declared
            selectors, of every status, to suggest: for input lacking a ['/']
            that exactly matches declared model ids, those models' selectors;
            for empty input, none; otherwise every declared selector. *)
    | Unknown_provider of {
        provider : Mentat_llm.Provider.t;
        known : Mentat_llm.Provider.t list;
      }
        (** [provider] is not declared. [known] lists the declared providers. *)
    | Unknown_model of {
        provider : Mentat_llm.Provider.t;
        model : string;
        known : string list;
      }
        (** [model] does not resolve for [provider]. [known] lists [provider]'s
            declared provider-local model ids. *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] formats {!message}. *)
end

(** {1:catalogs Catalogs} *)

type t
(** The type for a validated catalog. *)

val make : Declaration.t list -> t
(** [make declarations] is a catalog of [declarations] in order.

    Raises [Invalid_argument] if two declarations share a provider id. In v1 the
    catalog is assembled from built-in declarations by trusted code, so a
    duplicate is a programmer error. *)

val declarations : t -> Declaration.t list
(** [declarations t] is [t]'s declarations in declaration order. *)

val declaration : t -> Mentat_llm.Provider.t -> Declaration.t option
(** [declaration t provider] is [provider]'s declaration, if declared. *)

val models : ?include_hidden:bool -> t -> Model.t list
(** [models t] is the model metadata declared by [t] in declaration order.

    If [include_hidden] is [false], the default, models for which
    {!Model.visible} is [false] are omitted. *)

val models_for :
  ?include_hidden:bool ->
  t ->
  Mentat_llm.Provider.t ->
  (Model.t list, Error.t) result
(** [models_for t provider] is the model metadata declared by [provider].

    If [include_hidden] is [false], the default, non-visible models are omitted.
    Errors with [Unknown_provider] if [provider] is not declared. *)

val find : t -> Selector.t -> (Model.t, Error.t) result
(** [find t selector] resolves [selector] against [t].

    Resolution consults declared models of every status — so hidden and
    deprecated models resolve for history and diagnostics — then the provider's
    dynamic-model rule. Resolution is not eligibility: a caller that persists or
    runs a model must still check {!Model.selectable}. Errors with
    [Unknown_provider] or [Unknown_model]. *)

val resolve : t -> string -> (Model.t, Error.t) result
(** [resolve t input] parses [input] as a selector and resolves it with {!find}.
    Errors with [Invalid_selector] if [input] does not parse. *)
