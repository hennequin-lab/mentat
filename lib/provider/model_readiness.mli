(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Effective model readiness for provider-facing queries.

    A readiness snapshot is the credential-free, serializable projection of a
    {!Catalog.t} and the matching {!Account.Discovery.t} batch. It retains the
    provider declaration facts needed by readers, static {!Model.t} metadata,
    account observations, and checked model entitlement while keeping three
    different questions separate:

    - {!Eligibility.t} says whether static model lifecycle permits selection;
    - {!Availability.t} says what a checked account reported about the model id;
    - {!Actionability.t} says whether the provider route can act on the model
      now under its declared authentication requirement.

    Construct snapshots with {!of_catalog}. The resulting value performs no I/O
    and contains no credential material. It is suitable for protocol query
    responses through {!jsont}. *)

(** {1:facts Effective facts} *)

module Availability : sig
  (** Checked model entitlement. This is the nominal form of the
      [`Available]/[`Unavailable]/[`Unknown] trichotomy
      {!Account.model_available} returns as a polymorphic variant, carried into
      the readiness projection. *)

  type t = private
    | Available
        (** The route's listing or checked account listed the model id. *)
    | Unavailable
        (** The route's listing or checked account did not list the model id.
        *)
    | Unknown
        (** No listing or checked model list exists, including after credential
            resolution failure. This is absence of evidence, not
            unavailability. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same availability. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. *)
end

module Eligibility : sig
  (** Static model-selection eligibility.

      Eligibility depends only on {!Model.status}; account and entitlement
      observations never change it. *)

  type t = private
    | Eligible  (** The model is statically selectable. *)
    | Ineligible of Model.status
        (** The exact static lifecycle status that prevents selection. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] express the same eligibility and
      lifecycle reason. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. *)
end

module Actionability : sig
  (** Effective provider-route actionability.

      Actionability deliberately excludes static {!Eligibility.t}. A caller
      deciding whether to admit selection checks both facts and can therefore
      distinguish a lifecycle restriction from a live provider restriction. *)

  type t =
    | Actionable
        (** The route is not known to block the model. Unknown entitlement is
            actionable: it is not treated as proof of availability. *)
    | Provider_blocked of Account.Discovery.t
        (** Exact discovery that blocks the route: either a provider-local
            credential resolution failure, or a disconnected known account for a
            declaration that requires authentication. *)
    | Model_unavailable
        (** The provider route is otherwise usable, but its checked model list
            does not contain the model id. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same actionability fact. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics without credential material. *)
end

module Entry : sig
  (** Effective readiness for one catalog model. *)

  type t
  (** The type for one model entry. Values retain the exact static {!Model.t};
      the remaining facts are derived from the containing route's declaration
      and discovery. *)

  val model : t -> Model.t
  (** [model t] is [t]'s exact static or dynamically resolved model metadata. *)

  val availability : t -> Availability.t
  (** [availability t] is the checked entitlement observation for
      {!Model.id}[(model t)]. *)

  val eligibility : t -> Eligibility.t
  (** [eligibility t] is [t]'s static lifecycle eligibility. *)

  val actionability : t -> Actionability.t
  (** [actionability t] is [t]'s effective provider-route actionability. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] retain the same model and effective
      facts. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. *)
end

module Route : sig
  (** One declared provider route and its effective models. *)

  type t
  (** The type for a provider route. Model order is the declaration's exact
      model order followed by accepted dynamic ids from the checked account. *)

  val provider : t -> Mentat_llm.Provider.t
  (** [provider t] is [t]'s provider namespace. *)

  val display_name : t -> string option
  (** [display_name t] is the declaration's display name, if any. *)

  val auth_required : t -> bool
  (** [auth_required t] is the declaration's exact {!Auth.required} value. *)

  val discovery : t -> Account.Discovery.t
  (** [discovery t] is the exact credential-free discovery for [provider t]. *)

  val listed : t -> string list option
  (** [listed t] is the ids of the server listing this route was projected
      with, in server order, or [None] when no listing was supplied. When
      present it is the route's availability authority. *)

  val models : t -> Entry.t list
  (** [models t] is [t]'s effective model entries.

      Every declared model is retained, including statically hidden or
      ineligible models. When the route has a listing, listed models not
      declared statically follow the declared models in server order, each
      synthesized by the declaration's pure {!Declaration.resolve_listed}
      resolver; otherwise checked ids from the account follow them through
      {!Declaration.resolve_dynamic}. A declined id is absent rather than
      synthesized by this module. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same declaration facts,
      discovery, and effective models. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics without credential material. *)
end

(** {1:snapshots Readiness snapshots} *)

type t
(** The type for one effective model-readiness snapshot. Routes are unique and
    retain catalog declaration order. *)

val of_catalog :
  ?listings:(Mentat_llm.Provider.t * Listing.t) list ->
  Catalog.t ->
  discoveries:Account.Discovery.t list ->
  t
(** [of_catalog ?listings catalog ~discoveries] projects [catalog], its account
    discovery batch, and the last-observed server listings into effective
    readiness.

    Output routes follow catalog declaration order regardless of discovery input
    order. Declared models retain declaration order. [listings] defaults to
    [[]]; a route with a listing takes it as the availability authority for
    declared models, and its listed-but-undeclared entries are offered to the
    declaration's pure {!Declaration.resolve_listed} resolver in server order,
    accepted models following the declared models. A route without a listing
    takes the account path: for a known account with a checked model list,
    undeclared ids are offered to the declaration's pure dynamic resolver in
    {!Account.models} order. Declared ids never reach either resolver.

    Raises [Invalid_argument] if [discoveries] does not contain exactly one
    discovery for every catalog provider, contains an unknown or duplicate
    provider, or otherwise contradicts its provider route, or if [listings]
    contains an unknown or duplicate provider. This denotes a composition bug:
    [mentat.provider_runtime]'s full discovery operation guarantees catalog
    coverage and cardinality. A dynamic resolver may also raise under its
    documented provider-author invariant. *)

val routes : t -> Route.t list
(** [routes t] is [t]'s provider routes in catalog declaration order. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] contain the same ordered readiness
    facts. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] for diagnostics without credential material. *)

val jsont : t Jsont.t
(** [jsont] maps readiness snapshots to strict JSON objects with one ordered
    ["providers"] array.

    The encoding retains exact model metadata, display names, authentication
    requirements, discoveries, and each route's listing ids. Availability,
    eligibility, and actionability are derived from those owner facts when
    decoded, preventing redundant wire fields from drifting. Dynamic resolver
    closures and credential material cannot appear in the encoding. Decoding
    rejects unknown members, duplicate providers or model ids, and provider
    inconsistencies. *)
