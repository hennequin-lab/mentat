(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Provider declarations.

    A declaration is the provider-author extension value: a
    {!Mentat_llm.Provider.t} namespace paired with display metadata,
    authentication methods, known models, an optional default model, and an
    optional dynamic-model rule. It names {e declarations and policy}, never a
    client, credential store, endpoint, or transport.

    Declarations are pure. They read no environment, open no store, run no OAuth
    flow, and build no client. {!resolve_credential} and {!resolve_dynamic}
    resolve snapshots and lookups into values without performing I/O; the
    effectful provider driver in [mentat.provider_runtime] consumes those
    values. *)

(** {1:errors Resolution errors} *)

module Credential_error = Credential_error
(** Canonical credential resolution errors. *)

(** {1:declarations Declarations} *)

type t
(** The type for a provider declaration. Declarations are immutable values; they
    imply no client, credential, endpoint, or transport. *)

val make :
  Mentat_llm.Provider.t ->
  ?display_name:string ->
  ?auth:Auth.t ->
  ?default_model:Model.t ->
  ?dynamic:(string -> Model.t option) ->
  ?dynamic_listed:(Listing.Model.t -> Model.t option) ->
  Model.t list ->
  t
(** [make id ?display_name ?auth ?default_model ?dynamic ?dynamic_listed
     models] is provider declaration [id].

    [auth] defaults to {!Auth.none}. [models] is preserved in declaration order.
    [default_model] is matched by its {!Mentat_llm.Model.t} identity and the
    declared list element is stored, so {!default_model} always returns a member
    of {!models}.

    [dynamic], when present, interprets provider-local model ids not declared in
    [models]: a provider whose model set is owned by an external system (local
    weight files, a model daemon) synthesizes a {!Model.t} for ids it will
    interpret and returns [None] otherwise. The function must be pure and must
    synthesize a model with the declaration's own provider and the requested id
    — {!resolve_dynamic} checks this; whether an id resolves to real weights is
    the driver's runtime concern.

    [dynamic_listed], when present, interprets server-listed models not
    declared in [models]: it receives the {!Listing.Model.t} a driver lowered
    from the server — including the server-assigned wire-protocol family a
    pure function of the id could not know — and synthesizes a {!Model.t} for
    entries it will interpret. The function must be pure and must synthesize a
    model with the declaration's own provider and the listed id —
    {!resolve_listed} checks this.

    Raises [Invalid_argument] if [display_name] is empty, if a model's provider
    is not [id], if two models share the same provider-local id or canonical
    {!Mentat_llm.Model.t}, or if [default_model] is supplied but is not declared
    in [models] or is not {!Model.selectable}. *)

val id : t -> Mentat_llm.Provider.t
(** [id t] is [t]'s provider namespace. *)

val display_name : t -> string option
(** [display_name t] is [t]'s display name, if any. *)

val auth : t -> Auth.t
(** [auth t] is [t]'s authentication declaration. *)

val default_model : t -> Model.t option
(** [default_model t] is [t]'s declared default model, if any. *)

val models : t -> Model.t list
(** [models t] is [t]'s known models in declaration order. *)

val model : t -> Mentat_llm.Model.t -> Model.t option
(** [model t llm] is [llm]'s catalog metadata if [llm] is declared by [t]. *)

val resolve_dynamic : t -> string -> Model.t option
(** [resolve_dynamic t id] runs [t]'s dynamic-model rule on provider-local model
    id [id].

    It is [None] when [t] declares no rule or the rule declines [id], and
    [Some model] when the rule synthesizes [model]. [id] is not checked against
    {!models}; {!Catalog.find} consults declared models first and reaches the
    dynamic rule only for undeclared ids. Performs no I/O.

    Raises [Invalid_argument] if the rule synthesizes a model whose provider is
    not [id t] or whose id is not [id] — the rule is trusted provider-author
    code, and an inconsistent synthesis is a bug in it, not a lookup failure. *)

val interprets_listings : t -> bool
(** [interprets_listings t] is [true] iff [t] declares a listed-model or
    dynamic rule — the providers for which a server listing can add models
    beyond {!models}. Callers use it to skip listing refreshes that could
    never resolve anything. *)

val resolve_listed : t -> Listing.Model.t -> Model.t option
(** [resolve_listed t listed] runs [t]'s listed-model rule on server-listed
    model [listed].

    It is [None] when no rule accepts [listed]. When [t] declares no
    [dynamic_listed] rule, the plain [dynamic] rule applied to
    [Listing.Model.id listed] is the fallback, so a provider whose ids alone
    determine interpretation needs only one rule. [listed] is not checked
    against {!models}; {!Model_readiness.of_catalog} and selector resolution
    consult declared models first and reach this rule only for undeclared ids.
    Performs no I/O.

    Raises [Invalid_argument] if the rule synthesizes a model whose provider is
    not [id t] or whose id is not [Listing.Model.id listed] — the rule is
    trusted provider-author code, and an inconsistent synthesis is a bug in it,
    not a lookup failure. *)

val resolve_credential :
  t ->
  process:Credential.t list ->
  environment:(string * string) list ->
  store:Credential.Store.t ->
  ?name:Credential.Name.t ->
  unit ->
  (Credential.t option, Credential_error.t) result
(** [resolve_credential t ~process ~environment ~store ?name ()] resolves the
    credential [t] should use, purely, from the supplied snapshots.

    Precedence, highest first:
    + the first [process] credential whose provider is [id t];
    + the first declared environment variable in [environment] with a non-empty
      value, in declaration order, decoded as its declared credential kind;
    + the store slot [store] holds for [id t] under [name] (defaulting to
      {!Credential.Name.default}).

    It is [Ok None] when no candidate resolves. Missing {e required} auth is not
    diagnosed here — it surfaces when the driver builds the client. A candidate
    whose kind is not in [Auth.accepted_kinds (auth t)] errors with
    [Unsupported_credential_kind] and never falls through to a lower-precedence
    secret. Empty environment values read as unset. A non-empty value that is
    not valid UTF-8 errors with [Invalid_credential_text] rather than raising or
    falling through. Performs no I/O. *)
