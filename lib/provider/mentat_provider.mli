(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Pure provider, model, and account declarations with model selection.

    This library is the declaration-and-policy half of provider support. It
    annotates [mentat.llm] model identities with catalog metadata, composes
    provider declarations into a catalog, resolves model selectors and
    credential snapshots into values, and selects the eligible main model for a
    run. Every value here is a pure function of its inputs.

    A provider declaration is this module's own {!type:t}: its constructor
    {!make}, its accessors, its nested {!Auth} methods, and its pure
    {!resolve_credential} and {!resolve_dynamic} resolvers are the library's
    top-level surface. {!Selector}, {!Model}, {!Credential}, {!Account},
    {!Catalog}, and {!Selection} are the supporting vocabularies.

    {b The two waists.}

    - {!Model.t} is the annotation waist. {!Mentat_llm.Model.t} owns canonical
      request identity — the [(provider, api, id)] triple — and this library
      never redefines it: a {!Model.t} only annotates that identity with display
      name, limits, modalities, capabilities, tiered pricing, and lifecycle
      status. {!Selection} returns a {!Model.t}; callers extract {!Model.llm} to
      build a [Mentat_llm.Request].
    - {!Credential.t} is the secret-bearing waist. It pairs an inert secret with
      its provider route and provenance, and crosses from the pure
      {!resolve_credential} precedence ladder to the effectful provider driver
      without exposing storage machinery. Everything that may carry secret
      material lives under {!Credential}; credential-free {!Account.t} readiness
      facts are a different namespace and never carry secret material.

    {b The flow.} The composition root authors {!type:t} values — namespace,
    auth methods, models, default, dynamic rule — and assembles them with
    {!Catalog.make}. Per run, {!Selection.main} resolves the main model from the
    catalog, a caller-owned provider-preference predicate, an optional preferred
    model already resolved with {!Catalog.find}, and the run's
    {!Selection.Requirement.t} — deterministically, with catalog order breaking
    every tie. Separately, the caller snapshots process credentials, the
    environment, and the credential {!Credential.Store.t}, and
    {!resolve_credential} resolves them into the one {!Credential.t} the
    provider driver should use.

    {b What this library deliberately does not do.} It performs no I/O and links
    no transport: no HTTP client, credential-store or keychain access, OAuth
    runtime, [Eio], or [Unix]. It constructs no provider client and registers no
    driver — the executable composes these declarations with the effectful
    provider drivers, so the engine links selection without pulling transports.
    It reads no configuration and records no configuration provenance: callers
    resolve their configured model through {!Catalog.find} and keep the origin
    of that choice in their own config vocabulary — file paths, environment
    variable names, and config layers never enter this library. *)

(** {1:vocabularies Supporting vocabularies} *)

module Selector = Selector
(** Parsed [provider/model] selectors — pure syntax shared by configuration,
    catalog lookup, and diagnostics. *)

module Model = Model
(** Catalog metadata annotating a [mentat.llm] model identity. *)

module Media_support = Media_support
(** Per-API, per-channel image-media encodability — the channel/API half of the
    vision gate. *)

module Credential = Credential
(** Secret-bearing credentials: secret payloads, routed candidates, and inert
    store snapshots. *)

module Account = Account
(** Credential-free account readiness facts. *)

module Catalog = Catalog
(** Validated declaration sets with pure provider and model lookup. *)

module Model_readiness = Model_readiness
(** Credential-free, serializable effective model readiness. *)

module Selection = Selection
(** Deterministic main-model selection. *)

(** {1:declarations Provider declarations}

    The provider declaration is this library's central value: a namespace, its
    authentication methods, its known models, an optional default, and an
    optional dynamic-model rule, with the pure credential and dynamic resolvers
    that read snapshots into values. *)

module Auth = Auth
(** Provider authentication declarations — environment credential sources and
    login methods, and the credential kinds they accept. *)

include module type of Declaration
(** @inline *)
