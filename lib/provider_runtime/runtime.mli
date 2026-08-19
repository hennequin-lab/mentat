(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The assembled runtime: the single registration list and the credential
    store.

    [create] assembles the built-in providers exactly once and fails closed on
    any declaration/driver drift, so constructing a value {e is} the coverage
    proof. Every driver operation and the catalog input derive from the one
    list, so catalog and driver cannot disagree past construction. *)

type t

val create : config_dir:Eio.Fs.dir_ty Eio.Path.t -> t
(** [create ~config_dir] assembles the built-in providers and binds the
    credential store to [config_dir/auth.json]. Performs no I/O. Raises
    [Invalid_argument] on built-in drift: a provider declared twice, or a
    declared provider-defined login protocol with no driver handler. *)

val catalog : t -> Mentat_provider.Catalog.t
(** [catalog t] is the validated built-in catalog, constructed once with the
    runtime's registration coverage check. *)

val registration_for : t -> Mentat_llm.Provider.t -> Driver.registration option
(** [registration_for t provider] is the registration serving [provider]. *)

val store : t -> Credential_store.t
(** [store t] is the bound credential store. *)

(** {1:listings Retained server listings}

    The runtime keeps the last good server listing per provider — in-memory,
    process-lifetime state scoped by the credential fingerprint and the
    endpoint overrides each observation ran under. Nothing here is ever
    persisted: a stale slot dies with the process, and a rotated credential or
    moved endpoint invalidates it on the next lookup. *)

val remember_listing :
  t ->
  provider:Mentat_llm.Provider.t ->
  fingerprint:string option ->
  base_url:string option ->
  auth_base_url:string option ->
  Mentat_provider.Listing.t option ->
  unit
(** [remember_listing t ~provider ~fingerprint ~base_url ~auth_base_url
     listing] retains [listing] for [provider] under the given scope. A [None]
    listing — a failed or listing-less observation — keeps the previously
    retained listing when its scope still matches and discards it otherwise. *)

val listing_for :
  t ->
  provider:Mentat_llm.Provider.t ->
  fingerprint:string option ->
  base_url:string option ->
  auth_base_url:string option ->
  Mentat_provider.Listing.t option
(** [listing_for t ~provider ~fingerprint ~base_url ~auth_base_url] is the
    retained listing for [provider], if one was observed under exactly this
    credential fingerprint and these endpoint overrides. *)
